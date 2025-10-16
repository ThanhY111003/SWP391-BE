package swp.project.swp391.serviceImp.order;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.Dealer;
import swp.project.swp391.entity.InstallmentPlan;
import swp.project.swp391.entity.Order;
import swp.project.swp391.entity.User;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.DealerRepository;
import swp.project.swp391.repository.InstallmentPlanRepository;
import swp.project.swp391.repository.OrderRepository;
import swp.project.swp391.response.order.OrderResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.order.PaymentService;

import java.math.BigDecimal;
import java.time.LocalDate;

@Service
@RequiredArgsConstructor
@Transactional
public class PaymentServiceImpl implements PaymentService {

    private final OrderRepository orderRepo;
    private final InstallmentPlanRepository planRepo;
    private final DealerRepository dealerRepo;
    private final RbacGuard guard;

    @Override
    public OrderResponse confirmInstallmentPayment(Long orderId, Integer installmentNumber, User currentUser) {
        guard.require(guard.has(currentUser, "order.update_payment"));

        // 🔹 1. Lấy đơn hàng
        Order order = orderRepo.findById(orderId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

        if (!Boolean.TRUE.equals(order.getIsInstallment())) {
            throw new BaseException(ErrorHandler.INVALID_INSTALLMENT_REQUEST);
        }

        // 🔹 2. Lấy kỳ trả góp theo installmentNumber
        InstallmentPlan plan = planRepo.findByOrderIdAndInstallmentNumber(orderId, installmentNumber)
                .orElseThrow(() -> new BaseException(ErrorHandler.NOT_FOUND_INSTALLMENT_PLAN));

        if (plan.getStatus() == InstallmentPlan.InstallmentStatus.PAID) {
            throw new BaseException(ErrorHandler.WASPAIDED_REQUEST);
        }

        // ✅ 3. Cập nhật thanh toán kỳ này
        plan.setPaidAmount(plan.getInstallmentAmount());
        plan.setPaidDate(LocalDate.now());
        plan.setStatus(InstallmentPlan.InstallmentStatus.PAID);
        planRepo.saveAndFlush(plan);

        // ✅ 4. Reload lại order để tính toán chính xác
        order = orderRepo.findById(orderId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

        // ✅ 5. Cập nhật công nợ dealer
        Dealer dealer = order.getBuyerDealer();
        if (dealer != null) {
            BigDecimal currentDebt = dealer.getCurrentDebt() != null
                    ? dealer.getCurrentDebt() : BigDecimal.ZERO;

            dealer.setCurrentDebt(currentDebt.subtract(plan.getInstallmentAmount()).max(BigDecimal.ZERO));

            BigDecimal creditLimit = dealer.getLevel().getCreditLimit();
            dealer.setAvailableCredit(
                    creditLimit.subtract(dealer.getCurrentDebt()).max(BigDecimal.ZERO)
            );

            dealerRepo.save(dealer);
        }

        // ✅ 6. Nếu sau kỳ này đã trả hết → chuyển trạng thái COMPLETED
        if (order.isFullyPaid()) {
            order.setStatus(Order.OrderStatus.COMPLETED);
            order.setFullPaymentDate(LocalDate.now());
        }

        orderRepo.save(order);

        return OrderResponse.fromEntity(order);
    }

    @Override
    @Transactional
    public OrderResponse cancelOrder(Long orderId, User currentUser) {
        // ✅ 1. Chỉ Admin hoặc người có quyền "order.cancel" mới được hủy
        guard.require(guard.has(currentUser, "order.cancel"));

        // ✅ 2. Lấy order
        Order order = orderRepo.findById(orderId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

        // ✅ 3. Kiểm tra trạng thái
        if (order.getStatus() == Order.OrderStatus.CANCELLED) {
            throw new BaseException(ErrorHandler.ORDER_ALREADY_CANCELLED);
        }
        if (order.getStatus() == Order.OrderStatus.COMPLETED) {
            throw new BaseException(ErrorHandler.ORDER_ALREADY_PROCESSED);
        }

        // ✅ 4. Nếu đơn đã CONFIRMED mà chưa giao xe thì hủy → hoàn lại credit cho dealer
        Dealer dealer = order.getBuyerDealer();
        if (dealer != null) {
            BigDecimal currentDebt = dealer.getCurrentDebt() != null
                    ? dealer.getCurrentDebt() : BigDecimal.ZERO;

            // Hoàn lại toàn bộ nợ của đơn này (vì hủy)
            dealer.setCurrentDebt(currentDebt.subtract(order.getTotalAmount()).max(BigDecimal.ZERO));

            BigDecimal creditLimit = dealer.getLevel().getCreditLimit();
            BigDecimal availableCredit = creditLimit.subtract(dealer.getCurrentDebt()).max(BigDecimal.ZERO);
            dealer.setAvailableCredit(availableCredit);

            dealerRepo.save(dealer);
        }

        // ✅ 5. Cập nhật trạng thái & ghi chú
        order.setStatus(Order.OrderStatus.CANCELLED);
        order.setUpdatedAt(java.time.LocalDateTime.now());
        order.setNotes("Admin hủy đơn vào ngày " + LocalDate.now());

        orderRepo.save(order);

        return OrderResponse.fromEntity(order);
    }

}
