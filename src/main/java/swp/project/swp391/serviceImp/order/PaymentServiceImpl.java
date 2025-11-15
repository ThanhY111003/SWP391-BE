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
import swp.project.swp391.request.order.ConfirmDepositRequest;
import swp.project.swp391.request.order.ManualPaymentRequest;
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
        if (order.getStatus() != Order.OrderStatus.INSTALLMENT_ACTIVE) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Chỉ có thể xác nhận thanh toán cho đơn hàng đang hoạt động trả góp (INSTALLMENT_ACTIVE)");
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
        guard.require(guard.has(currentUser, "order.cancel_EVM"));

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
        // ❌ Các trạng thái KHÔNG ĐƯỢC HỦY
        if (order.getStatus() == Order.OrderStatus.SHIPPING) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Không thể hủy đơn hàng đang giao xe");
        }

        if (order.getStatus() == Order.OrderStatus.INSTALLMENT_ACTIVE) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Không thể hủy đơn hàng đang trả góp");
        }

        if (order.getStatus() == Order.OrderStatus.PARTIALLY_DELIVERED) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Không thể hủy đơn có xe lỗi đang xử lý");
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

    @Override
    @Transactional
    public OrderResponse manualPayment(Long orderId, ManualPaymentRequest request, User currentUser) {

        guard.require(guard.has(currentUser, "order.manual_pay"));

        Order order = orderRepo.findById(orderId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

        if (Boolean.TRUE.equals(order.getIsInstallment())) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Đơn trả góp không dùng manualPaidAmount (phải trả theo InstallmentPlan)");
        }
        if (order.isFullyPaid()) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Đơn hàng đã thanh toán đủ 100%, không thể thanh toán thêm");
        }


        if (order.getStatus() != Order.OrderStatus.CONFIRMED &&
                order.getStatus() != Order.OrderStatus.PAID) {

            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Chỉ đơn CONFIRMED hoặc PAID mới được nhập tiền trả thẳng");
        }

        BigDecimal pay = request.getPaidAmount();
        if (pay == null || pay.compareTo(BigDecimal.ZERO) <= 0) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Số tiền thanh toán không hợp lệ");
        }

        // ====== NEW LOGIC: 3-LẦN THANH TOÁN ======
        int attempt = order.getManualPaymentAttempts() == null ? 0 : order.getManualPaymentAttempts();
        BigDecimal total = order.getTotalAmount();
        BigDecimal alreadyPaid = order.getManualPaidAmount() == null
                ? BigDecimal.ZERO
                : order.getManualPaidAmount();

        BigDecimal remaining = total.subtract(alreadyPaid);

        if (attempt >= 3) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Đã vượt quá số lần thanh toán tối đa (3 lần)");
        }

        // -------- LẦN 1 --------
        if (attempt == 0) {
            BigDecimal min = total.multiply(BigDecimal.valueOf(0.5)); // ≥ 50% tổng đơn

            if (pay.compareTo(min) < 0) {
                throw new BaseException(ErrorHandler.INVALID_REQUEST,
                        "Lần thanh toán đầu tiên phải >= 50% tổng giá trị đơn hàng");
            }
        }

        // -------- LẦN 2 --------
        if (attempt == 1) {
            BigDecimal min = remaining.multiply(BigDecimal.valueOf(0.5)); // ≥ 25% remaining

            if (pay.compareTo(min) < 0) {
                throw new BaseException(ErrorHandler.INVALID_REQUEST,
                        "Lần thanh toán thứ hai phải >= 50% số tiền còn lại");
            }
        }

        // -------- LẦN 3 --------
        if (attempt == 2) {
            // phải TRẢ ĐỦ remaining
            if (pay.compareTo(remaining) != 0) {
                throw new BaseException(ErrorHandler.INVALID_REQUEST,
                        "Lần thanh toán cuối cùng phải trả toàn bộ số tiền còn lại: " + remaining);
            }
        }

        // Check không vượt quá tổng đơn
        if (pay.compareTo(remaining) > 0) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Thanh toán vượt quá số tiền còn lại");
        }

        // Update tiền đã trả
        BigDecimal newTotalPaid = alreadyPaid.add(pay);
        order.setManualPaidAmount(newTotalPaid);

        // Tăng số lần thanh toán
        order.setManualPaymentAttempts(attempt + 1);

        // Nếu là lần đầu tiên và đủ 50% → chuyển từ CONFIRMED → PAID
        if (attempt == 0 && pay.compareTo(total.multiply(BigDecimal.valueOf(0.5))) >= 0) {
            order.setStatus(Order.OrderStatus.PAID);
        }

        // Nếu trả đủ 100% → cũng giữ trạng thái PAID
        if (newTotalPaid.compareTo(total) == 0) {
            order.setFullPaymentDate(LocalDate.now());
        }


        // Ghi chú tùy chọn
        if (request.getNotes() != null) {
            order.setPaymentNotes(request.getNotes());
        }

        orderRepo.save(order);

        return OrderResponse.fromEntity(order);
    }

    @Override
    @Transactional
    public OrderResponse confirmDeposit(Long orderId, ConfirmDepositRequest request, User currentUser) {

        guard.require(guard.has(currentUser, "order.deposit_confirm"));

        Order order = orderRepo.findById(orderId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

        if (!Boolean.TRUE.equals(order.getIsInstallment())) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Đơn không phải trả góp, không thể xác nhận tiền cọc");
        }

        if (order.getStatus() != Order.OrderStatus.CONFIRMED) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Chỉ xác nhận cọc sau khi đơn CONFIRMED");
        }

        if (order.getDepositAmount() == null) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Đơn trả góp không có depositAmount");
        }

        // Khi dealer đã trả cọc → coi như paidAmount = depositAmount
        // Logic đã nằm trong getPaidAmount()

        // Cập nhật trạng thái trả góp
        order.setStatus(Order.OrderStatus.PAID);

        // Ghi chú
        if (request.getNotes() != null) {
            order.setPaymentNotes(request.getNotes());
        }

        orderRepo.save(order);

        return OrderResponse.fromEntity(order);
    }

}
