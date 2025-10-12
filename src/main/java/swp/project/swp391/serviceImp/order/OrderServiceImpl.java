package swp.project.swp391.serviceImp.order;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import swp.project.swp391.entity.*;
import swp.project.swp391.repository.*;
import swp.project.swp391.request.order.CreateOrderRequest;
import swp.project.swp391.response.order.OrderResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.order.OrderService;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

@Service
@RequiredArgsConstructor
@Slf4j
public class OrderServiceImpl implements OrderService {

    private final OrderRepository orderRepository;
    private final DealerRepository dealerRepository;
    private final UserRepository userRepository;
    private final VehicleModelRepository vehicleModelRepository;
    private final VehicleModelColorRepository vehicleModelColorRepository;
    private final VehiclePriceRepository vehiclePriceRepository;
    private final RbacGuard guard;  // Sử dụng RbacGuard để kiểm tra quyền


    @Override
    @Transactional
    public OrderResponse createOrder(CreateOrderRequest request, Long userId) {
        // Kiểm tra quyền người dùng trước khi tiếp tục
        guard.require(guard.has(validateAndGetUser(userId), "order.create"));

        log.info("Creating order for dealer: {}, isInstallment: {}", request.getDealerId(), request.getIsInstallment());

        // 1. Validate và lấy thông tin cơ bản
        User createdByUser = validateAndGetUser(userId);
        Dealer dealer = validateAndGetDealer(request.getDealerId());

        // 2. Tính tổng tiền và số lượng xe
        BigDecimal totalAmount = BigDecimal.ZERO;
        int totalQuantity = 0;
        List<OrderDetail> orderDetails = new ArrayList<>();

        for (CreateOrderRequest.OrderDetailRequest detailReq : request.getOrderDetails()) {
            VehicleModel vehicleModel = validateAndGetVehicleModel(detailReq.getVehicleModelId());
            VehicleModelColor vehicleColor = validateAndGetVehicleColor(detailReq.getVehicleColorId(), vehicleModel);

            // Lấy giá theo level của dealer
            BigDecimal unitPrice = getVehiclePriceForDealer(vehicleModel, dealer.getLevel());
            BigDecimal detailTotal = unitPrice.multiply(BigDecimal.valueOf(detailReq.getQuantity()));

            totalAmount = totalAmount.add(detailTotal);
            totalQuantity += detailReq.getQuantity();

            OrderDetail detail = OrderDetail.builder()
                    .vehicleModel(vehicleModel)
                    .vehicleColor(vehicleColor)
                    .quantity(detailReq.getQuantity())
                    .unitPrice(unitPrice)
                    .totalPrice(detailTotal)
                    .build();
            orderDetails.add(detail);
        }

        // 3. Kiểm tra số lượng xe tối đa
        validateMaxOrderQuantity(dealer, totalQuantity);

        // 4. Kiểm tra credit limit
        validateCreditLimit(dealer, totalAmount, request.getIsInstallment(), request.getDepositAmount());

        // 5. Kiểm tra trả góp (nếu có)
        if (Boolean.TRUE.equals(request.getIsInstallment())) {
            validateInstallmentRequest(request, dealer, totalAmount);
        }

        // 6. Tạo đơn hàng
        Order order = Order.builder()
                .orderCode(generateOrderCode())
                .status(Order.OrderStatus.PENDING)
                .totalAmount(totalAmount)
                .isInstallment(request.getIsInstallment())
                .orderDate(LocalDate.now())
                .expectedDeliveryDate(request.getExpectedDeliveryDate())
                .notes(request.getNotes())
                .paymentNotes(request.getPaymentNotes())
                .buyerDealer(dealer)
                .createdBy(createdByUser)
                .build();

        // 7. Thêm order details
        for (OrderDetail detail : orderDetails) {
            detail.setOrder(order);
        }
        order.setOrderDetails(orderDetails);

        // 8. Tạo kế hoạch trả góp (nếu có)
        if (Boolean.TRUE.equals(request.getIsInstallment())) {
            createInstallmentPlans(order, request, totalAmount);
        }

        // 9. Cập nhật debt của dealer
        updateDealerDebt(dealer, totalAmount, request.getIsInstallment(), request.getDepositAmount());

        // 10. Lưu đơn hàng
        Order savedOrder = orderRepository.save(order);

        log.info("Order created successfully: {}", savedOrder.getOrderCode());

        return OrderResponse.fromEntity(savedOrder);
    }

    // ===== Private Helper Methods =====

    private User validateAndGetUser(Long userId) {
        return userRepository.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User không tồn tại với ID: " + userId));
    }

    private Dealer validateAndGetDealer(Long dealerId) {
        Dealer dealer = dealerRepository.findById(dealerId)
                .orElseThrow(() -> new IllegalArgumentException("Dealer không tồn tại với ID: " + dealerId));

        if (!Boolean.TRUE.equals(dealer.getIsActive())) {
            throw new IllegalStateException("Dealer đã bị vô hiệu hóa");
        }

        return dealer;
    }

    private VehicleModel validateAndGetVehicleModel(Long modelId) {
        VehicleModel model = vehicleModelRepository.findById(modelId)
                .orElseThrow(() -> new IllegalArgumentException("Vehicle Model không tồn tại với ID: " + modelId));

        if (!Boolean.TRUE.equals(model.getIsActive())) {
            throw new IllegalStateException("Vehicle Model đã bị vô hiệu hóa: " + model.getName());
        }

        return model;
    }

    private VehicleModelColor validateAndGetVehicleColor(Long colorId, VehicleModel vehicleModel) {
        VehicleModelColor color = vehicleModelColorRepository.findById(colorId)
                .orElseThrow(() -> new IllegalArgumentException("Vehicle Color không tồn tại với ID: " + colorId));

        if (!color.getVehicleModel().getId().equals(vehicleModel.getId())) {
            throw new IllegalArgumentException("Màu này không thuộc model xe đã chọn");
        }

        return color;
    }

    private BigDecimal getVehiclePriceForDealer(VehicleModel vehicleModel, DealerLevel dealerLevel) {
        return vehiclePriceRepository.findByVehicleModelAndDealerLevel(vehicleModel, dealerLevel)
                .map(VehiclePrice::getWholesalePrice)
                .orElseThrow(() -> new IllegalStateException(
                        "Không tìm thấy giá cho model: " + vehicleModel.getName() +
                                " và level: " + dealerLevel.getLevelName()
                ));
    }


    private void validateMaxOrderQuantity(Dealer dealer, int totalQuantity) {
        Integer maxOrderQuantity = dealer.getLevel().getMaxOrderQuantity();

        if (maxOrderQuantity != null && totalQuantity > maxOrderQuantity) {
            throw new IllegalStateException(
                    String.format("Số lượng xe vượt quá giới hạn. Tối đa: %d, Yêu cầu: %d",
                            maxOrderQuantity, totalQuantity)
            );
        }
    }

    private void validateCreditLimit(Dealer dealer, BigDecimal totalAmount,
                                     Boolean isInstallment, BigDecimal depositAmount) {
        BigDecimal creditLimit = dealer.getLevel().getCreditLimit();
        BigDecimal currentDebt = dealer.getCurrentDebt();
        BigDecimal availableCredit = creditLimit.subtract(currentDebt);

        BigDecimal requiredCredit;
        if (Boolean.TRUE.equals(isInstallment)) {
            BigDecimal deposit = depositAmount != null ? depositAmount : BigDecimal.ZERO;
            requiredCredit = totalAmount.subtract(deposit);
        } else {
            requiredCredit = totalAmount;
        }

        if (requiredCredit.compareTo(availableCredit) > 0) {
            throw new IllegalStateException(
                    String.format("Vượt quá hạn mức tín dụng. Có sẵn: %s, Cần: %s",
                            availableCredit, requiredCredit)
            );
        }
    }

    private void validateInstallmentRequest(CreateOrderRequest request, Dealer dealer, BigDecimal totalAmount) {
        if (request.getInstallmentMonths() == null) {
            throw new IllegalArgumentException("Phải chọn số tháng trả góp");
        }

        Integer maxInstallmentMonths = dealer.getLevel().getMaxInstallmentMonths();
        if (maxInstallmentMonths == null) {
            throw new IllegalStateException("Level của dealer không hỗ trợ trả góp");
        }

        if (request.getInstallmentMonths() > maxInstallmentMonths) {
            throw new IllegalStateException(
                    String.format("Số tháng trả góp vượt quá giới hạn. Tối đa: %d tháng", maxInstallmentMonths)
            );
        }

        if (request.getDepositAmount() == null || request.getDepositAmount().compareTo(BigDecimal.ZERO) <= 0) {
            throw new IllegalArgumentException("Phải đặt cọc khi trả góp");
        }

        BigDecimal maxDeposit = totalAmount.multiply(new BigDecimal("0.5"));
        if (request.getDepositAmount().compareTo(maxDeposit) > 0) {
            throw new IllegalArgumentException(
                    String.format("Số tiền đặt cọc không được vượt quá 50%% tổng đơn hàng (tối đa: %s)", maxDeposit)
            );
        }

        BigDecimal minDeposit = totalAmount.multiply(new BigDecimal("0.1"));
        if (request.getDepositAmount().compareTo(minDeposit) < 0) {
            throw new IllegalArgumentException(
                    String.format("Số tiền đặt cọc phải ít nhất 10%% tổng đơn hàng (tối thiểu: %s)", minDeposit)
            );
        }
    }

    private void createInstallmentPlans(Order order, CreateOrderRequest request, BigDecimal totalAmount) {
        BigDecimal remainingAmount = totalAmount.subtract(request.getDepositAmount());
        BigDecimal monthlyAmount = remainingAmount.divide(
                BigDecimal.valueOf(request.getInstallmentMonths()),
                2,
                RoundingMode.HALF_UP
        );

        List<InstallmentPlan> plans = new ArrayList<>();

        for (int i = 1; i <= request.getInstallmentMonths(); i++) {
            LocalDate dueDate = LocalDate.now().plusMonths(i);

            BigDecimal installmentAmount;
            if (i == request.getInstallmentMonths()) {
                BigDecimal totalPaid = monthlyAmount.multiply(BigDecimal.valueOf(i - 1));
                installmentAmount = remainingAmount.subtract(totalPaid);
            } else {
                installmentAmount = monthlyAmount;
            }

            InstallmentPlan plan = InstallmentPlan.builder()
                    .order(order)
                    .installmentNumber(i)
                    .installmentAmount(installmentAmount)
                    .dueDate(dueDate)
                    .paidAmount(BigDecimal.ZERO)
                    .status(InstallmentPlan.InstallmentStatus.PENDING)
                    .build();

            plans.add(plan);
        }

        order.setInstallmentPlans(plans);
        order.setDepositPaidDate(LocalDate.now());
    }

    private void updateDealerDebt(Dealer dealer, BigDecimal totalAmount,
                                  Boolean isInstallment, BigDecimal depositAmount) {
        BigDecimal newDebt;

        if (Boolean.TRUE.equals(isInstallment)) {
            BigDecimal deposit = depositAmount != null ? depositAmount : BigDecimal.ZERO;
            newDebt = totalAmount.subtract(deposit);
        } else {
            newDebt = totalAmount;
        }

        dealer.setCurrentDebt(dealer.getCurrentDebt().add(newDebt));
        dealerRepository.save(dealer);
    }

    private String generateOrderCode() {
        String prefix = "ORD";
        String timestamp = String.valueOf(System.currentTimeMillis());
        String random = UUID.randomUUID().toString().substring(0, 8).toUpperCase();
        return prefix + "-" + timestamp + "-" + random;
    }
}
