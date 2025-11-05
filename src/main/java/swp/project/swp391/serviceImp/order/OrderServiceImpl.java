package swp.project.swp391.serviceImp.order;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.*;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.*;
import swp.project.swp391.request.order.CreateOrderRequest;
import swp.project.swp391.response.order.OrderResponse;
import swp.project.swp391.service.order.OrderService;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.*;

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

    @Override
    @Transactional
    public OrderResponse createOrder(CreateOrderRequest request, Long userId) {
        log.info("=== START CREATE ORDER ===");

        if (Boolean.FALSE.equals(request.getIsInstallment()) && request.getInstallmentMonths() != 0) {
            throw new IllegalArgumentException("Số tháng trả góp phải là 0 khi không sử dụng trả góp.");
        }
        // 1. XÁC MINH USER VÀ LẤY DEALER TỪ USER
        User createdByUser = validateAndGetUser(userId);
        Dealer dealer = validateAndGetDealerFromUser(createdByUser);

        log.info("Creating order for dealer: {} ({}), isInstallment: {}",
                dealer.getName(), dealer.getCode(), request.getIsInstallment());

        // 2. XÁC MINH VÀ TÍNH TOÁN CHI TIẾT ĐƠN HÀNG
        List<OrderDetail> orderDetails = new ArrayList<>();
        BigDecimal totalAmount = BigDecimal.ZERO;
        int totalQuantity = 0;

        for (CreateOrderRequest.OrderDetailRequest detailReq : request.getOrderDetails()) {
            VehicleModelColor vehicleModelColor = validateAndGetVehicleModelColor(detailReq.getVehicleModelColorId());
            VehicleModel vehicleModel = vehicleModelColor.getVehicleModel(); // Lấy từ quan hệ

            // Lấy giá với chiết khấu
            BigDecimal unitPrice = getVehiclePriceForDealer(vehicleModel, vehicleModelColor, dealer.getLevel());
            BigDecimal detailTotal = unitPrice.multiply(BigDecimal.valueOf(detailReq.getQuantity()));

            totalAmount = totalAmount.add(detailTotal);
            totalQuantity += detailReq.getQuantity();

            OrderDetail detail = OrderDetail.builder()
                    .vehicleModel(vehicleModel)
                    .vehicleModelColor(vehicleModelColor)
                    .quantity(detailReq.getQuantity())
                    .unitPrice(unitPrice)
                    .totalPrice(detailTotal)
                    .build();
            orderDetails.add(detail);
        }
        log.info("Total amount: {}, Total quantity: {}", totalAmount, totalQuantity);

        // 3. KIỂM TRA CÁC QUY TẮC KINH DOANH
        validateMaxOrderQuantity(dealer, totalQuantity);

        // 4. TÍNH TIỀN CỌC (nếu trả góp)
        BigDecimal depositAmount;
        BigDecimal remainingAmount;  // Khởi tạo remainingAmount là totalAmount

        if (Boolean.TRUE.equals(request.getIsInstallment())) {
            // Nếu là trả góp, tính cọc và tiền còn lại
            validateInstallmentRequest(request, dealer);
            depositAmount = calculateDepositAmount(dealer, totalAmount);  // Tính số tiền cọc
            remainingAmount = totalAmount.subtract(depositAmount);  // Trừ cọc để có số tiền còn lại
            log.info("Installment mode: deposit={}, remaining={}", depositAmount, remainingAmount);
        } else {
            // Nếu không trả góp, không tính cọc, tiền còn lại là tổng số tiền
            remainingAmount = totalAmount;  // Không có cọc, số tiền còn lại bằng tổng số tiền
            depositAmount = BigDecimal.ZERO; // Không cần cọc
        }



        // 5. KIỂM TRA HẠN MỨC TÍN DỤNG
        validateCreditLimit(dealer, remainingAmount);

        // 6. TẠO ĐƠN HÀNG
        Order order = Order.builder()
                .orderCode(generateOrderCode())
                .status(Order.OrderStatus.PENDING)
                .totalAmount(totalAmount)
                .depositAmount(depositAmount)
                .isInstallment(request.getIsInstallment())
                .orderDate(LocalDate.now())
                .notes(request.getNotes())
                .buyerDealer(dealer)
                .createdBy(createdByUser)
                .build();

        // 7. GẮN CHI TIẾT ĐƠN HÀNG
        for (OrderDetail detail : orderDetails) {
            detail.setOrder(order);
        }
        order.setOrderDetails(new HashSet<>(orderDetails));


        // 8. TẠO KẾ HOẠCH TRẢ GÓP (nếu có)
        if (Boolean.TRUE.equals(request.getIsInstallment())) {
            createInstallmentPlans(order, request.getInstallmentMonths(), remainingAmount);
        }

        // 9. CẬP NHẬT NỢ CỦA DEALER (chỉ cập nhật khi đơn hàng đã xác nhận hoặc đã thanh toán một phần)
        if (order.getStatus() == Order.OrderStatus.CONFIRMED || order.getStatus() == Order.OrderStatus.COMPLETED) {
            updateDealerDebt(dealer, remainingAmount);  // Chỉ cập nhật khi đơn hàng đã xác nhận hoặc đã thanh toán
        }


        // 10. LƯU ĐƠN HÀNG
        Order savedOrder = orderRepository.save(order);

        log.info("Order created successfully: {}", savedOrder.getOrderCode());
        log.info("=== END CREATE ORDER ===");

        return OrderResponse.fromEntity(savedOrder);
    }

    // ===== VALIDATE METHODS =====

    private User validateAndGetUser(Long userId) {
        return userRepository.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User không tồn tại với ID: " + userId));
    }

    private VehicleModelColor validateAndGetVehicleModelColor(Long id) {
        return vehicleModelColorRepository.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_MODEL_COLOR_NOT_FOUND));
    }


    private Dealer validateAndGetDealerFromUser(User user) {
        Dealer dealer = user.getDealer();
        if (dealer == null) {
            throw new IllegalStateException("User không có dealer liên kết.");
        }
        if (!Boolean.TRUE.equals(dealer.getIsActive())) {
            throw new IllegalStateException("Dealer đã bị vô hiệu hóa.");
        }
        return dealer;
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

    private void validateCreditLimit(Dealer dealer, BigDecimal requiredCredit) {
        BigDecimal creditLimit = dealer.getLevel().getCreditLimit();
        BigDecimal currentDebt = dealer.getCurrentDebt();
        BigDecimal availableCredit = creditLimit.subtract(currentDebt);

        if (requiredCredit.compareTo(availableCredit) > 0) {
            throw new IllegalStateException(
                    String.format("Vượt quá hạn mức tín dụng. Có sẵn: %s, Cần: %s",
                            availableCredit, requiredCredit)
            );
        }
    }

    private void validateInstallmentRequest(CreateOrderRequest request, Dealer dealer) {
        if (request.getInstallmentMonths() == null || request.getInstallmentMonths() < 1) {
            throw new IllegalArgumentException("Phải chọn số tháng trả góp hợp lệ");
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
    }

    // ===== CALCULATION METHODS =====
    private BigDecimal getVehiclePriceForDealer(
            VehicleModel vehicleModel,
            VehicleModelColor vehicleColor,
            DealerLevel dealerLevel) {

        BigDecimal finalPrice;

        // 1️⃣ Tìm giá theo VehicleModelColor + DealerLevel (bảng giá)
        Optional<VehiclePrice> vehiclePriceOpt = vehiclePriceRepository
                .findActiveByVehicleModelColorAndDealerLevel(vehicleColor, dealerLevel, LocalDate.now());

        if (vehiclePriceOpt.isPresent()) {
            // ✅ Có bảng giá: dùng giá wholesalePrice, KHÔNG áp discount
            VehiclePrice vp = vehiclePriceOpt.get();
            finalPrice = vp.getWholesalePrice();

            log.info("[PRICE] Using VehiclePrice for {} - {} (dealer level {}): {}",
                    vehicleModel.getName(),
                    vehicleColor.getColor().getColorName(),
                    dealerLevel.getLevelName(),
                    finalPrice);

        } else {
            // ❌ Không có bảng giá: fallback sang giá hãng + điều chỉnh màu + discountRate
            BigDecimal modelPrice = vehicleModel.getManufacturerPrice();
            if (modelPrice == null) {
                throw new IllegalStateException("Không tìm thấy giá gốc cho model: " + vehicleModel.getName());
            }

            BigDecimal priceAdjustment = vehicleColor.getPriceAdjustment() != null
                    ? vehicleColor.getPriceAdjustment()
                    : BigDecimal.ZERO;

            BigDecimal basePrice = modelPrice.add(priceAdjustment);
            BigDecimal discount = dealerLevel.getDiscountRate() != null
                    ? dealerLevel.getDiscountRate()
                    : BigDecimal.ZERO;

            // Chuyển discount về dạng tỷ lệ (nếu > 1)
            if (discount.compareTo(BigDecimal.ONE) > 0) {
                discount = discount.divide(BigDecimal.valueOf(100), 4, RoundingMode.HALF_UP);
                log.warn("[PRICE] Auto-converted discountRate > 1 to {} for level {}", discount, dealerLevel.getLevelName());
            }

            // Kiểm tra hợp lệ
            if (discount.compareTo(BigDecimal.ZERO) < 0 || discount.compareTo(BigDecimal.ONE) > 0) {
                throw new IllegalStateException("Discount rate không hợp lệ: " + discount);
            }

            finalPrice = basePrice.subtract(basePrice.multiply(discount)).setScale(2, RoundingMode.HALF_UP);

            log.info("[PRICE] Fallback pricing → model={}, adj={}, discount={}%, final={}",
                    modelPrice, priceAdjustment, discount.multiply(BigDecimal.valueOf(100)), finalPrice);
        }

        return finalPrice.setScale(2, RoundingMode.HALF_UP);
    }


    private BigDecimal calculateDepositAmount(Dealer dealer, BigDecimal totalAmount) {
        BigDecimal depositRate = dealer.getLevel().getDepositRate();

        if (depositRate == null) {
            throw new IllegalStateException("Deposit rate chưa được cấu hình cho level: " + dealer.getLevel().getLevelName());
        }

        // Auto-convert nếu depositRate > 1
        if (depositRate.compareTo(BigDecimal.ONE) > 0) {
            depositRate = depositRate.divide(BigDecimal.valueOf(100), 4, RoundingMode.HALF_UP);
            log.warn("Auto-converted deposit rate from {} to {}", dealer.getLevel().getDepositRate(), depositRate);
        }

        if (depositRate.compareTo(BigDecimal.ZERO) <= 0 || depositRate.compareTo(BigDecimal.ONE) > 0) {
            throw new IllegalStateException("Deposit rate không hợp lệ: " + depositRate);
        }

        return totalAmount.multiply(depositRate).setScale(2, RoundingMode.HALF_UP);
    }

    private void createInstallmentPlans(Order order, Integer installmentMonths, BigDecimal remainingAmount) {
        BigDecimal monthlyAmount = remainingAmount.divide(
                BigDecimal.valueOf(installmentMonths),
                2,
                RoundingMode.HALF_UP
        );

        List<InstallmentPlan> plans = new ArrayList<>();
        BigDecimal totalAllocated = BigDecimal.ZERO;

        for (int i = 1; i <= installmentMonths; i++) {
            LocalDate dueDate = LocalDate.now().plusMonths(i);

            BigDecimal installmentAmount;
            if (i == installmentMonths) {
                // Kỳ cuối: lấy phần còn lại để tránh lệch do làm tròn
                installmentAmount = remainingAmount.subtract(totalAllocated);
            } else {
                installmentAmount = monthlyAmount;
                totalAllocated = totalAllocated.add(monthlyAmount);
            }

            InstallmentPlan plan = InstallmentPlan.builder()
                    .order(order)
                    .installmentNumber(i)
                    .installmentAmount(installmentAmount)
                    .dueDate(dueDate)
                    .status(InstallmentPlan.InstallmentStatus.PENDING)
                    .build();

            plans.add(plan);
        }

        order.setInstallmentPlans(new HashSet<>(plans));

    }

    private void updateDealerDebt(Dealer dealer, BigDecimal additionalDebt) {
        dealer.setCurrentDebt(dealer.getCurrentDebt().add(additionalDebt));
        dealerRepository.save(dealer);

        log.info("Updated dealer debt: dealerId={}, newDebt={}", dealer.getId(), dealer.getCurrentDebt());
    }

    private String generateOrderCode() {
        String prefix = "ORD";
        String timestamp = String.valueOf(System.currentTimeMillis());
        String random = UUID.randomUUID().toString().substring(0, 8).toUpperCase();
        return prefix + "-" + timestamp + "-" + random;
    }
}