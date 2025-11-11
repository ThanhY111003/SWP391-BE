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
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.order.OrderService;

import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.*;

@Service
@RequiredArgsConstructor
@Slf4j
public class OrderServiceImpl implements OrderService {
    private final DealerRepository dealerRepo;
    private final OrderRepository orderRepository;
    private final DealerRepository dealerRepository;
    private final UserRepository userRepository;
    private final VehicleModelColorRepository vehicleModelColorRepository;
    private final VehiclePriceRepository vehiclePriceRepository;
    private final InventoryRepository inventoryRepo;
    private final DefectiveVehicleReportRepository reportRepo;
    private final VehicleInstanceRepository vehicleRepo;
    private final RbacGuard guard;

    // ========================= CANCEL ORDER =========================
    @Override
    @Transactional
    public void cancelOrder(Long orderId) {
        User current = guard.me();
        guard.require(guard.has(current, "order.cancel"));

        Order order = orderRepository.findById(orderId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

        if (!Objects.equals(order.getBuyerDealer().getId(), current.getDealer().getId())) {
            throw new BaseException(ErrorHandler.FORBIDDEN);
        }

        if (order.getStatus() != Order.OrderStatus.PENDING) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Chỉ có thể huỷ đơn hàng đang chờ duyệt (PENDING).");
        }

        order.setStatus(Order.OrderStatus.CANCELLED);
        order.setUpdatedAt(LocalDateTime.now());
        orderRepository.save(order);
    }

    @Override
    @Transactional
    public OrderResponse dealerConfirmReceived(Long orderId, User dealerUser) {
        guard.require(guard.has(dealerUser, "order.receive"));

        Order order = orderRepository.findById(orderId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

        if (order.getStatus() != Order.OrderStatus.SHIPPING
                && order.getStatus() != Order.OrderStatus.PARTIALLY_DELIVERED) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Chỉ đơn SHIPPING hoặc PARTIALLY_DELIVERED mới có thể xác nhận đã nhận");
        }

        Dealer dealer = order.getBuyerDealer();
        if (!Objects.equals(dealer.getId(), dealerUser.getDealer().getId()))
            throw new BaseException(ErrorHandler.FORBIDDEN, "Không thể xác nhận đơn của đại lý khác");

        List<VehicleInstance> vehicles = vehicleRepo.findByOrderId(orderId);
        int receivedCount = 0;
        int defectiveCount = 0;

        for (VehicleInstance v : vehicles) {
            try {
                // ✅ Kiểm tra xe có đang trong quá trình xử lý lỗi không
                boolean hasAnyDefectReport = reportRepo.existsByVehicleInstanceId(v.getId());
                boolean isRepairing = v.getStatus() == VehicleInstance.VehicleStatus.REPAIRING;

                // ❌ BỎ: boolean isRepairCompleted = reportRepo.existsByVehicleInstanceIdAndIsRepairCompletedTrue(v.getId());

                // ✅ Nếu xe có báo cáo lỗi (dù đã sửa xong hay chưa) → BỎ QUA, để xử lý ở confirmRepairedVehicle()
                if (hasAnyDefectReport || isRepairing) {
                    defectiveCount++;
                    log.info("⚠️ Xe {} có báo cáo lỗi, bỏ qua trong dealerConfirmReceived", v.getVin());
                    continue;
                }

                // ✅ Chỉ xử lý xe KHÔNG có báo cáo lỗi
                if (v.getStatus() == VehicleInstance.VehicleStatus.SHIPPING) {
                    v.setCurrentDealer(dealer);
                    v.setStatus(VehicleInstance.VehicleStatus.IN_STOCK);
                    vehicleRepo.save(v);

                    Inventory inv = inventoryRepo.lockByDealerIdAndVehicleModelColorId(
                            dealer.getId(), v.getVehicleModelColor().getId()
                    ).orElseGet(() -> {
                        Inventory newInv = Inventory.builder()
                                .dealer(dealer)
                                .vehicleModelColor(v.getVehicleModelColor())
                                .availableQuantity(0)
                                .reservedQuantity(0)
                                .totalQuantity(0)
                                .isActive(true)
                                .build();
                        return inventoryRepo.save(newInv);
                    });

                    inv.setAvailableQuantity(inv.getAvailableQuantity() + 1);
                    inv.setTotalQuantity(inv.getTotalQuantity() + 1);
                    inventoryRepo.save(inv);

                    receivedCount++;
                    log.info("✅ Đã nhập kho xe {}", v.getVin());
                }
            } catch (Exception e) {
                log.error("❌ Lỗi khi xử lý xe VIN {}: {}", v.getVin(), e.getMessage());
            }
        }

        // ✅ Cập nhật trạng thái đơn hàng
        if (defectiveCount > 0) {
            order.setStatus(Order.OrderStatus.PARTIALLY_DELIVERED);
        } else if (Boolean.TRUE.equals(order.getIsInstallment())) {
            order.setStatus(Order.OrderStatus.INSTALLMENT_ACTIVE);
        } else {
            order.setStatus(Order.OrderStatus.COMPLETED);
        }

        order.setUpdatedAt(LocalDateTime.now());
        orderRepository.save(order);

        // ✅ Quản lý công nợ
        if (Boolean.TRUE.equals(order.getIsInstallment())) {
            BigDecimal total = order.getTotalAmount();
            BigDecimal deposit = Optional.ofNullable(order.getDepositAmount()).orElse(BigDecimal.ZERO);

            BigDecimal incDebt = total.subtract(deposit);
            if (incDebt.compareTo(BigDecimal.ZERO) < 0) incDebt = BigDecimal.ZERO;

            BigDecimal currentDebt = Optional.ofNullable(dealer.getCurrentDebt()).orElse(BigDecimal.ZERO);
            dealer.setCurrentDebt(currentDebt.add(incDebt));

            BigDecimal creditLimit = dealer.getLevel().getCreditLimit();
            dealer.setAvailableCredit(creditLimit.subtract(dealer.getCurrentDebt()).max(BigDecimal.ZERO));

            dealerRepo.save(dealer);

            log.info("💰 Activated debt for dealer {}: +{} (Total debt now = {})",
                    dealer.getName(), incDebt, dealer.getCurrentDebt());
        }

        log.info("Dealer {} confirmed receipt: {} received, {} defective (orderId={})",
                dealer.getId(), receivedCount, defectiveCount, orderId);

        return OrderResponse.fromEntity(order);
    }

    // ========================= CREATE ORDER =========================
    @Override
    @Transactional
    public OrderResponse createOrder(CreateOrderRequest request, Long userId) {
        log.info("=== START CREATE ORDER ===");

        User createdByUser = validateAndGetUser(userId);

        guard.require(guard.has(createdByUser, "order.create"));

        if (Boolean.FALSE.equals(request.getIsInstallment()) && request.getInstallmentMonths() != 0) {
            throw new IllegalArgumentException("Số tháng trả góp phải là 0 khi không sử dụng trả góp.");
        }
        // 1️⃣ Lấy thông tin người dùng & đại lý
        Dealer dealer = validateAndGetDealerFromUser(createdByUser);

        log.info("Creating order for dealer: {} ({}), isInstallment: {}",
                dealer.getName(), dealer.getCode(), request.getIsInstallment());

        // 2️⃣ Xác minh & tính toán chi tiết đơn hàng
        List<OrderDetail> orderDetails = new ArrayList<>();
        BigDecimal totalAmount = BigDecimal.ZERO;
        int totalQuantity = 0;

        for (CreateOrderRequest.OrderDetailRequest detailReq : request.getOrderDetails()) {
            VehicleModelColor vehicleModelColor = validateAndGetVehicleModelColor(detailReq.getVehicleModelColorId());
            VehicleModel vehicleModel = vehicleModelColor.getVehicleModel();

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
                    .status(OrderDetail.OrderDetailStatus.PENDING) // ✅ trạng thái chi tiết mặc định
                    .build();
            orderDetails.add(detail);
        }

        log.info("Total amount: {}, Total quantity: {}", totalAmount, totalQuantity);

        // 3️⃣ Kiểm tra quy tắc nghiệp vụ
        validateMaxOrderQuantity(dealer, totalQuantity);

        // 4️⃣ Tính tiền cọc (nếu trả góp)
        BigDecimal depositAmount;
        BigDecimal remainingAmount;

        if (Boolean.TRUE.equals(request.getIsInstallment())) {
            validateInstallmentRequest(request, dealer);
            depositAmount = calculateDepositAmount(dealer, totalAmount);
            remainingAmount = totalAmount.subtract(depositAmount);
            log.info("Installment mode: deposit={}, remaining={}", depositAmount, remainingAmount);
        } else {
            remainingAmount = totalAmount;
            depositAmount = BigDecimal.ZERO;
        }

        // 5️⃣ Kiểm tra hạn mức tín dụng
        validateCreditLimit(dealer, remainingAmount);

        // 6️⃣ Tạo đơn hàng
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

        for (OrderDetail detail : orderDetails) {
            detail.setOrder(order);
        }
        order.setOrderDetails(new HashSet<>(orderDetails));

        // 7️⃣ Tạo kế hoạch trả góp (nếu có)
        if (Boolean.TRUE.equals(request.getIsInstallment())) {
            createInstallmentPlans(order, request.getInstallmentMonths(), remainingAmount);
        }

        // 8️⃣ Cập nhật nợ dealer nếu đơn được duyệt/hoàn tất
        if (order.getStatus() == Order.OrderStatus.CONFIRMED || order.getStatus() == Order.OrderStatus.COMPLETED) {
            updateDealerDebt(dealer, remainingAmount);
        }

        // 9️⃣ Lưu đơn hàng
        Order savedOrder = orderRepository.save(order);

        log.info("Order created successfully: {}", savedOrder.getOrderCode());
        log.info("=== END CREATE ORDER ===");

        return OrderResponse.fromEntity(savedOrder);
    }

    // ========================= VALIDATION =========================

    private User validateAndGetUser(Long userId) {
        return userRepository.findById(userId)
                .orElseThrow(() -> new IllegalArgumentException("User không tồn tại với ID: " + userId));
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

    private VehicleModelColor validateAndGetVehicleModelColor(Long id) {
        return vehicleModelColorRepository.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_MODEL_COLOR_NOT_FOUND));
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
        Integer months = request.getInstallmentMonths();
        if (months == null || months < 1) {
            throw new IllegalArgumentException("Phải chọn số tháng trả góp hợp lệ");
        }

        Integer maxInstallmentMonths = dealer.getLevel().getMaxInstallmentMonths();
        if (maxInstallmentMonths == null || maxInstallmentMonths == 0) {
            throw new IllegalStateException("Level của dealer không hỗ trợ trả góp");
        }

        if (months > maxInstallmentMonths) {
            throw new IllegalStateException(
                    String.format("Số tháng trả góp vượt quá giới hạn. Tối đa: %d tháng", maxInstallmentMonths)
            );
        }

        if (months % 3 != 0) {
            throw new IllegalStateException("Kỳ hạn trả góp chỉ được phép là 3, 6, 9 hoặc 12 tháng.");
        }
    }

    // ========================= CALCULATIONS =========================

    private BigDecimal getVehiclePriceForDealer(
            VehicleModel vehicleModel,
            VehicleModelColor vehicleColor,
            DealerLevel dealerLevel) {

        BigDecimal finalPrice;

        Optional<VehiclePrice> vehiclePriceOpt = vehiclePriceRepository
                .findActiveByVehicleModelColorAndDealerLevel(vehicleColor, dealerLevel, LocalDate.now());

        if (vehiclePriceOpt.isPresent()) {
            VehiclePrice vp = vehiclePriceOpt.get();
            finalPrice = vp.getWholesalePrice();

            log.info("[PRICE] Using VehiclePrice for {} - {} (dealer level {}): {}",
                    vehicleModel.getName(),
                    vehicleColor.getColor().getColorName(),
                    dealerLevel.getLevelName(),
                    finalPrice);
        } else {
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

            if (discount.compareTo(BigDecimal.ONE) > 0) {
                discount = discount.divide(BigDecimal.valueOf(100), 4, RoundingMode.HALF_UP);
                log.warn("[PRICE] Auto-converted discountRate > 1 to {} for level {}", discount, dealerLevel.getLevelName());
            }

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
