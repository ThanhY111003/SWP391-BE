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

        // ✅ FIX 1: Load lại dealer từ DB với level
        Dealer dealer = dealerRepo.findById(dealerUser.getDealer().getId())
                .orElseThrow(() -> new BaseException(ErrorHandler.DEALER_NOT_FOUND));

        // ✅ FIX 2: Force initialize level ngay lập tức
        DealerLevel dealerLevel = dealer.getLevel();
        if (dealerLevel == null) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Dealer không có level hợp lệ");
        }

        // Validation

        // ❌ Cấm confirm khi đang báo lỗi
        if (order.getStatus() == Order.OrderStatus.PARTIALLY_DELIVERED) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Đơn đang báo cáo lỗi, không thể xác nhận đã nhận");
        }

        // ❌ Đơn đã bị huỷ
        if (order.getStatus() == Order.OrderStatus.CANCELLED) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Đơn đã bị huỷ, không thể xác nhận");
        }

        // ❌ Đơn đã hoàn tất trước đó
        if (order.getStatus() == Order.OrderStatus.COMPLETED ||
                order.getStatus() == Order.OrderStatus.INSTALLMENT_ACTIVE) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Đơn đã hoàn tất, không thể xác nhận lại");
        }

        // ❌ Chỉ cho phép confirm nếu SHIPPING hoặc DEFECT_REJECTED
        if (order.getStatus() != Order.OrderStatus.SHIPPING &&
                order.getStatus() != Order.OrderStatus.DEFECT_REJECTED) {

            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Đơn không ở trạng thái hợp lệ để xác nhận đã nhận");
        }


        if (!Objects.equals(order.getBuyerDealer().getId(), dealer.getId())) {
            throw new BaseException(ErrorHandler.FORBIDDEN,
                    "Không thể xác nhận đơn của đại lý khác");
        }

        VehicleInstance v = order.getAssignedVehicle();
        if (v == null) {
            throw new BaseException(ErrorHandler.VEHICLE_NOT_ASSIGNED,
                    "Đơn hàng chưa được gắn xe");
        }

        if (v.getStatus() == VehicleInstance.VehicleStatus.PARTIALLY_DELIVERED) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Xe đang sửa chữa, không thể xác nhận đã nhận");
        }


        // ============================================================
        // 1️⃣ NHẬP XE VỀ ĐẠI LÝ
        // ============================================================
        if (v.getStatus() == VehicleInstance.VehicleStatus.SHIPPING) {

            v.setCurrentDealer(dealer);
            v.setStatus(VehicleInstance.VehicleStatus.IN_STOCK);

            BigDecimal finalPrice = getVehiclePriceForDealer(
                    v.getVehicleModelColor().getVehicleModel(),
                    v.getVehicleModelColor(),
                    dealerLevel
            );

            v.setCurrentValue(finalPrice);

            vehicleRepo.save(v);

            // Tìm hoặc tạo inventory
            Inventory inv = inventoryRepo
                    .lockByDealerIdAndVehicleModelColorId(
                            dealer.getId(),
                            v.getVehicleModelColor().getId()
                    )
                    .orElseGet(() -> inventoryRepo.save(
                            Inventory.builder()
                                    .dealer(dealer)
                                    .vehicleModelColor(v.getVehicleModelColor())
                                    .availableQuantity(0)
                                    .reservedQuantity(0)
                                    .totalQuantity(0)
                                    .isActive(true)
                                    .build()
                    ));

            inv.setAvailableQuantity(inv.getAvailableQuantity() + 1);
            inv.setTotalQuantity(inv.getTotalQuantity() + 1);
            inventoryRepo.save(inv);
        }

        // ============================================================
        // 2️⃣ CẬP NHẬT TRẠNG THÁI ĐƠN
        // ============================================================
        if (order.getIsInstallment()) {
            order.setStatus(Order.OrderStatus.INSTALLMENT_ACTIVE);
        } else {
            order.setStatus(Order.OrderStatus.COMPLETED);
        }

        order.setUpdatedAt(LocalDateTime.now());
        orderRepository.save(order);

        // ============================================================
        // 3️⃣ TRẢ GÓP → ACTIVATE DEBT (công nợ)
        // ============================================================
        if (order.getIsInstallment()) {

            BigDecimal deposit = Optional.ofNullable(order.getDepositAmount())
                    .orElse(BigDecimal.ZERO);

            BigDecimal creditPart = order.getTotalAmount().subtract(deposit);
            if (creditPart.compareTo(BigDecimal.ZERO) < 0) {
                creditPart = BigDecimal.ZERO;
            }

            BigDecimal currentDebt = Optional.ofNullable(dealer.getCurrentDebt())
                    .orElse(BigDecimal.ZERO);

            dealer.setCurrentDebt(currentDebt.add(creditPart));

            // ✅ Dùng dealerLevel đã load
            BigDecimal creditLimit = dealerLevel.getCreditLimit();
            dealer.setAvailableCredit(
                    creditLimit.subtract(dealer.getCurrentDebt()).max(BigDecimal.ZERO)
            );

            dealerRepo.save(dealer);
        }

        return OrderResponse.fromEntity(order);
    }


    // ========================= CREATE ORDER =========================
    @Override
    @Transactional
    public OrderResponse createOrder(CreateOrderRequest request, Long userId) {

        log.info("=== START CREATE ORDER (NEW FLOW) ===");

        // 1️⃣ Xác thực user
        User user = userRepository.findById(userId)
                .orElseThrow(() -> new BaseException(ErrorHandler.USER_NOT_FOUND));

        guard.require(guard.has(user, "order.create"));

        Dealer dealer = user.getDealer();
        if (dealer == null || Boolean.FALSE.equals(dealer.getIsActive())) {
            throw new BaseException(ErrorHandler.DEALER_NOT_FOUND);
        }
        LocalDate startOfMonth = LocalDate.now().withDayOfMonth(1);
        LocalDate endOfMonth = startOfMonth.plusMonths(1).minusDays(1);

        long count = orderRepository.countOrdersByDealerAndMonth(
                dealer.getId(),
                List.of(Order.OrderStatus.COMPLETED, Order.OrderStatus.INSTALLMENT_ACTIVE),
                startOfMonth,
                endOfMonth
        );

        Integer maxAllowed = dealer.getLevel().getMaxOrderQuantity();

        if (maxAllowed != null && count >= maxAllowed) {
            throw new BaseException(
                    ErrorHandler.INVALID_REQUEST,
                    "Đại lý đã đạt giới hạn số lượng đơn trong tháng (" + maxAllowed + ")."
            );
        }


        // 2️⃣ Lấy modelColor mà dealer yêu cầu
        VehicleModelColor color = vehicleModelColorRepository.findById(request.getVehicleModelColorId())
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_MODEL_COLOR_NOT_FOUND));

        VehicleModel model = color.getVehicleModel();

        // 3️⃣ Tính giá (1 đơn = 1 xe)
        BigDecimal unitPrice = getVehiclePriceForDealer(model, color, dealer.getLevel());
        BigDecimal totalAmount = unitPrice;
        BigDecimal depositAmount = BigDecimal.ZERO; // chưa trả gì
        BigDecimal remainingAmount = totalAmount;

        boolean isInstallment = Boolean.TRUE.equals(request.getIsInstallment());

        // ================================================================
        // 4️⃣ TRẢ GÓP → kiểm tra + trừ credit ngay khi tạo đơn
        // ================================================================
        if (isInstallment) {

            validateInstallmentRequest(request.getInstallmentMonths(), dealer);

            // Tiền cọc khách phải trả trước khi duyệt đơn
            depositAmount = calculateDepositAmount(dealer, totalAmount);

            // Khoản tín dụng cần “giữ”
            remainingAmount = totalAmount.subtract(depositAmount);

            validateCreditLimit(dealer, remainingAmount);

        }

        // ================================================================
        // 5️⃣ Tạo Order (KHÔNG còn OrderDetail)
        // ================================================================
        Order order = Order.builder()
                .orderCode(generateOrderCode())
                .status(Order.OrderStatus.PENDING)
                .totalAmount(totalAmount)
                .depositAmount(depositAmount)
                .isInstallment(isInstallment)
                .orderDate(LocalDate.now())
                .notes(request.getNotes())
                .buyerDealer(dealer)
                .createdBy(user)
                .vehicleModelColor(color)   // lưu thẳng request của dealer
                .modelNameSnapshot(model.getName())
                .colorNameSnapshot(color.getColor().getColorName())
                .build();

        // ================================================================
        // 6️⃣ Nếu trả góp → tạo InstallmentPlans
        // ================================================================
        if (isInstallment) {
            createInstallmentPlans(order, request.getInstallmentMonths(), remainingAmount);
        }

        // ================================================================
        // 7️⃣ Lưu đơn
        // ================================================================
        orderRepository.save(order);

        log.info("Order created successfully: {}", order.getOrderCode());
        log.info("=== END CREATE ORDER (NEW FLOW) ===");

        return OrderResponse.fromEntity(order);
    }


    // ========================= VALIDATION =========================


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

    private void validateInstallmentRequest(Integer months, Dealer dealer) {

        // ❗ 1. Không có trả góp thì bỏ qua
        if (months == null || months == 0) {
            throw new IllegalArgumentException("Phải chọn số tháng trả góp hợp lệ (3, 6, 9 hoặc 12)");
        }

        // ❗ 2. Level không hỗ trợ trả góp
        Integer maxMonths = dealer.getLevel().getMaxInstallmentMonths();
        if (maxMonths == null || maxMonths == 0) {
            throw new IllegalStateException(
                    "Level của đại lý không hỗ trợ trả góp."
            );
        }

        // ❗ 3. Không vượt quá giới hạn level
        if (months > maxMonths) {
            throw new IllegalStateException(
                    String.format("Số tháng trả góp vượt quá giới hạn. Tối đa: %d tháng", maxMonths)
            );
        }

        // ❗ 4. Chỉ cho phép 3, 6, 9, 12 tháng
        if (months % 3 != 0) {
            throw new IllegalArgumentException(
                    "Kỳ hạn trả góp chỉ được phép là 3, 6, 9, hoặc 12 tháng."
            );
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
