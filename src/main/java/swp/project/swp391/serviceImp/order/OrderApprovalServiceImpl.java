package swp.project.swp391.serviceImp.order;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.stereotype.Service;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.*;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.*;
import swp.project.swp391.response.order.OrderApproveResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.order.OrderApprovalService;
import swp.project.swp391.util.VinEngineGenerator;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Service
@RequiredArgsConstructor
@Slf4j
public class OrderApprovalServiceImpl implements OrderApprovalService {

    private final OrderRepository orderRepo;
    private final OrderDetailRepository orderDetailRepo;
    private final VehicleInstanceRepository vehicleRepo;
    private final InventoryRepository inventoryRepo;
    private final DealerRepository dealerRepo;
    private final RbacGuard guard;

    private static final int GEN_ATTEMPTS = 30;

    @Override
    @Transactional
    public OrderApproveResponse approveOrder(Long orderId, User currentUser) {
        guard.require(guard.has(currentUser, "order.approve"));

        Order order = orderRepo.findById(orderId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

        if (order.getStatus() != Order.OrderStatus.PENDING) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Chỉ có thể approve đơn hàng ở trạng thái PENDING");
        }

        Dealer dealer = order.getBuyerDealer();
        if (dealer == null) throw new BaseException(ErrorHandler.DEALER_NOT_FOUND);

        // Cache tạm để tránh đụng nhau trong 1 lần approve
        Set<String> existingVins = new HashSet<>();
        Set<String> existingEngines = new HashSet<>();

        int created = 0;

        // Tạo VehicleInstance cho từng OrderDetail
        List<OrderDetail> details = orderDetailRepo.findByOrderId(orderId);
        for (OrderDetail d : details) {
            int qty = d.getQuantity();

            Inventory inv = inventoryRepo.lockByDealerIdAndVehicleModelColorId(dealer.getId(), d.getVehicleModelColor().getId())
                    .orElseGet(() -> {
                        Inventory i = Inventory.builder()
                                .dealer(dealer)
                                .vehicleModelColor(d.getVehicleModelColor())
                                .totalQuantity(0)
                                .reservedQuantity(0)
                                .availableQuantity(0)
                                .isActive(true)
                                .build();
                        return inventoryRepo.save(i);
                    });

            for (int i = 0; i < qty; i++) {
                String vin = generateVinUnique(existingVins, d.getVehicleModel());
                String engine = generateEngineUnique(existingEngines, d.getVehicleModel());

                if (vehicleRepo.existsByVin(vin)) {
                    vin = generateVinUnique(existingVins, d.getVehicleModel());
                    if (vehicleRepo.existsByVin(vin)) {
                        throw new BaseException(ErrorHandler.VIN_ALREADY_EXISTS);
                    }
                }
                if (vehicleRepo.existsByEngineNumber(engine)) {
                    engine = generateEngineUnique(existingEngines, d.getVehicleModel());
                    if (vehicleRepo.existsByEngineNumber(engine)) {
                        throw new BaseException(ErrorHandler.ENGINE_NUMBER_ALREADY_EXISTS);
                    }
                }

                VehicleInstance v = VehicleInstance.builder()
                        .vin(vin)
                        .engineNumber(engine)
                        .vehicleModel(d.getVehicleModel())
                        .vehicleModelColor(d.getVehicleModelColor())
                        .manufacturingDate(LocalDate.now())
                        .status(VehicleInstance.VehicleStatus.IN_STOCK)
                        .isActive(true)
                        .currentDealer(dealer)
                        .currentValue(d.getUnitPrice())
                        .build();

                vehicleRepo.save(v);
                created++;

                existingVins.add(vin);
                existingEngines.add(engine);

                inv.setTotalQuantity(inv.getTotalQuantity() + 1);
                inv.setAvailableQuantity(inv.getAvailableQuantity() + 1);
            }

            inventoryRepo.save(inv);
        }

        // Cập nhật status order
        order.setStatus(Order.OrderStatus.CONFIRMED);

        // ===== XỬ LÝ THANH TOÁN =====
        if (Boolean.FALSE.equals(order.getIsInstallment())) {
            // TRẢ THẲNG: Coi như đã thanh toán toàn bộ khi approve
            log.info("Order {} is FULL PAYMENT - marking as COMPLETED and paid", order.getOrderCode());

            order.setStatus(Order.OrderStatus.COMPLETED);
            order.setFullPaymentDate(LocalDate.now());

            dealerRepo.save(dealer);

        } else {
            // TRẢ GÓP: giữ CONFIRMED, tăng nợ = remaining
            log.info("Order {} is INSTALLMENT - keeping CONFIRMED status", order.getOrderCode());

            BigDecimal total = order.getTotalAmount();
            BigDecimal deposit = order.getDepositAmount() == null ? BigDecimal.ZERO : order.getDepositAmount();
            BigDecimal paid = order.getPaidAmount() == null ? BigDecimal.ZERO : order.getPaidAmount();

            BigDecimal incDebt = total.subtract(deposit.add(paid));
            if (incDebt.compareTo(BigDecimal.ZERO) < 0) incDebt = BigDecimal.ZERO;

            BigDecimal currentDebt = dealer.getCurrentDebt();
            dealer.setCurrentDebt(currentDebt.add(incDebt));
            dealerRepo.save(dealer);
        }

        orderRepo.save(order);

        log.info("Order {} approved successfully. Created {} vehicle instances",
                order.getOrderCode(), created);

        return new OrderApproveResponse(
                order.getId(),
                order.getOrderCode(),
                order.getStatus().name(),
                created
        );
    }

    // ========= Helpers for unique VIN/Engine =========

    private String generateVinUnique(Set<String> cache, VehicleModel model) {
        String wmi = pickWmiFromModel(model);
        String vdsSeed = model.getModelCode();
        char plant = 'A';

        for (int i = 0; i < GEN_ATTEMPTS; i++) {
            String vin = VinEngineGenerator.generateVin(wmi, vdsSeed, plant);
            if (!cache.contains(vin) && !vehicleRepo.existsByVin(vin)) {
                cache.add(vin);
                return vin;
            }
            plant = nextPlant(plant);
        }
        throw new BaseException(ErrorHandler.INTERNAL_ERROR, "Không thể sinh VIN unique sau nhiều lần thử.");
    }

    private String generateEngineUnique(Set<String> cache, VehicleModel model) {
        String prefix = safePrefix(model.getModelCode(), 4);
        int length = 13;

        for (int i = 0; i < GEN_ATTEMPTS; i++) {
            String eng = VinEngineGenerator.generateEngineNumber(prefix, length);
            if (!cache.contains(eng) && !vehicleRepo.existsByEngineNumber(eng)) {
                cache.add(eng);
                return eng;
            }
            prefix = mutatePrefix(prefix);
        }
        throw new BaseException(ErrorHandler.INTERNAL_ERROR, "Không thể sinh số máy sau nhiều lần thử.");
    }

    private String safePrefix(String s, int maxLen) {
        if (s == null) return "ENG";
        String p = s.replaceAll("[^A-Za-z0-9]", "").toUpperCase();
        if (p.isEmpty()) p = "ENG";
        return p.length() > maxLen ? p.substring(0, maxLen) : p;
    }

    private String mutatePrefix(String p) {
        String allowed = "ABCDEFGHJKLMNPRSTUVWXYZ0123456789";
        char last = allowed.charAt((int)(Math.random() * allowed.length()));
        if (p.length() == 0) return "E" + last;
        return p.substring(0, Math.max(0, p.length() - 1)) + last;
    }

    private String pickWmiFromModel(VehicleModel model) {
        String brand = model.getBrand();
        if (brand != null) {
            String w = brand.replaceAll("[^A-Za-z0-9]", "").toUpperCase();
            if (w.length() >= 3) return w.substring(0,3);
        }
        String code = model.getModelCode();
        if (code != null) {
            String w = code.replaceAll("[^A-Za-z0-9]", "").toUpperCase();
            if (w.length() >= 3) return w.substring(0,3);
        }
        return "VF8"; // fallback
    }

    private char nextPlant(char plant) {
        String allowed = "ABCDEFGHJKLMNPRSTUVWXYZ";
        int idx = allowed.indexOf(Character.toUpperCase(plant));
        return allowed.charAt((idx + 1 + allowed.length()) % allowed.length());
    }
}