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
import swp.project.swp391.response.order.OrderResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.order.OrderApprovalService;
import swp.project.swp391.util.VinEngineGenerator;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.*;

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

        // ===== LẤY VÀ KIỂM TRA ĐƠN HÀNG =====
        Order order = orderRepo.findById(orderId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

        if (order.getStatus() != Order.OrderStatus.PENDING) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Chỉ có thể duyệt đơn hàng ở trạng thái PENDING");
        }

        Dealer dealer = order.getBuyerDealer();
        if (dealer == null) {
            throw new BaseException(ErrorHandler.DEALER_NOT_FOUND);
        }

        Set<String> existingVins = new HashSet<>();
        Set<String> existingEngines = new HashSet<>();
        int createdInstances = 0;

        // ===== DUYỆT TỪNG CHI TIẾT ĐƠN HÀNG =====
        List<OrderDetail> details = orderDetailRepo.findByOrderId(orderId);
        for (OrderDetail detail : details) {
            int qty = detail.getQuantity();

            // Khóa inventory theo dealer + modelColor
            Inventory inv = inventoryRepo.lockByDealerIdAndVehicleModelColorId(
                    dealer.getId(), detail.getVehicleModelColor().getId()
            ).orElseGet(() -> inventoryRepo.save(Inventory.builder()
                    .dealer(dealer)
                    .vehicleModelColor(detail.getVehicleModelColor())
                    .totalQuantity(0)
                    .reservedQuantity(0)
                    .availableQuantity(0)
                    .isActive(true)
                    .build()));

            for (int i = 0; i < qty; i++) {
                String vin = generateVinUnique(existingVins, detail.getVehicleModel());
                String engine = generateEngineUnique(existingEngines, detail.getVehicleModel());

                // Kiểm tra trùng VIN / số máy
                if (vehicleRepo.existsByVin(vin)) vin = generateVinUnique(existingVins, detail.getVehicleModel());
                if (vehicleRepo.existsByEngineNumber(engine)) engine = generateEngineUnique(existingEngines, detail.getVehicleModel());

                VehicleInstance instance = VehicleInstance.builder()
                        .vin(vin)
                        .engineNumber(engine)
                        .vehicleModel(detail.getVehicleModel())
                        .vehicleModelColor(detail.getVehicleModelColor())
                        .manufacturingDate(LocalDate.now())
                        .status(VehicleInstance.VehicleStatus.SHIPPING) // ✅ Xe đã được tạo, chờ giao
                        .isActive(true)
                        .currentDealer(order.getBuyerDealer())
                        .currentValue(detail.getUnitPrice())
                        .order(order) // ✅ Liên kết với đơn hàng
                        .build();

                vehicleRepo.save(instance);
                createdInstances++;

                existingVins.add(vin);
                existingEngines.add(engine);
            }

            inventoryRepo.save(inv);

            // ✅ Đánh dấu chi tiết đã xử lý
            detail.setStatus(OrderDetail.OrderDetailStatus.CONFIRMED);
            orderDetailRepo.save(detail);
        }

        // ===== CẬP NHẬT TRẠNG THÁI ĐƠN =====
        order.setStatus(Order.OrderStatus.CONFIRMED);
        orderRepo.save(order);

// ===== LOG THANH TOÁN =====
        if (Boolean.TRUE.equals(order.getIsInstallment())) {
            log.info("Order {} approved under INSTALLMENT mode — debt will activate when dealer confirms receipt.",
                    order.getOrderCode());
        } else {
            log.info("Order {} approved under FULL PAYMENT, waiting for shipping.", order.getOrderCode());
        }

        log.info("✅ Order {} approved successfully (CONFIRMED). Created {} vehicle instances.",
                order.getOrderCode(), createdInstances);

        return new OrderApproveResponse(
                order.getId(),
                order.getOrderCode(),
                order.getStatus().name(),
                createdInstances
        );

    }


    @Override
    @Transactional
    public OrderResponse markAsShipping(Long orderId, User currentUser) {
        guard.require(guard.has(currentUser, "order.ship"));
        Order order = orderRepo.findById(orderId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

        if (order.getStatus() != Order.OrderStatus.CONFIRMED)
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Chỉ đơn CONFIRMED mới có thể chuyển sang SHIPPING");

        order.setStatus(Order.OrderStatus.SHIPPING);
        orderRepo.save(order);
        return OrderResponse.fromEntity(order);
    }


    // ===================== VIN / ENGINE GENERATORS =====================

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
        throw new BaseException(ErrorHandler.INTERNAL_ERROR, "Không thể sinh số máy unique sau nhiều lần thử.");
    }

    private String safePrefix(String s, int maxLen) {
        if (s == null) return "ENG";
        String p = s.replaceAll("[^A-Za-z0-9]", "").toUpperCase();
        if (p.isEmpty()) p = "ENG";
        return p.length() > maxLen ? p.substring(0, maxLen) : p;
    }

    private String mutatePrefix(String p) {
        String allowed = "ABCDEFGHJKLMNPRSTUVWXYZ0123456789";
        char last = allowed.charAt((int) (Math.random() * allowed.length()));
        if (p.length() == 0) return "E" + last;
        return p.substring(0, Math.max(0, p.length() - 1)) + last;
    }

    private String pickWmiFromModel(VehicleModel model) {
        String brand = model.getBrand();
        if (brand != null) {
            String w = brand.replaceAll("[^A-Za-z0-9]", "").toUpperCase();
            if (w.length() >= 3) return w.substring(0, 3);
        }
        String code = model.getModelCode();
        if (code != null) {
            String w = code.replaceAll("[^A-Za-z0-9]", "").toUpperCase();
            if (w.length() >= 3) return w.substring(0, 3);
        }
        return "VF8"; // fallback
    }

    private char nextPlant(char plant) {
        String allowed = "ABCDEFGHJKLMNPRSTUVWXYZ";
        int idx = allowed.indexOf(Character.toUpperCase(plant));
        return allowed.charAt((idx + 1 + allowed.length()) % allowed.length());
    }
}
