package swp.project.swp391.serviceImp.order;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.*;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.*;
import swp.project.swp391.response.order.OrderApproveResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.order.OrderApprovalService;
import swp.project.swp391.util.VinEngineGenerator;

import java.time.LocalDate;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Service
@RequiredArgsConstructor
public class OrderApprovalServiceImpl implements OrderApprovalService {

    private final OrderRepository orderRepo;
    private final OrderDetailRepository orderDetailRepo;
    private final VehicleInstanceRepository vehicleRepo;
    private final InventoryRepository inventoryRepo;
    private final RbacGuard guard;

    // GIỮ CHỈ 1 BẢN
    private static final int GEN_ATTEMPTS = 30;

    @Override
    @Transactional
    public OrderApproveResponse approveOrder(Long orderId, User currentUser) {
        guard.require(guard.has(currentUser, "order.approve"));

        Order order = orderRepo.findById(orderId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

        if (order.getStatus() != Order.OrderStatus.PENDING) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST);
        }

        Dealer dealer = order.getBuyerDealer();
        if (dealer == null) throw new BaseException(ErrorHandler.DEALER_NOT_FOUND);

        // Cache tạm để tránh đụng nhau trong 1 lần approve
        Set<String> existingVins = new HashSet<>();
        Set<String> existingEngines = new HashSet<>();

        int created = 0;

        List<OrderDetail> details = orderDetailRepo.findByOrderId(orderId);
        for (OrderDetail d : details) {
            int qty = d.getQuantity();

            // lock inventory theo (dealer, model) để cộng số lượng an toàn
            Inventory inv = inventoryRepo.lockByDealerIdAndVehicleModelId(
                    dealer.getId(), d.getVehicleModel().getId()
            ).orElseGet(() -> {
                Inventory i = Inventory.builder()
                        .dealer(dealer)
                        .vehicleModel(d.getVehicleModel())
                        .totalQuantity(0)
                        .reservedQuantity(0)
                        .availableQuantity(0)
                        .isActive(true)
                        .build();
                return inventoryRepo.save(i);
            });

            for (int i = 0; i < qty; i++) {
                // === SINH VIN/ENGINE UNIQUE ===
                String vin = generateVinUnique(existingVins, d.getVehicleModel());      // <-- PASS model
                String engine = generateEngineUnique(existingEngines, d.getVehicleModel()); // <-- PASS model

                // Double-check DB uniqueness, nếu đụng thì regen lại (cũng PASS model)
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
                        .vehicleColor(d.getVehicleColor()) // VehicleModelColor
                        .manufacturingDate(LocalDate.now())
                        .status(VehicleInstance.VehicleStatus.IN_STOCK) // nhập thẳng kho dealer
                        .isActive(true)
                        .currentDealer(dealer)
                        .build();

                vehicleRepo.save(v);
                created++;

                existingVins.add(vin);
                existingEngines.add(engine);

                // cộng inventory (đơn giản: total+1, available+1)
                inv.setTotalQuantity(inv.getTotalQuantity() + 1);
                inv.setAvailableQuantity(inv.getAvailableQuantity() + 1);
            }

            inventoryRepo.save(inv);
        }

        order.setStatus(Order.OrderStatus.CONFIRMED);
        orderRepo.save(order);

        return new OrderApproveResponse(order.getId(), order.getOrderCode(), order.getStatus().name(), created);
    }

    // ========= Helpers for unique VIN/Engine =========

    // Tạo VIN duy nhất, kiểm tra cache + DB
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

    private char nextPlant(char plant) {
        String allowed = "ABCDEFGHJKLMNPRSTUVWXYZ"; // tránh I,O,Q
        int idx = allowed.indexOf(Character.toUpperCase(plant));
        return allowed.charAt((idx + 1 + allowed.length()) % allowed.length());
    }

    // Tạo EngineNumber duy nhất, kiểm tra cache + DB
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
}
