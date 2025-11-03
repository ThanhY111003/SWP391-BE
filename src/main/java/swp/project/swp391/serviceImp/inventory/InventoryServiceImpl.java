package swp.project.swp391.serviceImp.inventory;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import swp.project.swp391.entity.Inventory;
import swp.project.swp391.entity.Role;
import swp.project.swp391.entity.User;
import swp.project.swp391.entity.VehicleInstance;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.InventoryRepository;
import swp.project.swp391.repository.VehicleInstanceRepository;
import swp.project.swp391.response.inventory.InventoryResponse;
import swp.project.swp391.response.vehicle.VehicleInstanceResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.inventory.InventoryService;
import java.util.List;
import swp.project.swp391.constant.ErrorHandler;

@Service
@RequiredArgsConstructor
public class InventoryServiceImpl implements InventoryService {

    private final InventoryRepository inventoryRepo;
    private final RbacGuard guard;
    private final VehicleInstanceRepository vehicleRepo;

    @Override
    public List<InventoryResponse> getInventories(Long dealerId) {
        User current = guard.me();

        // Nếu là dealer, chỉ cho xem kho của chính mình
        if (isDealerRole(current)) {
            dealerId = current.getDealer().getId();
            return inventoryRepo.findByDealerId(dealerId)
                    .stream()
                    .map(InventoryResponse::fromEntity)
                    .toList();
        }

        // Nếu là hãng (admin / evm), có thể xem tất cả hoặc lọc theo dealerId
        return inventoryRepo.findAllByDealerIdNullable(dealerId)
                .stream()
                .map(InventoryResponse::fromEntity)
                .toList();
    }

    public List<VehicleInstanceResponse> getVehiclesByInventory(Long inventoryId) {
        Inventory inv = inventoryRepo.findById(inventoryId)
                .orElseThrow(() -> new BaseException(ErrorHandler.INVENTORY_NOT_FOUND));

        // ✅ Lấy các xe cùng dealer & cùng màu/model với inventory đó
        List<VehicleInstance> vehicles = vehicleRepo
                .findByCurrentDealerIdAndVehicleModelColorId(
                        inv.getDealer().getId(),
                        inv.getVehicleModelColor().getId()
                );

        return vehicles.stream()
                .map(VehicleInstanceResponse::fromEntity)
                .toList();
    }


    private boolean isDealerRole(User user) {
        return user.getRoles().stream()
                .map(Role::getName)
                .anyMatch(r -> r.equals("DEALER_MANAGER") || r.equals("DEALER_STAFF"));
    }
}

