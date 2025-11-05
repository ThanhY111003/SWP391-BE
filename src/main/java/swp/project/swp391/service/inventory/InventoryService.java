package swp.project.swp391.service.inventory;

import swp.project.swp391.response.inventory.InventoryResponse;
import swp.project.swp391.response.vehicle.VehicleInstanceResponse;

import java.util.List;

public interface InventoryService {
    List<InventoryResponse> getInventories(Long dealerId);
    List<VehicleInstanceResponse> getVehiclesByInventory(Long inventoryId);
}

