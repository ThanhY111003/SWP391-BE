package swp.project.swp391.controller.inventory;


import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.response.inventory.InventoryResponse;
import swp.project.swp391.response.vehicle.VehicleInstanceResponse;
import swp.project.swp391.service.inventory.InventoryService;

import java.util.List;

@RestController
@RequestMapping("/api/inventories")
@RequiredArgsConstructor
@Tag(name = "Inventory Management", description = "Quản lý tồn kho xe")
public class InventoryController {

    private final InventoryService inventoryService;

    @Operation(summary = "Lấy danh sách tồn kho",
            description = """
                    - Dealer chỉ thấy kho của chính mình.
                    - Hãng (EVM_STAFF / ADMIN) có thể xem toàn bộ hoặc lọc theo dealerId.
                    """)
    @GetMapping
    public ResponseEntity<ApiResponse<List<InventoryResponse>>> getInventories(
            @RequestParam(required = false) Long dealerId
    ) {
        List<InventoryResponse> response = inventoryService.getInventories(dealerId);
        return ResponseEntity.ok(ApiResponse.ok(response, "Lấy danh sách kho thành công"));
    }

    @Operation(summary = "Lấy danh sách xe thuộc một dòng kho cụ thể")
    @GetMapping("/{inventoryId}/vehicles")
    public ResponseEntity<ApiResponse<List<VehicleInstanceResponse>>> getVehiclesByInventory(
            @PathVariable Long inventoryId) {

        List<VehicleInstanceResponse> list = inventoryService.getVehiclesByInventory(inventoryId);
        return ResponseEntity.ok(ApiResponse.ok(list, "Lấy danh sách xe trong kho thành công"));
    }

}

