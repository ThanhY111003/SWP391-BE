package swp.project.swp391.controller.vehicle;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.constraints.NotNull;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.entity.VehicleInstance;
import swp.project.swp391.response.vehicle.VehicleInstanceResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.vehicle.VehicleInstanceService;

import java.util.List;

@RestController
@RequestMapping("/api/vehicle-instances")
@RequiredArgsConstructor
@Tag(name = "Quản lý xe (Vehicle Instance)", description = "Các API cho phép quản lý xe vật lý: tìm kiếm, cập nhật trạng thái, bán cho khách")
public class VehicleInstanceController {

    private final VehicleInstanceService service;
    private final RbacGuard guard;

    @Operation(summary = "Lấy danh sách xe (lọc theo đại lý, trạng thái, active)")
    @GetMapping
    public ResponseEntity<ApiResponse<List<VehicleInstanceResponse>>> getAll(
            @RequestParam(required = false) Long dealerId,
            @RequestParam(required = false) VehicleInstance.VehicleStatus status,
            @RequestParam(required = false, defaultValue = "false") Boolean activeOnly) {
        var list = service.getAll(dealerId, status, activeOnly);
        return ResponseEntity.ok(ApiResponse.ok(list, "Lấy danh sách xe thành công"));
    }

    @Operation(summary = "Lấy chi tiết xe theo ID")
    @GetMapping("/{id}")
    public ResponseEntity<ApiResponse<VehicleInstanceResponse>> getById(@PathVariable Long id) {
        return ResponseEntity.ok(ApiResponse.ok(service.getById(id), "Lấy thông tin xe thành công"));
    }


    @Operation(summary = "Gán xe cho khách hàng")
    @PostMapping("/{id}/assign-customer")
    public ResponseEntity<ApiResponse<Void>> assignToCustomer(
            @PathVariable Long id,
            @RequestParam @NotNull Long customerId) {
        var user = guard.me();
        service.assignToCustomer(id, customerId, user.getId());
        return ResponseEntity.ok(ApiResponse.okMsg("Gán xe cho khách hàng thành công"));
    }

    @Operation(summary = "Vô hiệu hóa xe")
    @PatchMapping("/{id}/deactivate")
    public ResponseEntity<ApiResponse<Void>> deactivate(@PathVariable Long id) {
        service.deactivate(id);
        return ResponseEntity.ok(ApiResponse.okMsg("Vô hiệu hóa xe thành công"));
    }

    @Operation(summary = "Kích hoạt lại xe")
    @PatchMapping("/{id}/activate")
    public ResponseEntity<ApiResponse<Void>> activate(@PathVariable Long id) {
        service.activate(id);
        return ResponseEntity.ok(ApiResponse.okMsg("Kích hoạt xe thành công"));
    }

    @Operation(summary = "Cập nhật trạng thái xe (IN_STOCK ↔ RESERVED)")
    @PutMapping("/{id}/status")
    public ResponseEntity<ApiResponse<VehicleInstanceResponse>> updateStatus(
            @PathVariable Long id,
            @RequestParam VehicleInstance.VehicleStatus status) {

        VehicleInstanceResponse response = service.updateStatus(id, status);
        return ResponseEntity.ok(ApiResponse.ok(response, "Cập nhật trạng thái xe thành công"));
    }

}
