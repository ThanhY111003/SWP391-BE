package swp.project.swp391.controller.vehicle;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import jakarta.validation.constraints.NotNull;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.entity.VehicleInstance;
import swp.project.swp391.request.vehicle.AssignVehicleRequest;
import swp.project.swp391.request.vehicle.TransferVehicleRequest;
import swp.project.swp391.request.vehicle.VehicleInstanceCreateRequest;
import swp.project.swp391.request.vehicle.VehicleInstanceUpdateRequest;
import swp.project.swp391.response.vehicle.CustomerVehicleResponse;
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
    @PostMapping("/assign-customer")
    public ResponseEntity<ApiResponse<CustomerVehicleResponse>> assignToCustomer(
            @RequestBody @Valid AssignVehicleRequest request) {

        // 🧩 Dùng service mới
        CustomerVehicleResponse response = service.assignToCustomer(request);

        return ResponseEntity.ok(ApiResponse.ok(response, "Gán xe cho khách hàng thành công"));
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
            @Parameter(
                    description = "Chỉ chấp nhận IN_STOCK hoặc RESERVED",
                    schema = @Schema(allowableValues = {"IN_STOCK", "RESERVED"})
            )
            @RequestParam VehicleInstance.VehicleStatus status) {

        VehicleInstanceResponse response = service.updateStatus(id, status);
        return ResponseEntity.ok(ApiResponse.ok(response, "Cập nhật trạng thái xe thành công"));
    }

    @Operation(summary = "Chuyển xe giữa các đại lý (chỉ dành cho ADMIN hoặc EVM_STAFF)")
    @PutMapping("/transfer")
    public ResponseEntity<ApiResponse<VehicleInstanceResponse>> transferVehicle(
            @RequestBody TransferVehicleRequest req) {
        VehicleInstanceResponse response = service.transferVehicle(req);
        return ResponseEntity.ok(ApiResponse.ok(response, "Chuyển xe giữa đại lý thành công"));
    }

    @Operation(summary = "Tạo xe mới nhập từ nhà máy")
    @PostMapping
    public ResponseEntity<ApiResponse<VehicleInstanceResponse>> create(
            @Valid @RequestBody VehicleInstanceCreateRequest req
    ) {
        var created = service.create(req);
        return ResponseEntity.ok(ApiResponse.ok(created, "Tạo xe thành công"));
    }

    @Operation(summary = "Cập nhật VIN, số máy, ngày sản xuất, màu/model")
    @PutMapping("/{id}")
    public ResponseEntity<ApiResponse<VehicleInstanceResponse>> update(
            @PathVariable Long id,
            @Valid @RequestBody VehicleInstanceUpdateRequest req
    ) {
        var updated = service.update(id, req);
        return ResponseEntity.ok(ApiResponse.ok(updated, "Cập nhật xe thành công"));
    }

}
