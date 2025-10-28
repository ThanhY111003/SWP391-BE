package swp.project.swp391.controller.vehicle;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.entity.User;
import swp.project.swp391.response.vehicle.VehicleModelColorResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.vehicle.VehicleModelColorService;

import java.math.BigDecimal;
import java.util.List;

@RestController
@RequestMapping("/api/vehicle-models/{modelId}/colors")
@RequiredArgsConstructor
@Tag(name = "Quản lý màu xe theo Model", description = "Các API cho phép gán, chỉnh sửa và quản lý màu sắc của từng model xe")
public class VehicleModelColorController {

    private final VehicleModelColorService service;
    private final RbacGuard guard;

    @Operation(
            summary = "Gán màu vào model xe",
            description = """
                Dành cho ADMIN hoặc EVM_STAFF có quyền 'vehicleModelColor.create'.
                Cho phép gán màu sẵn trong catalog vào model cụ thể, kèm chênh lệch giá tùy chọn.
                """
    )
    @PostMapping
    public ResponseEntity<ApiResponse<VehicleModelColorResponse>> assignColor(
            @PathVariable Long modelId,
            @RequestParam Long colorId,
            @RequestParam(defaultValue = "0") BigDecimal priceAdjustment) {

        User currentUser = guard.me();
        VehicleModelColorResponse response =
                service.assignColorToModel(modelId, colorId, priceAdjustment, currentUser);

        return ResponseEntity.status(HttpStatus.CREATED)
                .body(ApiResponse.ok(response, "Gán màu cho model thành công"));
    }

    @Operation(summary = "Lấy danh sách màu của model", description = "Trả về danh sách màu đã gán cho một model cụ thể")
    @GetMapping
    public ResponseEntity<ApiResponse<List<VehicleModelColorResponse>>> getColorsByModel(
            @PathVariable Long modelId) {

        List<VehicleModelColorResponse> list = service.getColorsByModel(modelId);
        return ResponseEntity.ok(ApiResponse.ok(list, "Lấy danh sách màu của model thành công"));
    }

    @Operation(summary = "Cập nhật chênh lệch giá của màu", description = "Cập nhật phần chênh lệch giá cho một màu đã gán với model cụ thể")
    @PatchMapping("/{colorId}/price-adjustment")
    public ResponseEntity<ApiResponse<VehicleModelColorResponse>> updatePriceAdjustment(
            @PathVariable Long modelId,
            @PathVariable Long colorId,
            @RequestParam BigDecimal newAdjustment) {

        User currentUser = guard.me();
        VehicleModelColorResponse response =
                service.updatePriceAdjustment(modelId, colorId, newAdjustment, currentUser);

        return ResponseEntity.ok(ApiResponse.ok(response, "Cập nhật chênh lệch giá thành công"));
    }

    @Operation(summary = "Gỡ màu khỏi model", description = "Xóa quan hệ giữa model và màu cụ thể, yêu cầu quyền 'vehicleModelColor.delete'")
    @DeleteMapping("/{colorId}")
    public ResponseEntity<ApiResponse<Void>> unassignColor(
            @PathVariable Long modelId,
            @PathVariable Long colorId) {

        User currentUser = guard.me();
        service.unassignColor(modelId, colorId, currentUser);
        return ResponseEntity.ok(ApiResponse.okMsg("Gỡ màu khỏi model thành công"));
    }
}
