package swp.project.swp391.controller.vehicle;

import io.swagger.v3.oas.annotations.Operation;
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
public class VehicleModelColorController {

    private final VehicleModelColorService service;
    private final RbacGuard guard;

    // --------------------------------------------------------
    // ASSIGN COLOR TO MODEL
    // --------------------------------------------------------
    @Operation(
            summary = "Gán màu vào model xe",
            description = """
                Dành cho ADMIN hoặc EVM_STAFF có quyền 'vehicleModelColor.create'.
                Cho phép gán 1 màu có sẵn trong catalog vào model cụ thể,
                với mức chênh lệch giá tùy chọn.
                """
    )
    @PostMapping("/{colorId}/assign")
    public ResponseEntity<ApiResponse<VehicleModelColorResponse>> assignColor(
            @PathVariable Long modelId,
            @PathVariable Long colorId,
            @RequestParam(defaultValue = "0") BigDecimal priceAdjustment) {

        User currentUser = guard.me();
        VehicleModelColorResponse response = service.assignColorToModel(modelId, colorId, priceAdjustment, currentUser);

        return ResponseEntity.status(HttpStatus.CREATED)
                .body(ApiResponse.ok(response, "Gán màu cho model thành công."));
    }

    // --------------------------------------------------------
    // GET COLORS OF MODEL
    // --------------------------------------------------------
    @Operation(summary = "Lấy danh sách màu của model cụ thể")
    @GetMapping
    public ResponseEntity<ApiResponse<List<VehicleModelColorResponse>>> getColorsByModel(
            @PathVariable Long modelId) {
        List<VehicleModelColorResponse> list = service.getColorsByModel(modelId);
        return ResponseEntity.ok(ApiResponse.ok(list, "Lấy danh sách màu của model thành công."));
    }

    // --------------------------------------------------------
    // UPDATE PRICE ADJUSTMENT
    // --------------------------------------------------------
    @Operation(summary = "Cập nhật chênh lệch giá của màu trên model")
    @PutMapping("/{colorId}/price-adjustment")
    public ResponseEntity<ApiResponse<VehicleModelColorResponse>> updatePriceAdjustment(
            @PathVariable Long modelId,
            @PathVariable Long colorId,
            @RequestParam BigDecimal newAdjustment) {

        User currentUser = guard.me();
        VehicleModelColorResponse response = service.updatePriceAdjustment(modelId, colorId, newAdjustment, currentUser);
        return ResponseEntity.ok(ApiResponse.ok(response, "Cập nhật chênh lệch giá thành công."));
    }

    // --------------------------------------------------------
    // UNASSIGN COLOR
    // --------------------------------------------------------
    @Operation(summary = "Xóa gán màu khỏi model (unassign)")
    @DeleteMapping("/{colorId}/unassign")
    public ResponseEntity<ApiResponse<Void>> unassignColor(
            @PathVariable Long modelId,
            @PathVariable Long colorId) {

        User currentUser = guard.me();
        service.unassignColor(modelId, colorId, currentUser);
        return ResponseEntity.ok(ApiResponse.ok(null, "Gỡ màu khỏi model thành công."));
    }
}
