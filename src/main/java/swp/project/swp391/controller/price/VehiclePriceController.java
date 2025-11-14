package swp.project.swp391.controller.price;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.request.price.VehiclePriceRequest;
import swp.project.swp391.response.price.VehiclePriceResponse;
import swp.project.swp391.service.price.VehiclePriceService;

import java.time.LocalDate;
import java.util.List;

@RestController
@RequestMapping("/api/vehicle-prices")
@RequiredArgsConstructor
@Tag(
        name = "Quản lý bảng giá xe (VehiclePrice)",
        description = "Các API cho phép quản lý bảng giá cho từng cấp đại lý và từng màu xe"
)
public class VehiclePriceController {

    private final VehiclePriceService vehiclePriceService;

    // --------------------------------------------------------
    // GET ALL
    // --------------------------------------------------------
    @GetMapping
    @Operation(summary = "Lấy danh sách bảng giá (lọc theo cấp đại lý, ngày bắt đầu và ngày kết thúc nếu có)")
    public ResponseEntity<ApiResponse<List<VehiclePriceResponse>>> getAll(
            @RequestParam(required = false) Long dealerLevelId,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
            LocalDate startDate,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE)
            LocalDate endDate
    ) {
        var list = vehiclePriceService.getAll(dealerLevelId, startDate, endDate);
        return ResponseEntity.ok(ApiResponse.ok(list, "Lấy danh sách bảng giá thành công"));
    }

    // --------------------------------------------------------
    // GET BY ID
    // --------------------------------------------------------
    @GetMapping("/{id}")
    @Operation(summary = "Lấy chi tiết bảng giá theo ID")
    public ResponseEntity<ApiResponse<VehiclePriceResponse>> getById(@PathVariable Long id) {
        var resp = vehiclePriceService.getById(id);
        return ResponseEntity.ok(ApiResponse.ok(resp, "Lấy chi tiết bảng giá thành công"));
    }

    // --------------------------------------------------------
    // CREATE
    // --------------------------------------------------------
    @PostMapping
    @Operation(summary = "Tạo mới bảng giá cho model + màu + cấp đại lý")
    public ResponseEntity<ApiResponse<VehiclePriceResponse>> create(
            @Valid @RequestBody VehiclePriceRequest request) {
        var created = vehiclePriceService.create(request);
        return ResponseEntity.ok(ApiResponse.ok(created, "Tạo bảng giá thành công"));
    }

    // --------------------------------------------------------
    // UPDATE
    // --------------------------------------------------------
    @PutMapping("/{id}")
    @Operation(summary = "Cập nhật bảng giá (giá trị, ngày hiệu lực, ngày hết hạn)")
    public ResponseEntity<ApiResponse<VehiclePriceResponse>> update(
            @PathVariable Long id,
            @Valid @RequestBody VehiclePriceRequest request) {
        var updated = vehiclePriceService.update(id, request);
        return ResponseEntity.ok(ApiResponse.ok(updated, "Cập nhật bảng giá thành công"));
    }

    // --------------------------------------------------------
    // DEACTIVATE
    // --------------------------------------------------------
    @PatchMapping("/{id}/deactivate")
    @Operation(summary = "Vô hiệu hóa bảng giá")
    public ResponseEntity<ApiResponse<Void>> deactivate(@PathVariable Long id) {
        vehiclePriceService.deactivate(id);
        return ResponseEntity.ok(ApiResponse.okMsg("Vô hiệu hóa bảng giá thành công"));
    }

    // --------------------------------------------------------
    // ACTIVATE
    // --------------------------------------------------------
    @PatchMapping("/{id}/activate")
    @Operation(summary = "Kích hoạt lại bảng giá")
    public ResponseEntity<ApiResponse<Void>> activate(@PathVariable Long id) {
        vehiclePriceService.activate(id);
        return ResponseEntity.ok(ApiResponse.okMsg("Kích hoạt bảng giá thành công"));
    }
}
