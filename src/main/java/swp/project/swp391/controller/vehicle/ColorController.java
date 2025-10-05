package swp.project.swp391.controller.vehicle;

import io.swagger.v3.oas.annotations.Operation;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.entity.User;
import swp.project.swp391.request.vehicle.ColorRequest;
import swp.project.swp391.response.vehicle.ColorResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.vehicle.ColorService;

import java.util.List;

@RestController
@RequestMapping("/api/colors")
@RequiredArgsConstructor
public class ColorController {

    private final ColorService service;
    private final RbacGuard guard;

    // --------------------------------------------------------
    // CREATE
    // --------------------------------------------------------
    @Operation(
            summary = "Tạo màu mới",
            description = """
                Dành cho ADMIN hoặc EVM_STAFF có quyền 'color.create'.
                Bao gồm tên màu và mã hex (#RRGGBB).
                """
    )
    @PostMapping("/create")
    public ResponseEntity<ApiResponse<ColorResponse>> createColor(
            @Valid @RequestBody ColorRequest request) {

        User currentUser = guard.me();
        ColorResponse response = service.createColor(request, currentUser);

        return ResponseEntity.status(HttpStatus.CREATED)
                .body(ApiResponse.ok(response, "Tạo màu mới thành công."));
    }

    // --------------------------------------------------------
    // GET ALL
    // --------------------------------------------------------
    @Operation(
            summary = "Lấy danh sách tất cả màu sắc",
            description = "Thêm ?includeInactive=true để lấy cả các màu đã vô hiệu hóa."
    )
    @GetMapping
    public ResponseEntity<ApiResponse<List<ColorResponse>>> getAllColors() {

        List<ColorResponse> list = service.getAllColors();
        return ResponseEntity.ok(ApiResponse.ok(list, "Lấy danh sách màu thành công."));
    }

    // --------------------------------------------------------
    // GET BY ID
    // --------------------------------------------------------
    @Operation(summary = "Lấy chi tiết màu theo ID")
    @GetMapping("/{id}")
    public ResponseEntity<ApiResponse<ColorResponse>> getColorById(@PathVariable Long id) {
        ColorResponse response = service.getColorById(id);
        return ResponseEntity.ok(ApiResponse.ok(response, "Lấy chi tiết màu thành công."));
    }

    // --------------------------------------------------------
    // UPDATE
    // --------------------------------------------------------
    @Operation(
            summary = "Cập nhật thông tin màu",
            description = """
                Cho phép cập nhật tên và mã hex.
                Nếu đổi mã hex sẽ kiểm tra trùng.
                Yêu cầu quyền 'color.update'.
                """
    )
    @PutMapping("/{id}")
    public ResponseEntity<ApiResponse<ColorResponse>> updateColor(
            @PathVariable Long id,
            @Valid @RequestBody ColorRequest request) {

        User currentUser = guard.me();
        ColorResponse response = service.updateColor(id, request, currentUser);
        return ResponseEntity.ok(ApiResponse.ok(response, "Cập nhật màu thành công."));
    }

    // --------------------------------------------------------
    // INACTIVE
    // --------------------------------------------------------
    @Operation(
            summary = "Vô hiệu hóa màu",
            description = "Ẩn màu khỏi hệ thống, yêu cầu quyền 'color.update'."
    )
    @PutMapping("/{id}/inactive")
    public ResponseEntity<ApiResponse<ColorResponse>> inactiveColor(@PathVariable Long id) {
        User currentUser = guard.me();
        ColorResponse response = service.inactiveColor(id, currentUser);
        return ResponseEntity.ok(ApiResponse.ok(response, "Vô hiệu hóa màu thành công."));
    }

    // --------------------------------------------------------
    // REACTIVE
    // --------------------------------------------------------
    @Operation(
            summary = "Kích hoạt lại màu",
            description = "Cho phép kích hoạt lại màu đã bị vô hiệu hóa, yêu cầu quyền 'color.update'."
    )
    @PutMapping("/{id}/reactive")
    public ResponseEntity<ApiResponse<ColorResponse>> reactiveColor(@PathVariable Long id) {
        User currentUser = guard.me();
        ColorResponse response = service.reactiveColor(id, currentUser);
        return ResponseEntity.ok(ApiResponse.ok(response, "Kích hoạt lại màu thành công."));
    }
}
