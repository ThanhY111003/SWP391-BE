package swp.project.swp391.controller.dealerLevel;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.request.dealerLevel.CreateDealerLevelRequest;
import swp.project.swp391.request.dealerLevel.EditDealerLevelRequest;
import swp.project.swp391.response.dealerLevel.DealerLevelResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.dealerLevel.DealerLevelService;

import java.util.List;

@RestController
@RequestMapping("/api/dealer-levels")
@RequiredArgsConstructor
@Tag(name = "Quản lý cấp độ đại lý (Dealer Level)", description = "Các API quản trị cấp độ đại lý cho hệ thống B2B")
public class DealerLevelController {

    private final DealerLevelService dealerLevelService;
    private final RbacGuard guard;

    @Operation(summary = "Tạo cấp độ đại lý", description = "Thêm mới một cấp độ đại lý trong hệ thống")
    @PostMapping
    public ResponseEntity<ApiResponse<DealerLevelResponse>> createDealerLevel(
            @Valid @RequestBody CreateDealerLevelRequest request) {
        guard.require(guard.has(guard.me(), "dealerLevel.create"));

        DealerLevelResponse dealerLevelResponse = dealerLevelService.createDealerLevel(request);
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(ApiResponse.ok(dealerLevelResponse, "Tạo cấp độ đại lý thành công"));
    }

    @Operation(summary = "Cập nhật cấp độ đại lý", description = "Chỉnh sửa thông tin của cấp độ đại lý theo ID")
    @PutMapping("/{id}")
    public ResponseEntity<ApiResponse<DealerLevelResponse>> updateDealerLevel(
            @PathVariable Long id,
            @Valid @RequestBody EditDealerLevelRequest request) {
        guard.require(guard.has(guard.me(), "dealerLevel.update"));

        DealerLevelResponse dealerLevelResponse = dealerLevelService.editDealerLevel(id, request);
        return ResponseEntity.ok(ApiResponse.ok(dealerLevelResponse, "Cập nhật cấp độ đại lý thành công"));
    }

    @Operation(summary = "Lấy danh sách cấp độ đại lý", description = "Trả về danh sách tất cả cấp độ đại lý hiện có")
    @GetMapping
    public ResponseEntity<ApiResponse<List<DealerLevelResponse>>> getAllDealerLevels() {
        guard.require(guard.has(guard.me(), "dealerLevel.read.all"));

        List<DealerLevelResponse> dealerLevelResponses = dealerLevelService.getAllDealerLevels();
        return ResponseEntity.ok(ApiResponse.ok(dealerLevelResponses, "Lấy danh sách cấp độ đại lý thành công"));
    }

    @Operation(summary = "Lấy chi tiết cấp độ đại lý", description = "Trả về thông tin chi tiết của một cấp độ đại lý theo ID")
    @GetMapping("/{id}")
    public ResponseEntity<ApiResponse<DealerLevelResponse>> getDealerLevel(@PathVariable Long id) {
        guard.require(guard.has(guard.me(), "dealerLevel.read"));

        DealerLevelResponse dealerLevelResponse = dealerLevelService.getDealerLevelById(id);
        return ResponseEntity.ok(ApiResponse.ok(dealerLevelResponse, "Lấy thông tin cấp độ đại lý thành công"));
    }

    @Operation(summary = "Xóa cấp độ đại lý", description = "Xóa một cấp độ đại lý khỏi hệ thống")
    @DeleteMapping("/{id}")
    public ResponseEntity<ApiResponse<Void>> deleteDealerLevel(@PathVariable Long id) {
        guard.require(guard.has(guard.me(), "dealerLevel.delete"));

        dealerLevelService.deleteDealerLevel(id);
        return ResponseEntity.ok(ApiResponse.okMsg("Xóa cấp độ đại lý thành công"));
    }
}
