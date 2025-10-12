package swp.project.swp391.controller.dealerLevel;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.request.dealerLevel.CreateDealerLevelRequest;
import swp.project.swp391.request.dealerLevel.EditDealerLevelRequest;
import swp.project.swp391.response.dealerLevel.DealerLevelResponse;
import swp.project.swp391.service.dealerLevel.DealerLevelService;
import swp.project.swp391.security.RbacGuard;

import jakarta.validation.Valid;
import java.util.List;

@RestController
@RequestMapping("/api/dealer-levels")
@RequiredArgsConstructor
public class DealerLevelController {

    private final DealerLevelService dealerLevelService;
    private final RbacGuard guard;

    // Tạo Dealer Level
    @PostMapping("/create")
    public ResponseEntity<ApiResponse<DealerLevelResponse>> createDealerLevel(@Valid @RequestBody CreateDealerLevelRequest request) {
        guard.require(guard.has(guard.me(), "dealerLevel.create"));

        DealerLevelResponse dealerLevelResponse = dealerLevelService.createDealerLevel(request);
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(ApiResponse.ok(dealerLevelResponse, "Tạo cấp độ đại lý thành công"));
    }

    // Cập nhật Dealer Level
    @PutMapping("/edit/{id}")
    public ResponseEntity<ApiResponse<DealerLevelResponse>> editDealerLevel(@PathVariable Long id, @Valid @RequestBody EditDealerLevelRequest request) {
        guard.require(guard.has(guard.me(), "dealerLevel.update"));

        DealerLevelResponse dealerLevelResponse = dealerLevelService.editDealerLevel(id, request);
        return ResponseEntity.ok(ApiResponse.ok(dealerLevelResponse, "Cập nhật cấp độ đại lý thành công"));
    }

    // Lấy tất cả Dealer Levels
    @GetMapping("/all")
    public ResponseEntity<ApiResponse<List<DealerLevelResponse>>> getAllDealerLevels() {
        guard.require(guard.has(guard.me(), "dealerLevel.read.all"));

        List<DealerLevelResponse> dealerLevelResponses = dealerLevelService.getAllDealerLevels();
        return ResponseEntity.ok(ApiResponse.ok(dealerLevelResponses, "Lấy danh sách cấp độ đại lý thành công"));
    }

    // Lấy thông tin chi tiết của một Dealer Level
    @GetMapping("/{id}")
    public ResponseEntity<ApiResponse<DealerLevelResponse>> getDealerLevel(@PathVariable Long id) {
        guard.require(guard.has(guard.me(), "dealerLevel.read"));

        DealerLevelResponse dealerLevelResponse = dealerLevelService.getDealerLevelById(id);
        return ResponseEntity.ok(ApiResponse.ok(dealerLevelResponse, "Lấy thông tin cấp độ đại lý thành công"));
    }

    // Xóa Dealer Level
    @DeleteMapping("/delete/{id}")
    public ResponseEntity<ApiResponse<Void>> deleteDealerLevel(@PathVariable Long id) {
        guard.require(guard.has(guard.me(), "dealerLevel.delete"));

        dealerLevelService.deleteDealerLevel(id);
        return ResponseEntity.ok(ApiResponse.ok(null, "Xóa cấp độ đại lý thành công"));
    }
}
