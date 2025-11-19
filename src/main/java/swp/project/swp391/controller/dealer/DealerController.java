package swp.project.swp391.controller.dealer;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.entity.User;
import swp.project.swp391.request.dealer.DealerRequest;
import swp.project.swp391.response.dealer.DealerResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.dealer.DealerService;

import java.security.Principal;
import java.util.List;

@RestController
@RequestMapping("/api/dealers")
@RequiredArgsConstructor
@Tag(name = "Quản lý đại lý (Dealer)", description = "Các API phục vụ quản trị đại lý cho Admin và EVM")
public class DealerController {

    private final DealerService dealerService;
    private final RbacGuard guard;

    @Operation(summary = "Tạo mới Dealer", description = "Tạo mới một đại lý (chỉ Admin/EVM được phép)")
    @PostMapping
    public ResponseEntity<ApiResponse<DealerResponse>> createDealer(@Valid @RequestBody DealerRequest dealerRequest) {
        User currentUser = guard.me();
        DealerResponse dealerResponse = dealerService.createDealer(dealerRequest, currentUser);
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(ApiResponse.ok(dealerResponse, "Tạo Dealer thành công"));
    }

    @Operation(summary = "Cập nhật thông tin Dealer", description = "Chỉnh sửa thông tin của một Dealer cụ thể")
    @PutMapping("/{dealerId}")
    public ResponseEntity<ApiResponse<DealerResponse>> updateDealer(
            @PathVariable Long dealerId,
            @Valid @RequestBody DealerRequest dealerRequest,
            Principal principal) {
        User currentUser = principal != null ? guard.me() : null;
        DealerResponse updatedDealer = dealerService.editDealer(dealerId, dealerRequest, currentUser);
        return ResponseEntity.ok(ApiResponse.ok(updatedDealer, "Cập nhật Dealer thành công"));
    }

    @Operation(summary = "Vô hiệu hóa Dealer", description = "Chuyển Dealer sang trạng thái không hoạt động (inactive)")
    @PatchMapping("/{dealerId}/deactivate")
    public ResponseEntity<ApiResponse<DealerResponse>> deactivateDealer(@PathVariable Long dealerId) {
        User currentUser = guard.me();
        DealerResponse dealerResponse = dealerService.inactiveDealer(dealerId, currentUser);
        return ResponseEntity.ok(ApiResponse.ok(dealerResponse, "Dealer đã được vô hiệu hóa"));
    }

    @Operation(summary = "Kích hoạt lại Dealer", description = "Kích hoạt lại Dealer từ trạng thái inactive")
    @PatchMapping("/{dealerId}/activate")
    public ResponseEntity<ApiResponse<DealerResponse>> activateDealer(@PathVariable Long dealerId) {
        User currentUser = guard.me();
        DealerResponse dealerResponse = dealerService.reactivateDealer(dealerId, currentUser);
        return ResponseEntity.ok(ApiResponse.ok(dealerResponse, "Dealer đã được kích hoạt lại"));
    }

    @GetMapping
    public ResponseEntity<ApiResponse<List<DealerResponse>>> getAllDealers(Principal principal) {
        User currentUser = guard.me();

        // ✅ Nếu bạn chỉ cho phép hãng xem tất cả dealer
        guard.require(guard.has(currentUser, "dealer.read.all"));

        List<DealerResponse> dealerResponses = dealerService.getAllDealers(currentUser);
        return ResponseEntity.ok(ApiResponse.ok(dealerResponses, "Lấy danh sách Dealer thành công"));
    }


    @Operation(summary = "Lấy chi tiết Dealer", description = "Trả về thông tin chi tiết của một Dealer theo ID")
    @GetMapping("/{dealerId}")
    public ResponseEntity<ApiResponse<DealerResponse>> getDealer(
            @PathVariable Long dealerId,
            Principal principal) {
        User currentUser = principal != null ? guard.me() : null;
        DealerResponse dealerResponse = dealerService.getDealer(dealerId, currentUser);
        return ResponseEntity.ok(ApiResponse.ok(dealerResponse, "Lấy thông tin Dealer thành công"));
    }
}
