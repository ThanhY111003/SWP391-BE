package swp.project.swp391.controller.dealer;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.entity.User;
import swp.project.swp391.request.dealer.DealerRequest;
import swp.project.swp391.response.dealer.DealerResponse;
import swp.project.swp391.service.dealer.DealerService;
import swp.project.swp391.security.RbacGuard;

import jakarta.validation.Valid;
import java.security.Principal;
import java.util.List;

@RestController
@RequestMapping("/api/dealers")
@RequiredArgsConstructor
public class DealerController {

    private final DealerService dealerService;
    private final RbacGuard guard;

    @PostMapping("/create")
    public ResponseEntity<ApiResponse<DealerResponse>> createDealer(@Valid @RequestBody DealerRequest dealerRequest) {
        User currentUser = guard.me();
        DealerResponse dealerResponse = dealerService.createDealer(dealerRequest, currentUser);
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(ApiResponse.ok(dealerResponse, "Tạo Dealer thành công"));
    }

    @PatchMapping("/{dealerId}/inactive")
    public ResponseEntity<ApiResponse<DealerResponse>> inactiveDealer(@PathVariable Long dealerId) {
        User currentUser = guard.me();
        DealerResponse dealerResponse = dealerService.inactiveDealer(dealerId, currentUser);
        return ResponseEntity.ok(ApiResponse.ok(dealerResponse, "Đã chuyển Dealer sang trạng thái inactive"));
    }

    @PatchMapping("/{dealerId}/reactivate")
    public ResponseEntity<ApiResponse<DealerResponse>> reactivateDealer(@PathVariable Long dealerId) {
        User currentUser = guard.me();
        DealerResponse dealerResponse = dealerService.reactivateDealer(dealerId, currentUser);
        return ResponseEntity.ok(ApiResponse.ok(dealerResponse, "Dealer đã được kích hoạt lại"));
    }

    @GetMapping("/all")
    public ResponseEntity<ApiResponse<List<DealerResponse>>> getAllDealers(Principal principal) {
        User currentUser = principal != null ? guard.me() : null;
        List<DealerResponse> dealerResponses = dealerService.getAllDealers(currentUser);
        return ResponseEntity.ok(ApiResponse.ok(dealerResponses, "Lấy danh sách Dealer thành công"));
    }

    @GetMapping("/{dealerId}")
    public ResponseEntity<ApiResponse<DealerResponse>> getDealer(@PathVariable Long dealerId, Principal principal) {
        User currentUser = principal != null ? guard.me() : null;
        DealerResponse dealerResponse = dealerService.getDealer(dealerId, currentUser);
        return ResponseEntity.ok(ApiResponse.ok(dealerResponse, "Lấy thông tin Dealer thành công"));
    }

    @PutMapping("/{dealerId}")
    public ResponseEntity<ApiResponse<DealerResponse>> editDealer(
            @PathVariable Long dealerId,
            @Valid @RequestBody DealerRequest dealerRequest,
            Principal principal) {
        User currentUser = principal != null ? guard.me() : null;
        DealerResponse updatedDealer = dealerService.editDealer(dealerId, dealerRequest, currentUser);
        return ResponseEntity.ok(ApiResponse.ok(updatedDealer, "Cập nhật Dealer thành công"));
    }
}
