package swp.project.swp391.controller.warranty;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.response.warranty.DealerWarrantyRepairResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.entity.User;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.service.warranty.DealerWarrantyRepairService;

import java.util.List;

@RestController
@RequestMapping("/api/warranty/dealer")
@RequiredArgsConstructor
@Tag(name = "Vehicle damaged", description = "Xử lý yêu cầu bảo hành/sửa chữa từ Dealer")
public class DealerWarrantyRepairController {

    private final DealerWarrantyRepairService warrantyService;
    private final RbacGuard guard;

    @Operation(summary = "Tạo yêu cầu bảo hành/sửa chữa cho xe(dealer)")
    @PostMapping("/{vehicleId}/request")
    public ResponseEntity<ApiResponse<DealerWarrantyRepairResponse>> createRequest(
            @PathVariable Long vehicleId,
            @RequestParam String reason) {
        User me = guard.me();
        DealerWarrantyRepairResponse res = warrantyService.createRequest(vehicleId, reason, me);
        return ResponseEntity.ok(ApiResponse.ok(res, "Tạo yêu cầu bảo hành thành công"));
    }

    @GetMapping("/my")
    @Operation(summary = "Lấy danh sách yêu cầu bảo hành/sửa chữa của dealer hiện tại(dealer)")
    public ResponseEntity<ApiResponse<List<DealerWarrantyRepairResponse>>> getMyRequests() {
        User me = guard.me();
        List<DealerWarrantyRepairResponse> res = warrantyService.getMyRequests(me);
        return ResponseEntity.ok(ApiResponse.ok(res, "Lấy danh sách yêu cầu bảo hành"));
    }

    @PatchMapping("/{id}/approve")
    @Operation(summary = "Phê duyệt yêu cầu bảo hành/sửa chữa(hãng)")
    public ResponseEntity<ApiResponse<DealerWarrantyRepairResponse>> approve(@PathVariable Long id) {
        User me = guard.me();
        DealerWarrantyRepairResponse res = warrantyService.approveRequest(id, me);
        return ResponseEntity.ok(ApiResponse.ok(res, "Phê duyệt yêu cầu bảo hành"));
    }

    @PatchMapping("/{id}/complete")
    @Operation(summary = "Hoàn tất sửa chữa xe(hãng)")
    public ResponseEntity<ApiResponse<DealerWarrantyRepairResponse>> complete(@PathVariable Long id) {
        User me = guard.me();
        DealerWarrantyRepairResponse res = warrantyService.completeRepair(id, me);
        return ResponseEntity.ok(ApiResponse.ok(res, "Hoàn tất sửa chữa"));
    }

    @PatchMapping("/{id}/confirm")
    @Operation(summary = "Dealer xác nhận đã nhận lại xe sau sửa chữa(dealer)")
    public ResponseEntity<ApiResponse<DealerWarrantyRepairResponse>> confirm(@PathVariable Long id) {
        User me = guard.me();
        DealerWarrantyRepairResponse res = warrantyService.confirmReceived(id, me);
        return ResponseEntity.ok(ApiResponse.ok(res, "Dealer xác nhận nhận lại xe"));
    }

    @PatchMapping("/{id}/reject")
    @Operation(summary = "Từ chối yêu cầu bảo hành/sửa chữa (hãng)")
    public ResponseEntity<ApiResponse<DealerWarrantyRepairResponse>> reject(@PathVariable Long id) {
        User me = guard.me();
        DealerWarrantyRepairResponse res = warrantyService.rejectRequest(id, me);
        return ResponseEntity.ok(ApiResponse.ok(res, "Từ chối yêu cầu bảo hành"));
    }


    @PatchMapping("/{id}/cancel")
    @Operation(summary = "Huỷ yêu cầu bảo hành/sửa chữa (dealer)")
    public ResponseEntity<ApiResponse<DealerWarrantyRepairResponse>> cancel(@PathVariable Long id) {
        User me = guard.me();
        DealerWarrantyRepairResponse res = warrantyService.cancelRequest(id, me);
        return ResponseEntity.ok(ApiResponse.ok(res, "Huỷ yêu cầu bảo hành thành công"));
    }

    @GetMapping("/dealer/{dealerId}")
    @Operation(summary = "Hãng xem danh sách yêu cầu bảo hành/sửa chữa của một dealer cụ thể(hãng)")
    public ResponseEntity<ApiResponse<List<DealerWarrantyRepairResponse>>> getRequestsByDealer(
            @PathVariable Long dealerId) {
        User me = guard.me();
        List<DealerWarrantyRepairResponse> res = warrantyService.getRequestsByDealer(dealerId, me);
        return ResponseEntity.ok(ApiResponse.ok(res, "Lấy danh sách yêu cầu bảo hành của dealer"));
    }
}
