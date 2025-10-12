// controller/order/AdminOrderController.java
package swp.project.swp391.controller.order;

import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.response.order.OrderApproveResponse;
import swp.project.swp391.response.order.OrderDetailResponse;
import swp.project.swp391.response.order.OrderSummaryResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.order.OrderApprovalService;
import swp.project.swp391.service.order.OrderService;

import java.util.List;

@RestController
@RequestMapping("/api/admin/orders")
@RequiredArgsConstructor
public class AdminOrderController {

    private final OrderApprovalService approvalService;
    private final RbacGuard guard;

    @Operation(summary = "Approve đơn -> auto generate VIN/engine & nhập inventory dealer")
    @PutMapping("/{orderId}/approve")
    public ResponseEntity<ApiResponse<OrderApproveResponse>> approve(@PathVariable Long orderId) {
        OrderApproveResponse res = approvalService.approveOrder(orderId, guard.me());
        return ResponseEntity.ok(ApiResponse.ok(res, "Approve thành công."));
    }
}
