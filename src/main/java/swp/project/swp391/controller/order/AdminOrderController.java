// controller/order/AdminOrderController.java
package swp.project.swp391.controller.order;

import io.swagger.v3.oas.annotations.Operation;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.response.order.OrderApproveResponse;
import swp.project.swp391.response.order.OrderResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.order.OrderApprovalService;
import swp.project.swp391.service.order.OrderQueryService;
import swp.project.swp391.service.order.PaymentService;

import java.util.List;

@RestController
@RequestMapping("/api/admin/orders")
@RequiredArgsConstructor
public class AdminOrderController {

    private final OrderApprovalService approvalService;
    private final PaymentService paymentService;
    private final RbacGuard guard;
    private final OrderQueryService adminOrderQueryService; // << DI tên bean riêng

    @Operation(summary = "Danh sách đơn hàng (admin)")
    @GetMapping
    public ResponseEntity<ApiResponse<List<OrderResponse>>> getAll() {
        List<OrderResponse> res = adminOrderQueryService.getAllOrders(guard.me());
        return ResponseEntity.ok(ApiResponse.ok(res, "Lấy danh sách đơn hàng thành công."));
    }

    @Operation(summary = "Chi tiết đơn hàng theo ID (admin)")
    @GetMapping("/{orderId}")
    public ResponseEntity<ApiResponse<OrderResponse>> getById(@PathVariable Long orderId) {
        OrderResponse res = adminOrderQueryService.getOrderById(orderId, guard.me());
        return ResponseEntity.ok(ApiResponse.ok(res, "Lấy chi tiết đơn hàng thành công."));
    }

    @Operation(summary = "Approve đơn -> generate VIN/Engine & nhập inventory dealer")
    @PutMapping("/{orderId}/approve")
    public ResponseEntity<ApiResponse<OrderApproveResponse>> approve(@PathVariable Long orderId) {
        OrderApproveResponse res = approvalService.approveOrder(orderId, guard.me());
        return ResponseEntity.ok(ApiResponse.ok(res, "Approve thành công."));
    }

    @Operation(summary = "Xác nhận thanh toán một kỳ trả góp cho đơn CONFIRMED")
    @PostMapping("/{orderId}/installments/{installmentNumber}/confirm")
    public ResponseEntity<ApiResponse<OrderResponse>> confirmInstallmentPayment(
            @PathVariable Long orderId,
            @PathVariable Integer installmentNumber
    ) {
        OrderResponse res = paymentService.confirmInstallmentPayment(orderId, installmentNumber, guard.me());
        return ResponseEntity.ok(ApiResponse.ok(res, "Xác nhận kỳ trả góp thành công."));
    }

    @Operation(summary = "Admin hủy đơn hàng (chỉ khi chưa hoàn tất)")
    @PutMapping("/{orderId}/cancel")
    public ResponseEntity<ApiResponse<OrderResponse>> cancelOrder(@PathVariable Long orderId) {
        OrderResponse res = paymentService.cancelOrder(orderId, guard.me());
        return ResponseEntity.ok(ApiResponse.ok(res, "Hủy đơn hàng thành công."));
    }

}
