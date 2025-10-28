package swp.project.swp391.controller.order;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
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
@Tag(name = "Quản lý đơn hàng (Admin)", description = "Các API quản trị đơn hàng cho Admin – duyệt, hủy, xác nhận thanh toán")
public class AdminOrderController {

    private final OrderApprovalService approvalService;
    private final PaymentService paymentService;
    private final RbacGuard guard;
    private final OrderQueryService adminOrderQueryService; // Bean dành riêng cho Admin

    @Operation(summary = "Lấy danh sách đơn hàng", description = "Trả về danh sách tất cả đơn hàng trong hệ thống (dành cho Admin)")
    @GetMapping
    public ResponseEntity<ApiResponse<List<OrderResponse>>> getAll() {
        List<OrderResponse> res = adminOrderQueryService.getAllOrders(guard.me());
        return ResponseEntity.ok(ApiResponse.ok(res, "Lấy danh sách đơn hàng thành công"));
    }

    @Operation(summary = "Xem chi tiết đơn hàng", description = "Trả về thông tin chi tiết của một đơn hàng theo ID")
    @GetMapping("/{orderId}")
    public ResponseEntity<ApiResponse<OrderResponse>> getById(@PathVariable Long orderId) {
        OrderResponse res = adminOrderQueryService.getOrderById(orderId, guard.me());
        return ResponseEntity.ok(ApiResponse.ok(res, "Lấy chi tiết đơn hàng thành công"));
    }

    @Operation(summary = "Duyệt đơn hàng (Approve)", description = "Phê duyệt đơn hàng, tự động tạo VIN/Engine và nhập hàng vào kho Dealer tương ứng")
    @PatchMapping("/{orderId}/approve")
    public ResponseEntity<ApiResponse<OrderApproveResponse>> approve(@PathVariable Long orderId) {
        OrderApproveResponse res = approvalService.approveOrder(orderId, guard.me());
        return ResponseEntity.ok(ApiResponse.ok(res, "Duyệt đơn hàng thành công"));
    }

    @Operation(summary = "Xác nhận thanh toán kỳ trả góp", description = "Xác nhận Admin đã nhận thanh toán cho một kỳ trả góp của đơn hàng CONFIRMED")
    @PostMapping("/{orderId}/installments/{installmentNumber}/confirm")
    public ResponseEntity<ApiResponse<OrderResponse>> confirmInstallmentPayment(
            @PathVariable Long orderId,
            @PathVariable Integer installmentNumber
    ) {
        OrderResponse res = paymentService.confirmInstallmentPayment(orderId, installmentNumber, guard.me());
        return ResponseEntity.ok(ApiResponse.ok(res, "Xác nhận thanh toán kỳ trả góp thành công"));
    }

    @Operation(summary = "Hủy đơn hàng", description = "Admin hủy đơn hàng khi đơn chưa hoàn tất hoặc chưa giao xe")
    @PatchMapping("/{orderId}/cancel")
    public ResponseEntity<ApiResponse<OrderResponse>> cancelOrder(@PathVariable Long orderId) {
        OrderResponse res = paymentService.cancelOrder(orderId, guard.me());
        return ResponseEntity.ok(ApiResponse.ok(res, "Hủy đơn hàng thành công"));
    }
}
