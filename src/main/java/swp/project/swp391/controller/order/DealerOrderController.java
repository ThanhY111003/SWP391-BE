package swp.project.swp391.controller.order;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.entity.User;
import swp.project.swp391.request.order.CreateOrderRequest;
import swp.project.swp391.response.order.OrderResponse;
import swp.project.swp391.response.vehicle.VehicleInstanceResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.order.OrderQueryService;
import swp.project.swp391.service.order.OrderService;

import java.util.List;

@RestController
@RequestMapping("/api/dealer/orders")
@RequiredArgsConstructor
@Tag(name = "Quản lý đơn hàng (Dealer)", description = "Các API để Dealer tạo và xem các đơn hàng của chính mình")
public class DealerOrderController {

    private final OrderService orderService;
    private final RbacGuard guard;
    private final OrderQueryService dealerOrderQueryService; // Bean riêng cho Dealer

    @Operation(summary = "Tạo yêu cầu nhập xe", description = "Dealer tạo đơn hàng nhập xe mới vào hệ thống")
    @PostMapping
    public ResponseEntity<ApiResponse<OrderResponse>> createOrder(@Valid @RequestBody CreateOrderRequest request) {
        User me = guard.me();
        OrderResponse res = orderService.createOrder(request, me.getId());
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(ApiResponse.ok(res, "Tạo yêu cầu nhập xe thành công"));
    }

    @Operation(summary = "Xem danh sách đơn hàng của Dealer", description = "Trả về danh sách tất cả các đơn hàng thuộc Dealer hiện tại")
    @GetMapping
    public ResponseEntity<ApiResponse<List<OrderResponse>>> getMyOrders() {
        User me = guard.me();
        List<OrderResponse> res = dealerOrderQueryService.getAllOrders(me);
        return ResponseEntity.ok(ApiResponse.ok(res, "Lấy danh sách đơn hàng thành công"));
    }

    @Operation(summary = "Xem chi tiết đơn hàng của Dealer", description = "Trả về thông tin chi tiết một đơn hàng thuộc Dealer hiện tại theo ID")
    @GetMapping("/{orderId}")
    public ResponseEntity<ApiResponse<OrderResponse>> getMyOrderById(@PathVariable Long orderId) {
        User me = guard.me();
        OrderResponse res = dealerOrderQueryService.getOrderById(orderId, me);
        return ResponseEntity.ok(ApiResponse.ok(res, "Lấy chi tiết đơn hàng thành công"));
    }

    @DeleteMapping("/{orderId}/cancel")
    @Operation(summary = "Huỷ đơn hàng (Dealer)", description = "Chỉ huỷ được đơn đang ở trạng thái PENDING.")
    public ResponseEntity<ApiResponse<Void>> cancelOrder(@PathVariable Long orderId) {
        orderService.cancelOrder(orderId);
        return ResponseEntity.ok(ApiResponse.ok(null, "Huỷ đơn hàng thành công"));
    }

    @Operation(summary = "Lấy danh sách xe trong đơn hàng", description = "Trả về danh sách các xe (VehicleInstance) thuộc đơn hàng của Dealer hiện tại")
    @GetMapping("/{orderId}/vehicles")
    public ResponseEntity<ApiResponse<List<VehicleInstanceResponse>>> getVehiclesByOrder(
            @PathVariable Long orderId
    ) {
        User me = guard.me();
        List<VehicleInstanceResponse> vehicles = dealerOrderQueryService.getVehiclesByOrder(orderId, me);
        return ResponseEntity.ok(ApiResponse.ok(vehicles, "Lấy danh sách xe trong đơn hàng thành công"));
    }

    @Operation(summary = "Dealer xác nhận đã nhận xe", description = "Dealer xác nhận đơn hàng SHIPPING, xe đạt chuẩn và thêm vào kho.")
    @PatchMapping("/{orderId}/confirm-received")
    public ResponseEntity<ApiResponse<OrderResponse>> dealerConfirmReceived(@PathVariable Long orderId) {
        User me = guard.me();
        OrderResponse res = orderService.dealerConfirmReceived(orderId, me);
        return ResponseEntity.ok(ApiResponse.ok(res, "Dealer xác nhận đã nhận xe thành công"));
    }

}
