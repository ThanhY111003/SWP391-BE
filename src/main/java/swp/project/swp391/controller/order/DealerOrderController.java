// controller/order/DealerOrderController.java
package swp.project.swp391.controller.order;

import io.swagger.v3.oas.annotations.Operation;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.entity.User;
import swp.project.swp391.request.order.CreateOrderRequest;
import swp.project.swp391.response.order.OrderResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.order.OrderQueryService;
import swp.project.swp391.service.order.OrderService;

import java.util.List;

@RestController
@RequestMapping("/api/dealer/orders")
@RequiredArgsConstructor
public class DealerOrderController {

    private final OrderService orderService;
    private final RbacGuard guard;
    private final OrderQueryService dealerOrderQueryService; // << bean riêng cho dealer

    @Operation(summary = "Dealer tạo yêu cầu nhập xe")
    @PostMapping("/create")
    public ResponseEntity<ApiResponse<OrderResponse>> createOrder(@Valid @RequestBody CreateOrderRequest request) {
        User me = guard.me();
        OrderResponse res = orderService.createOrder(request, me.getId());
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(ApiResponse.ok(res, "Tạo yêu cầu nhập xe thành công."));
    }

    @Operation(summary = "Dealer xem danh sách đơn của chính mình")
    @GetMapping
    public ResponseEntity<ApiResponse<List<OrderResponse>>> getMyOrders() {
        User me = guard.me();
        List<OrderResponse> res = dealerOrderQueryService.getAllOrders(me);
        return ResponseEntity.ok(ApiResponse.ok(res, "Lấy danh sách đơn thành công."));
    }

    @Operation(summary = "Dealer xem chi tiết đơn của chính mình")
    @GetMapping("/{orderId}")
    public ResponseEntity<ApiResponse<OrderResponse>> getMyOrderById(@PathVariable Long orderId) {
        User me = guard.me();
        OrderResponse res = dealerOrderQueryService.getOrderById(orderId, me);
        return ResponseEntity.ok(ApiResponse.ok(res, "Lấy chi tiết đơn thành công."));
    }
}

