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
import swp.project.swp391.service.order.OrderService;

@RestController
@RequestMapping("/api/dealer/orders")
@RequiredArgsConstructor
public class DealerOrderController {

    private final OrderService orderService;
    private final RbacGuard guard;

    @Operation(
            summary = "Dealer tạo yêu cầu nhập xe",
            description = """
                Dealer chọn model, màu và số lượng -> tạo Order ở trạng thái PENDING.
                Giá: VehiclePrice (Model + DealerLevel) + chênh màu (VehicleModelColor.priceAdjustment).
                Có thể truyền deposit và installmentMonths (optional).
                """
    )
    @PostMapping("/create")
    public ResponseEntity<ApiResponse<OrderResponse>> createOrder(
            @Valid @RequestBody CreateOrderRequest request) {

        // Lấy thông tin người dùng hiện tại
        User currentUser = guard.me();

        // Tạo đơn hàng (quyền đã được kiểm tra trong service)
        OrderResponse orderResponse = orderService.createOrder(request, currentUser.getId());

        // Trả về kết quả sau khi tạo đơn hàng
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(ApiResponse.ok(orderResponse, "Tạo yêu cầu nhập xe thành công."));
    }
}
