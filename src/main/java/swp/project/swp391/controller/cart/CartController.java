package swp.project.swp391.controller.cart;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.request.cart.AddCartItemRequest;
import swp.project.swp391.response.cart.CartResponse;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.service.cart.CartService;

import java.util.List;

@RestController
@RequestMapping("/api/cart")
@RequiredArgsConstructor
@Tag(name = "Cart", description = "Giỏ hàng tạm của Dealer")
public class CartController {

    private final CartService cartService;

    @Operation(summary = "Lấy giỏ hàng hiện tại của người dùng")
    @GetMapping
    public ResponseEntity<ApiResponse<CartResponse>> getMyCart() {
        return ResponseEntity.ok(ApiResponse.ok(cartService.getMyCart(), "Lấy giỏ hàng thành công"));
    }

    @Operation(summary = "Thêm xe vào giỏ hàng")
    @PostMapping("/items")
    public ResponseEntity<ApiResponse<CartResponse>> addToCart(@RequestBody AddCartItemRequest req) {
        return ResponseEntity.ok(ApiResponse.ok(cartService.addToCart(req), "Thêm xe vào giỏ hàng thành công"));
    }

    @Operation(summary = "Xoá 1 item khỏi giỏ hàng")
    @DeleteMapping("/items/{itemId}")
    public ResponseEntity<ApiResponse<Void>> removeItem(@PathVariable Long itemId) {
        cartService.removeItem(itemId);
        return ResponseEntity.ok(ApiResponse.okMsg("Đã xoá xe khỏi giỏ hàng"));
    }

    @Operation(summary = "Xoá toàn bộ giỏ hàng")
    @DeleteMapping("/clear")
    public ResponseEntity<ApiResponse<Void>> clearCart() {
        cartService.clearCart();
        return ResponseEntity.ok(ApiResponse.okMsg("Đã xoá toàn bộ giỏ hàng"));
    }
    
    @Operation(summary = "Cập nhật số lượng sản phẩm trong giỏ")
    @PutMapping("/items/{itemId}/quantity")
    public ResponseEntity<ApiResponse<CartResponse>> updateItemQuantity(
            @PathVariable Long itemId,
            @RequestParam int quantity) {

        CartResponse response = cartService.updateItemQuantity(itemId, quantity);
        return ResponseEntity.ok(ApiResponse.ok(response, "Cập nhật số lượng thành công"));
    }

}
