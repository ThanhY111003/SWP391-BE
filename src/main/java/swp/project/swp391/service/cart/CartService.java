package swp.project.swp391.service.cart;

import swp.project.swp391.request.cart.AddCartItemRequest;
import swp.project.swp391.response.cart.CartResponse;

public interface CartService {
    CartResponse addToCart(AddCartItemRequest req);
    CartResponse updateItemQuantity(Long itemId, int newQuantity);
    CartResponse getMyCart();
    void removeItem(Long itemId);
    void clearCart();
}
