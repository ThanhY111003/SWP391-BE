package swp.project.swp391.serviceImp.cart;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.*;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.*;
import swp.project.swp391.request.cart.AddCartItemRequest;
import swp.project.swp391.response.cart.CartResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.cart.CartService;

import java.util.HashSet;

@Service
@RequiredArgsConstructor
public class CartServiceImpl implements CartService {

    private final CartRepository cartRepo;
    private final CartItemRepository itemRepo;
    private final VehicleModelColorRepository colorRepo;
    private final RbacGuard guard;

    @Override
    @Transactional
    public CartResponse addToCart(AddCartItemRequest req) {
        User user = guard.me();
        Dealer dealer = user.getDealer();
        if (dealer == null)
            throw new BaseException(ErrorHandler.DEALER_NOT_FOUND);

        Cart cart = cartRepo.findByUserId(user.getId())
                .orElseGet(() -> cartRepo.save(Cart.builder()
                        .user(user)
                        .dealer(dealer)
                        .build()));
        // ✅ Phòng hờ: cart cũ trong DB có thể null items
        if (cart.getItems() == null) {
            cart.setItems(new HashSet<>());
        }

        VehicleModelColor color = colorRepo.findById(req.getVehicleModelColorId())
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_MODEL_COLOR_NOT_FOUND));

        CartItem item = cart.getItems().stream()
                .filter(i -> i.getVehicleModelColor().getId().equals(req.getVehicleModelColorId()))
                .findFirst()
                .orElse(null);

        if (item == null) {
            item = CartItem.builder()
                    .cart(cart)
                    .vehicleModelColor(color)
                    .quantity(req.getQuantity())
                    .build();
            cart.getItems().add(item);
        } else {
            item.setQuantity(item.getQuantity() + req.getQuantity());
        }

        cartRepo.save(cart);
        return CartResponse.fromEntity(cart);
    }

    @Override
    @Transactional
    public CartResponse updateItemQuantity(Long itemId, int newQuantity) {
        User user = guard.me();

        // 1️⃣ Tìm item trong giỏ
        CartItem item = itemRepo.findById(itemId)
                .orElseThrow(() -> new BaseException(ErrorHandler.CART_ITEM_NOT_FOUND));

        // 2️⃣ Kiểm tra quyền sở hữu — item này phải thuộc giỏ của user hiện tại
        if (!item.getCart().getUser().getId().equals(user.getId())) {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Bạn không thể chỉnh sửa giỏ hàng của người khác.");
        }

        // 3️⃣ Validate quantity
        if (newQuantity < 0) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Số lượng không hợp lệ (phải ≥ 0).");
        }

        if (newQuantity == 0) {
            // Nếu bằng 0 → xóa luôn item khỏi giỏ
            itemRepo.delete(item);
        } else {
            // Cập nhật quantity mới
            item.setQuantity(newQuantity);
            itemRepo.save(item);
        }

        // 4️⃣ Trả lại giỏ hàng mới nhất
        Cart cart = cartRepo.findByUserId(user.getId())
                .orElseThrow(() -> new BaseException(ErrorHandler.CART_NOT_FOUND));

        return CartResponse.fromEntity(cart);
    }


    @Override
    public CartResponse getMyCart() {
        User user = guard.me();
        Cart cart = cartRepo.findByUserId(user.getId())
                .orElseThrow(() -> new BaseException(ErrorHandler.CART_NOT_FOUND));
        return CartResponse.fromEntity(cart);
    }

    @Override
    @Transactional
    public void removeItem(Long itemId) {
        CartItem item = itemRepo.findById(itemId)
                .orElseThrow(() -> new BaseException(ErrorHandler.CART_ITEM_NOT_FOUND));
        itemRepo.delete(item);
    }

    @Override
    @Transactional
    public void clearCart() {
        User user = guard.me();
        cartRepo.deleteByUserId(user.getId());
    }
}
