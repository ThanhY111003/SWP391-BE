package swp.project.swp391.serviceImp.cart;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.hibernate.Hibernate;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.*;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.*;
import swp.project.swp391.request.cart.AddCartItemRequest;
import swp.project.swp391.response.cart.CartResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.cart.CartService;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.HashSet;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class CartServiceImpl implements CartService {

    private final CartRepository cartRepo;
    private final CartItemRepository itemRepo;
    private final VehicleModelColorRepository colorRepo;
    private final VehiclePriceRepository priceRepo;
    private final RbacGuard guard;
    private final DealerRepository dealerRepo;
    // ===========================================================
    // ✅ 1. Add to cart
    // ===========================================================
    @Override
    @Transactional
    public CartResponse addToCart(AddCartItemRequest req) {
        User user = guard.me();
        // ✅ Ép reload dealer để đảm bảo có session và level
        Dealer dealer = dealerRepo.findById(user.getDealer().getId())
                .orElseThrow(() -> new BaseException(ErrorHandler.DEALER_NOT_FOUND));

        // ✅ Dùng fetch join để tránh lazy khi load giỏ hàng
        Cart cart = cartRepo.findByUserIdWithAllRelations(user.getId())
                .orElseGet(() -> cartRepo.save(Cart.builder()
                        .user(user)
                        .dealer(dealer)
                        .items(new HashSet<>())
                        .build()));

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
        return buildCartResponse(cart, dealer);
    }

    // ===========================================================
    // ✅ 2. Update item quantity (+ hoặc - giống Shopee)
    // ===========================================================
    @Override
    @Transactional
    public CartResponse updateItemQuantity(Long itemId, int newQuantity) {
        User user = guard.me();

        CartItem item = itemRepo.findById(itemId)
                .orElseThrow(() -> new BaseException(ErrorHandler.CART_ITEM_NOT_FOUND));

        if (!item.getCart().getUser().getId().equals(user.getId())) {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Bạn không thể chỉnh sửa giỏ hàng của người khác.");
        }

        if (newQuantity < 0) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Số lượng không hợp lệ (phải ≥ 0).");
        }

        if (newQuantity == 0) {
            itemRepo.delete(item);
        } else {
            item.setQuantity(newQuantity);
            itemRepo.save(item);
        }

        // ✅ refetch dealer để đảm bảo có session
        Dealer dealer = dealerRepo.findById(user.getDealer().getId())
                .orElseThrow(() -> new BaseException(ErrorHandler.DEALER_NOT_FOUND));

        // ✅ fetch cart đầy đủ quan hệ
        Cart cart = cartRepo.findByUserIdWithAllRelations(user.getId())
                .orElseThrow(() -> new BaseException(ErrorHandler.CART_NOT_FOUND));

        return buildCartResponse(cart, dealer);
    }


    // ===========================================================
    // ✅ 3. Lấy giỏ hàng của chính mình
    // ===========================================================
    @Override
    @Transactional(readOnly = true)
    public CartResponse getMyCart() {
        User user = guard.me();
        Cart cart = cartRepo.findByUserIdWithAllRelations(user.getId())
                .orElseThrow(() -> new BaseException(ErrorHandler.CART_NOT_FOUND));

        return buildCartResponse(cart, cart.getDealer());
    }

    // ===========================================================
    // ✅ 4. Xóa item khỏi giỏ
    // ===========================================================
    @Override
    @Transactional
    public void removeItem(Long itemId) {
        CartItem item = itemRepo.findById(itemId)
                .orElseThrow(() -> new BaseException(ErrorHandler.CART_ITEM_NOT_FOUND));
        itemRepo.delete(item);
    }

    // ===========================================================
    // ✅ 5. Xóa toàn bộ giỏ hàng
    // ===========================================================
    @Override
    @Transactional
    public void clearCart() {
        User user = guard.me();
        cartRepo.deleteByUserId(user.getId());
    }

    // ===========================================================
    // ✅ 6. Tính giá và map về response
    // ===========================================================
    private CartResponse buildCartResponse(Cart cart, Dealer dealer) {
        Hibernate.initialize(dealer.getLevel()); // tránh LazyInitializationException

        DealerLevel dealerLevel = dealer.getLevel();
        LocalDate today = LocalDate.now();

        List<CartResponse.Item> items = cart.getItems().stream().map(i -> {
            VehicleModelColor color = i.getVehicleModelColor();

            BigDecimal unitPrice = priceRepo.findActiveByVehicleModelColorAndDealerLevel(
                            color, dealerLevel, today
                    ).map(VehiclePrice::getWholesalePrice)
                    .orElse(color.getVehicleModel().getManufacturerPrice()); // fallback

            BigDecimal totalPrice = unitPrice.multiply(BigDecimal.valueOf(i.getQuantity()));

            return CartResponse.Item.builder()
                    .id(i.getId())
                    .modelName(color.getVehicleModel().getName())
                    .colorName(color.getColor().getColorName())
                    .quantity(i.getQuantity())
                    .unitPrice(unitPrice)
                    .totalPrice(totalPrice)
                    .build();
        }).collect(Collectors.toList());

        BigDecimal cartTotal = items.stream()
                .map(CartResponse.Item::getTotalPrice)
                .reduce(BigDecimal.ZERO, BigDecimal::add);

        return CartResponse.fromEntity(cart, cartTotal, items);
    }
}
