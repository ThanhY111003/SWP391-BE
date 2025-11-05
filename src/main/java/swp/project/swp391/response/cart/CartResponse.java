package swp.project.swp391.response.cart;

import lombok.*;
import swp.project.swp391.entity.Cart;

import java.math.BigDecimal;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CartResponse {
    private Long id;
    private String dealerName;
    private String userFullName;
    private List<Item> items;
    private BigDecimal cartTotal; // ✅ tổng giá trị giỏ hàng

    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class Item {
        private Long id;        
        private Long vehicleModelColorId;
        private String modelName;
        private String colorName;
        private Integer quantity;
        private BigDecimal unitPrice;  // ✅ giá 1 xe
        private BigDecimal totalPrice; // ✅ giá * số lượng
    }

    public static CartResponse fromEntity(Cart entity, BigDecimal cartTotal, List<Item> items) {
        return CartResponse.builder()
                .id(entity.getId())
                .dealerName(entity.getDealer().getName())
                .userFullName(entity.getUser().getFullName())
                .items(items)
                .cartTotal(cartTotal)
                .build();
    }
}
