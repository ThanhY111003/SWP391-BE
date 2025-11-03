package swp.project.swp391.response.cart;

import lombok.*;
import swp.project.swp391.entity.Cart;

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

    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class Item {
        private Long id;
        private String modelName;
        private String colorName;
        private Integer quantity;
    }

    public static CartResponse fromEntity(Cart entity) {
        return CartResponse.builder()
                .id(entity.getId())
                .dealerName(entity.getDealer().getName())
                .userFullName(entity.getUser().getFullName())
                .items(entity.getItems().stream().map(i ->
                        Item.builder()
                                .id(i.getId())
                                .modelName(i.getVehicleModelColor().getVehicleModel().getName())
                                .colorName(i.getVehicleModelColor().getColor().getColorName())
                                .quantity(i.getQuantity())
                                .build()
                ).collect(Collectors.toList()))
                .build();
    }
}
