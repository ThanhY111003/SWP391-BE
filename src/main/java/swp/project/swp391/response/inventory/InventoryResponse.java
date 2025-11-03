package swp.project.swp391.response.inventory;

import lombok.*;
import swp.project.swp391.entity.Inventory;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class InventoryResponse {
    private Long id;
    private String dealerName;
    private String modelName;
    private String colorName;
    private Integer totalQuantity;
    private Integer reservedQuantity;
    private Integer availableQuantity;
    private Boolean isActive;

    public static InventoryResponse fromEntity(Inventory entity) {
        return InventoryResponse.builder()
                .id(entity.getId())
                .dealerName(entity.getDealer().getName())
                .modelName(entity.getVehicleModelColor().getVehicleModel().getName())
                .colorName(entity.getVehicleModelColor().getColor().getColorName())
                .totalQuantity(entity.getTotalQuantity())
                .reservedQuantity(entity.getReservedQuantity())
                .availableQuantity(entity.getAvailableQuantity())
                .isActive(entity.getIsActive())
                .build();
    }
}

