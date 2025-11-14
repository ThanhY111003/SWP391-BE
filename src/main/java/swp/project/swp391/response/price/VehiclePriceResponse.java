package swp.project.swp391.response.price;

import lombok.*;
import swp.project.swp391.entity.VehiclePrice;

import java.math.BigDecimal;
import java.time.LocalDate;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class VehiclePriceResponse {
    private Long id;
    private BigDecimal wholesalePrice;
    private LocalDate effectiveFrom;
    private LocalDate effectiveTo;
    private Boolean isActive;
    private String vehicleModelName;
    private String colorName;
    private String dealerLevelName;

    public static VehiclePriceResponse fromEntity(VehiclePrice vp) {
        return VehiclePriceResponse.builder()
                .id(vp.getId())
                .wholesalePrice(vp.getWholesalePrice())
                .effectiveFrom(vp.getEffectiveFrom())
                .effectiveTo(vp.getEffectiveTo())
                .isActive(vp.getIsActive())
                .vehicleModelName(vp.getVehicleModelColor().getVehicleModel().getName())
                .colorName(vp.getVehicleModelColor().getColor().getColorName())
                .dealerLevelName(vp.getDealerLevel().getLevelName())
                .build();
    }
}

