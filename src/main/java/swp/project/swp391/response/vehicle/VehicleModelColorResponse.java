package swp.project.swp391.response.vehicle;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.math.BigDecimal;

@Data
@AllArgsConstructor
public class VehicleModelColorResponse {
    private Long id;
    private Long vehicleModelId;
    private Long colorId;
    private String colorName;
    private String hexCode;
    private BigDecimal priceAdjustment;
    private String imageUrl;
    private Boolean isActive;
}
