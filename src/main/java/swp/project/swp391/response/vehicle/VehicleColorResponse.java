package swp.project.swp391.response.vehicle;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.math.BigDecimal;

@Data
@AllArgsConstructor
public class VehicleColorResponse {
    private Long id;
    private String colorName;
    private String hexCode;
    private BigDecimal priceAdjustment;
    private Long vehicleModelId;
}
