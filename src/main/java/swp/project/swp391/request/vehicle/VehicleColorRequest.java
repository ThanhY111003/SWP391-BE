package swp.project.swp391.request.vehicle;

import lombok.Data;

import java.math.BigDecimal;

@Data
public class VehicleColorRequest {
    private String colorName;
    private String hexCode;
    private BigDecimal priceAdjustment;
    private Long vehicleModelId;
}
