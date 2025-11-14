package swp.project.swp391.request.vehicle;

import lombok.Getter;
import lombok.Setter;

import java.math.BigDecimal;

@Getter
@Setter
public class VehicleModelColorUpdateRequest {
    private BigDecimal priceAdjustment;
    private String imageUrl;
}
