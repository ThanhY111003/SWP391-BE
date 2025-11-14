package swp.project.swp391.response;

import lombok.AllArgsConstructor;
import lombok.Getter;
import lombok.NoArgsConstructor;
import lombok.Setter;

import java.math.BigDecimal;
import java.time.LocalDate;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
public class VehiclePriceResponse {
    private Long id;
    private BigDecimal wholesalePrice;
    private LocalDate effectiveFrom;
    private LocalDate effectiveTo;
    private boolean isActive;
    private String vehicleModelColor;
    private String dealerLevel;
}

