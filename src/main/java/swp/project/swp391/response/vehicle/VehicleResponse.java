package swp.project.swp391.response.vehicle;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Data
@AllArgsConstructor
public class VehicleResponse {
    private Long id;
    private String vin;
    private BigDecimal currentPrice;
    private String status;
    private String location;
    private LocalDateTime manufacturingDate;
    private LocalDateTime warrantyStartDate;
    private LocalDateTime warrantyEndDate;
    private Boolean isActive;
    private Long vehicleModelId;
    private Long vehicleColorId;
}
