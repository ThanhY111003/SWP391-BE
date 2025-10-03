package swp.project.swp391.response.vehicle;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.math.BigDecimal;

@Data
@AllArgsConstructor
public class VehicleModelResponse {
    private Long id;
    private String name;
    private String modelCode;
    private String brand;
    private Integer year;
    private BigDecimal basePrice;
    private Boolean isActive;
}
