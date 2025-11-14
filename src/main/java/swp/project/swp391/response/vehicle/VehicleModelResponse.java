// response/vehicle/VehicleModelResponse.java
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
    private Integer batteryCapacity;
    private Integer rangeKm;
    private Integer chargingTime;
    private Integer maxSpeed;
    private BigDecimal acceleration;
    private Integer seatingCapacity;
    private BigDecimal cargoVolume;
    private BigDecimal manufacturerPrice;
    private String imageUrl;
    private Boolean isActive;
    private String Description;
}
