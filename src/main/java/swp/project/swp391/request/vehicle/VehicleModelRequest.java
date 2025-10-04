// request/vehicle/VehicleModelRequest.java
package swp.project.swp391.request.vehicle;

import lombok.Data;

import java.math.BigDecimal;

@Data
public class VehicleModelRequest {
    private String name;
    private String modelCode;      // unique
    private String description;
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
}
