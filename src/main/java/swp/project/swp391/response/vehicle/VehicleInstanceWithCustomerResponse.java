package swp.project.swp391.response.vehicle;

import lombok.*;
import swp.project.swp391.entity.VehicleInstance;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class VehicleInstanceWithCustomerResponse {

    private Long id;
    private String vin;
    private String engineNumber;
    private String status;
    private String modelName;
    private String colorName;
    private String dealerName;
    private Boolean isActive;
    private LocalDate manufacturingDate;
    private BigDecimal currentValue;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    private String customerName; // chỉ thêm field này

    public static VehicleInstanceWithCustomerResponse fromEntity(VehicleInstance entity) {

        String customerName = null;

        if (entity.getCustomerVehicle() != null &&
                entity.getCustomerVehicle().getCustomer() != null) {

            customerName = entity.getCustomerVehicle().getCustomer().getFullName();
        }

        return VehicleInstanceWithCustomerResponse.builder()
                .id(entity.getId())
                .vin(entity.getVin())
                .engineNumber(entity.getEngineNumber())
                .status(entity.getStatus().name())
                .modelName(entity.getVehicleModel().getName())
                .colorName(entity.getVehicleModelColor().getColor().getColorName())
                .dealerName(entity.getCurrentDealer() != null ? entity.getCurrentDealer().getName() : null)
                .isActive(entity.getIsActive())
                .manufacturingDate(entity.getManufacturingDate())
                .currentValue(entity.getCurrentValue())
                .createdAt(entity.getCreatedAt())
                .updatedAt(entity.getUpdatedAt())
                .customerName(customerName)
                .build();
    }
}
