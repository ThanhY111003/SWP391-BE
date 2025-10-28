package swp.project.swp391.response.vehicle;

import lombok.*;
import swp.project.swp391.entity.CustomerVehicle;
import swp.project.swp391.entity.VehicleInstance;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CustomerVehicleResponse {

    private Long id;
    private BigDecimal salePrice;
    private LocalDate saleDate;

    private LocalDate warrantyStartDate;
    private LocalDate warrantyEndDate;

    private String notes;

    // Thông tin xe
    private String vin;
    private String modelName;
    private String colorName;

    // Đại lý và người bán
    private String dealerName;
    private String soldByUserName;

    private LocalDateTime createdAt;

    /** Ánh xạ entity → response */
    public static CustomerVehicleResponse fromEntity(CustomerVehicle cv) {
        VehicleInstance v = cv.getVehicleInstance();

        return CustomerVehicleResponse.builder()
                .id(cv.getId())
                .salePrice(cv.getSalePrice())
                .saleDate(cv.getSaleDate())
                .warrantyStartDate(cv.getCustomerWarrantyStartDate())
                .warrantyEndDate(cv.getCustomerWarrantyEndDate())
                .notes(cv.getNotes())
                .vin(v != null ? v.getVin() : null)
                .modelName(v != null && v.getVehicleModel() != null ? v.getVehicleModel().getName() : null)
                .colorName(v != null && v.getVehicleColor() != null && v.getVehicleColor().getColor() != null
                        ? v.getVehicleColor().getColor().getColorName() : null)
                .dealerName(cv.getSoldByDealer() != null ? cv.getSoldByDealer().getName() : null)
                .soldByUserName(cv.getSoldByUser() != null ? cv.getSoldByUser().getFullName() : null)
                .createdAt(cv.getCreatedAt())
                .build();
    }
}
