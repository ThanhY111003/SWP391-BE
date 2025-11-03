package swp.project.swp391.response.vehicle;

import lombok.*;
import swp.project.swp391.entity.CustomerVehicle;

import java.math.BigDecimal;
import java.time.LocalDate;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CustomerVehicleResponse {
    private Long id;
    private String vin;
    private String modelName;
    private String colorName;
    private String customerName;
    private String dealerName;
    private String soldByUserName;
    private BigDecimal salePrice;
    private LocalDate saleDate;
    private LocalDate warrantyStartDate;
    private LocalDate warrantyEndDate;

    public static CustomerVehicleResponse fromEntity(CustomerVehicle entity) {
        return CustomerVehicleResponse.builder()
                .id(entity.getId())
                .vin(entity.getVehicleInstance().getVin())
                .modelName(entity.getVehicleInstance().getVehicleModel().getName())
                .colorName(entity.getVehicleInstance().getVehicleModelColor().getColor().getColorName())
                .customerName(entity.getCustomer().getFullName())
                .dealerName(entity.getSoldByDealer().getName())
                .soldByUserName(entity.getSoldByUser().getFullName())
                .salePrice(entity.getSalePrice())
                .saleDate(entity.getSaleDate())
                .warrantyStartDate(entity.getCustomerWarrantyStartDate())
                .warrantyEndDate(entity.getCustomerWarrantyEndDate())
                .build();
    }
}
