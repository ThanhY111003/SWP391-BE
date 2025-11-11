package swp.project.swp391.response.defective;

import lombok.*;
import swp.project.swp391.entity.VehicleInstance;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class RepairedVehicleResponse {
    private Long id;
    private String vin;
    private String engineNumber;
    private String modelName;
    private String colorName;
    private String status;
    private String currentDealerName;

    public static RepairedVehicleResponse fromEntity(VehicleInstance v) {
        return RepairedVehicleResponse.builder()
                .id(v.getId())
                .vin(v.getVin())
                .engineNumber(v.getEngineNumber())
                .modelName(v.getVehicleModel() != null ? v.getVehicleModel().getName() : null)
                .colorName(v.getVehicleModelColor() != null ? v.getVehicleModelColor().getColor().getColorName(): null)
                .status(v.getStatus() != null ? v.getStatus().name() : null)
                .currentDealerName(v.getCurrentDealer() != null ? v.getCurrentDealer().getName() : null)
                .build();
    }
}
