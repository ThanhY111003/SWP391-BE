package swp.project.swp391.response.defective;

import lombok.*;
import swp.project.swp391.entity.DefectiveVehicleReport;
import swp.project.swp391.entity.VehicleInstance;

import java.time.LocalDateTime;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DefectiveVehicleReportResponse {

    private Long id;
    private Long vehicleId;
    private String vin;
    private String engineNumber;
    private String modelName;
    private String colorName;
    private String reason;
    private Boolean isApproved;
    private Boolean isRepairCompleted;
    private LocalDateTime reportedAt;

    public static DefectiveVehicleReportResponse fromEntity(DefectiveVehicleReport report) {
        VehicleInstance v = report.getVehicleInstance();

        return DefectiveVehicleReportResponse.builder()
                .id(report.getId())
                .vehicleId(v.getId())
                .vin(v.getVin())
                .engineNumber(v.getEngineNumber())
                .modelName(v.getVehicleModel() != null ? v.getVehicleModel().getName() : null)
                .colorName(v.getVehicleModelColor() != null ? v.getVehicleModelColor().getColor().getColorName() : null)
                .reason(report.getReason())
                .isApproved(report.getIsApproved())
                .isRepairCompleted(report.getIsRepairCompleted())
                .reportedAt(report.getReportedAt())
                .build();
    }
}
