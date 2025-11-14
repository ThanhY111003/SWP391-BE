package swp.project.swp391.response.warranty;

import lombok.*;
import swp.project.swp391.entity.DealerWarrantyRepairReport;
import swp.project.swp391.entity.VehicleInstance;
import java.time.LocalDateTime;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DealerWarrantyRepairResponse {

    private Long id;
    private Long vehicleId;
    private String vin;
    private String modelName;
    private String colorName;
    private String reason;
    private String status;        // ← Thay cho 3 boolean cũ
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    public static DealerWarrantyRepairResponse fromEntity(DealerWarrantyRepairReport report) {
        VehicleInstance v = report.getVehicleInstance();
        return DealerWarrantyRepairResponse.builder()
                .id(report.getId())
                .vehicleId(v.getId())
                .vin(v.getVin())
                .modelName(v.getVehicleModel().getName())
                .colorName(v.getVehicleModelColor().getColor().getColorName())
                .reason(report.getReason())
                .status(report.getStatus().name())  // Lấy tên enum
                .createdAt(report.getCreatedAt())
                .updatedAt(report.getUpdatedAt())
                .build();
    }
}
