package swp.project.swp391.response.vehicle;

import lombok.*;
import java.util.ArrayList;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class VehicleImportResult {

    private Integer totalRows;           // Tổng số dòng trong file
    private Integer successCount;        // Số xe import thành công
    private Integer failureCount;        // Số xe bị lỗi

    @Builder.Default
    private List<VehicleImportSuccess> successRecords = new ArrayList<>();

    @Builder.Default
    private List<VehicleImportError> errorRecords = new ArrayList<>();

    @Getter
    @Setter
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class VehicleImportSuccess {
        private Integer rowNumber;
        private String vin;
        private String engineNumber;
        private Long vehicleId;
        private String modelName;
        private String colorName;
    }

    @Getter
    @Setter
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class VehicleImportError {
        private Integer rowNumber;
        private String vin;
        private String engineNumber;
        private Long vehicleModelColorId;
        private String errorMessage;
    }
}