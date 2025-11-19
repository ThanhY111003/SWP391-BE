package swp.project.swp391.request.vehicle;

import lombok.*;
import java.time.LocalDate;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class VehicleImportRow {
    private Integer rowNumber;
    private String vin;
    private String engineNumber;
    private String modelName;        // ✅ Thay vì vehicleModelColorId
    private String colorName;        // ✅ Thêm colorName
    private LocalDate manufacturingDate;
}