package swp.project.swp391.request.vehicle;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import lombok.Getter;
import lombok.Setter;

import java.time.LocalDate;

@Getter
@Setter
public class VehicleInstanceUpdateRequest {

    @NotBlank(message = "VIN không được để trống")
    @Pattern(
            regexp = "^[A-HJ-NPR-Z0-9]{17}$",
            message = "VIN phải có 17 ký tự, chỉ gồm chữ/số, không chứa I, O, Q"
    )
    private String vin;

    @NotBlank(message = "Số máy không được để trống")
    private String engineNumber;

    private LocalDate manufacturingDate;

    private Long vehicleModelColorId;
}
