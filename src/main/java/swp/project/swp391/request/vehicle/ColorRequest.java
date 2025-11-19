package swp.project.swp391.request.vehicle;

import jakarta.validation.constraints.NotBlank;
import jakarta.validation.constraints.Pattern;
import lombok.Data;

@Data
public class ColorRequest {
    @NotBlank(message = "Tên màu không được để trống.")
    private String colorName;

    @NotBlank(message = "Mã màu (hex) không được để trống.")
    @Pattern(regexp = "^#([A-Fa-f0-9]{6})$", message = "Mã màu phải hợp lệ, ví dụ: #FFFFFF")
    private String hexCode;
}
