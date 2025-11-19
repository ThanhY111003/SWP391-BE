package swp.project.swp391.response.vehicle;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class ColorResponse {
    private Long id;
    private String colorName;
    private String hexCode;
    private Boolean isActive;
}
