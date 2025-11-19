package swp.project.swp391.request.price;

import jakarta.validation.constraints.NotNull;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDate;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class VehiclePriceRequest {
    @NotNull(message = "Giá bán buôn không được để trống")
    private BigDecimal wholesalePrice;

    @NotNull(message = "Ngày hiệu lực bắt đầu không được để trống")
    private LocalDate effectiveFrom;

    private LocalDate effectiveTo;

    @NotNull(message = "Mã màu xe theo model không được để trống")
    private Long vehicleModelColorId;

    @NotNull(message = "Mã cấp đại lý không được để trống")
    private Long dealerLevelId;
}

