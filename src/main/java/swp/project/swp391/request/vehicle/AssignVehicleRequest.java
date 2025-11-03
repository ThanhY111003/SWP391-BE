package swp.project.swp391.request.vehicle;

import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDate;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class AssignVehicleRequest {
    private Long vehicleId;
    private Long customerId;
    private BigDecimal salePrice;
    private LocalDate warrantyStartDate;
    private LocalDate warrantyEndDate;
}
