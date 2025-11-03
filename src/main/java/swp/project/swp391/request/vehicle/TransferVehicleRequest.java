package swp.project.swp391.request.vehicle;

import jakarta.validation.constraints.NotNull;
import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class TransferVehicleRequest {

    @NotNull(message = "vehicleInstanceId không được để trống")
    private Long vehicleInstanceId;

    @NotNull(message = "targetDealerId không được để trống")
    private Long targetDealerId;
}
