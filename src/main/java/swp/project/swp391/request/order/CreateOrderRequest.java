package swp.project.swp391.request.order;

import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import lombok.*;

import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CreateOrderRequest {

    @NotNull(message = "Phải chọn phương thức thanh toán")
    private Boolean isInstallment;

    @Min(value = 0, message = "Số tháng trả góp phải từ 0 trở lên")
    @Max(value = 12, message = "Số tháng trả góp không được quá 12 tháng")
    private Integer installmentMonths; // Số tháng trả góp

    @Size(max = 1000, message = "Ghi chú không được quá 1000 ký tự")
    private String notes;

    @NotNull(message = "Phải chọn mẫu xe + màu")
    private Long vehicleModelColorId;
}