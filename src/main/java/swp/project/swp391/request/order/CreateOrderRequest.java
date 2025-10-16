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
    @Max(value = 60, message = "Số tháng trả góp không được quá 60 tháng")
    private Integer installmentMonths; // Số tháng trả góp

    @Size(max = 1000, message = "Ghi chú không được quá 1000 ký tự")
    private String notes;

    // ===== Chi tiết đơn hàng =====
    @NotEmpty(message = "Đơn hàng phải có ít nhất 1 sản phẩm")
    @Valid
    private List<OrderDetailRequest> orderDetails;

    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class OrderDetailRequest {

        @NotNull(message = "VehicleModelColorId không được để trống")
        private Long vehicleModelColorId;

        @NotNull(message = "Số lượng không được để trống")
        @Min(value = 1, message = "Số lượng phải lớn hơn 0")
        private Integer quantity;
    }

}