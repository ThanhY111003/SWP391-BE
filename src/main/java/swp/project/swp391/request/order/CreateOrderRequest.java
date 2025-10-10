package swp.project.swp391.request.order;

import jakarta.validation.Valid;
import jakarta.validation.constraints.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CreateOrderRequest {

    @NotNull(message = "Dealer ID không được để trống")
    private Long dealerId;

    @NotNull(message = "Phải chọn phương thức thanh toán")
    private Boolean isInstallment;

    // ===== Thông tin trả góp (nếu isInstallment = true) =====
    @DecimalMin(value = "0.0", inclusive = false, message = "Số tiền đặt cọc phải lớn hơn 0")
    private BigDecimal depositAmount; // Số tiền đặt cọc

    @Min(value = 1, message = "Số tháng trả góp phải từ 1 trở lên")
    @Max(value = 60, message = "Số tháng trả góp không được quá 60 tháng")
    private Integer installmentMonths; // Số tháng trả góp

    // ===== Thông tin đơn hàng =====
    private LocalDate expectedDeliveryDate;

    @Size(max = 1000, message = "Ghi chú không được quá 1000 ký tự")
    private String notes;

    @Size(max = 1000, message = "Ghi chú thanh toán không được quá 1000 ký tự")
    private String paymentNotes;

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
        @NotNull(message = "Vehicle Model ID không được để trống")
        private Long vehicleModelId;

        @NotNull(message = "Vehicle Color ID không được để trống")
        private Long vehicleColorId;

        @NotNull(message = "Số lượng không được để trống")
        @Min(value = 1, message = "Số lượng phải lớn hơn 0")
        private Integer quantity;
    }
}