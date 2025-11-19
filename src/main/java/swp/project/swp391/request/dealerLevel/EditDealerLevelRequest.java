package swp.project.swp391.request.dealerLevel;

import lombok.Data;
import jakarta.validation.constraints.*;

import java.math.BigDecimal;

@Data
public class EditDealerLevelRequest {

    @NotEmpty(message = "Tên cấp độ không được để trống.")
    private String levelName;              // Tên cấp độ của đại lý (có thể sửa)

    @NotNull(message = "Tỷ lệ giảm giá không được để trống.")
    @DecimalMin(value = "0.00", inclusive = true, message = "Tỷ lệ giảm giá phải lớn hơn hoặc bằng 0.")
    @DecimalMax(value = "100.00", inclusive = true, message = "Tỷ lệ giảm giá không thể vượt quá 100.")
    private BigDecimal discountRate;       // Tỷ lệ giảm giá (có thể sửa)

    @NotNull(message = "Số lượng đơn hàng tối đa không được để trống.")
    @Min(value = 1, message = "Số lượng đơn hàng tối đa phải lớn hơn hoặc bằng 1.")
    private Integer maxOrderQuantity;      // Số lượng đơn hàng tối đa (có thể sửa)

    @NotNull(message = "Tỷ lệ đặt cọc không được để trống.")
    @DecimalMin(value = "0.00", inclusive = true, message = "Tỷ lệ đặt cọc phải lớn hơn hoặc bằng 0.")
    private BigDecimal depositRate;      // Tỷ lệ đặt cọc (có thể sửa)

    @NotNull(message = "Hạn mức tín dụng không được để trống.")
    @DecimalMin(value = "0.00", inclusive = true, message = "Hạn mức tín dụng phải lớn hơn hoặc bằng 0.")
    private BigDecimal creditLimit;        // Hạn mức tín dụng (có thể sửa)

    @NotNull(message = "Số tháng trả góp tối đa không được để trống.")
    @Min(value = 0, message = "Số tháng trả góp tối đa phải >= 0.")
    @Max(value = 12, message = "Số tháng trả góp tối đa không thể vượt quá 12 tháng.")
    private Integer maxInstallmentMonths;  // Số tháng trả góp tối đa cho cấp độ

    @Size(max = 500, message = "Mô tả không được vượt quá 500 ký tự.")
    private String description;            // Mô tả (có thể sửa)
}
