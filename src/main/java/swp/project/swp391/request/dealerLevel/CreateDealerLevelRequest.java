package swp.project.swp391.request.dealerLevel;

import lombok.Data;
import jakarta.validation.constraints.*;

import java.math.BigDecimal;

@Data
public class CreateDealerLevelRequest {

    @NotEmpty(message = "Tên cấp độ không được để trống.")
    private String levelName;  // Tên cấp độ của đại lý

    @NotNull(message = "Số thứ tự cấp độ không được để trống.")
    @Min(value = 1, message = "Số thứ tự cấp độ phải lớn hơn hoặc bằng 1.")
    private Integer levelNumber;  // Số thứ tự cấp độ

    @NotNull(message = "Tỷ lệ giảm giá không được để trống.")
    @DecimalMin(value = "0.00", inclusive = true, message = "Tỷ lệ giảm giá phải lớn hơn hoặc bằng 0.")
    @DecimalMax(value = "100.00", inclusive = true, message = "Tỷ lệ giảm giá không thể vượt quá 100.")
    private BigDecimal discountRate;  // Tỷ lệ giảm giá

    @NotNull(message = "Số lượng đơn hàng tối đa không được để trống.")
    @Min(value = 1, message = "Số lượng đơn hàng tối đa phải lớn hơn hoặc bằng 1.")
    private Integer maxOrderQuantity;  // Số lượng đơn hàng tối đa

    @NotNull(message = "Hạn mức tín dụng không được để trống.")
    @DecimalMin(value = "0.00", inclusive = true, message = "Hạn mức tín dụng phải lớn hơn hoặc bằng 0.")
    private BigDecimal creditLimit;  // Hạn mức tín dụng

    @NotNull(message = "Số tháng trả góp tối đa không được để trống.")
    @Min(value = 1, message = "Số tháng trả góp tối đa phải lớn hơn hoặc bằng 1.")
    @Max(value = 36, message = "Số tháng trả góp tối đa không thể vượt quá 36 tháng.")
    private Integer maxInstallmentMonths;  // Số tháng trả góp tối đa cho cấp độ

    @Size(max = 500, message = "Mô tả không được vượt quá 500 ký tự.")
    private String description;  // Mô tả về cấp độ
}
