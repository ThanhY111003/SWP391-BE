package swp.project.swp391.response.dealerLevel;

import lombok.AllArgsConstructor;
import lombok.Data;

import java.math.BigDecimal;

@Data
@AllArgsConstructor
public class DealerLevelResponse {

    private Long id;                      // ID của DealerLevel
    private String levelName;             // Tên cấp độ của đại lý
    private Integer levelNumber;          // Số thứ tự cấp độ
    private BigDecimal discountRate;      // Tỷ lệ giảm giá cho DealerLevel
    private BigDecimal depositRate;       // Tỷ lệ đặt cọc cho DealerLevel
    private Integer maxOrderQuantity;     // Số lượng đơn hàng tối đa cho cấp độ
    private BigDecimal creditLimit;       // Hạn mức tín dụng của DealerLevel
    private Integer maxInstallmentMonths; // Số tháng trả góp tối đa cho DealerLevel
    private String description;           // Mô tả về cấp độ
}
