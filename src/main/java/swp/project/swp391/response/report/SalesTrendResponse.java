package swp.project.swp391.response.report;

import lombok.*;

import java.math.BigDecimal;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class SalesTrendResponse {
    private String period;       // day / month / year / ALL_TIME
    private Long soldCount;      // Số lượng xe bán
    private BigDecimal totalRevenue; // Tổng doanh thu
}
