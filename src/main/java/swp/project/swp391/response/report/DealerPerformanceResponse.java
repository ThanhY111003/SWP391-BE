package swp.project.swp391.response.report;

import lombok.*;
import java.math.BigDecimal;

@Getter @Setter @Builder @NoArgsConstructor @AllArgsConstructor
public class DealerPerformanceResponse {
    private String dealerName;
    private Long totalOrders;
    private Long completedOrders;
    private Long cancelledOrders;
    private BigDecimal revenue;
    private Double successRate;
}
