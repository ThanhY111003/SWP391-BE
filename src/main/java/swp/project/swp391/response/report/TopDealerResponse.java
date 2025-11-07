package swp.project.swp391.response.report;

import lombok.*;
import java.math.BigDecimal;

@Getter @Setter @Builder @NoArgsConstructor @AllArgsConstructor
public class TopDealerResponse {
    private Long dealerId;
    private String dealerName;
    private Long totalOrders;
    private Long totalVehicles;
    private BigDecimal totalAmount;
}
