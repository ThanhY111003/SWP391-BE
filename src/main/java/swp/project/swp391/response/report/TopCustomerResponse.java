package swp.project.swp391.response.report;

import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDate;

@Getter @Setter @Builder
@NoArgsConstructor @AllArgsConstructor
public class TopCustomerResponse {
    private Long customerId;
    private String customerName;
    private Long totalVehicles;
    private BigDecimal totalSpent;
    private LocalDate lastPurchase;
}
