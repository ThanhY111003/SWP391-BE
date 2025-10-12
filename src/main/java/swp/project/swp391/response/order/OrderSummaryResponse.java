package swp.project.swp391.response.order;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;

@Data
@AllArgsConstructor
@NoArgsConstructor
public class OrderSummaryResponse {
    private Long id;
    private String orderCode;
    private String status;
    private BigDecimal totalAmount;
    private String buyerDealerName;
    private LocalDate orderDate;
}
