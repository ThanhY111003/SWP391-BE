package swp.project.swp391.request.order;

import lombok.Data;
import java.math.BigDecimal;

@Data
public class ManualPaymentRequest {
    private BigDecimal paidAmount;   // số tiền dealer trả
    private String notes;            // tùy chọn
}
