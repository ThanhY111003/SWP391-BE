package swp.project.swp391.response.report;

import lombok.*;

import java.math.BigDecimal;

@Getter @Setter @Builder
@NoArgsConstructor @AllArgsConstructor
public class TopModelResponse {
    private String modelName;
    private String colorName;
    private Long soldCount;
    private BigDecimal totalRevenue;
}
