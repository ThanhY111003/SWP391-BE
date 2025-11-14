package swp.project.swp391.response.report;

import lombok.*;

@Getter @Setter @Builder @NoArgsConstructor @AllArgsConstructor
public class InventorySummaryResponse {
    private String modelName;
    private String colorName;
    private Integer total;
    private Integer available;
    private Integer reserved;
}
