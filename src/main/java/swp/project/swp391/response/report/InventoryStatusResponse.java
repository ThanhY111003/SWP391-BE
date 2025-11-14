package swp.project.swp391.response.report;

import lombok.*;

@Getter @Setter @Builder
@NoArgsConstructor @AllArgsConstructor
public class InventoryStatusResponse {
    private String modelName;
    private String colorName;
    private Integer totalStock;
    private Integer available;
    private Integer reserved;
    private Long soldCount;
}
