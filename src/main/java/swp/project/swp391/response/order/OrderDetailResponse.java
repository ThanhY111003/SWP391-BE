package swp.project.swp391.response.order;

import lombok.AllArgsConstructor;
import lombok.Builder;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

@Data
@Builder
@AllArgsConstructor
@NoArgsConstructor
public class OrderDetailResponse {
    private Long id;
    private String orderCode;
    private String status;
    private LocalDate orderDate;

    private DealerInfo dealer;          // thông tin đại lý mua xe
    private BigDecimal totalAmount;     // tổng tiền đơn hàng
    private BigDecimal depositAmount;   // số tiền đặt cọc
    private Integer installmentMonths;  // số tháng trả góp (nếu có)
    private String notes;               // ghi chú (nếu có)
    private List<Item> items;           // danh sách xe trong đơn

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class DealerInfo {
        private Long id;
        private String name;
        private String level;
    }

    @Data
    @AllArgsConstructor
    @NoArgsConstructor
    @Builder
    public static class Item {
        private String modelName;
        private String colorName;
        private BigDecimal unitPrice;
        private Integer quantity;
        private BigDecimal totalPrice;
    }
}
