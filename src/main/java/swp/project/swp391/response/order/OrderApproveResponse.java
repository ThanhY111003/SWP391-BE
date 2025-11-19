// response/order/OrderApproveResponse.java
package swp.project.swp391.response.order;

import lombok.AllArgsConstructor;
import lombok.Data;

@Data
@AllArgsConstructor
public class OrderApproveResponse {
    private Long orderId;
    private String orderCode;
    private String status; // CONFIRMED
    private int created;  // số xe đã tạo & nhập vào dealer
}
