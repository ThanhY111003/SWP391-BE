// service/order/OrderApprovalService.java
package swp.project.swp391.service.order;

import swp.project.swp391.entity.User;
import swp.project.swp391.response.order.OrderApproveResponse;

public interface OrderApprovalService {
    OrderApproveResponse approveOrder(Long orderId, User currentUser);
}
