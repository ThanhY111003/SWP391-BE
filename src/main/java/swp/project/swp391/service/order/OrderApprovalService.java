package swp.project.swp391.service.order;

import swp.project.swp391.entity.User;
import swp.project.swp391.response.order.OrderApproveResponse;
import swp.project.swp391.response.order.OrderResponse;

public interface OrderApprovalService {
    OrderApproveResponse approveOrder(Long orderId, User currentUser);
    OrderResponse markAsShipping(Long orderId, User currentUser);
}
