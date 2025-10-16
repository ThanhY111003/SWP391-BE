package swp.project.swp391.service.order;

import swp.project.swp391.entity.User;
import swp.project.swp391.response.order.OrderResponse;

import java.util.List;

public interface OrderQueryService {
    List<OrderResponse> getAllOrders(User currentUser);                 // có thể thêm lọc/paging sau
    OrderResponse getOrderById(Long id, User currentUser);


}

