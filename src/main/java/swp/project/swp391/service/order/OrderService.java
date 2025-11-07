package swp.project.swp391.service.order;


import jakarta.transaction.Transactional;
import swp.project.swp391.request.order.CreateOrderRequest;
import swp.project.swp391.response.order.OrderResponse;

public interface OrderService {

    OrderResponse createOrder(CreateOrderRequest request, Long userId);

    public void cancelOrder(Long orderId);


}