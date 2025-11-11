package swp.project.swp391.service.order;


import swp.project.swp391.entity.User;
import swp.project.swp391.request.order.CreateOrderRequest;
import swp.project.swp391.response.order.OrderResponse;

public interface OrderService {

    OrderResponse createOrder(CreateOrderRequest request, Long userId);

    void cancelOrder(Long orderId);

    OrderResponse dealerConfirmReceived(Long orderId, User dealerUser);

}