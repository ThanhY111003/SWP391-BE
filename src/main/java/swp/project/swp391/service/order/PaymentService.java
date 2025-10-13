package swp.project.swp391.service.order;

import swp.project.swp391.entity.User;
import swp.project.swp391.response.order.OrderResponse;

import java.math.BigDecimal;

public interface PaymentService {
    OrderResponse cancelOrder(Long orderId, User currentUser);
    OrderResponse confirmInstallmentPayment(Long orderId, Integer installmentNumber, User currentUser);
}


