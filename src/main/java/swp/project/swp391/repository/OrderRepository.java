package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.Dealer;
import swp.project.swp391.entity.Order;
import swp.project.swp391.entity.User;

import java.util.List;
import java.util.Optional;

@Repository
public interface OrderRepository extends JpaRepository<Order, Long> {
    Optional<Order> findByOrderCode(String orderCode);
    List<Order> findByBuyerDealer(Dealer buyerDealer);
    List<Order> findByCreatedBy(User user);
    List<Order> findByStatus(Order.OrderStatus status);
    List<Order> findByBuyerDealerAndStatus(Dealer buyerDealer, Order.OrderStatus status);
}
