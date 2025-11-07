package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import swp.project.swp391.entity.Order;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

public interface OrderRepository extends JpaRepository<Order, Long> {

    // Dealer xem chi tiết 1 đơn của chính mình
    Optional<Order> findByIdAndBuyerDealerId(Long orderId, Long dealerId);

    // (tuỳ chọn) nếu muốn load luôn details/plans để tránh N+1 khi build DTO
    @EntityGraph(attributePaths = {"orderDetails", "installmentPlans"})
    List<Order> findAllByBuyerDealerId(Long dealerId);

    @EntityGraph(attributePaths = {"orderDetails", "installmentPlans"})
    Optional<Order> findOneByIdAndBuyerDealerId(Long orderId, Long dealerId);

    List<Order> findByOrderDateBetween(LocalDate fromDate, LocalDate toDate);
}
