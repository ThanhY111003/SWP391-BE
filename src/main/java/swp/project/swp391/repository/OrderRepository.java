package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.EntityGraph;
import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.Order;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Repository
public interface OrderRepository extends JpaRepository<Order, Long> {

    Optional<Order> findByAssignedVehicleId(Long vehicleId);

    Optional<Order> findByIdAndBuyerDealerId(Long orderId, Long dealerId);

    // Load các quan hệ cần thiết (không còn orderDetails nữa)
    @EntityGraph(attributePaths = {
            "installmentPlans",
            "assignedVehicle",
            "vehicleModelColor",
            "buyerDealer",
            "createdBy"
    })
    List<Order> findAllByBuyerDealerId(Long dealerId);

    @EntityGraph(attributePaths = {
            "installmentPlans",
            "assignedVehicle",
            "vehicleModelColor",
            "buyerDealer",
            "createdBy"
    })
    Optional<Order> findOneByIdAndBuyerDealerId(Long orderId, Long dealerId);

    List<Order> findByOrderDateBetween(LocalDate fromDate, LocalDate toDate);
}

