package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.Order;
import swp.project.swp391.entity.OrderDetail;
import swp.project.swp391.entity.VehicleModelColor;
import swp.project.swp391.entity.VehicleModel;

import java.time.LocalDate;
import java.util.List;

@Repository
public interface OrderDetailRepository extends JpaRepository<OrderDetail, Long> {
    List<OrderDetail> findByOrderId(Long orderId);

    List<OrderDetail> findByOrder_OrderDateBetween(LocalDate fromDate, LocalDate toDate);
}
