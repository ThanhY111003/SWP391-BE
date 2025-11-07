package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.*;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Repository
public interface CustomerVehicleRepository extends JpaRepository<CustomerVehicle, Long> {
    Optional<CustomerVehicle> findByVehicleInstance(VehicleInstance vehicleInstance);
    // ✅ Lấy danh sách CustomerVehicle theo dealer và khoảng thời gian bán
    List<CustomerVehicle> findBySoldByDealer_IdAndSaleDateBetween(
            Long dealerId,
            LocalDate fromDate,
            LocalDate toDate
    );
    Long countByVehicleInstance_VehicleModelColorAndSoldByDealer_Id(VehicleModelColor color, Long dealerId);


    // ✅ (Tùy chọn) Query native tối ưu hơn khi dataset lớn
    @Query("""
        SELECT c FROM CustomerVehicle c
        WHERE (:dealerId IS NULL OR c.soldByDealer.id = :dealerId)
        AND c.saleDate BETWEEN :fromDate AND :toDate
    """)
    List<CustomerVehicle> findByDealerAndDateRange(
            @Param("dealerId") Long dealerId,
            @Param("fromDate") LocalDate fromDate,
            @Param("toDate") LocalDate toDate
    );
}
