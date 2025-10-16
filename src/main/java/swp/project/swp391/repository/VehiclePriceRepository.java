package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.DealerLevel;
import swp.project.swp391.entity.VehicleModelColor;
import swp.project.swp391.entity.VehiclePrice;

import java.time.LocalDate;
import java.util.Optional;

@Repository
public interface VehiclePriceRepository extends JpaRepository<VehiclePrice, Long> {

    /**
     * Tìm giá xe theo màu xe và level dealer
     * Chỉ lấy giá đang active và trong khoảng thời gian hiệu lực
     */
    @Query("SELECT vp FROM VehiclePrice vp " +
            "WHERE vp.vehicleModelColor = :vehicleColor " +
            "AND vp.dealerLevel = :dealerLevel " +
            "AND vp.isActive = true " +
            "AND vp.effectiveFrom <= :date " +
            "AND (vp.effectiveTo IS NULL OR vp.effectiveTo >= :date) " +
            "ORDER BY vp.effectiveFrom DESC")
    Optional<VehiclePrice> findActiveByVehicleModelColorAndDealerLevel(
            @Param("vehicleColor") VehicleModelColor vehicleColor,
            @Param("dealerLevel") DealerLevel dealerLevel,
            @Param("date") LocalDate date
    );

    /**
     * Alternative: Tìm giá mới nhất cho vehicleColor và dealerLevel
     * (không check effective date)
     */
    @Query("SELECT vp FROM VehiclePrice vp " +
            "WHERE vp.vehicleModelColor = :vehicleColor " +
            "AND vp.dealerLevel = :dealerLevel " +
            "AND vp.isActive = true " +
            "ORDER BY vp.createdAt DESC " +
            "LIMIT 1")
    Optional<VehiclePrice> findLatestByVehicleModelColorAndDealerLevel(
            @Param("vehicleColor") VehicleModelColor vehicleColor,
            @Param("dealerLevel") DealerLevel dealerLevel
    );
}