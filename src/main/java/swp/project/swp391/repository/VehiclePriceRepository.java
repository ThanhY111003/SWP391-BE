package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.DealerLevel;
import swp.project.swp391.entity.VehicleModelColor;
import swp.project.swp391.entity.VehiclePrice;

import java.time.LocalDate;
import java.util.List;
import java.util.Optional;

@Repository
public interface VehiclePriceRepository extends JpaRepository<VehiclePrice, Long> {
    @Query("SELECT vp FROM VehiclePrice vp " +
            "JOIN FETCH vp.vehicleModelColor vmc " +
            "JOIN FETCH vmc.vehicleModel " +
            "JOIN FETCH vmc.color " +
            "JOIN FETCH vp.dealerLevel")
    List<VehiclePrice> findAllWithDetails();

    @Query("SELECT vp FROM VehiclePrice vp " +
            "WHERE vp.vehicleModelColor = :vehicleModelColor " +
            "AND vp.dealerLevel = :dealerLevel " +
            "AND vp.isActive = true " +
            "AND vp.effectiveFrom <= :date " +
            "AND (vp.effectiveTo IS NULL OR vp.effectiveTo >= :date)")
    Optional<VehiclePrice> findActiveByVehicleModelColorAndDealerLevel(
            @Param("vehicleModelColor") VehicleModelColor vehicleModelColor,
            @Param("dealerLevel") DealerLevel dealerLevel,
            @Param("date") LocalDate date
    );

    @Query("SELECT vp FROM VehiclePrice vp " +
            "JOIN FETCH vp.vehicleModelColor vmc " +
            "JOIN FETCH vmc.vehicleModel " +
            "JOIN FETCH vmc.color " +
            "JOIN FETCH vp.dealerLevel dl " +
            "WHERE (:dealerLevelId IS NULL OR dl.id = :dealerLevelId) " +
            "AND (:startDate IS NULL OR vp.effectiveTo >= :startDate OR vp.effectiveTo IS NULL) " +
            "AND (:endDate IS NULL OR vp.effectiveFrom <= :endDate)")
    List<VehiclePrice> findAllFiltered(@Param("dealerLevelId") Long dealerLevelId,
                                       @Param("startDate") LocalDate startDate,
                                       @Param("endDate") LocalDate endDate);

    @Query("SELECT CASE WHEN COUNT(vp) > 0 THEN true ELSE false END FROM VehiclePrice vp " +
            "WHERE vp.vehicleModelColor = :vehicleModelColor " +
            "AND vp.dealerLevel = :dealerLevel " +
            "AND (vp.effectiveTo IS NULL OR vp.effectiveTo >= :effectiveFrom) " +
            "AND (:effectiveTo IS NULL OR vp.effectiveFrom <= :effectiveTo)")
    boolean existsByOverlap(@Param("vehicleModelColor") VehicleModelColor vehicleModelColor,
                            @Param("dealerLevel") DealerLevel dealerLevel,
                            @Param("effectiveFrom") LocalDate effectiveFrom,
                            @Param("effectiveTo") LocalDate effectiveTo);

    @Query("SELECT CASE WHEN COUNT(vp) > 0 THEN true ELSE false END FROM VehiclePrice vp " +
            "WHERE vp.id <> :id " +
            "AND vp.vehicleModelColor = :vehicleModelColor " +
            "AND vp.dealerLevel = :dealerLevel " +
            "AND (vp.effectiveTo IS NULL OR vp.effectiveTo >= :effectiveFrom) " +
            "AND (:effectiveTo IS NULL OR vp.effectiveFrom <= :effectiveTo)")
    boolean existsByOverlapExcludeId(@Param("id") Long id,
                                     @Param("vehicleModelColor") VehicleModelColor vehicleModelColor,
                                     @Param("dealerLevel") DealerLevel dealerLevel,
                                     @Param("effectiveFrom") LocalDate effectiveFrom,
                                     @Param("effectiveTo") LocalDate effectiveTo);
}
