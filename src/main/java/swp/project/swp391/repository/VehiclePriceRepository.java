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

public interface VehiclePriceRepository extends JpaRepository<VehiclePrice, Long> {

    @Query("""
    SELECT v FROM VehiclePrice v
    WHERE v.dealerLevel.id = :dealerLevelId
      AND v.isActive = true
      AND v.effectiveFrom <= :today
      AND (v.effectiveTo IS NULL OR v.effectiveTo >= :today)
""")
    List<VehiclePrice> findAllActiveAndEffectiveByDealerLevel(
            @Param("dealerLevelId") Long dealerLevelId,
            @Param("today") LocalDate today);


    List<VehiclePrice> findByDealerLevelId(Long dealerLevelId);

    boolean existsByVehicleModelColorAndDealerLevelAndEffectiveFrom(
            VehicleModelColor vehicleModelColor,
            DealerLevel dealerLevel,
            LocalDate effectiveFrom);

    @Query("""
        SELECT v FROM VehiclePrice v 
        WHERE v.vehicleModelColor = :vehicleModelColor 
          AND v.dealerLevel = :dealerLevel 
          AND v.isActive = true
          AND :date BETWEEN v.effectiveFrom AND COALESCE(v.effectiveTo, :date)
    """)
    Optional<VehiclePrice> findActiveByVehicleModelColorAndDealerLevel(
            @Param("vehicleModelColor") VehicleModelColor vehicleModelColor,
            @Param("dealerLevel") DealerLevel dealerLevel,
            @Param("date") LocalDate date);

    @Query("""
    SELECT CASE WHEN COUNT(vp) > 0 THEN true ELSE false END
    FROM VehiclePrice vp
    WHERE vp.vehicleModelColor = :color
      AND vp.dealerLevel = :level
      AND vp.isActive = true
      AND (
          (:effectiveTo IS NULL AND vp.effectiveTo IS NULL)
          OR (vp.effectiveTo IS NULL OR :effectiveFrom <= vp.effectiveTo)
          AND (:effectiveTo IS NULL OR :effectiveTo >= vp.effectiveFrom)
      )
""")
    boolean existsByOverlap(
            @Param("color") VehicleModelColor color,
            @Param("level") DealerLevel level,
            @Param("effectiveFrom") LocalDate effectiveFrom,
            @Param("effectiveTo") LocalDate effectiveTo
    );

    @Query("""
    SELECT CASE WHEN COUNT(vp) > 0 THEN true ELSE false END
    FROM VehiclePrice vp
    WHERE vp.id <> :excludeId
      AND vp.vehicleModelColor = :color
      AND vp.dealerLevel = :level
      AND vp.isActive = true
      AND (
          (:effectiveTo IS NULL AND vp.effectiveTo IS NULL)
          OR (vp.effectiveTo IS NULL OR :effectiveFrom <= vp.effectiveTo)
          AND (:effectiveTo IS NULL OR :effectiveTo >= vp.effectiveFrom)
      )
""")
    boolean existsByOverlapExcludeId(
            @Param("excludeId") Long excludeId,
            @Param("color") VehicleModelColor color,
            @Param("level") DealerLevel level,
            @Param("effectiveFrom") LocalDate effectiveFrom,
            @Param("effectiveTo") LocalDate effectiveTo
    );

    @Query("""
    SELECT vp FROM VehiclePrice vp
    WHERE (:dealerLevelId IS NULL OR vp.dealerLevel.id = :dealerLevelId)
      AND (:startDate IS NULL OR vp.effectiveFrom >= :startDate)
      AND (:endDate IS NULL OR (vp.effectiveTo IS NULL OR vp.effectiveTo <= :endDate))
    ORDER BY vp.dealerLevel.id, vp.effectiveFrom DESC
""")
    List<VehiclePrice> findAllFiltered(
            @Param("dealerLevelId") Long dealerLevelId,
            @Param("startDate") LocalDate startDate,
            @Param("endDate") LocalDate endDate
    );

}
