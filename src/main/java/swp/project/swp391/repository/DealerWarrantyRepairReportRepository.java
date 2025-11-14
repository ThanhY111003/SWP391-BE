package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.DealerWarrantyRepairReport;
import swp.project.swp391.entity.VehiclePrice;

import java.util.List;
import java.util.Optional;

@Repository
public interface DealerWarrantyRepairReportRepository extends JpaRepository<DealerWarrantyRepairReport, Long> {

    // ✅ Lấy danh sách yêu cầu theo dealer
    List<DealerWarrantyRepairReport> findByDealerId(Long dealerId);

    boolean existsByVehicleInstanceIdAndStatusIn(Long vehicleInstanceId, List<DealerWarrantyRepairReport.WarrantyStatus> statuses);

    @Query("""
    SELECT vp FROM VehiclePrice vp
    WHERE vp.vehicleModelColor.id = :colorId
      AND vp.dealerLevel.id = :levelId
      AND vp.isActive = true
      AND (vp.effectiveTo IS NULL OR vp.effectiveTo >= CURRENT_DATE)
      AND vp.effectiveFrom <= CURRENT_DATE
    ORDER BY vp.effectiveFrom DESC
    LIMIT 1
""")
    Optional<VehiclePrice> findActivePriceByColorAndDealerLevel(Long colorId, Long levelId);

}
