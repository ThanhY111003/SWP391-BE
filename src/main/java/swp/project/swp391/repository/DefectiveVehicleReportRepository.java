package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.DefectiveVehicleReport;

import java.util.List;

@Repository
public interface DefectiveVehicleReportRepository extends JpaRepository<DefectiveVehicleReport, Long> {
    List<DefectiveVehicleReport> findByVehicleInstanceOrderId(Long orderId);
    boolean existsByVehicleInstanceId(Long vehicleId);
    // ✅ Kiểm tra xem xe có báo lỗi chưa được duyệt không
    boolean existsByVehicleInstanceIdAndIsApprovedFalse(Long vehicleInstanceId);

    boolean existsByVehicleInstanceIdAndIsRepairCompletedTrue(Long vehicleInstanceId);

}

