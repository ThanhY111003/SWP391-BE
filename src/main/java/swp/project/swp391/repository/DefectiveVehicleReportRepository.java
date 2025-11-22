package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.DefectiveVehicleReport;

import java.util.List;
import java.util.Optional;

@Repository
public interface DefectiveVehicleReportRepository extends JpaRepository<DefectiveVehicleReport, Long> {
    List<DefectiveVehicleReport> findByVehicleInstanceOrderId(Long orderId);
    boolean existsByVehicleInstanceId(Long vehicleId);

    Optional<DefectiveVehicleReport> findByVehicleInstanceId(Long vehicleId);

}

