package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.VehicleInstance;

@Repository
public interface VehicleRepository extends JpaRepository<VehicleInstance, Long> {
    boolean existsByVin(String vin);
}
