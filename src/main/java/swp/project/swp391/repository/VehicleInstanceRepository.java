package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.Dealer;
import swp.project.swp391.entity.VehicleInstance;
import swp.project.swp391.entity.VehicleModel;

import java.util.List;
import java.util.Optional;

@Repository
public interface VehicleInstanceRepository extends JpaRepository<VehicleInstance, Long> {
    boolean existsByVin(String vin);
    boolean existsByEngineNumber(String engineNumber);
    List<VehicleInstance> findByIsActiveTrue();
}
