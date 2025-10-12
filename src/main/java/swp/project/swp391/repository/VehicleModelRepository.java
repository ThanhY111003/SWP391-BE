package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.VehicleModel;

import java.util.List;
import java.util.Optional;

@Repository
public interface VehicleModelRepository extends JpaRepository<VehicleModel, Long> {
    Optional<VehicleModel> findByModelCode(String modelCode);
    List<VehicleModel> findByIsActiveTrue();
    List<VehicleModel> findByBrand(String brand);
    List<VehicleModel> findByYear(Integer year);
    List<VehicleModel> findByIsActive(Boolean isActive);

}
