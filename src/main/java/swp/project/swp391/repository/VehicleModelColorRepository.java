package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.Color;
import swp.project.swp391.entity.VehicleModel;
import swp.project.swp391.entity.VehicleModelColor;

import java.util.List;
import java.util.Optional;

@Repository
public interface VehicleModelColorRepository extends JpaRepository<VehicleModelColor, Long> {

    boolean existsByColor(Color color);

    boolean existsByVehicleModelAndColor(VehicleModel model, Color color);

    Optional<VehicleModelColor> findByVehicleModelIdAndColorId(Long modelId, Long colorId);

    List<VehicleModelColor> findByVehicleModelId(Long modelId);




}
