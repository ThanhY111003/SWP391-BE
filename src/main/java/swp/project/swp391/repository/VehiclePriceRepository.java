package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.DealerLevel;
import swp.project.swp391.entity.VehicleModel;
import swp.project.swp391.entity.VehiclePrice;

import java.util.List;
import java.util.Optional;

@Repository
public interface VehiclePriceRepository extends JpaRepository<VehiclePrice, Long> {
    List<VehiclePrice> findByVehicleModel(VehicleModel vehicleModel);
    List<VehiclePrice> findByDealerLevel(DealerLevel dealerLevel);
    Optional<VehiclePrice> findByVehicleModelAndDealerLevel(VehicleModel vehicleModel, DealerLevel dealerLevel);
    List<VehiclePrice> findByIsActive(Boolean isActive);
}
