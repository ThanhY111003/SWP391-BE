package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.Dealer;
import swp.project.swp391.entity.Inventory;
import swp.project.swp391.entity.VehicleModel;

import java.util.List;
import java.util.Optional;

@Repository
public interface InventoryRepository extends JpaRepository<Inventory, Long> {
    Optional<Inventory> findByDealerAndVehicleModel(Dealer dealer, VehicleModel vehicleModel);
    List<Inventory> findByDealer(Dealer dealer);
    List<Inventory> findByVehicleModel(VehicleModel vehicleModel);
    List<Inventory> findByDealerAndIsActive(Dealer dealer, Boolean isActive);
}
