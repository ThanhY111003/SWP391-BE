package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.Dealer;
import swp.project.swp391.entity.Inventory;
import swp.project.swp391.entity.VehicleModel;

import jakarta.persistence.LockModeType;
import java.util.List;
import java.util.Optional;

@Repository
public interface InventoryRepository extends JpaRepository<Inventory, Long> {

    Optional<Inventory> findByDealerAndVehicleModel(Dealer dealer, VehicleModel vehicleModel);

    @Query("select i from Inventory i where i.dealer.id = :dealerId and i.vehicleModel.id = :vehicleModelId")
    Optional<Inventory> findByDealerIdAndVehicleModelId(Long dealerId, Long vehicleModelId);

    List<Inventory> findByDealer(Dealer dealer);

    List<Inventory> findByDealerId(Long dealerId);

    boolean existsByDealerAndVehicleModel(Dealer dealer, VehicleModel vehicleModel);

    // Lấy bản ghi để update số lượng một cách an toàn (tránh race condition)
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("select i from Inventory i where i.dealer.id = :dealerId and i.vehicleModel.id = :vehicleModelId")
    Optional<Inventory> lockByDealerIdAndVehicleModelId(Long dealerId, Long vehicleModelId);
}
