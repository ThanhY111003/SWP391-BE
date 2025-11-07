package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Lock;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.Dealer;
import swp.project.swp391.entity.Inventory;
import swp.project.swp391.entity.VehicleModel;

import jakarta.persistence.LockModeType;
import java.util.List;
import java.util.Optional;

@Repository
public interface InventoryRepository extends JpaRepository<Inventory, Long> {
    @Lock(LockModeType.PESSIMISTIC_WRITE)
    @Query("SELECT i FROM Inventory i WHERE i.dealer.id = :dealerId AND i.vehicleModelColor.id = :vehicleModelColorId")
    Optional<Inventory> lockByDealerIdAndVehicleModelColorId(Long dealerId, Long vehicleModelColorId);

    @Query("SELECT i FROM Inventory i " +
            "WHERE i.dealer.id = :dealerId " +
            "AND i.vehicleModelColor.id = :vehicleModelColorId")
    Optional<Inventory> findByDealerIdAndVehicleModelColorId(
            @Param("dealerId") Long dealerId,
            @Param("vehicleModelColorId") Long vehicleModelColorId
    );

    List<Inventory> findByDealerId(Long dealerId);

    @Query("SELECT i FROM Inventory i WHERE (:dealerId IS NULL OR i.dealer.id = :dealerId)")
    List<Inventory> findAllByDealerIdNullable(@Param("dealerId") Long dealerId);

    List<Inventory> findByDealer_Id(Long dealerId);

}
