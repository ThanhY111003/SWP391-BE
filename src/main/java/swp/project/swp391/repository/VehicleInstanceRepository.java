package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.Dealer;
import swp.project.swp391.entity.VehicleInstance;
import swp.project.swp391.entity.VehicleModel;

import java.util.List;
import java.util.Optional;

@Repository
public interface VehicleInstanceRepository extends JpaRepository<VehicleInstance, Long> {

    List<VehicleInstance> findByOrderId(Long orderId);

    boolean existsByVin(String vin);

    boolean existsByEngineNumber(String engineNumber);

    List<VehicleInstance> findByCurrentDealerIdAndVehicleModelColorId(Long dealerId, Long vehicleModelColorId);

    @Query("SELECT v FROM VehicleInstance v " +
            "LEFT JOIN FETCH v.currentDealer " +
            "LEFT JOIN FETCH v.vehicleModel " +
            "LEFT JOIN FETCH v.vehicleModelColor vmc " +
            "LEFT JOIN FETCH vmc.color " +
            "LEFT JOIN FETCH v.order o " +
            "LEFT JOIN FETCH o.buyerDealer " +
            "WHERE v.id = :id")
    Optional<VehicleInstance> findWithRelationsById(@Param("id") Long id);

    boolean existsByOrderIdAndStatusIn(Long orderId, List<VehicleInstance.VehicleStatus> statuses);

}
