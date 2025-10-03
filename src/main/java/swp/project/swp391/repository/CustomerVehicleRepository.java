package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.*;

import java.util.List;
import java.util.Optional;

@Repository
public interface CustomerVehicleRepository extends JpaRepository<CustomerVehicle, Long> {
    List<CustomerVehicle> findByCustomer(Customer customer);
    Optional<CustomerVehicle> findByVehicleInstance(VehicleInstance vehicleInstance);
    List<CustomerVehicle> findBySoldByDealer(Dealer dealer);
    List<CustomerVehicle> findBySoldByUser(User user);
}
