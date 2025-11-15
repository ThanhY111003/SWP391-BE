package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.VehicleModel;

import java.util.List;
import java.util.Optional;

@Repository
public interface VehicleModelRepository extends JpaRepository<VehicleModel, Long> {
    Optional<VehicleModel> findByModelCode(String modelCode);
    List<VehicleModel> findByIsActiveTrue();
    // ✅ Tìm Model bằng tên (case-insensitive)
    @Query("SELECT vm FROM VehicleModel vm " +
            "WHERE LOWER(TRIM(vm.name)) = LOWER(TRIM(:name)) " +
            "AND vm.isActive = true")
    Optional<VehicleModel> findByNameIgnoreCase(@Param("name") String name);
}
