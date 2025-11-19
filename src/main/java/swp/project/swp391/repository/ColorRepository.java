package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.Color;

import java.util.Optional;

@Repository
public interface ColorRepository extends JpaRepository<Color, Long> {


    boolean existsByHexCode(String hexCode);

    @Query("SELECT c FROM Color c " +
            "WHERE LOWER(TRIM(c.colorName)) = LOWER(TRIM(:name)) " +
            "AND c.isActive = true")
    Optional<Color> findByColorNameIgnoreCase(@Param("name") String name);
}
