package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.DealerLevel;

import java.util.Optional;

@Repository
public interface DealerLevelRepository extends JpaRepository<DealerLevel, Long> {
    Optional<DealerLevel> findByLevelNumber(Integer levelNumber);
    Optional<DealerLevel> findByLevelName(String levelName);
}
