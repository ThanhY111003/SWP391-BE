package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.Color;

import java.util.List;
import java.util.Optional;

@Repository
public interface ColorRepository extends JpaRepository<Color, Long> {

    // tìm theo mã màu (hex), dùng trong SampleDataInitializer
    Optional<Color> findByHexCode(String hexCode);

    boolean existsByHexCode(String hexCode);

    List<Color> findByIsActiveTrue();
}
