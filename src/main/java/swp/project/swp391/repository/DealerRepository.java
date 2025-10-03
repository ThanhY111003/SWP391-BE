package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.Dealer;
import swp.project.swp391.entity.DealerLevel;

import java.util.List;
import java.util.Optional;

@Repository
public interface DealerRepository extends JpaRepository<Dealer, Long> {
    Optional<Dealer> findByCode(String code);
    List<Dealer> findByLevel(DealerLevel level);
    List<Dealer> findByRegion(Dealer.Region region);
    List<Dealer> findByIsActive(Boolean isActive);
    // ✨ Thêm các method bạn đang dùng trong service:
    boolean existsByEmail(String email);
    boolean existsByPhoneNumber(String phoneNumber);
    Optional<Dealer> findByIdAndIsActive(Long id, Boolean isActive);

    // (tuỳ chọn, hữu ích cho validations khác)
    boolean existsByCode(String code);
    Optional<Dealer> findByEmail(String email);
    Optional<Dealer> findByPhoneNumber(String phoneNumber);
}
