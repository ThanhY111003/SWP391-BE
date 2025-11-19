package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.Dealer;
import swp.project.swp391.entity.DealerLevel;

import java.util.List;
import java.util.Optional;

@Repository
public interface DealerRepository extends JpaRepository<Dealer, Long> {

    boolean existsByEmail(String email);

    boolean existsByPhoneNumber(String phoneNumber);


    boolean existsByCode(String code);

    Optional<Dealer> findByCode(String code);
}

