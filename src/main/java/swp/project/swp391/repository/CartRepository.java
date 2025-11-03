package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import swp.project.swp391.entity.Cart;

import java.util.Optional;

public interface CartRepository extends JpaRepository<Cart, Long> {
    Optional<Cart> findByUserId(Long userId);
    void deleteByUserId(Long userId);
}
