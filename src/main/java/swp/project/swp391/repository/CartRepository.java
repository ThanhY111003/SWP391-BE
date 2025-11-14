package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import swp.project.swp391.entity.Cart;

import java.util.Optional;

public interface CartRepository extends JpaRepository<Cart, Long> {
    Optional<Cart> findByUserId(Long userId);
    void deleteByUserId(Long userId);
    @Query("""
    SELECT c FROM Cart c
    JOIN FETCH c.dealer d
    JOIN FETCH d.level
    JOIN FETCH c.user
    LEFT JOIN FETCH c.items i
    LEFT JOIN FETCH i.vehicleModelColor vmc
    LEFT JOIN FETCH vmc.vehicleModel
    LEFT JOIN FETCH vmc.color
    WHERE c.user.id = :userId
""")
    Optional<Cart> findByUserIdWithAllRelations(@Param("userId") Long userId);

}
