package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import swp.project.swp391.entity.CartItem;

public interface CartItemRepository extends JpaRepository<CartItem, Long> {

}
