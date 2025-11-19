package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.InstallmentPlan;

import java.math.BigDecimal;
import java.util.Optional;

@Repository
public interface InstallmentPlanRepository extends JpaRepository<InstallmentPlan, Long> {

    // ✅ tìm theo số thứ tự kỳ và đơn hàng
    Optional<InstallmentPlan> findByOrderIdAndInstallmentNumber(Long orderId, Integer installmentNumber);
}
