package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.data.jpa.repository.Query;
import org.springframework.data.repository.query.Param;
import swp.project.swp391.entity.InstallmentPlan;

import java.math.BigDecimal;
import java.util.Optional;

public interface InstallmentPlanRepository extends JpaRepository<InstallmentPlan, Long> {

    @Query("SELECT SUM(p.paidAmount) FROM InstallmentPlan p WHERE p.order.id = :orderId")
    Optional<BigDecimal> sumPaidAmountByOrderId(@Param("orderId") Long orderId);

    // ✅ tìm theo số thứ tự kỳ và đơn hàng
    Optional<InstallmentPlan> findByOrderIdAndInstallmentNumber(Long orderId, Integer installmentNumber);
}
