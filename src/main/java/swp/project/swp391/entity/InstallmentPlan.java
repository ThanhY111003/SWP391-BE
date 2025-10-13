package swp.project.swp391.entity;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Entity
@Table(name = "installment_plans")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class InstallmentPlan {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "order_id", nullable = false)
    private Order order;

    @Column(name = "installment_number", nullable = false)
    private Integer installmentNumber; // Thứ tự kỳ trả góp (1, 2, 3...)

    @Column(name = "installment_amount", nullable = false, precision = 15, scale = 2)
    private BigDecimal installmentAmount; // Số tiền phải trả mỗi kỳ

    @Column(name = "due_date", nullable = false)
    private LocalDate dueDate; // Ngày đến hạn của kỳ trả góp

    @Column(name = "paid_amount", nullable = false, precision = 15, scale = 2)
    @Builder.Default
    private BigDecimal paidAmount = BigDecimal.ZERO; // Số tiền đã trả cho kỳ này

    @Column(name = "paid_date")
    private LocalDate paidDate; // Ngày thực tế thanh toán kỳ này

    @Column(name = "status", nullable = false)
    @Enumerated(EnumType.STRING)
    @Builder.Default
    private InstallmentStatus status = InstallmentStatus.PENDING;

    @Column(name = "notes", columnDefinition = "TEXT")
    private String notes;

    @Column(name = "created_at")
    private LocalDateTime createdAt;

    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    // ===== Phương thức tính toán =====

    /**
     * Tính số tiền còn lại của kỳ này
     */
    @Transient
    public BigDecimal getRemainingAmount() {
        if (installmentAmount == null || paidAmount == null) {
            return BigDecimal.ZERO;
        }
        return installmentAmount.subtract(paidAmount);
    }

    /**
     * Kiểm tra kỳ này đã trả đủ chưa
     */
    @Transient
    public boolean isFullyPaid() {
        return getRemainingAmount().compareTo(BigDecimal.ZERO) <= 0;
    }

    /**
     * Kiểm tra đã quá hạn chưa
     */
    @Transient
    public boolean isOverdue() {
        if (status == InstallmentStatus.PAID) {
            return false;
        }
        return dueDate != null && dueDate.isBefore(LocalDate.now());
    }

    /**
     * Lấy số ngày quá hạn
     */
    @Transient
    public long getOverdueDays() {
        if (!isOverdue()) {
            return 0;
        }
        return java.time.temporal.ChronoUnit.DAYS.between(dueDate, LocalDate.now());
    }

    // ===== Lifecycle callbacks =====
    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
        updatedAt = LocalDateTime.now();
        updateStatus();
    }

    @PreUpdate
    protected void onUpdate() {
        updatedAt = LocalDateTime.now();
        updateStatus();
    }

    /**
     * Tự động cập nhật status dựa trên paid amount
     */
    private void updateStatus() {
        if (paidAmount == null || installmentAmount == null) {
            return;
        }

        if (isFullyPaid()) {
            status = InstallmentStatus.PAID;
            if (paidDate == null) {
                paidDate = LocalDate.now();
            }
        } else if (isOverdue()) {
            status = InstallmentStatus.OVERDUE;
        } else {
            status = InstallmentStatus.PENDING;
        }
    }

    public enum InstallmentStatus {
        PENDING,    // Chờ thanh toán
        PAID,       // Đã thanh toán đủ
        OVERDUE     // Quá hạn
    }
}