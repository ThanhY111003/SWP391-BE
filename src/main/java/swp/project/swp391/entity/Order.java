package swp.project.swp391.entity;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

@Entity
@Table(name = "orders")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Order {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "order_code", unique = true, nullable = false)
    private String orderCode;

    @Column(name = "status", nullable = false)
    @Enumerated(EnumType.STRING)
    @Builder.Default
    private OrderStatus status = OrderStatus.PENDING;

    // ===== Thông tin tiền =====
    @Column(name = "total_amount", nullable = false, precision = 15, scale = 2)
    private BigDecimal totalAmount;

    @Column(name = "deposit_amount", precision = 15, scale = 2)
    private BigDecimal depositAmount; // Số tiền cọc (nếu trả góp)

    // ===== Thông tin thanh toán =====

    @Column(name = "full_payment_date")
    private LocalDate fullPaymentDate; // Ngày thanh toán đầy đủ

    @Column(name = "payment_notes", columnDefinition = "TEXT")
    private String paymentNotes;

    @Column(name = "is_installment", nullable = false)
    @Builder.Default
    private Boolean isInstallment = false;  // Phân biệt trả thẳng hay trả góp

    // ===== Thông tin đơn hàng =====
    @Column(name = "order_date", nullable = false)
    private LocalDate orderDate;

    @Column(name = "expected_delivery_date")
    private LocalDate expectedDeliveryDate;

    @Column(name = "actual_delivery_date")
    private LocalDate actualDeliveryDate;

    @Column(columnDefinition = "TEXT")
    private String notes;

    @Column(name = "created_at")
    private LocalDateTime createdAt;

    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    // ===== Relationships =====
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "buyer_dealer_id", nullable = false)
    private Dealer buyerDealer;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "created_by_user_id", nullable = false)
    private User createdBy;

    @OneToMany(mappedBy = "order", cascade = CascadeType.ALL, fetch = FetchType.LAZY, orphanRemoval = true)
    private Set<OrderDetail> orderDetails = new HashSet<>();

    @OneToMany(mappedBy = "order", cascade = CascadeType.ALL, fetch = FetchType.LAZY, orphanRemoval = true)
    private Set<InstallmentPlan> installmentPlans = new HashSet<>();


    // ===== Phương thức tính toán (KHÔNG lưu vào DB) =====


    @Transient
    public BigDecimal getPaidAmount() {
        if (Boolean.FALSE.equals(isInstallment)) {
            // Trả thẳng: trả hết khi hoàn tất
            return (status == OrderStatus.COMPLETED) ? totalAmount : BigDecimal.ZERO;
        } else {
            // Trả góp: cọc + tổng các kỳ đã trả
            BigDecimal paidInstallments = installmentPlans == null ? BigDecimal.ZERO :
                    installmentPlans.stream()
                            .map(InstallmentPlan::getPaidAmount)
                            .reduce(BigDecimal.ZERO, BigDecimal::add);

            BigDecimal deposit = depositAmount != null ? depositAmount : BigDecimal.ZERO;
            return deposit.add(paidInstallments);
        }
    }

    @Transient
    public BigDecimal getRemainingAmount() {
        if (totalAmount == null) {
            return BigDecimal.ZERO;
        }
        // Trừ tiền cọc và số tiền đã trả
        BigDecimal remaining = totalAmount.subtract(getDepositAmount()).subtract(getPaidAmount());
        // Đảm bảo không có giá trị âm
        return remaining.max(BigDecimal.ZERO);
    }


    @Transient
    public boolean isFullyPaid() {
        return getRemainingAmount().compareTo(BigDecimal.ZERO) <= 0;
    }

    @Transient
    public BigDecimal getPaymentProgress() {
        if (totalAmount == null || totalAmount.compareTo(BigDecimal.ZERO) == 0) {
            return BigDecimal.ZERO;
        }
        return getPaidAmount()
                .divide(totalAmount, 4, BigDecimal.ROUND_HALF_UP)
                .multiply(BigDecimal.valueOf(100));
    }

    // ===== Lifecycle callbacks =====
    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
        updatedAt = LocalDateTime.now();
        if (orderDate == null) {
            orderDate = LocalDate.now();
        }
        validatePaymentType();
    }

    @PreUpdate
    protected void onUpdate() {
        updatedAt = LocalDateTime.now();
        validatePaymentType();
    }

    /**
     * Validate logic trả thẳng/trả góp
     */
    private void validatePaymentType() {
        if (Boolean.FALSE.equals(isInstallment)) {
            // Trả thẳng KHÔNG được có installment plans
            if (installmentPlans != null && !installmentPlans.isEmpty()) {
                throw new IllegalStateException(
                        "Đơn hàng trả thẳng không thể có kế hoạch trả góp"
                );
            }
        }
        // Note: Không validate ngược lại vì có thể tạo order trước, thêm plans sau
    }

    public enum OrderStatus {
        PENDING,     // Chờ xác nhận
        CONFIRMED,   // Đã xác nhận
        COMPLETED,   // Hoàn thành
        CANCELLED    // Đã hủy
    }
}