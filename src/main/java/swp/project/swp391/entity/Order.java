package swp.project.swp391.entity;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.HashSet;
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

    @Enumerated(EnumType.STRING)
    @Column(name = "status", nullable = false)
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

    @Column
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

    @Column
    private String notes;

    @Column(name = "manual_paid_amount", precision = 15, scale = 2)
    private BigDecimal manualPaidAmount;

    @Column(name = "manual_payment_attempts")
    @Builder.Default
    private Integer manualPaymentAttempts = 0;

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

    private String modelNameSnapshot;

    private String colorNameSnapshot;

    @OneToMany(mappedBy = "order", cascade = CascadeType.ALL, fetch = FetchType.LAZY, orphanRemoval = true)
    private Set<InstallmentPlan> installmentPlans = new HashSet<>();

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "vehicle_model_color_id")
    private VehicleModelColor vehicleModelColor;   // khách yêu cầu

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "vehicle_instance_id")
    private VehicleInstance assignedVehicle;
    // admin gắn xe cụ thể


    // ====== PHƯƠNG THỨC TÍNH TOÁN (Transient) ======

    @Transient
    public BigDecimal getPaidAmount() {
        // ===== TRẢ THẲNG =====
        if (!Boolean.TRUE.equals(isInstallment)) {
            BigDecimal manual = manualPaidAmount != null ? manualPaidAmount : BigDecimal.ZERO;
            return manual;
        }

        // ===== TRẢ GÓP =====

        // ✅ LOGIC MỚI: Chỉ tính deposit khi đã xác nhận (status = PAID trở đi)
        if (this.status == OrderStatus.PENDING || this.status == OrderStatus.CONFIRMED) {
            // Chưa xác nhận cọc → chưa trả gì
            return BigDecimal.ZERO;
        }

        // Đã xác nhận cọc (PAID trở đi) → tính deposit + installments đã trả
        BigDecimal paidInstallments = installmentPlans == null ? BigDecimal.ZERO :
                installmentPlans.stream()
                        .filter(plan -> plan.getPaidAmount() != null)
                        .map(InstallmentPlan::getPaidAmount)
                        .reduce(BigDecimal.ZERO, BigDecimal::add);

        BigDecimal deposit = depositAmount != null ? depositAmount : BigDecimal.ZERO;

        return deposit.add(paidInstallments);
    }


    @Transient
    public BigDecimal getRemainingAmount() {
        if (totalAmount == null) return BigDecimal.ZERO;

        // ===== TRẢ GÓP =====
        if (Boolean.TRUE.equals(isInstallment)) {
            // ✅ LOGIC MỚI:
            // - Nếu chưa xác nhận cọc → remaining = totalAmount
            // - Nếu đã xác nhận cọc → remaining = totalAmount - (deposit + installments)

            if (this.status == OrderStatus.PENDING || this.status == OrderStatus.CONFIRMED) {
                return totalAmount;  // ✅ Chưa trả gì
            }
        }

        // ===== TRẢ THẲNG HOẶC TRẢ GÓP ĐÃ XÁC NHẬN CỌC =====
        BigDecimal remaining = totalAmount.subtract(getPaidAmount());
        return remaining.max(BigDecimal.ZERO);
    }


    @Transient
    public boolean isFullyPaid() {
        return getRemainingAmount().compareTo(BigDecimal.ZERO) <= 0;
    }

    @Transient
    public BigDecimal getPaymentProgress() {
        if (totalAmount == null || totalAmount.compareTo(BigDecimal.ZERO) == 0) return BigDecimal.ZERO;
        return getPaidAmount()
                .divide(totalAmount, 4, BigDecimal.ROUND_HALF_UP)
                .multiply(BigDecimal.valueOf(100));
    }


    // ===== Lifecycle callbacks =====
    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
        updatedAt = LocalDateTime.now();
        if (orderDate == null) orderDate = LocalDate.now();
        validatePaymentType();
    }

    @PreUpdate
    protected void onUpdate() {
        updatedAt = LocalDateTime.now();
        validatePaymentType();
    }

    // ===== VALIDATION =====
    private void validatePaymentType() {
        if (Boolean.FALSE.equals(isInstallment)) {
            if (installmentPlans != null && !installmentPlans.isEmpty()) {
                throw new IllegalStateException("Đơn trả thẳng không thể có kế hoạch trả góp");
            }
        }
    }

    // ===== ENUMS =====
    public enum OrderStatus {
        PENDING,             // Dealer tạo đơn
        CONFIRMED,           // Hãng duyệt đơn
        SHIPPING,            // Hãng đang giao xe
        INSTALLMENT_ACTIVE,  // Dealer đã nhận, đang trả góp
        PARTIALLY_DELIVERED, // Có xe lỗi, đang chờ xử lý
        COMPLETED,           // Hoàn tất (trả góp xong hoặc trả thẳng)
        CANCELLED,
        PAID
    }
}
