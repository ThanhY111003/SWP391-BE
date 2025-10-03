package swp.project.swp391.entity;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;

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
    private OrderStatus status = OrderStatus.DRAFT;

    // ===== Thông tin tiền =====
    @Column(name = "total_amount", nullable = false, precision = 15, scale = 2)
    private BigDecimal totalAmount;

    @Column(name = "deposit_amount", precision = 15, scale = 2)
    private BigDecimal depositAmount = BigDecimal.ZERO;

    @Column(name = "paid_amount", precision = 15, scale = 2)
    private BigDecimal paidAmount = BigDecimal.ZERO;

    @Column(name = "remaining_amount", precision = 15, scale = 2)
    private BigDecimal remainingAmount;

    // ===== Thông tin ngày thanh toán =====
    @Column(name = "deposit_paid_date")
    private LocalDate depositPaidDate;

    @Column(name = "full_payment_date")
    private LocalDate fullPaymentDate;

    @Column(name = "payment_due_date")
    private LocalDate paymentDueDate;

    @Column(name = "payment_notes", columnDefinition = "TEXT")
    private String paymentNotes;

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
    private List<OrderDetail> orderDetails;

    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
        updatedAt = LocalDateTime.now();
        if (orderDate == null) {
            orderDate = LocalDate.now();
        }
        calculateRemainingAmount();
    }

    @PreUpdate
    protected void onUpdate() {
        updatedAt = LocalDateTime.now();
        calculateRemainingAmount();
    }

    private void calculateRemainingAmount() {
        if (totalAmount != null && paidAmount != null) {
            this.remainingAmount = totalAmount.subtract(paidAmount);
        }
    }

    public enum OrderStatus {
        DRAFT,              // Nháp
        PENDING,            // Chờ xác nhận
        CONFIRMED,          // Đã xác nhận
        DEPOSIT_PAID,       // Đã đặt cọc
        IN_PRODUCTION,      // Đang sản xuất
        IN_TRANSIT,         // Đang vận chuyển
        DELIVERED,          // Đã giao hàng
        PAID,               // Đã thanh toán đủ
        COMPLETED,          // Hoàn thành
        CANCELLED           // Đã hủy
    }
}