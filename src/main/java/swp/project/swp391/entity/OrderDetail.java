package swp.project.swp391.entity;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;

@Entity
@Table(name = "order_details")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class OrderDetail {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "quantity", nullable = false)
    private Integer quantity = 1;

    @Column(name = "unit_price", nullable = false, precision = 15, scale = 2)
    private BigDecimal unitPrice;

    @Column(name = "total_price", nullable = false, precision = 15, scale = 2)
    private BigDecimal totalPrice;

    /**
     * Trạng thái chi tiết đơn hàng
     */
    @Enumerated(EnumType.STRING)
    @Column(name = "status", nullable = false)
    @Builder.Default
    private OrderDetailStatus status = OrderDetailStatus.PENDING;

    /**
     * Ghi chú xử lý (khi xe lỗi hoặc đổi xe)
     */
    @Column(name = "resolution_note", columnDefinition = "NVARCHAR(MAX)")
    private String resolutionNote;

    // ===== Relationships =====
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "order_id", nullable = false)
    private Order order;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "vehicle_model_id", nullable = false)
    private VehicleModel vehicleModel;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "vehicle_model_color_id", nullable = false)
    private VehicleModelColor vehicleModelColor;

    @Column(name = "vehicle_model_name_snapshot", nullable = false)
    private String vehicleModelNameSnapshot;  // ✅ VD: "VF 10"

    @Column(name = "vehicle_color_name_snapshot", nullable = false)
    private String vehicleColorNameSnapshot;

    // ====== Lifecycle ======
    @PrePersist
    @PreUpdate
    protected void calculateTotalPrice() {
        if (quantity != null && unitPrice != null) {
            this.totalPrice = unitPrice.multiply(BigDecimal.valueOf(quantity));
        }
    }

    // ====== ENUM ======
    public enum OrderDetailStatus {
        PENDING,     // Chờ xử lý (vừa tạo)
        CONFIRMED,    // Hãng đã duyệt
        SHIPPING,    // Đang vận chuyển
        DELIVERED,   // Đã giao thành công
        CANCELLED    // Bị hủy riêng chi tiết
    }
}
