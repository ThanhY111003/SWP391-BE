package swp.project.swp391.entity;

import jakarta.persistence.*;
import lombok.*;
import java.time.LocalDateTime;

@Entity
@Table(name = "dealer_warranty_repair_reports")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class DealerWarrantyRepairReport {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    /** Xe cần bảo hành */
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "vehicle_instance_id", nullable = false)
    private VehicleInstance vehicleInstance;

    @Column(name = "status", nullable = false)
    @Enumerated(EnumType.STRING)
    @Builder.Default
    private WarrantyStatus status = WarrantyStatus.PENDING;


    /** Dealer gửi yêu cầu */
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "dealer_id", nullable = false)
    private Dealer dealer;

    /** Mô tả lỗi / lý do */
    @Column(nullable = false)
    private String reason;

    private LocalDateTime createdAt;

    private LocalDateTime updatedAt;

    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
        updatedAt = createdAt;
    }

    @PreUpdate
    protected void onUpdate() {
        updatedAt = LocalDateTime.now();
    }
    public enum WarrantyStatus {
        PENDING,        // Dealer gửi yêu cầu, chờ hãng duyệt
        APPROVED,       // Hãng đã duyệt (đang sửa)
        REJECTED,       // Hãng từ chối
        CANCELLED,      // Dealer huỷ
        COMPLETED,      // Hãng sửa xong
        RECEIVED        // Dealer xác nhận đã nhận xe
    }
}
