package swp.project.swp391.entity;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Entity
@Table(name = "customer_vehicles")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CustomerVehicle {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "sale_price", nullable = false, precision = 15, scale = 2)
    private BigDecimal salePrice;

    @Column(name = "sale_date", nullable = false)
    private LocalDate saleDate;

    // Bảo hành cho khách hàng (bắt đầu từ ngày mua)
    @Column(name = "customer_warranty_start_date")
    private LocalDate customerWarrantyStartDate;

    @Column(name = "customer_warranty_end_date")
    private LocalDate customerWarrantyEndDate;

    @Lob
    @Column
    private String notes;

    @Column(name = "created_at")
    private LocalDateTime createdAt;

    @OneToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "vehicle_instance_id", nullable = false, unique = true)
    private VehicleInstance vehicleInstance;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "customer_id", nullable = false)
    private Customer customer;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "sold_by_dealer_id", nullable = false)
    private Dealer soldByDealer;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "sold_by_user_id", nullable = false)
    private User soldByUser;

    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
        if (saleDate == null) {
            saleDate = LocalDate.now();
        }
    }
}