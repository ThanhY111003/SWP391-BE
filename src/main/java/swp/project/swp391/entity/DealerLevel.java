package swp.project.swp391.entity;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "dealer_levels")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class DealerLevel {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "level_name", unique = true, nullable = false)
    private String levelName;

    @Column(name = "level_number", unique = true, nullable = false)
    private Integer levelNumber;

    @Column(name = "discount_rate", precision = 5, scale = 2)
    private BigDecimal discountRate;

    @Column(name = "max_order_quantity")
    private Integer maxOrderQuantity;

    @Column(name = "credit_limit", precision = 15, scale = 2)
    private BigDecimal creditLimit;

    @Column(columnDefinition = "TEXT")
    private String description;

    @Column(name = "is_active")
    private Boolean isActive = true;

    @Column(name = "created_at")
    private LocalDateTime createdAt;

    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    @OneToMany(mappedBy = "level", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<Dealer> dealers;

    @OneToMany(mappedBy = "dealerLevel", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<VehiclePrice> vehiclePrices;

    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
        updatedAt = LocalDateTime.now();
    }

    @PreUpdate
    protected void onUpdate() {
        updatedAt = LocalDateTime.now();
    }
}