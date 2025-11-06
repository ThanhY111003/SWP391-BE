package swp.project.swp391.entity;

import jakarta.persistence.*;
import lombok.*;
import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "dealers")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Dealer {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String name;

    @Column(unique = true, nullable = false)
    private String code;

    @Column(columnDefinition = "NVARCHAR(MAX)")
    private String address;

    @Column(name = "phone_number")
    private String phoneNumber;

    @Column(name = "email")
    private String email;

    @Column(name = "region")
    @Enumerated(EnumType.STRING)
    private Region region;

    @Column(name = "current_debt", precision = 15, scale = 2)
    private BigDecimal currentDebt = BigDecimal.ZERO;

    @Column(name = "available_credit", precision = 15, scale = 2)
    private BigDecimal availableCredit;

    @Column(name = "is_active")
    private Boolean isActive = true;

    @Column(name = "created_at")
    private LocalDateTime createdAt;

    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "level_id", nullable = false)
    private DealerLevel level;

    @OneToMany(mappedBy = "dealer", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<User> users;

    @OneToMany(mappedBy = "buyerDealer", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<Order> purchaseOrders;

    @OneToMany(mappedBy = "dealer", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<Inventory> inventories;

    @OneToMany(mappedBy = "currentDealer", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<VehicleInstance> vehicleInstances;

    @OneToMany(mappedBy = "soldByDealer", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<CustomerVehicle> vehicleSales;

    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
        updatedAt = LocalDateTime.now();
        if (currentDebt == null) {
            currentDebt = BigDecimal.ZERO;
        }
        calculateAvailableCredit();
    }

    @PreUpdate
    protected void onUpdate() {
        updatedAt = LocalDateTime.now();
        calculateAvailableCredit();
    }

    private void calculateAvailableCredit() {
        if (level != null && level.getCreditLimit() != null) {
            this.availableCredit = level.getCreditLimit().subtract(
                    currentDebt != null ? currentDebt : BigDecimal.ZERO
            );
        }
    }

    public enum Region {
        NORTH,
        CENTRAL,
        SOUTH
    }
}