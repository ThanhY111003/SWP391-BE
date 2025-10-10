package swp.project.swp391.entity;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Entity
@Table(name = "vehicle_instances")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class VehicleInstance {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "vin", unique = true, nullable = false, length = 17)
    private String vin;

    @Column(name = "engine_number", unique = true)
    private String engineNumber;

    @Column(name = "status", nullable = false)
    @Enumerated(EnumType.STRING)
    @Builder.Default
    private VehicleStatus status = VehicleStatus.IN_STOCK;

    @Column(name = "manufacturing_date")
    private LocalDate manufacturingDate;

    @Column(name = "current_value", precision = 15, scale = 2)
    private BigDecimal currentValue;

    @Column(name = "is_active")
    private Boolean isActive = true;

    @Column(name = "created_at")
    private LocalDateTime createdAt;

    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "vehicle_model_id", nullable = false)
    private VehicleModel vehicleModel;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "vehicle_color_id", nullable = false)
    private VehicleModelColor vehicleColor;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "current_dealer_id")
    private Dealer currentDealer;

    @OneToOne(mappedBy = "vehicleInstance", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private CustomerVehicle customerVehicle;

    @PrePersist
    protected void onCreate() {
        createdAt = LocalDateTime.now();
        updatedAt = LocalDateTime.now();
    }

    @PreUpdate
    protected void onUpdate() {
        updatedAt = LocalDateTime.now();
    }

    public enum VehicleStatus {
        IN_STOCK,
        RESERVED,
        SOLD
    }
}