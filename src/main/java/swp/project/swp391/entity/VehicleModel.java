package swp.project.swp391.entity;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "vehicle_models")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class VehicleModel {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String name;

    @Column(name = "model_code", unique = true, nullable = false)
    private String modelCode;

    @Column(columnDefinition = "TEXT")
    private String description;

    @Column(name = "brand")
    private String brand;

    @Column(name = "year")
    private Integer year;

    @Column(name = "battery_capacity")
    private Integer batteryCapacity;

    @Column(name = "range_km")
    private Integer rangeKm;

    @Column(name = "charging_time")
    private Integer chargingTime;

    @Column(name = "max_speed")
    private Integer maxSpeed;

    @Column(name = "acceleration")
    private BigDecimal acceleration;

    @Column(name = "seating_capacity")
    private Integer seatingCapacity;

    @Column(name = "cargo_volume")
    private BigDecimal cargoVolume;

    @Column(name = "manufacturer_price", precision = 15, scale = 2)
    private BigDecimal manufacturerPrice;

    @Column(name = "image_url")
    private String imageUrl;


    @Column(name = "is_active")
    private Boolean isActive = true;

    @Column(name = "created_at")
    private LocalDateTime createdAt;

    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    @OneToMany(mappedBy = "vehicleModel", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<VehicleColor> vehicleColors;

    @OneToMany(mappedBy = "vehicleModel", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<VehicleInstance> vehicleInstances;

    @OneToMany(mappedBy = "vehicleModel", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<Inventory> inventories;

    @OneToMany(mappedBy = "vehicleModel", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<VehiclePrice> vehiclePrices;

    @OneToMany(mappedBy = "vehicleModel", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<OrderDetail> orderDetails;

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