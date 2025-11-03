package swp.project.swp391.entity;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;
import java.util.List;

@Entity
@Table(name = "vehicle_model_colors",
        uniqueConstraints = @UniqueConstraint(name = "uk_model_color", columnNames = {"vehicle_model_id", "color_id"}),
        indexes = {
                @Index(name = "idx_vmc_model", columnList = "vehicle_model_id"),
                @Index(name = "idx_vmc_active", columnList = "is_active")
        })
@Getter @Setter @NoArgsConstructor @AllArgsConstructor @Builder
public class VehicleModelColor {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    // model
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "vehicle_model_id",
            foreignKey = @ForeignKey(name = "fk_vmc_model"))
    private VehicleModel vehicleModel;

    // màu từ catalog
    @ManyToOne(fetch = FetchType.LAZY, optional = false)
    @JoinColumn(name = "color_id",
            foreignKey = @ForeignKey(name = "fk_vmc_color"))
    private Color color;

    @OneToMany(mappedBy = "vehicleModelColor", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<VehiclePrice> vehiclePrices;


    // chênh lệch giá theo từng model–màu
    @Column(name = "price_adjustment", precision = 18, scale = 2, nullable = false)
    @Builder.Default
    private BigDecimal priceAdjustment = BigDecimal.ZERO;

    // (tuỳ chọn) ảnh chiếc xe với màu này
    @Column(name = "image_url")
    private String imageUrl;

    @Column(name = "is_active")
    @Builder.Default
    private Boolean isActive = true;

    @OneToMany(mappedBy = "vehicleModelColor", cascade = CascadeType.ALL, orphanRemoval = true)
    private List<Inventory> inventories;

}
