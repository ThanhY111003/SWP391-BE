package swp.project.swp391.entity;

import jakarta.persistence.*;
import lombok.*;

import java.math.BigDecimal;

@Entity
@Table(name = "vehicle_colors")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class VehicleColor {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    private String colorName;

    private String hexCode; // mã màu, VD: #FFFFFF

    @Column(name = "price_adjustment")
    private BigDecimal priceAdjustment; // chênh lệch giá (+/-)

    @ManyToOne
    @JoinColumn(name = "vehicle_model_id")
    private VehicleModel vehicleModel;

    @Column(name = "is_active", nullable = false)
    private Boolean isActive;
}

