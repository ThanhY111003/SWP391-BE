package swp.project.swp391.entity;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "cart_items")
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class CartItem {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    /** Số lượng xe cùng model/color */
    @Column(nullable = false)
    private Integer quantity = 1;

    /** Liên kết đến Cart */
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "cart_id", nullable = false)
    private Cart cart;

    /** Liên kết đến VehicleModelColor (model + color) */
    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "vehicle_model_color_id", nullable = false)
    private VehicleModelColor vehicleModelColor;
}
