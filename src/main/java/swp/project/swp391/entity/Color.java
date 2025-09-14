package swp.project.swp391.entity;

import jakarta.persistence.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.math.BigDecimal;
import java.time.LocalDateTime;
import java.util.List;

@Entity
@Table(name = "colors")
@Data
@NoArgsConstructor
@AllArgsConstructor
public class Color {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String name;

    @Column(name = "color_code", unique = true, nullable = false)
    private String colorCode;

    @Column(name = "hex_code")
    private String hexCode;

    @Column(name = "is_metallic")
    private Boolean isMetallic = false;

    @Column(name = "is_pearl")
    private Boolean isPearl = false;

    @Column(name = "price_adjustment")
    private BigDecimal priceAdjustment = BigDecimal.ZERO;

    @Column(name = "is_active")
    private Boolean isActive = true;

    @Column(name = "created_at")
    private LocalDateTime createdAt;

    @Column(name = "updated_at")
    private LocalDateTime updatedAt;

    // Mối quan hệ với Vehicle (One-to-Many)
    @OneToMany(mappedBy = "color", cascade = CascadeType.ALL, fetch = FetchType.LAZY)
    private List<Vehicle> vehicles;

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
