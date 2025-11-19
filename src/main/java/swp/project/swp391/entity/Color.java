package swp.project.swp391.entity;

import jakarta.persistence.*;
import lombok.*;

@Entity
@Table(name = "colors",
        uniqueConstraints = @UniqueConstraint(name = "uk_colors_hex", columnNames = {"hex_code"}),
        indexes = @Index(name = "idx_colors_status", columnList = "is_active"))
@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class Color {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "color_name", nullable = false, length = 100)
    private String colorName;

    @Column(name = "hex_code", nullable = false, length = 7) // #FFFFFF
    private String hexCode;

    @Column(name = "is_active", nullable = false)
    private Boolean isActive = true;
}

