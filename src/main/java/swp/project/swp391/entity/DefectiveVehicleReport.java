package swp.project.swp391.entity;

import jakarta.persistence.*;
import lombok.*;

import java.time.LocalDateTime;

@Entity
@Table(name = "defective_vehicle_reports")
@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class DefectiveVehicleReport {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @ManyToOne(fetch = FetchType.LAZY)
    @JoinColumn(name = "vehicle_instance_id", nullable = false)
    private VehicleInstance vehicleInstance;

    @Column(name = "is_repair_completed")
    private Boolean isRepairCompleted = false;

    private String reason;

    private LocalDateTime reportedAt;

    private Boolean isApproved = false; // Hãng đã xác nhận lỗi chưa
}
