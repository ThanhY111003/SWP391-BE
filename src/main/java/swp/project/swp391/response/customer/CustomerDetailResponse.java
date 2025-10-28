package swp.project.swp391.response.customer;

import lombok.*;
import swp.project.swp391.entity.Customer;
import swp.project.swp391.response.vehicle.CustomerVehicleResponse;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
public class CustomerDetailResponse {

    private Long id;
    private String fullName;
    private String phoneNumber;
    private String email;
    private String idNumber;
    private LocalDate dateOfBirth;
    private String gender;
    private String address;
    private String notes;
    private Boolean isActive;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    /** Danh sách xe khách hàng đã mua */
    private List<CustomerVehicleResponse> vehiclesPurchased;

    public static CustomerDetailResponse fromEntity(Customer entity) {
        return CustomerDetailResponse.builder()
                .id(entity.getId())
                .fullName(entity.getFullName())
                .phoneNumber(entity.getPhoneNumber())
                .email(entity.getEmail())
                .idNumber(entity.getIdNumber())
                .dateOfBirth(entity.getDateOfBirth())
                .gender(entity.getGender() != null ? entity.getGender().name() : null)
                .address(entity.getAddress())
                .notes(entity.getNotes())
                .isActive(entity.getIsActive())
                .createdAt(entity.getCreatedAt())
                .updatedAt(entity.getUpdatedAt())
                .vehiclesPurchased(
                        entity.getVehiclesPurchased() != null
                                ? entity.getVehiclesPurchased().stream()
                                .map(CustomerVehicleResponse::fromEntity)
                                .collect(Collectors.toList())
                                : null
                )
                .build();
    }
}
