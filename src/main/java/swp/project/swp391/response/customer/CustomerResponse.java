package swp.project.swp391.response.customer;

import lombok.*;
import swp.project.swp391.entity.Customer;
import java.time.LocalDate;
import java.time.LocalDateTime;

@Getter @Setter @Builder
@NoArgsConstructor @AllArgsConstructor
public class CustomerResponse {
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

    public static CustomerResponse fromEntity(Customer c) {
        return CustomerResponse.builder()
                .id(c.getId())
                .fullName(c.getFullName())
                .phoneNumber(c.getPhoneNumber())
                .email(c.getEmail())
                .idNumber(c.getIdNumber())
                .dateOfBirth(c.getDateOfBirth())
                .gender(c.getGender() != null ? c.getGender().name() : null)
                .address(c.getAddress())
                .notes(c.getNotes())
                .isActive(true) // nếu có field này
                .createdAt(c.getCreatedAt())
                .updatedAt(c.getUpdatedAt())
                .build();
    }
}
