package swp.project.swp391.response.user;

import lombok.Builder;
import lombok.Data;
import swp.project.swp391.entity.User;

import java.time.LocalDate;
import java.util.Set;
import java.util.stream.Collectors;

@Data
@Builder
public class UserDetailResponse {
    private Long id;
    private String username;
    private String email;
    private String fullName;
    private String phoneNumber;
    private String idNumber;
    private LocalDate dateOfBirth;
    private String gender;
    private String address;
    private Boolean active;
    private Set<String> roles;
    private Long dealerId;
    private String dealerName;

    public static UserDetailResponse fromEntity(User user) {
        return UserDetailResponse.builder()
                .id(user.getId())
                .username(user.getUsername())
                .email(user.getEmail())
                .fullName(user.getFullName())
                .phoneNumber(user.getPhoneNumber())
                .idNumber(user.getIdNumber())
                .dateOfBirth(user.getDateOfBirth())
                .gender(user.getGender() != null ? user.getGender().name() : null)
                .address(user.getAddress())
                .active(user.getIsActive())
                .roles(user.getRoles().stream()
                        .map(r -> r.getName())
                        .collect(Collectors.toSet()))
                .dealerId(user.getDealer() != null ? user.getDealer().getId() : null)
                .dealerName(user.getDealer() != null ? user.getDealer().getName() : null)
                .build();
    }
}
