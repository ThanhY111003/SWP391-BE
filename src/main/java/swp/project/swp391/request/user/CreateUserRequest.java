package swp.project.swp391.request.user;

import jakarta.validation.constraints.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

// ========== Create User Request (Admin/Manager tạo user) ==========

@Data
@NoArgsConstructor
@AllArgsConstructor
public class CreateUserRequest {
    @NotBlank(message = "Email không được để trống")
    @Email(message = "Email không hợp lệ")
    private String email;

    @NotBlank(message = "Họ tên không được để trống")
    private String fullName;

    @NotBlank(message = "Số điện thoại không được để trống")
    @Pattern(regexp = "^[0-9]{10,11}$", message = "Số điện thoại phải có 10 hoặc 11 chữ số")
    private String phoneNumber;

    @NotBlank(message = "Số CMND/CCCD không được để trống")
    private String idNumber;

    @NotBlank(message = "Ngày sinh không được để trống")
    @Pattern(regexp = "^\\d{4}-\\d{2}-\\d{2}$", message = "Ngày sinh phải theo định dạng yyyy-MM-dd")
    private String dateOfBirth;

    @NotBlank(message = "Giới tính không được để trống")
    @Pattern(regexp = "^(MALE|FEMALE|OTHER)$", message = "Giới tính phải là MALE, FEMALE hoặc OTHER")
    private String gender;

    private String address;

    @NotBlank(message = "Role không được để trống")
    @Pattern(regexp = "^(EVM_STAFF|DEALER_MANAGER|DEALER_STAFF)$",
            message = "Role phải là EVM_STAFF, DEALER_MANAGER hoặc DEALER_STAFF")
    private String roleName;

    // Chỉ bắt buộc khi tạo DEALER_MANAGER hoặc DEALER_STAFF
    private Long dealerId;
}