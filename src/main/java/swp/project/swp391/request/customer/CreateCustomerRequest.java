package swp.project.swp391.request.customer;

import jakarta.validation.constraints.*;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

/**
 * Request tạo khách hàng mới (Customer)
 * Dành cho Dealer Manager / Dealer Staff tạo hồ sơ khách hàng
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class CreateCustomerRequest {

    @NotBlank(message = "Họ tên khách hàng không được để trống")
    private String fullName;

    @NotBlank(message = "Số điện thoại không được để trống")
    @Pattern(regexp = "^[0-9]{10,11}$", message = "Số điện thoại phải có 10 hoặc 11 chữ số")
    private String phoneNumber;

    @NotBlank(message = "Email không được để trống")
    @Email(message = "Email không hợp lệ")
    private String email;

    @NotBlank(message = "Số CMND/CCCD không được để trống")
    @Pattern(regexp = "^[0-9]{9,12}$", message = "Số CMND/CCCD phải có từ 9 đến 12 chữ số")
    private String idNumber;

    @NotBlank(message = "Ngày sinh không được để trống")
    @Pattern(regexp = "^\\d{4}-\\d{2}-\\d{2}$", message = "Ngày sinh phải theo định dạng yyyy-MM-dd")
    private String dateOfBirth;

    @NotBlank(message = "Giới tính không được để trống")
    @Pattern(regexp = "^(MALE|FEMALE|OTHER)$", message = "Giới tính phải là MALE, FEMALE hoặc OTHER")
    private String gender;

    @NotBlank(message = "Địa chỉ không được để trống")
    private String address;

    private String notes;
}
