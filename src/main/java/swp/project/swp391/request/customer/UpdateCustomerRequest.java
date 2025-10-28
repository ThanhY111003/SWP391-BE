package swp.project.swp391.request.customer;

import jakarta.validation.constraints.Pattern;
import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class UpdateCustomerRequest {

    private String fullName;

    @Pattern(regexp = "^[0-9]{10,11}$", message = "Số điện thoại phải có 10 hoặc 11 chữ số")
    private String phoneNumber;

    @Pattern(regexp = "^[\\w.%+-]+@[\\w.-]+\\.[a-zA-Z]{2,6}$", message = "Email không hợp lệ")
    private String email;

    @Pattern(regexp = "^[0-9]{9,12}$", message = "Số CMND/CCCD phải có từ 9 đến 12 chữ số")
    private String idNumber;

    @Pattern(regexp = "^\\d{4}-\\d{2}-\\d{2}$", message = "Ngày sinh phải theo định dạng yyyy-MM-dd")
    private String dateOfBirth;

    @Pattern(regexp = "^(MALE|FEMALE|OTHER)$", message = "Giới tính phải là MALE, FEMALE hoặc OTHER")
    private String gender;

    private String address;
    private String notes;
}
