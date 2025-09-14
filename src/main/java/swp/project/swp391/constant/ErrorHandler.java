// File: swp.project.swp391.constant.ErrorHandler.java

package swp.project.swp391.constant;

import lombok.Getter;
import org.springframework.http.HttpStatus;

@Getter
@SuppressWarnings("SpellCheckingInspection")
public enum ErrorHandler {

    // Authentication Errors for Login and Register
    USERNAME_ALREADY_EXISTS("AUTH_001", "Tên đăng nhập đã tồn tại", HttpStatus.CONFLICT),
    EMAIL_ALREADY_EXISTS("AUTH_002", "Email đã tồn tại", HttpStatus.CONFLICT),
    INVALID_CREDENTIALS("AUTH_003", "Tên đăng nhập hoặc mật khẩu không đúng", HttpStatus.UNAUTHORIZED),

    // Custom Validation Errors (for data format, etc.)
    INVALID_DATE_FORMAT("VALIDATION_001", "Định dạng ngày sinh không hợp lệ. Vui lòng sử dụng định dạng dd-MM-yyyy", HttpStatus.BAD_REQUEST),

    // General Errors
    VALIDATION_ERROR("GEN_001", "Lỗi xác thực dữ liệu", HttpStatus.BAD_REQUEST),
    INTERNAL_SERVER_ERROR("GEN_002", "Lỗi máy chủ nội bộ", HttpStatus.INTERNAL_SERVER_ERROR),

    // Role Errors
    ROLE_NOT_FOUND("ROLE_001", "Không tìm thấy vai trò", HttpStatus.NOT_FOUND),

    // User Error
    ID_NUMBER_ALREADY_EXISTS("USER_002", "Số CMND/CCCD đã tồn tại", HttpStatus.CONFLICT),
    PHONE_NUMBER_ALREADY_EXISTS("USER_001", "Số điện thoại đã tồn tại", HttpStatus.CONFLICT),

    //Token Errors
    INVALID_TOKEN("AUTH_004", "Token không hợp lệ hoặc đã hết hạn", HttpStatus.UNAUTHORIZED);

    private final String errorCode;
    private final String message;
    private final HttpStatus httpStatus;

    ErrorHandler(String errorCode, String message, HttpStatus httpStatus) {
        this.errorCode = errorCode;
        this.message = message;
        this.httpStatus = httpStatus;
    }
}