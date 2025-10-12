package swp.project.swp391.api;

import com.fasterxml.jackson.annotation.JsonInclude;
import lombok.*;

@Getter
@Setter
@Builder
@NoArgsConstructor
@AllArgsConstructor
@JsonInclude(JsonInclude.Include.NON_NULL)
public class ApiResponse<T> {
    private boolean success;  // true/false
    private String code;      // ví dụ: OK / CREATED / VALIDATION_ERROR / ERR_...
    private String message;   // thông điệp thân thiện
    private T data;           // payload (có thể là DTO, Map lỗi, v.v.)

    /* ---------------- SUCCESS ---------------- */

    // Thành công + chỉ có data
    public static <T> ApiResponse<T> ok(T data) {
        return ApiResponse.<T>builder()
                .success(true)
                .code("OK")
                .data(data)
                .build();
    }

    // Thành công + data + message
    public static <T> ApiResponse<T> ok(T data, String message) {
        return ApiResponse.<T>builder()
                .success(true)
                .code("OK")
                .message(message)
                .data(data)
                .build();
    }

    // Thành công chỉ có message (không có data)
    public static ApiResponse<Void> okMsg(String message) {
        return ApiResponse.<Void>builder()
                .success(true)
                .code("OK")
                .message(message)
                .build();
    }

    /* ---------------- ERROR ---------------- */

    // Thất bại + message
    public static ApiResponse<Void> error(String code, String message) {
        return ApiResponse.<Void>builder()
                .success(false)
                .code(code)
                .message(message)
                .build();
    }

    // Thất bại + message + data (vd: map lỗi)
    public static <T> ApiResponse<T> error(String code, String message, T data) {
        return ApiResponse.<T>builder()
                .success(false)
                .code(code)
                .message(message)
                .data(data)
                .build();
    }
}
