package swp.project.swp391.controller.auth;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.request.auth.*;
import swp.project.swp391.response.auth.LoginResponse;
import swp.project.swp391.response.auth.RefreshTokenResponse;
import swp.project.swp391.service.auth.AuthService;

@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
@Tag(name = "Xác thực (B2B)", description = "Các API xác thực & quản trị tài khoản nội bộ (Admin/EVM/Dealer)")
public class AuthController {

    private final AuthService authService;

    @Operation(summary = "Đăng nhập", description = "Xác thực và trả về Access Token + Refresh Token")
    @PostMapping("/login")
    public ResponseEntity<ApiResponse<LoginResponse>> login(@Valid @RequestBody LoginRequest request) {
        LoginResponse data = authService.login(request);
        return ResponseEntity.ok(ApiResponse.ok(data, "Đăng nhập thành công"));
    }

    @Operation(summary = "Làm mới Access Token", description = "Dùng Refresh Token để lấy Access Token mới")
    @PostMapping("/refresh-token")
    public ResponseEntity<ApiResponse<RefreshTokenResponse>> refreshToken(@Valid @RequestBody RefreshTokenRequest request) {
        RefreshTokenResponse data = authService.refreshToken(request);
        return ResponseEntity.ok(ApiResponse.ok(data, "Làm mới Access Token thành công"));
    }

    @Operation(summary = "Quên mật khẩu - gửi OTP")
    @PostMapping("/forgot-password")
    public ResponseEntity<ApiResponse<Void>> forgotPassword(@Valid @RequestBody ForgotPasswordRequest request) {
        authService.forgotPassword(request.getEmail());
        return ResponseEntity.ok(ApiResponse.okMsg("OTP đặt lại mật khẩu đã được gửi về email"));
    }

    @Operation(summary = "Gửi lại OTP quên mật khẩu")
    @PostMapping("/forgot-password/resend-otp")
    public ResponseEntity<ApiResponse<Void>> resendForgotPasswordOtp(@Valid @RequestBody ForgotPasswordRequest request) {
        authService.resendForgotPasswordOtp(request.getEmail());
        return ResponseEntity.ok(ApiResponse.okMsg("OTP mới đã được gửi về email"));
    }

    @Operation(summary = "Đặt lại mật khẩu bằng OTP")
    @PostMapping("/reset-password")
    public ResponseEntity<ApiResponse<Void>> resetPassword(@Valid @RequestBody ResetPasswordRequest request) {
        authService.resetPassword(request);
        return ResponseEntity.ok(ApiResponse.okMsg("Đặt lại mật khẩu thành công"));
    }

    @Operation(summary = "Đổi mật khẩu (đã đăng nhập)")
    @PostMapping("/change-password")
    public ResponseEntity<ApiResponse<Void>> changePassword(@Valid @RequestBody ChangePasswordRequest request) {
        authService.changePassword(request);
        return ResponseEntity.ok(ApiResponse.okMsg("Đổi mật khẩu thành công"));
    }
}
