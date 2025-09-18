package swp.project.swp391.controller.auth;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.request.auth.*;
import swp.project.swp391.response.auth.LoginResponse;
import swp.project.swp391.service.auth.AuthService;
import swp.project.swp391.response.auth.RefreshTokenResponse;
import swp.project.swp391.response.auth.RegisterResponse; // Thêm import này

import java.util.Map;

@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
@Tag(name = "Xác thực", description = "Các API quản lý xác thực người dùng")
public class AuthController {

    private final AuthService authService;

    @Operation(summary = "Đăng ký tài khoản", description = "Tạo mới một tài khoản người dùng")
    @PostMapping("/register")
    public ResponseEntity<RegisterResponse> register(@Valid @RequestBody RegisterRequest request) {
        RegisterResponse response = authService.register(request);
        return ResponseEntity.status(HttpStatus.CREATED).body(response);
    }


    @Operation(summary = "Đăng nhập", description = "Xác thực người dùng và trả về JWT token")
    @PostMapping("/login")
    public ResponseEntity<LoginResponse> login(@RequestBody @Valid LoginRequest request) {
        // Giữ nguyên
        return ResponseEntity.ok(authService.login(request));
    }

    @Operation(summary = "Làm mới Access Token", description = "Sử dụng Refresh Token để lấy Access Token mới")
    @PostMapping("/refresh-token")
    public ResponseEntity<RefreshTokenResponse> refreshToken(@RequestBody @Valid RefreshTokenRequest request) {
        // Giữ nguyên
        return ResponseEntity.ok(authService.refreshToken(request));
    }

    @PostMapping("/verify-otp")
    public ResponseEntity<?> verifyOtp(@RequestBody VerifyOtpRequest request) {
        // Giữ nguyên
        try {
            authService.verifyOtp(request.getOtp());
            return ResponseEntity.ok(Map.of("message", "Tài khoản của bạn đã được xác minh thành công!"));
        } catch (BaseException e) {
            return ResponseEntity.status(e.getErrorHandler().getStatus()).body(Map.of("message", e.getMessage()));
        }
    }

    @PostMapping("/request-otp")
    public ResponseEntity<?> requestNewOtp(@RequestBody RequestNewOtpRequest request) {
        // Giữ nguyên
        try {
            authService.requestNewOtp(request.getEmail());
            return ResponseEntity.ok(Map.of("message", "Mã OTP mới đã được gửi đến email của bạn."));
        } catch (BaseException e) {
            return ResponseEntity.status(e.getErrorHandler().getStatus()).body(Map.of("message", e.getMessage()));
        }
    }
}