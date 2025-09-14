package swp.project.swp391.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.responses.ApiResponses;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.request.LoginRequest;
import swp.project.swp391.response.LoginResponse;
import swp.project.swp391.request.RegisterRequest;
import swp.project.swp391.response.RegisterResponse;
import swp.project.swp391.service.AuthService;

import swp.project.swp391.request.RefreshTokenRequest; // Import lớp request mới
import swp.project.swp391.response.RefreshTokenResponse; // Import lớp response mới

@RestController
@RequestMapping("/api/auth")
@RequiredArgsConstructor
@Tag(name = "Xác thực", description = "Các API quản lý xác thực người dùng")
public class AuthController {

    private final AuthService authService;

    @Operation(summary = "Đăng ký tài khoản", description = "Tạo mới một tài khoản người dùng")
    @PostMapping("/register")
    public ResponseEntity<RegisterResponse> register(@RequestBody @Valid RegisterRequest request) {
        return ResponseEntity.ok(authService.register(request));
    }

    @Operation(summary = "Đăng nhập", description = "Xác thực người dùng và trả về JWT token")
    @PostMapping("/login")
    public ResponseEntity<LoginResponse> login(@RequestBody @Valid LoginRequest request) {
        return ResponseEntity.ok(authService.login(request));
    }

    @Operation(summary = "Làm mới Access Token", description = "Sử dụng Refresh Token để lấy Access Token mới")
    @PostMapping("/refresh-token")
    public ResponseEntity<RefreshTokenResponse> refreshToken(@RequestBody @Valid RefreshTokenRequest request) {
        return ResponseEntity.ok(authService.refreshToken(request));
    }
}