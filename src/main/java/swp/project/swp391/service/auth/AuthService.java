package swp.project.swp391.service.auth;

import swp.project.swp391.request.auth.LoginRequest;
import swp.project.swp391.request.auth.RefreshTokenRequest;
import swp.project.swp391.response.auth.LoginResponse;
import swp.project.swp391.request.auth.RegisterRequest;
import swp.project.swp391.response.auth.RefreshTokenResponse;
import swp.project.swp391.response.auth.RegisterResponse;

public interface AuthService {
    RegisterResponse register(RegisterRequest request);
    LoginResponse login(LoginRequest request);
    RefreshTokenResponse refreshToken(RefreshTokenRequest request);
    void verifyOtp(String otp);
    void requestNewOtp(String email);
}
