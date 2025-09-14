package swp.project.swp391.service;

import swp.project.swp391.request.LoginRequest;
import swp.project.swp391.request.RefreshTokenRequest;
import swp.project.swp391.response.LoginResponse;
import swp.project.swp391.request.RegisterRequest;
import swp.project.swp391.response.RefreshTokenResponse;
import swp.project.swp391.response.RegisterResponse;

public interface AuthService {
    RegisterResponse register(RegisterRequest request);
    LoginResponse login(LoginRequest request);
    RefreshTokenResponse refreshToken(RefreshTokenRequest request);
}
