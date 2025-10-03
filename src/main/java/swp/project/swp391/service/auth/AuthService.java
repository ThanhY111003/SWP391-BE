package swp.project.swp391.service.auth;

import swp.project.swp391.request.auth.*;
import swp.project.swp391.response.auth.LoginResponse;
import swp.project.swp391.response.auth.RefreshTokenResponse;

public interface AuthService {

    void resendForgotPasswordOtp(String email);

    LoginResponse login(LoginRequest request);

    RefreshTokenResponse refreshToken(RefreshTokenRequest request);

    void forgotPassword(String email);

    void resetPassword(ResetPasswordRequest request);

    void changePassword(ChangePasswordRequest request);
}