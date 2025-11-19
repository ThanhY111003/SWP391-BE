package swp.project.swp391.serviceImp.auth;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.Role;
import swp.project.swp391.entity.User;
import swp.project.swp391.entity.VerificationToken;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.UserRepository;
import swp.project.swp391.repository.VerificationTokenRepository;
import swp.project.swp391.request.auth.*;
import swp.project.swp391.response.auth.LoginResponse;
import swp.project.swp391.response.auth.RefreshTokenResponse;
import swp.project.swp391.security.JwtService;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.auth.AuthService;
import swp.project.swp391.service.auth.EmailService;
import java.time.LocalDateTime;
import java.util.Random;

@Slf4j
@Service
@RequiredArgsConstructor
public class AuthServiceImpl implements AuthService {
    private final VerificationTokenRepository tokenRepository;
    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;
    private final JwtService jwtService;
    private final AuthenticationManager authenticationManager;
    private final UserDetailsService userDetailsService;
    private final EmailService emailService;
    private final RbacGuard guard;

    @PersistenceContext
    private EntityManager entityManager;

    private User me() {
        return (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
    }

    private String generateOtp() {
        Random random = new Random();
        int otp = 100000 + random.nextInt(900000);
        return String.valueOf(otp);
    }

    /**
     * Login (chỉ cho staff)
     */
    @Override
    @Transactional
    public LoginResponse login(LoginRequest request) {
        try {
            Authentication authentication = authenticationManager.authenticate(
                    new UsernamePasswordAuthenticationToken(
                            request.getUsername(),
                            request.getPassword()
                    )
            );

            User user = (User) authentication.getPrincipal();

            // Check active
            // sau khi authenticate xong
            if (Boolean.FALSE.equals(user.getIsActive())) {
                throw new BaseException(ErrorHandler.ACCOUNT_BLOCKED);
            }

            String accessToken = jwtService.generateAccessToken(user);
            String refreshToken = jwtService.generateRefreshToken(user);

            user.setLastLogin(LocalDateTime.now());
            userRepository.save(user);

            return LoginResponse.builder()
                    .token(accessToken)
                    .refreshToken(refreshToken)
                    .roleName(user.getRoles().stream().findFirst().map(Role::getName).orElse(""))
                    .mustChangePassword(Boolean.TRUE.equals(user.getMustChangePassword())) // <— thêm field này
                    .message("Đăng nhập thành công")
                    .build();

        } catch (AuthenticationException ex) {
            log.warn("Login failed for user: {}", request.getUsername());
            throw new BaseException(ErrorHandler.INVALID_CREDENTIALS);
        }
    }

    /**
     * Refresh token
     */
    @Override
    public RefreshTokenResponse refreshToken(RefreshTokenRequest request) {
        String refreshToken = request.getRefreshToken();
        try {
            String username = jwtService.extractUsername(refreshToken);

            if (username != null) {
                UserDetails userDetails = userDetailsService.loadUserByUsername(username);

                if (jwtService.isTokenValid(refreshToken, userDetails)) {
                    String newAccessToken = jwtService.generateAccessToken(userDetails);
                    return RefreshTokenResponse.builder()
                            .token(newAccessToken)
                            .build();
                }
            }
        } catch (Exception e) {
            log.error("Refresh token error", e);
        }
        throw new BaseException(ErrorHandler.INVALID_TOKEN);
    }

    /**
     * Forgot password - gửi OTP
     */
    @Override
    @Transactional
    public void forgotPassword(String email) {
        User user = userRepository.findByEmail(email)
                .orElseThrow(() -> new BaseException(ErrorHandler.USER_NOT_FOUND));

        String otp = generateOtp();

        VerificationToken vt = tokenRepository.findByUser(user).orElse(null);
        if (vt == null) {
            vt = new VerificationToken();
            vt.setUser(user);
        }
        vt.setToken(otp);
        vt.setExpirationDate(LocalDateTime.now().plusMinutes(5));
        tokenRepository.save(vt);

        String emailBody = String.format("""
        <html>
          <body style="font-family: Arial, sans-serif; background-color: #f4f4f4; padding:20px;">
            <div style="max-width:500px; margin:auto; background:#fff; padding:30px; border-radius:10px; box-shadow:0 2px 8px rgba(0,0,0,0.1);">
              <h2 style="color:#2e86de; text-align:center;">Đặt lại mật khẩu</h2>
              <p style="text-align:center;">Xin chào <b>%s</b>,</p>
              <p style="text-align:center; color:#555;">Mã OTP để đặt lại mật khẩu:</p>
              <h1 style="color:#e74c3c; text-align:center; letter-spacing:5px;">%s</h1>
              <p style="text-align:center; color:#555;">Mã có hiệu lực trong 5 phút.</p>
            </div>
          </body>
        </html>
        """, user.getFullName(), otp);

        emailService.sendEmail(user.getEmail(), "Yêu cầu đặt lại mật khẩu", emailBody);
    }

    /**
     * Reset password với OTP
     */
    @Override
    @Transactional
    public void resetPassword(ResetPasswordRequest request) {
        if (!request.isPasswordConfirmed()) {
            throw new BaseException(ErrorHandler.PASSWORD_NOT_MATCH);
        }

        User user = userRepository.findByEmail(request.getEmail())
                .orElseThrow(() -> new BaseException(ErrorHandler.USER_NOT_FOUND));

        VerificationToken token = tokenRepository.findByToken(request.getOtp())
                .orElseThrow(() -> new BaseException(ErrorHandler.INVALID_TOKEN));

        if (!token.getUser().getEmail().equals(request.getEmail())) {
            throw new BaseException(ErrorHandler.INVALID_TOKEN);
        }

        if (token.getExpirationDate().isBefore(LocalDateTime.now())) {
            tokenRepository.delete(token);
            throw new BaseException(ErrorHandler.OTP_EXPIRED);
        }

        user.setPassword(passwordEncoder.encode(request.getNewPassword()));
        user.setMustChangePassword(true);
        userRepository.save(user);

        tokenRepository.delete(token);
    }

    /**
     * Change password (khi đã login)
     */
    @Override
    @Transactional
    public void changePassword(ChangePasswordRequest request) {
        User user = me();

        if (!request.isPasswordConfirmed()) {
            throw new BaseException(ErrorHandler.PASSWORD_NOT_MATCH);
        }

        if (!passwordEncoder.matches(request.getOldPassword(), user.getPassword())) {
            throw new BaseException(ErrorHandler.INVALID_OLD_PASSWORD);
        }
        user.setPassword(passwordEncoder.encode(request.getNewPassword()));
        user.setMustChangePassword(false);
        userRepository.save(user);
    }


    /**
     * Gửi lại OTP quên mật khẩu
     */
    @Override
    @Transactional
    public void resendForgotPasswordOtp(String rawEmail) {
        // Chuẩn hoá email (tránh lệch hoa/thường, khoảng trắng)
        String email = rawEmail.trim().toLowerCase();

        User user = userRepository.findByEmail(email)
                .orElseThrow(() -> new BaseException(ErrorHandler.USER_NOT_FOUND));

        LocalDateTime now = LocalDateTime.now();

        // Lấy (hoặc không) token hiện có
        VerificationToken vt = tokenRepository.findByUser(user).orElse(null);

        // Nếu còn hạn thì không cho resend
        if (vt != null && vt.getExpirationDate() != null && vt.getExpirationDate().isAfter(now)) {
            throw new BaseException(ErrorHandler.REQUEST_OTP_TOO_SOON);
        }

        // Sinh OTP mới
        String otp = generateOtp();

        // Nếu chưa có bản ghi -> tạo mới; nếu có -> cập nhật tại chỗ (KHÔNG xoá)
        if (vt == null) {
            vt = new VerificationToken();
            vt.setUser(user);
        }
        vt.setToken(otp);
        vt.setExpirationDate(now.plusMinutes(5));
        tokenRepository.save(vt); // upsert an toàn, không vi phạm unique(user_id)

        // Gửi email OTP
        String emailBody = String.format("""
        <html>
          <body style="font-family: Arial, sans-serif; background-color: #f4f4f4; padding:20px;">
            <div style="max-width:500px; margin:auto; background:#fff; padding:30px; border-radius:10px; box-shadow:0 2px 8px rgba(0,0,0,0.1);">
              <h2 style="color:#2e86de; text-align:center;">OTP mới - Đặt lại mật khẩu</h2>
              <p style="text-align:center;">Xin chào <b>%s</b>, đây là mã OTP mới để đặt lại mật khẩu:</p>
              <h1 style="color:#e74c3c; text-align:center; letter-spacing:5px;">%s</h1>
              <p style="text-align:center; color:#555;">Mã có hiệu lực trong 5 phút. Vui lòng không chia sẻ với bất kỳ ai.</p>
            </div>
          </body>
        </html>
        """, user.getFullName(), otp);

        emailService.sendEmail(user.getEmail(), "OTP mới - Đặt lại mật khẩu", emailBody);
        log.info("Resent forgot-password OTP to {}", user.getEmail());
    }


}