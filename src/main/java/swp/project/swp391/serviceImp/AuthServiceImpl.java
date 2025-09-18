package swp.project.swp391.serviceImp;

import jakarta.persistence.EntityManager;
import jakarta.persistence.PersistenceContext;
import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.AuthenticationException;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.Customer;
import swp.project.swp391.entity.Role;
import swp.project.swp391.entity.User;
import swp.project.swp391.entity.VerificationToken;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.CustomerRepository;
import swp.project.swp391.repository.RoleRepository;
import swp.project.swp391.repository.UserRepository;
import swp.project.swp391.repository.VerificationTokenRepository;
import swp.project.swp391.request.LoginRequest;
import swp.project.swp391.request.RefreshTokenRequest;
import swp.project.swp391.request.RegisterRequest;
import swp.project.swp391.response.LoginResponse;
import swp.project.swp391.response.RefreshTokenResponse;
import swp.project.swp391.response.RegisterResponse;
import swp.project.swp391.security.JwtService;
import swp.project.swp391.service.AuthService;
import swp.project.swp391.service.EmailService;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.HashSet;
import java.util.Random;
import java.util.Set;

@Slf4j
@Service
@RequiredArgsConstructor
public class AuthServiceImpl implements AuthService {
    private final VerificationTokenRepository tokenRepository;
    private final CustomerRepository customerRepository;
    private final UserRepository userRepository;
    private final RoleRepository roleRepository;
    private final PasswordEncoder passwordEncoder;
    private final JwtService jwtService;
    private final AuthenticationManager authenticationManager;
    private final UserDetailsService userDetailsService;
    private final EmailService emailService;
    @PersistenceContext
    private EntityManager entityManager;

    // Helper method to generate a 6-digit OTP
    private String generateOtp() {
        Random random = new Random();
        int otp = 100000 + random.nextInt(900000);
        return String.valueOf(otp);
    }

    @Override
    @Transactional
    public RegisterResponse register(RegisterRequest request) {

        // 1. Kiểm tra sự tồn tại của tài khoản
        if (userRepository.findByEmail(request.getEmail()).isPresent()) {
            throw new BaseException(ErrorHandler.EMAIL_ALREADY_EXISTS);
        }
        if (userRepository.findByUsername(request.getUsername()).isPresent()) {
            throw new BaseException(ErrorHandler.USERNAME_ALREADY_EXISTS);
        }
        if (userRepository.findByPhoneNumber(request.getPhoneNumber()).isPresent()) {
            throw new BaseException(ErrorHandler.PHONE_NUMBER_ALREADY_EXISTS);
        }
        if (customerRepository.findByIdNumber(request.getIdNumber()).isPresent()) {
            throw new BaseException(ErrorHandler.ID_NUMBER_ALREADY_EXISTS);
        }

        // 2. Tạo đối tượng Customer
        Customer.Gender gender;
        try {
            gender = Customer.Gender.valueOf(request.getGender().toUpperCase());
        } catch (IllegalArgumentException e) {
            throw new BaseException(ErrorHandler.INVALID_GENDER);
        }

        Customer.IncomeLevel incomeLevel;
        try {
            incomeLevel = Customer.IncomeLevel.valueOf(request.getIncomeLevel().toUpperCase());
        } catch (IllegalArgumentException e) {
            throw new BaseException(ErrorHandler.INVALID_INCOME_LEVEL);
        }

        Customer customer = Customer.builder()
                .idNumber(request.getIdNumber())
                .dateOfBirth(LocalDate.parse(request.getDateOfBirth(), DateTimeFormatter.ofPattern("yyyy-MM-dd")).atStartOfDay())
                .address(request.getAddress())
                .gender(gender) // gán biến đã validate
                .occupation(request.getOccupation())
                .incomeLevel(incomeLevel) // gán biến đã validate
                .build();


        // 3. Tạo User nhưng KHÔNG GÁN VAI TRÒ
        User user = User.builder()
                .username(request.getUsername())
                .password(passwordEncoder.encode(request.getPassword()))
                .email(request.getEmail())
                .fullName(request.getFullName())
                .phoneNumber(request.getPhoneNumber())
                .customer(customer)
                .isVerified(false)
                .createdAt(LocalDateTime.now())
                .build();

        // 4. Liên kết và lưu
        customer.setUser(user);
        userRepository.save(user);

        // 5. Tạo và gửi token xác nhận (OTP)
        String otp = generateOtp();
        VerificationToken verificationToken = new VerificationToken(otp, user);
        verificationToken.setExpirationDate(LocalDateTime.now().plusMinutes(1));
        tokenRepository.save(verificationToken);

        String emailBody = """
        <html>
          <body style="font-family: Arial, sans-serif; background-color: #f4f4f4; padding:20px;">
            <div style="max-width:500px; margin:auto; background:#fff; padding:30px; border-radius:10px; box-shadow:0 2px 8px rgba(0,0,0,0.1);">
              <h2 style="color:#2e86de; text-align:center;">Xác thực tài khoản</h2>
              <p style="text-align:center;">Mã OTP của bạn là:</p>
              <h1 style="color:#e74c3c; text-align:center; letter-spacing:5px;">""" + otp + """
              </h1>
              <p style="text-align:center; color:#555;">Mã này có hiệu lực trong 1 phút.<br>Vui lòng không chia sẻ với bất kỳ ai.</p>
              <p style="text-align:center; font-size:12px; color:#aaa;">© 2025 SWP391 Team</p>
            </div>
          </body>
        </html>
        """;

        emailService.sendEmail(user.getEmail(), "Mã OTP mới", emailBody);


        // 6. Trả về phản hồi
        return RegisterResponse.builder()
                .success(true)
                .message("Đăng ký thành công. Vui lòng nhập mã OTP đã được gửi đến email của bạn.")
                .username(user.getUsername())
                .email(user.getEmail())
                .fullName(user.getFullName())
                .registrationTime(LocalDateTime.now())
                .build();
    }

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
            String accessToken = jwtService.generateAccessToken(user);
            String refreshToken = jwtService.generateRefreshToken(user);

            user.setLastLogin(LocalDateTime.now());
            userRepository.save(user);

            return LoginResponse.builder()
                    .token(accessToken)
                    .refreshToken(refreshToken)
                    .roleName(user.getRoles().stream().findFirst().map(Role::getName).orElse("USER"))
                    .message("Đăng nhập thành công")
                    .build();
        } catch (AuthenticationException ex) {
            String errorMessage = ErrorHandler.INVALID_CREDENTIALS.getMessage();
            log.warn("Đăng nhập thất bại cho người dùng '{}': {}", request.getUsername(), errorMessage);

            throw new BaseException(ErrorHandler.INVALID_CREDENTIALS);
        }
    }

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
            // Log the exception
        }
        throw new BaseException(ErrorHandler.INVALID_TOKEN);
    }

    @Transactional
    public void verifyOtp(String otp) {
        VerificationToken verificationToken = tokenRepository.findByToken(otp)
                .orElseThrow(() -> new BaseException(ErrorHandler.INVALID_TOKEN));
        if (verificationToken.getExpirationDate().isBefore(LocalDateTime.now())) {
            tokenRepository.delete(verificationToken);
            throw new BaseException(ErrorHandler.INVALID_TOKEN);
        }

        // Cấp role và xác minh tài khoản
        User user = verificationToken.getUser();
        user.setIsVerified(true);
        user.setIsActive(true);
        Role userRole = roleRepository.findByName("USER").orElseThrow(() -> new BaseException(ErrorHandler.ROLE_NOT_FOUND));
        user.setRoles(new HashSet<>(Set.of(userRole)));
        userRepository.save(user);

        // Xóa token sau khi xác minh thành công
        tokenRepository.delete(verificationToken);
    }

    @Override
    @Transactional
    public void requestNewOtp(String email) {
        User user = userRepository.findByEmail(email)
                .orElseThrow(() -> new BaseException(ErrorHandler.USER_NOT_FOUND));

        if (user.getIsVerified()) {
            throw new BaseException(ErrorHandler.ACCOUNT_ALREADY_VERIFIED);
        }

        // Tìm token cũ
        VerificationToken oldToken = tokenRepository.findByUser(user).orElse(null);

        if (oldToken != null) {
            // Kiểm tra xem token cũ đã hết hạn chưa
            if (oldToken.getExpirationDate().isAfter(LocalDateTime.now())) {
                // Nếu chưa hết hạn, không cho phép yêu cầu mã mới
                throw new BaseException(ErrorHandler.REQUEST_OTP_TOO_SOON); // Hoặc một lỗi phù hợp hơn
            }
            // Nếu đã hết hạn, xóa token cũ
            tokenRepository.deleteByUser(user);
            entityManager.flush();
        }

        // Tạo và lưu token mới
        String otp = generateOtp();
        VerificationToken newVerificationToken = new VerificationToken(otp, user);
        newVerificationToken.setExpirationDate(LocalDateTime.now().plusMinutes(1));
        tokenRepository.save(newVerificationToken);

        // HTML UI cho OTP
        String emailBody = String.format("""
        <html>
          <body style="font-family: Arial, sans-serif; background-color: #f4f4f4; padding:20px;">
            <div style="max-width:500px; margin:auto; background:#fff; padding:30px; border-radius:10px; box-shadow:0 2px 8px rgba(0,0,0,0.1);">
              <h2 style="color:#2e86de; text-align:center;">Xác thực tài khoản</h2>
              <p style="text-align:center;">Mã OTP của bạn là:</p>
              <h1 style="color:#e74c3c; text-align:center; letter-spacing:5px;">%s</h1>
              <p style="text-align:center; color:#555;">Mã này có hiệu lực trong 1 phút.<br>Vui lòng không chia sẻ với bất kỳ ai.</p>
              <p style="text-align:center; font-size:12px; color:#aaa;">© 2025 SWP391 Team</p>
            </div>
          </body>
        </html>
        """, otp);

        emailService.sendEmail(user.getEmail(), "Mã OTP mới", emailBody);

    }
}