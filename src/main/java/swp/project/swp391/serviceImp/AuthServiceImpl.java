package swp.project.swp391.serviceImp;

import lombok.RequiredArgsConstructor;
import org.springframework.security.authentication.AuthenticationManager;
import org.springframework.security.authentication.UsernamePasswordAuthenticationToken;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService; // Import this
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import swp.project.swp391.constant.ErrorHandler; // Import this
import swp.project.swp391.entity.Customer;
import swp.project.swp391.entity.Role;
import swp.project.swp391.entity.User;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.CustomerRepository;
import swp.project.swp391.repository.RoleRepository;
import swp.project.swp391.repository.UserRepository;
import swp.project.swp391.request.LoginRequest;
import swp.project.swp391.request.RefreshTokenRequest;
import swp.project.swp391.request.RegisterRequest;
import swp.project.swp391.response.LoginResponse;
import swp.project.swp391.response.RefreshTokenResponse;
import swp.project.swp391.response.RegisterResponse;
import swp.project.swp391.security.JwtService;
import swp.project.swp391.service.AuthService;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.Set;

@Service
@RequiredArgsConstructor
public class AuthServiceImpl implements AuthService {

    private final CustomerRepository customerRepository;
    private final UserRepository userRepository;
    private final RoleRepository roleRepository;
    private final PasswordEncoder passwordEncoder;
    private final JwtService jwtService;
    private final AuthenticationManager authenticationManager;
    private final UserDetailsService userDetailsService; // Đổi từ UserService thành UserDetailsService

    @Override
    public RegisterResponse register(RegisterRequest request) {
        // Kiểm tra xem người dùng đã tồn tại chưa
        if (userRepository.existsByUsername(request.getUsername())) {
            throw new BaseException(ErrorHandler.USERNAME_ALREADY_EXISTS);
        }
        if (userRepository.existsByEmail(request.getEmail())) {
            throw new BaseException(ErrorHandler.EMAIL_ALREADY_EXISTS);
        }
        // Kiểm tra số điện thoại (từ bảng User)
        if (userRepository.existsByPhoneNumber(request.getPhoneNumber())) {
            throw new BaseException(ErrorHandler.PHONE_NUMBER_ALREADY_EXISTS);
        }
        if (customerRepository.existsByIdNumber(request.getIdNumber())) {
            throw new BaseException(ErrorHandler.ID_NUMBER_ALREADY_EXISTS);
        }

        // 1. Tạo đối tượng Customer trước với các trường riêng
        Customer customer = Customer.builder()
                .idNumber(request.getIdNumber())
                .dateOfBirth(LocalDate.parse(request.getDateOfBirth(), DateTimeFormatter.ofPattern("yyyy-MM-dd")).atStartOfDay())
                .address(request.getAddress())
                .gender(Customer.Gender.valueOf(request.getGender().toUpperCase()))
                .occupation(request.getOccupation())
                .incomeLevel(Customer.IncomeLevel.valueOf(request.getIncomeLevel().toUpperCase()))
                .build();

        // 2. Lấy vai trò (Role) mặc định và tạo đối tượng User
        Role userRole = roleRepository.findByName("USER")
                .orElseThrow(() -> new BaseException(ErrorHandler.ROLE_NOT_FOUND));

        User user = User.builder()
                .username(request.getUsername())
                .password(passwordEncoder.encode(request.getPassword()))
                .email(request.getEmail())
                .fullName(request.getFullName())
                .phoneNumber(request.getPhoneNumber())
                .customer(customer) // Liên kết với Customer
                .roles(Set.of(userRole))
                .isActive(true)
                .build();

        // 3. Liên kết hai đối tượng và lưu
        customer.setUser(user);

        userRepository.save(user);

        // 4. Trả về phản hồi
        return RegisterResponse.builder()
                .success(true)
                .message("Đăng ký thành công")
                .username(user.getUsername())
                .email(user.getEmail())
                .fullName(user.getFullName())
                .registrationTime(LocalDateTime.now())
                .build();
    }

    @Override
    public LoginResponse login(LoginRequest request) {
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
                .message("Đăng nhập thành công")
                .build();
    }

    @Override
    public RefreshTokenResponse refreshToken(RefreshTokenRequest request) {
        String refreshToken = request.getRefreshToken();
        try {
            String username = jwtService.extractUsername(refreshToken);

            // Trích xuất username thành công, tiếp tục xử lý
            if (username != null) {
                UserDetails userDetails = userDetailsService.loadUserByUsername(username);

                // Kiểm tra tính hợp lệ của refresh token
                if (jwtService.isTokenValid(refreshToken, userDetails)) {
                    String newAccessToken = jwtService.generateAccessToken(userDetails);
                    return RefreshTokenResponse.builder()
                            .token(newAccessToken)
                            .build();
                }
            }
        } catch (Exception e) {
            // Bắt tất cả các ngoại lệ liên quan đến token (hết hạn, sai chữ ký, v.v.)
            // Điều này đảm bảo rằng không có ngoại lệ NullPointerException nào bị ném ra
        }

        // Nếu username là null, token không hợp lệ, hoặc có bất kỳ ngoại lệ nào khác xảy ra
        throw new BaseException(ErrorHandler.INVALID_TOKEN);
    }
}