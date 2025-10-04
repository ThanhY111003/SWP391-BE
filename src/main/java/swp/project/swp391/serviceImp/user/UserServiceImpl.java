package swp.project.swp391.serviceImp.user;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.Dealer;
import swp.project.swp391.entity.Role;
import swp.project.swp391.entity.User;
import swp.project.swp391.entity.VerificationToken;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.DealerRepository;
import swp.project.swp391.repository.RoleRepository;
import swp.project.swp391.repository.UserRepository;
import swp.project.swp391.repository.VerificationTokenRepository;
import swp.project.swp391.request.user.CreateUserRequest;
import swp.project.swp391.response.user.UserResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.user.UserService;
import swp.project.swp391.service.auth.EmailService;

import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.Random;
import java.util.Set;

@Slf4j
@Service
@RequiredArgsConstructor
public class UserServiceImpl implements UserService {

    private final UserRepository userRepository;
    private final RoleRepository roleRepository;                // <— thêm
    private final DealerRepository dealerRepository;            // <— thêm
    private final VerificationTokenRepository tokenRepository;  // <— thêm
    private final PasswordEncoder passwordEncoder;              // <— thêm
    private final EmailService emailService;                    // <— thêm
    private final RbacGuard guard;

    // ===== Helper: lấy current user từ SecurityContext =====
    private User me() {
        return (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
    }

    // ===== Helper: sinh OTP 6 chữ số =====
    private String generateOtp() {
        Random random = new Random();
        int otp = 100000 + random.nextInt(900000);
        return String.valueOf(otp);
    }

    // ===== Helper: validate quyền tạo user theo role hiện tại =====
    private void validateCreateUserPermission(User currentUser, String targetRoleName) {
        String currentRoleName = currentUser.getRoles().iterator().next().getName();

        switch (currentRoleName) {
            case "ADMIN":
                // Admin tạo được tất cả trừ ADMIN
                if ("ADMIN".equals(targetRoleName)) {
                    throw new BaseException(ErrorHandler.FORBIDDEN);
                }
                break;

            case "EVM_STAFF":
                // EVM_STAFF chỉ tạo được DEALER_MANAGER, DEALER_STAFF
                if (!"DEALER_MANAGER".equals(targetRoleName) && !"DEALER_STAFF".equals(targetRoleName)) {
                    throw new BaseException(ErrorHandler.FORBIDDEN);
                }
                guard.require(guard.has(currentUser, "user.create"));
                break;

            case "DEALER_MANAGER":
                // DEALER_MANAGER chỉ tạo được DEALER_STAFF
                if (!"DEALER_STAFF".equals(targetRoleName)) {
                    throw new BaseException(ErrorHandler.FORBIDDEN);
                }
                guard.require(guard.has(currentUser, "user.create"));
                break;

            default:
                throw new BaseException(ErrorHandler.FORBIDDEN);
        }
    }

    /**
     * Tạo user mới (chỉ Admin/EVM_STAFF/DEALER_MANAGER)
     * - Admin: Tạo được EVM_STAFF, DEALER_MANAGER, DEALER_STAFF
     * - EVM_STAFF: Tạo được DEALER_MANAGER, DEALER_STAFF
     * - DEALER_MANAGER: Tạo được DEALER_STAFF (trong dealer của mình)
     */
    @Override
    @Transactional
    public void createUser(CreateUserRequest request) {
        User currentUser = me();

        // 1) Quyền
        validateCreateUserPermission(currentUser, request.getRoleName());

        // 2) Unique
        if (userRepository.findByEmail(request.getEmail()).isPresent()) {
            throw new BaseException(ErrorHandler.EMAIL_ALREADY_EXISTS);
        }
        if (userRepository.findByPhoneNumber(request.getPhoneNumber()).isPresent()) {
            throw new BaseException(ErrorHandler.PHONE_NUMBER_ALREADY_EXISTS);
        }

        // 3) Role
        Role role = roleRepository.findByName(request.getRoleName())
                .orElseThrow(() -> new BaseException(ErrorHandler.ROLE_NOT_FOUND));

        // 4) Dealer (nếu cần)
        Dealer dealer = null;
        if ("DEALER_MANAGER".equals(request.getRoleName()) || "DEALER_STAFF".equals(request.getRoleName())) {
            if (request.getDealerId() == null) throw new BaseException(ErrorHandler.DEALER_REQUIRED);
            dealer = dealerRepository.findById(request.getDealerId())
                    .orElseThrow(() -> new BaseException(ErrorHandler.DEALER_NOT_FOUND));

            if ("DEALER_MANAGER".equals(currentUser.getRoles().iterator().next().getName())) {
                if (currentUser.getDealer() == null || !currentUser.getDealer().getId().equals(dealer.getId())) {
                    throw new BaseException(ErrorHandler.FORBIDDEN);
                }
            }
        }

        // 5) Gender
        User.Gender gender;
        try {
            gender = User.Gender.valueOf(request.getGender().toUpperCase());
        } catch (IllegalArgumentException e) {
            throw new BaseException(ErrorHandler.INVALID_GENDER);
        }

        // 6) Sinh mật khẩu tạm
        String rawTempPassword = generateTempPassword(); // ví dụ: 12 ký tự, có chữ hoa/thường/số/ký tự đặc biệt

        // 7) Tạo user (kích hoạt luôn, không OTP)
        User user = User.builder()
                .username(request.getEmail())
                .password(passwordEncoder.encode(rawTempPassword))
                .email(request.getEmail())
                .fullName(request.getFullName())
                .phoneNumber(request.getPhoneNumber())
                .idNumber(request.getIdNumber())
                .dateOfBirth(LocalDate.parse(request.getDateOfBirth(), DateTimeFormatter.ofPattern("yyyy-MM-dd")))
                .address(request.getAddress())
                .gender(gender)
                .dealer(dealer)
                .roles(Set.of(role))
                .isActive(true)              // ✅ cho phép đăng nhập
                .mustChangePassword(true)    // ✅ bắt buộc đổi mật khẩu sau khi login
                .build();

        userRepository.save(user);

        // 8) Gửi email thông báo mật khẩu tạm + hướng dẫn đổi
        String emailBody = String.format("""
    <html>
      <body style="font-family: Arial, sans-serif; background-color: #f4f4f4; padding:20px;">
        <div style="max-width:600px; margin:auto; background:#fff; padding:30px; border-radius:10px; box-shadow:0 2px 8px rgba(0,0,0,0.1);">
          <h2 style="color:#2e86de; text-align:center;">Tài khoản 5bike Dealer System</h2>
          <p>Xin chào <b>%s</b>,</p>
          <p>Tài khoản của bạn đã được tạo với vai trò: <b>%s</b></p>
          <ul>
            <li><b>Username:</b> %s</li>
            <li><b>Mật khẩu tạm thời:</b> <code>%s</code></li>
          </ul>
          <p style="color:#e67e22;"><b>Vui lòng đăng nhập và đổi mật khẩu ngay ở lần đăng nhập đầu tiên.</b></p>
          <p style="font-size:12px; color:#999;">Không chia sẻ mật khẩu cho bất kỳ ai.</p>
          <p style="text-align:center; font-size:12px; color:#aaa;">© 2025 5bike Dealer System</p>
        </div>
      </body>
    </html>
    """, user.getFullName(), role.getDisplayName(), user.getUsername(), rawTempPassword);

        emailService.sendEmail(user.getEmail(), "Tài khoản mới & mật khẩu tạm thời", emailBody);

        log.info("Created new user (no OTP): {} with role: {} by: {}",
                user.getEmail(), role.getName(), currentUser.getUsername());
    }


    @Override
    @Transactional
    public ApiResponse<Void> inactiveUser(Long userId) {
        User currentUser = me();
        guard.require(guard.has(currentUser, "user.inactive"));

        User target = userRepository.findById(userId)
                .orElseThrow(() -> new BaseException(ErrorHandler.USER_NOT_FOUND));

        boolean isAdmin = target.getRoles().stream().anyMatch(r -> "ADMIN".equals(r.getName()));
        if (isAdmin) {
            throw new BaseException(ErrorHandler.FORBIDDEN);
        }

        target.setIsActive(false);
        userRepository.save(target);

        return ApiResponse.okMsg("User đã bị vô hiệu hóa thành công");
    }

    @Override
    @Transactional
    public ApiResponse<Void> reactivateUser(Long userId) {
        User currentUser = me();
        guard.require(guard.has(currentUser, "user.reactivate"));

        User target = userRepository.findById(userId)
                .orElseThrow(() -> new BaseException(ErrorHandler.USER_NOT_FOUND));

        target.setIsActive(true);
        userRepository.save(target);

        return ApiResponse.okMsg("User đã được kích hoạt lại thành công");
    }

    @Override
    @Transactional(Transactional.TxType.SUPPORTS) // tương đương readOnly với jakarta.transaction
    public ApiResponse<List<UserResponse>> getAllUsers() {
        User currentUser = me();
        guard.require(guard.has(currentUser, "user.read"));

        List<UserResponse> data = userRepository.findAll()
                .stream()
                .map(UserResponse::fromEntity)
                .toList();

        return ApiResponse.ok(data);
    }
    private String generateTempPassword() {
        // ví dụ: độ dài 12, gồm hoa/thường/số/ký tự đặc biệt
        final String UPPER = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
        final String LOWER = "abcdefghijklmnopqrstuvwxyz";
        final String DIGIT = "0123456789";
        final String SPEC  = "!@#$%^&*()-_=+[]{}";

        String all = UPPER + LOWER + DIGIT + SPEC;
        StringBuilder sb = new StringBuilder();
        java.util.Random rnd = new java.util.Random();

        // Ít nhất 1 ký tự thuộc mỗi nhóm
        sb.append(UPPER.charAt(rnd.nextInt(UPPER.length())));
        sb.append(LOWER.charAt(rnd.nextInt(LOWER.length())));
        sb.append(DIGIT.charAt(rnd.nextInt(DIGIT.length())));
        sb.append(SPEC.charAt(rnd.nextInt(SPEC.length())));

        // Phần còn lại random
        for (int i = 0; i < 8; i++) {
            sb.append(all.charAt(rnd.nextInt(all.length())));
        }
        // Trộn
        return sb.chars()
                .mapToObj(c -> (char) c)
                .sorted((a,b) -> rnd.nextInt(3)-1)
                .collect(StringBuilder::new, StringBuilder::append, StringBuilder::append)
                .toString();
    }

}
