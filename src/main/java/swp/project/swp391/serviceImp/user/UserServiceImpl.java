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
import swp.project.swp391.request.user.UpdateUserProfileRequest;
import swp.project.swp391.response.user.UserDetailResponse;
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

        String currentRole = currentUser.getRoles().iterator().next().getName();
        String targetRole = target.getRoles().iterator().next().getName();

        // === 1️⃣ ADMIN: được phép vô hiệu hóa tất cả ===
        if ("ADMIN".equals(currentRole)) {
            // không giới hạn
        }
        // === 2️⃣ EVM_STAFF: chỉ được vô hiệu hóa DEALER_MANAGER & DEALER_STAFF ===
        else if ("EVM_STAFF".equals(currentRole)) {
            if (!targetRole.equals("DEALER_MANAGER") && !targetRole.equals("DEALER_STAFF")) {
                throw new BaseException(ErrorHandler.FORBIDDEN, "EVM_STAFF chỉ được vô hiệu hóa user đại lý");
            }
        }
        // === 3️⃣ DEALER_MANAGER: chỉ được vô hiệu hóa DEALER_STAFF trong đại lý mình ===
        else if ("DEALER_MANAGER".equals(currentRole)) {
            if (!"DEALER_STAFF".equals(targetRole)) {
                throw new BaseException(ErrorHandler.FORBIDDEN, "DEALER_MANAGER chỉ được vô hiệu hóa nhân viên trong đại lý");
            }
            if (target.getDealer() == null ||
                    currentUser.getDealer() == null ||
                    !target.getDealer().getId().equals(currentUser.getDealer().getId())) {
                throw new BaseException(ErrorHandler.FORBIDDEN, "Chỉ được vô hiệu hóa nhân viên trong đại lý của bạn");
            }
        }
        // === 4️⃣ DEALER_STAFF: không được quyền ===
        else {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Không có quyền vô hiệu hóa người dùng khác");
        }

        target.setIsActive(false);
        userRepository.save(target);

        log.info("User {} (role: {}) vô hiệu hóa user {} (role: {})",
                currentUser.getUsername(), currentRole, target.getUsername(), targetRole);

        return ApiResponse.okMsg("User đã bị vô hiệu hóa thành công");
    }


    @Override
    @Transactional
    public ApiResponse<Void> reactivateUser(Long userId) {
        User currentUser = me();
        guard.require(guard.has(currentUser, "user.reactivate"));

        User target = userRepository.findById(userId)
                .orElseThrow(() -> new BaseException(ErrorHandler.USER_NOT_FOUND));

        String currentRole = currentUser.getRoles().iterator().next().getName();
        String targetRole = target.getRoles().iterator().next().getName();

        // === 1️⃣ ADMIN: được phép kích hoạt tất cả ===
        if ("ADMIN".equals(currentRole)) {
            // no restriction
        }
        // === 2️⃣ EVM_STAFF: chỉ được kích hoạt DEALER_MANAGER & DEALER_STAFF ===
        else if ("EVM_STAFF".equals(currentRole)) {
            if (!targetRole.equals("DEALER_MANAGER") && !targetRole.equals("DEALER_STAFF")) {
                throw new BaseException(ErrorHandler.FORBIDDEN, "EVM_STAFF chỉ được kích hoạt user đại lý");
            }
        }
        // === 3️⃣ DEALER_MANAGER: chỉ được kích hoạt DEALER_STAFF trong đại lý mình ===
        else if ("DEALER_MANAGER".equals(currentRole)) {
            if (!"DEALER_STAFF".equals(targetRole)) {
                throw new BaseException(ErrorHandler.FORBIDDEN, "DEALER_MANAGER chỉ được kích hoạt nhân viên trong đại lý");
            }
            if (target.getDealer() == null ||
                    currentUser.getDealer() == null ||
                    !target.getDealer().getId().equals(currentUser.getDealer().getId())) {
                throw new BaseException(ErrorHandler.FORBIDDEN, "Chỉ được kích hoạt nhân viên trong đại lý của bạn");
            }
        }
        // === 4️⃣ DEALER_STAFF: không được quyền ===
        else {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Không có quyền kích hoạt người dùng khác");
        }

        target.setIsActive(true);
        userRepository.save(target);

        log.info("User {} (role: {}) kích hoạt user {} (role: {})",
                currentUser.getUsername(), currentRole, target.getUsername(), targetRole);

        return ApiResponse.okMsg("User đã được kích hoạt lại thành công");
    }


    @Override
    @Transactional(Transactional.TxType.SUPPORTS)
    public ApiResponse<List<UserResponse>> getAllUsers() {
        User currentUser = me();
        guard.require(guard.has(currentUser, "user.read"));

        // Tên role chính của người đang đăng nhập
        String currentRole = currentUser.getRoles().iterator().next().getName();

        List<User> users = userRepository.findAll();

        // === ADMIN: thấy tất cả ===
        if ("ADMIN".equals(currentRole)) {
            // full access
        }

        // === EVM_STAFF: thấy tất cả trừ ADMIN ===
        else if ("EVM_STAFF".equals(currentRole)) {
            users = users.stream()
                    .filter(u -> u.getRoles().stream()
                            .noneMatch(r -> "ADMIN".equals(r.getName())))
                    .toList();
        }

        // === DEALER_MANAGER: chỉ thấy user cùng dealer ===
        else if ("DEALER_MANAGER".equals(currentRole)) {
            Long dealerId = currentUser.getDealer() != null ? currentUser.getDealer().getId() : null;
            users = users.stream()
                    .filter(u -> u.getDealer() != null && u.getDealer().getId().equals(dealerId))
                    .toList();
        }

        // === DEALER_STAFF: không có quyền xem danh sách user ===
        else {
            throw new BaseException(ErrorHandler.FORBIDDEN);
        }

        List<UserResponse> data = users.stream()
                .map(UserResponse::fromEntity)
                .toList();

        return ApiResponse.ok(data);
    }


    @Override
    @Transactional(Transactional.TxType.SUPPORTS)
    public ApiResponse<UserDetailResponse> getUserById(Long id) {
        User currentUser = me();
        guard.require(guard.has(currentUser, "user.read"));

        User target = userRepository.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.USER_NOT_FOUND));

        String currentRole = currentUser.getRoles().iterator().next().getName();
        String targetRole = target.getRoles().iterator().next().getName();

        // === EVM_STAFF không được xem ADMIN ===
        if ("EVM_STAFF".equals(currentRole) && "ADMIN".equals(targetRole)) {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Không thể xem thông tin ADMIN");
        }

        // === DEALER_MANAGER chỉ được xem user trong dealer của mình ===
        if ("DEALER_MANAGER".equals(currentRole)) {
            if (target.getDealer() == null ||
                    currentUser.getDealer() == null ||
                    !target.getDealer().getId().equals(currentUser.getDealer().getId())) {
                throw new BaseException(ErrorHandler.FORBIDDEN, "Chỉ được xem user trong đại lý của bạn");
            }
        }

        // === DEALER_STAFF không được xem chi tiết user khác ===
        if ("DEALER_STAFF".equals(currentRole)) {
            if (!currentUser.getId().equals(target.getId())) {
                throw new BaseException(ErrorHandler.FORBIDDEN, "Không có quyền xem thông tin người khác");
            }
        }

        return ApiResponse.ok(UserDetailResponse.fromEntity(target));
    }


    @Override
    @Transactional(Transactional.TxType.REQUIRED)
    public ApiResponse<UserDetailResponse> getMyProfile() {
        User currentUser = me();

        // load lại user có dealer đầy đủ
        User user = userRepository.findById(currentUser.getId())
                .orElseThrow(() -> new BaseException(ErrorHandler.USER_NOT_FOUND));

        return ApiResponse.ok(UserDetailResponse.fromEntity(user));
    }

    @Override
    @Transactional
    public ApiResponse<UserDetailResponse> updateMyProfile(UpdateUserProfileRequest req) {
        User currentUser = me();
        String role = currentUser.getRoles().iterator().next().getName();

        // Admin không được chỉnh profile cá nhân
        if ("ADMIN".equals(role)) {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Admin không được chỉnh sửa thông tin cá nhân");
        }

        User managedUser = userRepository.findById(currentUser.getId())
                .orElseThrow(() -> new BaseException(ErrorHandler.USER_NOT_FOUND));

        // 🔹 Check trùng email, phone, idNumber (trừ chính mình)
        userRepository.findByEmail(req.getEmail())
                .filter(u -> !u.getId().equals(managedUser.getId()))
                .ifPresent(u -> { throw new BaseException(ErrorHandler.EMAIL_ALREADY_EXISTS); });

        userRepository.findByPhoneNumber(req.getPhoneNumber())
                .filter(u -> !u.getId().equals(managedUser.getId()))
                .ifPresent(u -> { throw new BaseException(ErrorHandler.PHONE_NUMBER_ALREADY_EXISTS); });

        userRepository.findByIdNumber(req.getIdNumber())
                .filter(u -> !u.getId().equals(managedUser.getId()))
                .ifPresent(u -> { throw new BaseException(ErrorHandler.ID_NUMBER_ALREADY_EXISTS); });

        // Cập nhật thông tin
        updateAllowedFields(managedUser, req);
        userRepository.save(managedUser);

        // Ép load dealer (tránh LazyInitializationException)
        if (managedUser.getDealer() != null) managedUser.getDealer().getName();

        return ApiResponse.ok(UserDetailResponse.fromEntity(managedUser));
    }


    @Override
    @Transactional
    public ApiResponse<UserDetailResponse> updateUserProfile(Long id, UpdateUserProfileRequest req) {
        User currentUser = me();
        guard.require(guard.has(currentUser, "user.update"));

        User target = userRepository.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.USER_NOT_FOUND));

        String currentRole = currentUser.getRoles().iterator().next().getName();
        String targetRole = target.getRoles().iterator().next().getName();

        // ===== PHÂN QUYỀN =====
        if ("ADMIN".equals(currentRole)) {
            if ("ADMIN".equals(targetRole)) {
                throw new BaseException(ErrorHandler.FORBIDDEN, "Không thể chỉnh sửa thông tin ADMIN khác");
            }
        } else if ("EVM_STAFF".equals(currentRole)) {
            if (!targetRole.equals("DEALER_MANAGER") && !targetRole.equals("DEALER_STAFF")) {
                throw new BaseException(ErrorHandler.FORBIDDEN, "Chỉ được chỉnh sửa thông tin user thuộc đại lý");
            }
        } else if ("DEALER_MANAGER".equals(currentRole)) {
            if (!"DEALER_STAFF".equals(targetRole)) {
                throw new BaseException(ErrorHandler.FORBIDDEN, "Chỉ được chỉnh sửa nhân viên trong đại lý");
            }
            if (target.getDealer() == null ||
                    currentUser.getDealer() == null ||
                    !target.getDealer().getId().equals(currentUser.getDealer().getId())) {
                throw new BaseException(ErrorHandler.FORBIDDEN, "Chỉ được chỉnh sửa nhân viên trong đại lý của bạn");
            }
        } else {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Không có quyền chỉnh sửa người khác");
        }

        // 🔹 Check trùng email, phone, idNumber (trừ chính target)
        userRepository.findByEmail(req.getEmail())
                .filter(u -> !u.getId().equals(target.getId()))
                .ifPresent(u -> { throw new BaseException(ErrorHandler.EMAIL_ALREADY_EXISTS); });

        userRepository.findByPhoneNumber(req.getPhoneNumber())
                .filter(u -> !u.getId().equals(target.getId()))
                .ifPresent(u -> { throw new BaseException(ErrorHandler.PHONE_NUMBER_ALREADY_EXISTS); });

        userRepository.findByIdNumber(req.getIdNumber())
                .filter(u -> !u.getId().equals(target.getId()))
                .ifPresent(u -> { throw new BaseException(ErrorHandler.ID_NUMBER_ALREADY_EXISTS); });

        // Cập nhật thông tin
        updateAllowedFields(target, req);
        userRepository.save(target);

        // Ép load dealer trước khi map
        if (target.getDealer() != null) target.getDealer().getName();

        return ApiResponse.ok(UserDetailResponse.fromEntity(target));
    }



    @Override
    @Transactional
    public ApiResponse<Void> assignUserToDealer(Long userId, Long dealerId) {
        User currentUser = me();
        guard.require(guard.has(currentUser, "user.assignDealer"));

        if (!List.of("ADMIN", "EVM_STAFF").contains(currentUser.getRoles().iterator().next().getName())) {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Chỉ Admin hoặc EVM Staff mới được gán đại lý cho user");
        }

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new BaseException(ErrorHandler.USER_NOT_FOUND));
        Dealer dealer = dealerRepository.findById(dealerId)
                .orElseThrow(() -> new BaseException(ErrorHandler.DEALER_NOT_FOUND));

        user.setDealer(dealer);
        userRepository.save(user);

        return ApiResponse.okMsg("Gán user vào đại lý thành công");
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

    private void updateAllowedFields(User target, UpdateUserProfileRequest req) {
        if (req.getEmail() != null) target.setEmail(req.getEmail());
        if (req.getFullName() != null) target.setFullName(req.getFullName());
        if (req.getPhoneNumber() != null) target.setPhoneNumber(req.getPhoneNumber());
        if (req.getIdNumber() != null) target.setIdNumber(req.getIdNumber());
        if (req.getAddress() != null) target.setAddress(req.getAddress());

        if (req.getDateOfBirth() != null && !req.getDateOfBirth().isBlank()) {
            target.setDateOfBirth(LocalDate.parse(req.getDateOfBirth(), DateTimeFormatter.ofPattern("yyyy-MM-dd")));
        }

        if (req.getGender() != null && !req.getGender().isBlank()) {
            try {
                target.setGender(User.Gender.valueOf(req.getGender().toUpperCase()));
            } catch (IllegalArgumentException e) {
                throw new BaseException(ErrorHandler.INVALID_GENDER, "Giới tính không hợp lệ: " + req.getGender());
            }
        }
    }


}
