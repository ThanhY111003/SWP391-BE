package swp.project.swp391.serviceImp.role;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.Permission;
import swp.project.swp391.entity.Role;
import swp.project.swp391.entity.User;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.PermissionRepository;
import swp.project.swp391.repository.RoleRepository;
import swp.project.swp391.repository.UserRepository;
import swp.project.swp391.response.role.PermissionResponse;
import swp.project.swp391.response.role.RoleDetailResponse;
import swp.project.swp391.response.role.RoleResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.role.RoleService;
import swp.project.swp391.config.DefaultRoleConfig;


import java.util.List;
import java.util.Set;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class RoleServiceImpl implements RoleService {
    private final RoleRepository roleRepository;
    private final PermissionRepository permissionRepository;
    private final RbacGuard guard;
    private final UserRepository userRepository;
    private final DefaultRoleConfig defaultRoleConfig;

    private User me() {
        return (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
    }

    @Override
    public List<RoleResponse> getAllRoles() {
        User currentUser = me();
        guard.require(guard.has(currentUser, "role.read"));

        return roleRepository.findAll().stream()
                .map(role -> new RoleResponse(
                        role.getId(),
                        role.getName(),
                        role.getDisplayName(),
                        role.getDescription(),
                        role.getIsCustomized()
                ))
                .toList();
    }

    @Override
    public List<PermissionResponse> getAllPermissions() {
        guard.require(guard.has(me(), "role.read"));

        return permissionRepository.findAll(org.springframework.data.domain.Sort.by("id"))
                .stream()
                .map(p -> new PermissionResponse(
                        p.getId(),
                        p.getName(),
                        p.getDisplayName(),
                        p.getDescription(),
                        p.getResource(),
                        p.getAction(),
                        p.getIsActive()
                ))
                .toList();
    }

    @Override
    public RoleDetailResponse getRoleById(Long roleId) {
        User currentUser = me();
        guard.require(guard.has(currentUser, "role.read"));

        Role role = roleRepository.findById(roleId)
                .orElseThrow(() -> new RuntimeException("Role not found: " + roleId));

        return RoleDetailResponse.fromEntity(role);
    }

    @Override
    @Transactional
    public RoleDetailResponse addPermissionsToRole(Long roleId, Set<Long> permissionIds) {
        User currentUser = me();
        guard.require(guard.has(currentUser, "role.update"));

        Role role = roleRepository.findById(roleId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ROLE_NOT_FOUND));

        // No-op nếu không có gì để thêm
        if (permissionIds == null || permissionIds.isEmpty()) {
            return RoleDetailResponse.fromEntity(role);
        }

        // Lấy sẵn các ID quyền hiện có để so theo ID, tránh phụ thuộc equals/hashCode
        Set<Long> existingIds = role.getPermissions() == null
                ? new java.util.HashSet<>()
                : role.getPermissions().stream().map(Permission::getId).collect(java.util.stream.Collectors.toSet());

        // Batch load tất cả permission theo IDs
        java.util.List<Permission> loaded = permissionRepository.findAllById(permissionIds);

        // (Tuỳ chọn: strict) kiểm tra ID nào không tồn tại
        if (loaded.size() != permissionIds.size()) {
            java.util.Set<Long> foundIds = loaded.stream().map(Permission::getId).collect(java.util.stream.Collectors.toSet());
            java.util.Set<Long> missing = new java.util.HashSet<>(permissionIds);
            missing.removeAll(foundIds);
            // Bạn có thể: throw lỗi, hoặc chỉ cảnh báo và bỏ qua.
            throw new BaseException(ErrorHandler.PERMISSION_NOT_FOUND);
        }

        // Chỉ thêm những quyền chưa có
        java.util.List<Permission> toAdd = loaded.stream()
                .filter(p -> !existingIds.contains(p.getId()))
                .toList();

        if (toAdd.isEmpty()) {
            return RoleDetailResponse.fromEntity(role); // không có thay đổi
        }

        role.getPermissions().addAll(toAdd);
        role.setIsCustomized(true); // đã custom → seeder không đè lại
        Role saved = roleRepository.save(role);

        return RoleDetailResponse.fromEntity(saved);
    }


    @Override
    @Transactional
    public RoleDetailResponse removePermissionsFromRole(Long roleId, Set<Long> permissionIds) {
        User currentUser = me();
        guard.require(guard.has(currentUser, "role.update"));

        Role role = roleRepository.findById(roleId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ROLE_NOT_FOUND));

        if (permissionIds == null || permissionIds.isEmpty()) {
            return RoleDetailResponse.fromEntity(role); // no-op
        }

        // so theo ID để an toàn
        java.util.Set<Long> ids = new java.util.HashSet<>(permissionIds);
        boolean changed = role.getPermissions().removeIf(p -> ids.contains(p.getId()));

        if (changed) {
            role.setIsCustomized(true);
            role = roleRepository.save(role);
        }
        return RoleDetailResponse.fromEntity(role);
    }


    /**
     * Reset role về cấu hình mặc định
     * ✨ Set isCustomized = false, restart app sẽ tự động áp dụng config từ code
     */
    @Override
    @Transactional
    public RoleDetailResponse resetRoleToDefault(Long roleId) {
        User currentUser = me();
        guard.require(guard.has(currentUser, "role.reset"));

        Role role = roleRepository.findById(roleId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ROLE_NOT_FOUND));

        // 1) Xóa hiện tại
        role.getPermissions().clear();

        // 2) Nạp mặc định từ code
        List<String> defaultNames = defaultRoleConfig.getDefaultPermissions(role.getName());
        if (defaultNames.isEmpty()) {
            // Không có cấu hình — tùy bạn: hoặc giữ rỗng, hoặc throw cảnh báo
            // throw new BaseException(ErrorHandler.PERMISSION_NOT_FOUND);
        } else {
            List<Permission> defaults = permissionRepository.findByNameIn(defaultNames);

            // (tuỳ chọn) kiểm tra thiếu quyền nào trong DB
            if (defaults.size() != defaultNames.size()) {
                // quyền nào chưa có trong bảng permissions → báo lỗi/ghi log
                Set<String> found = defaults.stream().map(Permission::getName).collect(Collectors.toSet());
                List<String> missing = defaultNames.stream().filter(n -> !found.contains(n)).toList();
                // log.warn("Missing permissions: {}", missing);
            }

            role.setPermissions(new java.util.LinkedHashSet<>(defaults));
        }

        // 3) Đánh dấu là "đúng theo mặc định"
        role.setIsCustomized(false);

        Role saved = roleRepository.save(role);
        return RoleDetailResponse.fromEntity(saved);
    }


    @Override
    @Transactional
    public void assignRoleToUser(Long userId, Long roleId) {
        User currentUser = me();
        guard.require(guard.has(currentUser, "role.assign"));

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new BaseException(ErrorHandler.USER_NOT_FOUND));

        Role role = roleRepository.findById(roleId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ROLE_NOT_FOUND));

        if (user.getRoles() == null) {
            user.setRoles(new java.util.LinkedHashSet<>());
        }
        // add-only, bỏ qua trùng
        if (!user.getRoles().contains(role)) {
            user.getRoles().add(role);
            userRepository.save(user);
        }
    }

    @Override
    @Transactional
    public void unassignRoleFromUser(Long userId, Long roleId) {
        User currentUser = me();
        guard.require(guard.has(currentUser, "role.unassign"));

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new BaseException(ErrorHandler.USER_NOT_FOUND));

        Role role = roleRepository.findById(roleId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ROLE_NOT_FOUND));

        if (user.getRoles() != null && user.getRoles().remove(role)) {
            userRepository.save(user);
        }
    }

}