package swp.project.swp391.serviceImp.role;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import swp.project.swp391.entity.Permission;
import swp.project.swp391.entity.Role;
import swp.project.swp391.entity.User;
import swp.project.swp391.repository.PermissionRepository;
import swp.project.swp391.repository.RoleRepository;
import swp.project.swp391.repository.UserRepository;
import swp.project.swp391.response.role.RoleDetailResponse;
import swp.project.swp391.response.role.RoleResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.role.RoleService;

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

    private User me() {
        return (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
    }

    @Override
    public List<RoleResponse> getAllRoles() {
        User currentUser = me();
        guard.require(guard.has(currentUser, "role.read.all"));

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
    public RoleDetailResponse getRoleById(Long roleId) {
        User currentUser = me();
        guard.require(guard.has(currentUser, "role.read.all"));

        Role role = roleRepository.findById(roleId)
                .orElseThrow(() -> new RuntimeException("Role not found: " + roleId));

        return RoleDetailResponse.fromEntity(role);
    }

    /**
     * Assign danh sách permissions vào role (ghi đè hoàn toàn)
     * ✨ Tự động set isCustomized = true
     */
    @Override
    @Transactional
    public RoleDetailResponse assignPermissionsToRole(Long roleId, Set<Long> permissionIds) {
        User currentUser = me();
        guard.require(guard.has(currentUser, "role.update"));

        Role role = roleRepository.findById(roleId)
                .orElseThrow(() -> new RuntimeException("Role not found: " + roleId));

        Set<Permission> permissions = permissionIds.stream()
                .map(id -> permissionRepository.findById(id)
                        .orElseThrow(() -> new RuntimeException("Permission not found: " + id)))
                .collect(Collectors.toSet());

        role.setPermissions(permissions);
        role.setIsCustomized(true); // ✨ Đánh dấu đã custom

        Role savedRole = roleRepository.save(role);
        return RoleDetailResponse.fromEntity(savedRole);
    }

    /**
     * Thêm 1 permission vào role (không xóa permissions cũ)
     * ✨ Tự động set isCustomized = true
     */
    @Override
    @Transactional
    public RoleDetailResponse addPermissionToRole(Long roleId, Long permissionId) {
        User currentUser = me();
        guard.require(guard.has(currentUser, "role.update"));

        Role role = roleRepository.findById(roleId)
                .orElseThrow(() -> new RuntimeException("Role not found: " + roleId));

        Permission permission = permissionRepository.findById(permissionId)
                .orElseThrow(() -> new RuntimeException("Permission not found: " + permissionId));

        role.getPermissions().add(permission);
        role.setIsCustomized(true); // ✨ Đánh dấu đã custom

        Role savedRole = roleRepository.save(role);
        return RoleDetailResponse.fromEntity(savedRole);
    }

    /**
     * Gỡ 1 permission ra khỏi role
     * ✨ Tự động set isCustomized = true
     */
    @Override
    @Transactional
    public RoleDetailResponse removePermissionFromRole(Long roleId, Long permissionId) {
        User currentUser = me();
        guard.require(guard.has(currentUser, "role.update"));

        Role role = roleRepository.findById(roleId)
                .orElseThrow(() -> new RuntimeException("Role not found: " + roleId));

        Permission permission = permissionRepository.findById(permissionId)
                .orElseThrow(() -> new RuntimeException("Permission not found: " + permissionId));

        role.getPermissions().remove(permission);
        role.setIsCustomized(true); // ✨ Đánh dấu đã custom

        Role savedRole = roleRepository.save(role);
        return RoleDetailResponse.fromEntity(savedRole);
    }

    /**
     * Reset role về cấu hình mặc định
     * ✨ Set isCustomized = false, restart app sẽ tự động áp dụng config từ code
     */
    @Override
    @Transactional
    public RoleDetailResponse resetRoleToDefault(Long roleId) {
        User currentUser = me();
        guard.require(guard.has(currentUser, "role.update"));

        Role role = roleRepository.findById(roleId)
                .orElseThrow(() -> new RuntimeException("Role not found: " + roleId));

        // Xóa hết permissions hiện tại
        role.getPermissions().clear();

        // Đánh dấu chưa custom -> restart sẽ tự động set lại từ code
        role.setIsCustomized(false);

        Role savedRole = roleRepository.save(role);
        return RoleDetailResponse.fromEntity(savedRole);
    }
    @Override
    @Transactional
    public void unassignRoleFromUser(Long userId, Long roleId) {
        User currentUser = me();
        guard.require(guard.has(currentUser, "user.update"));

        User user = userRepository.findById(userId)
                .orElseThrow(() -> new RuntimeException("User not found: " + userId));

        Role role = roleRepository.findById(roleId)
                .orElseThrow(() -> new RuntimeException("Role not found: " + roleId));

        user.getRoles().remove(role);
        userRepository.save(user);
    }
}