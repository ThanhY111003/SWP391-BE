package swp.project.swp391.service.role;

import swp.project.swp391.response.role.PermissionResponse;
import swp.project.swp391.response.role.RoleDetailResponse;
import swp.project.swp391.response.role.RoleResponse;

import java.util.List;
import java.util.Set;

public interface RoleService {

    List<RoleResponse> getAllRoles();

    List<PermissionResponse> getAllPermissions();

    RoleDetailResponse getRoleById(Long roleId);

    // ✅ Thêm hàng loạt (add-only, bỏ qua trùng)
    RoleDetailResponse addPermissionsToRole(Long roleId, Set<Long> permissionIds);

    RoleDetailResponse removePermissionsFromRole(Long roleId, Set<Long> permissionIds);

    RoleDetailResponse resetRoleToDefault(Long roleId);

    void assignRoleToUser(Long userId, Long roleId);

    void unassignRoleFromUser(Long userId, Long roleId);

}