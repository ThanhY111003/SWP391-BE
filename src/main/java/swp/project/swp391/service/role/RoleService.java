package swp.project.swp391.service.role;

import swp.project.swp391.response.role.RoleDetailResponse;
import swp.project.swp391.response.role.RoleResponse;

import java.util.List;
import java.util.Set;

public interface RoleService {

    List<RoleResponse> getAllRoles();

    RoleDetailResponse getRoleById(Long roleId);

    RoleDetailResponse assignPermissionsToRole(Long roleId, Set<Long> permissionIds);

    RoleDetailResponse addPermissionToRole(Long roleId, Long permissionId);

    RoleDetailResponse removePermissionFromRole(Long roleId, Long permissionId);

    RoleDetailResponse resetRoleToDefault(Long roleId);

    void unassignRoleFromUser(Long userId, Long roleId);
}