package swp.project.swp391.controller.role;

import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.request.role.AssignPermissionsRequest;
import swp.project.swp391.response.role.ResetRoleResponse;
import swp.project.swp391.response.role.RoleDetailResponse;
import swp.project.swp391.response.role.RoleResponse;
import swp.project.swp391.service.role.RoleService;

import java.util.List;

@RestController
@RequestMapping("/api/roles")
@RequiredArgsConstructor
public class RoleController {

    private final RoleService roleService;

    @GetMapping
    public ResponseEntity<ApiResponse<List<RoleResponse>>> getAllRoles() {
        List<RoleResponse> roles = roleService.getAllRoles();
        return ResponseEntity.ok(ApiResponse.ok(roles, "Lấy danh sách tất cả roles thành công"));
    }

    @GetMapping("/{id}")
    public ResponseEntity<ApiResponse<RoleDetailResponse>> getRoleById(@PathVariable Long id) {
        RoleDetailResponse role = roleService.getRoleById(id);
        return ResponseEntity.ok(ApiResponse.ok(role, "Lấy thông tin role thành công"));
    }

    @PutMapping("/{id}/permissions")
    public ResponseEntity<ApiResponse<RoleDetailResponse>> assignPermissions(
            @PathVariable Long id,
            @RequestBody AssignPermissionsRequest request
    ) {
        RoleDetailResponse role = roleService.assignPermissionsToRole(id, request.getPermissionIds());
        return ResponseEntity.ok(ApiResponse.ok(role, "Gán danh sách permissions cho role thành công"));
    }

    @PostMapping("/{roleId}/permissions/{permissionId}")
    public ResponseEntity<ApiResponse<RoleDetailResponse>> addPermission(
            @PathVariable Long roleId,
            @PathVariable Long permissionId
    ) {
        RoleDetailResponse role = roleService.addPermissionToRole(roleId, permissionId);
        return ResponseEntity.ok(ApiResponse.ok(role, "Thêm permission vào role thành công"));
    }

    @DeleteMapping("/{roleId}/permissions/{permissionId}")
    public ResponseEntity<ApiResponse<RoleDetailResponse>> removePermission(
            @PathVariable Long roleId,
            @PathVariable Long permissionId
    ) {
        RoleDetailResponse role = roleService.removePermissionFromRole(roleId, permissionId);
        return ResponseEntity.ok(ApiResponse.ok(role, "Xoá permission khỏi role thành công"));
    }

    @PostMapping("/{id}/reset")
    public ResponseEntity<ApiResponse<ResetRoleResponse>> resetToDefault(@PathVariable Long id) {
        RoleDetailResponse role = roleService.resetRoleToDefault(id);
        ResetRoleResponse response = new ResetRoleResponse(
                "Role đã được reset về cấu hình mặc định. Vui lòng restart ứng dụng để áp dụng permissions từ code.",
                role
        );
        return ResponseEntity.ok(ApiResponse.ok(response, "Reset role về mặc định thành công"));
    }

    @DeleteMapping("/users/{userId}/roles/{roleId}")
    public ResponseEntity<ApiResponse<Void>> unassignRoleFromUser(
            @PathVariable Long userId,
            @PathVariable Long roleId
    ) {
        roleService.unassignRoleFromUser(userId, roleId);
        return ResponseEntity.ok(ApiResponse.okMsg("Role đã được gỡ khỏi user thành công"));
    }
}
