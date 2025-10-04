package swp.project.swp391.controller.role;

import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.request.role.RolePermissionBulkRequest;
import swp.project.swp391.response.role.PermissionResponse;
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

    @GetMapping("/all")
    public ResponseEntity<ApiResponse<List<RoleResponse>>> getAllRoles() {
        List<RoleResponse> roles = roleService.getAllRoles();
        return ResponseEntity.ok(ApiResponse.ok(roles, "Lấy danh sách tất cả roles thành công"));
    }

    // RoleController.java
    @GetMapping("/permissions/all")
    public ResponseEntity<ApiResponse<List<PermissionResponse>>> getAllPermissions() {
        List<PermissionResponse> data = roleService.getAllPermissions();
        return ResponseEntity.ok(ApiResponse.ok(data, "Lấy danh sách permissions thành công"));
    }


    @GetMapping("/get-role/{id}")
    public ResponseEntity<ApiResponse<RoleDetailResponse>> getRoleById(@PathVariable Long id) {
        RoleDetailResponse role = roleService.getRoleById(id);
        return ResponseEntity.ok(ApiResponse.ok(role, "Lấy thông tin role thành công"));
    }


    @PostMapping("/{roleId}/permissions:remove")
    public ResponseEntity<ApiResponse<RoleDetailResponse>> removePermissions(
            @PathVariable Long roleId,
            @RequestBody @jakarta.validation.Valid RolePermissionBulkRequest body
    ) {
        RoleDetailResponse role = roleService.removePermissionsFromRole(roleId, body.getPermissionIds());
        return ResponseEntity.ok(ApiResponse.ok(role, "Gỡ permissions khỏi role thành công"));
    }

    // ✅ Bulk add (add-only, bỏ qua trùng)
    @PostMapping("/{roleId}/permissions:add")
    public ResponseEntity<ApiResponse<RoleDetailResponse>> addPermissions(
            @PathVariable Long roleId,
            @RequestBody @jakarta.validation.Valid RolePermissionBulkRequest body
    ) {
        RoleDetailResponse role = roleService.addPermissionsToRole(roleId, body.getPermissionIds());
        return ResponseEntity.ok(ApiResponse.ok(role, "Thêm permissions vào role thành công"));
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

    // Gán 1 role cho 1 user
    @PostMapping("assign/users/{userId}/roles/{roleId}")
    public ResponseEntity<ApiResponse<Void>> assignRoleToUser(
            @PathVariable Long userId,
            @PathVariable Long roleId
    ) {
        roleService.assignRoleToUser(userId, roleId);
        return ResponseEntity.ok(ApiResponse.okMsg("Gán role cho user thành công"));
    }

    // Gỡ 1 role khỏi 1 user
    @DeleteMapping("/unassign/users/{userId}/roles/{roleId}")
    public ResponseEntity<ApiResponse<Void>> unassignRoleFromUser(
            @PathVariable Long userId,
            @PathVariable Long roleId
    ) {
        roleService.unassignRoleFromUser(userId, roleId);
        return ResponseEntity.ok(ApiResponse.okMsg("Gỡ role khỏi user thành công"));
    }

}
