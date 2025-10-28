package swp.project.swp391.controller.role;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
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
@Tag(name = "Quản lý vai trò & phân quyền (Role/Permission)", description = "Các API phục vụ quản trị vai trò, quyền và gán quyền cho người dùng")
public class RoleController {

    private final RoleService roleService;

    @Operation(summary = "Lấy danh sách tất cả Roles", description = "Trả về danh sách đầy đủ các vai trò (roles) trong hệ thống")
    @GetMapping
    public ResponseEntity<ApiResponse<List<RoleResponse>>> getAllRoles() {
        List<RoleResponse> roles = roleService.getAllRoles();
        return ResponseEntity.ok(ApiResponse.ok(roles, "Lấy danh sách roles thành công"));
    }

    @Operation(summary = "Lấy danh sách tất cả Permissions", description = "Trả về toàn bộ danh sách quyền (permissions) trong hệ thống")
    @GetMapping("/permissions")
    public ResponseEntity<ApiResponse<List<PermissionResponse>>> getAllPermissions() {
        List<PermissionResponse> data = roleService.getAllPermissions();
        return ResponseEntity.ok(ApiResponse.ok(data, "Lấy danh sách permissions thành công"));
    }

    @Operation(summary = "Lấy thông tin chi tiết của một Role", description = "Trả về thông tin chi tiết và các permissions của một role theo ID")
    @GetMapping("/{id}")
    public ResponseEntity<ApiResponse<RoleDetailResponse>> getRoleById(@PathVariable Long id) {
        RoleDetailResponse role = roleService.getRoleById(id);
        return ResponseEntity.ok(ApiResponse.ok(role, "Lấy thông tin role thành công"));
    }

    @Operation(summary = "Thêm permissions vào Role", description = "Thêm danh sách quyền mới vào role, bỏ qua quyền đã tồn tại")
    @PostMapping("/{roleId}/permissions/add")
    public ResponseEntity<ApiResponse<RoleDetailResponse>> addPermissions(
            @PathVariable Long roleId,
            @RequestBody @Valid RolePermissionBulkRequest body
    ) {
        RoleDetailResponse role = roleService.addPermissionsToRole(roleId, body.getPermissionIds());
        return ResponseEntity.ok(ApiResponse.ok(role, "Thêm permissions vào role thành công"));
    }

    @Operation(summary = "Xóa permissions khỏi Role", description = "Gỡ danh sách quyền khỏi role theo ID")
    @DeleteMapping("/{roleId}/permissions/remove")
    public ResponseEntity<ApiResponse<RoleDetailResponse>> removePermissions(
            @PathVariable Long roleId,
            @RequestBody @Valid RolePermissionBulkRequest body
    ) {
        RoleDetailResponse role = roleService.removePermissionsFromRole(roleId, body.getPermissionIds());
        return ResponseEntity.ok(ApiResponse.ok(role, "Gỡ permissions khỏi role thành công"));
    }

    @Operation(summary = "Reset Role về mặc định", description = "Đặt lại Role về cấu hình mặc định (yêu cầu restart ứng dụng để áp dụng permissions mới)")
    @PostMapping("/{id}/reset")
    public ResponseEntity<ApiResponse<ResetRoleResponse>> resetToDefault(@PathVariable Long id) {
        RoleDetailResponse role = roleService.resetRoleToDefault(id);
        ResetRoleResponse response = new ResetRoleResponse(
                "Role đã được reset về cấu hình mặc định. Vui lòng restart ứng dụng để áp dụng permissions từ code.",
                role
        );
        return ResponseEntity.ok(ApiResponse.ok(response, "Reset role về mặc định thành công"));
    }

    @Operation(summary = "Gán Role cho User", description = "Gán một vai trò cụ thể cho người dùng")
    @PostMapping("/assign/users/{userId}/roles/{roleId}")
    public ResponseEntity<ApiResponse<Void>> assignRoleToUser(
            @PathVariable Long userId,
            @PathVariable Long roleId
    ) {
        roleService.assignRoleToUser(userId, roleId);
        return ResponseEntity.ok(ApiResponse.okMsg("Gán role cho user thành công"));
    }

    @Operation(summary = "Gỡ Role khỏi User", description = "Xóa vai trò đã được gán khỏi người dùng")
    @DeleteMapping("/assign/users/{userId}/roles/{roleId}")
    public ResponseEntity<ApiResponse<Void>> unassignRoleFromUser(
            @PathVariable Long userId,
            @PathVariable Long roleId
    ) {
        roleService.unassignRoleFromUser(userId, roleId);
        return ResponseEntity.ok(ApiResponse.okMsg("Gỡ role khỏi user thành công"));
    }
}
