package swp.project.swp391.request.role;

import jakarta.validation.constraints.NotEmpty;
import jakarta.validation.constraints.NotNull;
import lombok.Data;

import java.util.Set;

/**
 * Dùng cho 2 API:
 *  - POST /api/roles/{roleId}/permissions:add
 *  - POST /api/roles/{roleId}/permissions:remove
 *
 * Yêu cầu: truyền danh sách permissionId cần thêm/gỡ.
 */
@Data
public class RolePermissionBulkRequest {

    @NotNull(message = "permissionIds không được null")
    @NotEmpty(message = "permissionIds không được rỗng")
    private Set<Long> permissionIds;
}
