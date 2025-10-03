package swp.project.swp391.request.role;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

import java.util.Set;

/**
 * Request để gán danh sách permissions vào role
 */
@Data
@NoArgsConstructor
@AllArgsConstructor
public class AssignPermissionsRequest {
    private Set<Long> permissionIds;
}