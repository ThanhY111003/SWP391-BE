package swp.project.swp391.response.role;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class ResetRoleResponse {
    private String message;
    private RoleDetailResponse role;
}
