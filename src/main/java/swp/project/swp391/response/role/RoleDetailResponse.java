package swp.project.swp391.response.role;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import swp.project.swp391.entity.Role;

import java.util.Set;
import java.util.stream.Collectors;

@Data
@NoArgsConstructor
@AllArgsConstructor
public class RoleDetailResponse {
    private Long id;
    private String name;
    private String displayName;
    private String description;
    private Boolean isActive;
    private Boolean isCustomized;
    private Set<PermissionInfo> permissions;

    @Data
    @NoArgsConstructor
    @AllArgsConstructor
    public static class PermissionInfo {
        private Long id;
        private String name;
        private String displayName;
        private String description;
        private String resource;
        private String action;
    }

    /**
     * Convert từ Entity sang Response
     */
    public static RoleDetailResponse fromEntity(Role role) {
        Set<PermissionInfo> permissionInfos = role.getPermissions().stream()
                .map(p -> new PermissionInfo(
                        p.getId(),
                        p.getName(),
                        p.getDisplayName(),
                        p.getDescription(),
                        p.getResource(),
                        p.getAction()
                ))
                .collect(Collectors.toSet());

        return new RoleDetailResponse(
                role.getId(),
                role.getName(),
                role.getDisplayName(),
                role.getDescription(),
                role.getIsActive(),
                role.getIsCustomized(),
                permissionInfos
        );
    }
}
