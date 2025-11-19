package swp.project.swp391.response.role;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;
import swp.project.swp391.entity.Role;

import java.util.LinkedHashSet;
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

    // Dùng LinkedHashSet để giữ nguyên thứ tự chèn (đã sort trước đó)
    private LinkedHashSet<PermissionInfo> permissions;

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
     * Convert từ Entity sang Response (permissions đã sort theo ID)
     */
    public static RoleDetailResponse fromEntity(Role role) {
        LinkedHashSet<PermissionInfo> permissionInfos = role.getPermissions().stream()
                .sorted(java.util.Comparator.comparing(p -> p.getId())) // sort theo id
                .map(p -> new PermissionInfo(
                        p.getId(),
                        p.getName(),
                        p.getDisplayName(),
                        p.getDescription(),
                        p.getResource(),
                        p.getAction()
                ))
                .collect(Collectors.toCollection(LinkedHashSet::new));

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
