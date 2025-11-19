
package swp.project.swp391.response.role;

import lombok.AllArgsConstructor;
import lombok.Data;
import lombok.NoArgsConstructor;

@Data
@AllArgsConstructor // <- tạo constructor với đủ 7 tham số
@NoArgsConstructor
public class PermissionResponse {
    private Long id;
    private String name;
    private String displayName;
    private String description;
    private String resource;
    private String action;
    private Boolean isActive;
}
