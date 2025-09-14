package swp.project.swp391.response;

import lombok.Builder;
import lombok.Data;

@Data
@Builder
public class RefreshTokenResponse {
    private String token; // Access Token mới
}