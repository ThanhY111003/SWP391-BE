package swp.project.swp391.controller.user;

import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import swp.project.swp391.response.ApiResponse;
import swp.project.swp391.service.user.UserService;

@RestController
@RequestMapping("/api/admin/users")
@RequiredArgsConstructor
public class AdminUserController {
    private final UserService userService;

    @PutMapping("/{id}/inactive")
    public ResponseEntity<ApiResponse<?>> inactiveUser(@PathVariable Long id) {
        return ResponseEntity.ok(userService.inactiveUser(id));
    }

    @PutMapping("/{id}/reactivate")
    public ResponseEntity<ApiResponse<?>> reactivateUser(@PathVariable Long id) {
        return ResponseEntity.ok(userService.reactivateUser(id));
    }
}
