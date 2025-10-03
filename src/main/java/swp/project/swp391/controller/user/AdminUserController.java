package swp.project.swp391.controller.user;

import io.swagger.v3.oas.annotations.Operation;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.request.user.CreateUserRequest;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.response.user.CreateUserResponse;
import swp.project.swp391.response.user.UserResponse;
import swp.project.swp391.service.role.RoleService;
import swp.project.swp391.service.user.UserService;

import java.util.List;

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

    @GetMapping("/get-all-users")
    public ResponseEntity<ApiResponse<List<UserResponse>>> getAllUsers() {
        return ResponseEntity.ok(userService.getAllUsers());
    }

    @Operation(
            summary = "Tạo người dùng nội bộ",
            description = """
            ADMIN: tạo được EVM_STAFF, DEALER_MANAGER, DEALER_STAFF;
            EVM_STAFF: tạo được DEALER_MANAGER, DEALER_STAFF;
            DEALER_MANAGER: tạo được DEALER_STAFF (trong dealer của mình).
            Email sẽ nhận OTP để xác thực & đặt mật khẩu lần đầu.
            """
    )
    @PostMapping("/create-users")
    public ResponseEntity<ApiResponse<Void>> createUser(@Valid @RequestBody CreateUserRequest request) {
        userService.createUser(request);
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(ApiResponse.okMsg("Tạo tài khoản thành công. OTP đã gửi đến email người dùng."));
    }
}
