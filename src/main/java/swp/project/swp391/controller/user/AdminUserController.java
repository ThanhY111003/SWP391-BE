package swp.project.swp391.controller.user;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.request.user.CreateUserRequest;
import swp.project.swp391.response.user.UserResponse;
import swp.project.swp391.service.user.UserService;

import java.util.List;

@RestController
@RequestMapping("/api/admin/users")
@RequiredArgsConstructor
@Tag(name = "Quản lý người dùng (Admin)", description = "Các API dành cho Admin quản trị, tạo và quản lý tài khoản nội bộ")
public class AdminUserController {

    private final UserService userService;

    @Operation(summary = "Lấy danh sách người dùng", description = "Trả về danh sách tất cả người dùng nội bộ trong hệ thống")
    @GetMapping
    public ResponseEntity<ApiResponse<List<UserResponse>>> getAllUsers() {
        return ResponseEntity.ok(userService.getAllUsers());
    }

    @Operation(summary = "Tạo người dùng nội bộ", description = """
        ADMIN: tạo được EVM_STAFF, DEALER_MANAGER, DEALER_STAFF.  
        EVM_STAFF: tạo được DEALER_MANAGER, DEALER_STAFF.  
        DEALER_MANAGER: tạo được DEALER_STAFF (trong dealer của mình).  
        Hệ thống sẽ gửi email OTP để xác thực & đặt mật khẩu lần đầu.
        """)
    @PostMapping
    public ResponseEntity<ApiResponse<Void>> createUser(@Valid @RequestBody CreateUserRequest request) {
        userService.createUser(request);
        return ResponseEntity.status(HttpStatus.CREATED)
                .body(ApiResponse.okMsg("Tạo tài khoản thành công. Tên đăng nhập và mật khẩu đã được gửi đến email người dùng."));
    }

    @Operation(summary = "Vô hiệu hóa người dùng", description = "Chuyển trạng thái người dùng sang không hoạt động (inactive)")
    @PatchMapping("/{id}/deactivate")
    public ResponseEntity<ApiResponse<?>> deactivateUser(@PathVariable Long id) {
        return ResponseEntity.ok(userService.inactiveUser(id));
    }

    @Operation(summary = "Kích hoạt lại người dùng", description = "Kích hoạt lại người dùng từ trạng thái inactive")
    @PatchMapping("/{id}/activate")
    public ResponseEntity<ApiResponse<?>> activateUser(@PathVariable Long id) {
        return ResponseEntity.ok(userService.reactivateUser(id));
    }
}
