package swp.project.swp391.service.user;

import swp.project.swp391.response.ApiResponse;

public interface UserService {
    ApiResponse<?> inactiveUser(Long userId);
    ApiResponse<?> reactivateUser(Long userId);
}


