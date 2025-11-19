package swp.project.swp391.service.user;

import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.request.user.CreateUserRequest;
import swp.project.swp391.request.user.UpdateUserProfileRequest;
import swp.project.swp391.response.user.UserDetailResponse;
import swp.project.swp391.response.user.UserResponse;

import java.util.List;

public interface UserService {
    void createUser(CreateUserRequest request);

    ApiResponse<?> inactiveUser(Long userId);

    ApiResponse<?> reactivateUser(Long userId);

    ApiResponse<List<UserResponse>> getAllUsers();

    ApiResponse<UserDetailResponse> getUserById(Long id);

    ApiResponse<UserDetailResponse> getMyProfile();

    ApiResponse<UserDetailResponse> updateMyProfile(UpdateUserProfileRequest request);

    ApiResponse<UserDetailResponse> updateUserProfile(Long id, UpdateUserProfileRequest request);

    ApiResponse<Void> assignUserToDealer(Long userId, Long dealerId);


}
