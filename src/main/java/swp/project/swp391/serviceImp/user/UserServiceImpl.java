package swp.project.swp391.serviceImp.user;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.User;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.UserRepository;
import swp.project.swp391.response.ApiResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.user.UserService;

@Service
@RequiredArgsConstructor
public class UserServiceImpl implements UserService {
    private final UserRepository userRepository;
    private final RbacGuard guard;

    @Override
    @Transactional
    public ApiResponse<?> inactiveUser(Long userId) {
        User currentUser = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        guard.require(guard.has(currentUser, "user.inactive"));

        User target = userRepository.findById(userId)
                .orElseThrow(() -> new BaseException(ErrorHandler.USER_NOT_FOUND));

        boolean isAdmin = target.getRoles().stream().anyMatch(r -> r.getName().equals("ADMIN"));
        if (isAdmin) {
            throw new BaseException(ErrorHandler.FORBIDDEN);
        }

        target.setIsActive(false);
        userRepository.save(target);

        return ApiResponse.builder()
                .success(true)
                .message("User đã bị vô hiệu hóa thành công")
                .build();
    }

    @Override
    @Transactional
    public ApiResponse<?> reactivateUser(Long userId) {
        User currentUser = (User) SecurityContextHolder.getContext().getAuthentication().getPrincipal();
        guard.require(guard.has(currentUser, "user.reactivate"));

        User target = userRepository.findById(userId)
                .orElseThrow(() -> new BaseException(ErrorHandler.USER_NOT_FOUND));

        target.setIsActive(true);
        userRepository.save(target);

        return ApiResponse.builder()
                .success(true)
                .message("User đã được kích hoạt lại thành công")
                .build();
    }

}
