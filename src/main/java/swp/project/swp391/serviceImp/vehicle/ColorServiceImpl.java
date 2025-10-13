package swp.project.swp391.serviceImp.vehicle;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.Color;
import swp.project.swp391.entity.User;
import swp.project.swp391.entity.VehicleModelColor;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.ColorRepository;
import swp.project.swp391.repository.VehicleModelColorRepository;
import swp.project.swp391.request.vehicle.ColorRequest;
import swp.project.swp391.response.vehicle.ColorResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.vehicle.ColorService;

import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class ColorServiceImpl implements ColorService {
    private final VehicleModelColorRepository vehicleModelRepository;
    private final ColorRepository colorRepo;
    private final RbacGuard guard;

    // ------------------ CREATE ------------------
    @Override
    @Transactional
    public ColorResponse createColor(ColorRequest request, User currentUser) {
        guard.require(guard.has(currentUser, "color.create"));

        if (colorRepo.existsByHexCode(request.getHexCode())) {
            throw new BaseException(ErrorHandler.COLOR_ALREADY_EXISTS);
        }

        Color color = Color.builder()
                .colorName(request.getColorName())
                .hexCode(request.getHexCode())
                .isActive(true)
                .build();
        colorRepo.save(color);

        return map(color);
    }

    // ------------------ UPDATE ------------------
    @Override
    @Transactional
    public ColorResponse updateColor(Long id, ColorRequest request, User currentUser) {
        guard.require(guard.has(currentUser, "color.update"));

        Color color = colorRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.COLOR_NOT_FOUND));

        // Nếu đổi mã hex thì kiểm tra trùng
        if (!color.getHexCode().equals(request.getHexCode())
                && colorRepo.existsByHexCode(request.getHexCode())) {
            throw new BaseException(ErrorHandler.COLOR_ALREADY_EXISTS);
        }

        color.setColorName(request.getColorName());
        color.setHexCode(request.getHexCode());
        colorRepo.save(color);
        return map(color);
    }

    // ------------------ INACTIVE ------------------
    @Override
    @Transactional
    public ColorResponse inactiveColor(Long id, User currentUser) {
        guard.require(guard.has(currentUser, "color.inactive"));

        Color color = colorRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.COLOR_NOT_FOUND));

        // Kiểm tra xem màu có đang được gán vào mô hình nào không
        if (isColorAssignedToModel(color)) {
            throw new BaseException(ErrorHandler.COLOR_ASSIGNED_TO_MODEL);
        }

        color.setIsActive(false);
        colorRepo.save(color);
        return map(color);
    }

    // ------------------ Helper ------------------
// Phương thức kiểm tra màu có đang được gán vào mô hình không
    private boolean isColorAssignedToModel(Color color) {
        return vehicleModelRepository.existsByColor(color);
    }


    // ------------------ REACTIVE ------------------
    @Override
    @Transactional
    public ColorResponse reactiveColor(Long id, User currentUser) {
        guard.require(guard.has(currentUser, "color.reactive"));

        Color color = colorRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.COLOR_NOT_FOUND));
        color.setIsActive(true);
        colorRepo.save(color);
        return map(color);
    }

    // ------------------ GET ALL ------------------
    @Override
    public List<ColorResponse> getAllColors() {
        List<Color> colors = colorRepo.findAll();
        return colors.stream().map(this::map).collect(Collectors.toList());
    }

    // ------------------ GET BY ID ------------------
    @Override
    public ColorResponse getColorById(Long id) {
        Color color = colorRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.COLOR_NOT_FOUND));
        return map(color);
    }

    // ------------------ Helper ------------------
    private ColorResponse map(Color c) {
        return new ColorResponse(c.getId(), c.getColorName(), c.getHexCode(), c.getIsActive());
    }
}
