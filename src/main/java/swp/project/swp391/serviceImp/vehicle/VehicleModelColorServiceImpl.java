package swp.project.swp391.serviceImp.vehicle;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.*;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.*;
import swp.project.swp391.response.vehicle.VehicleModelColorResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.vehicle.VehicleModelColorService;

import java.math.BigDecimal;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class VehicleModelColorServiceImpl implements VehicleModelColorService {

    private final VehicleModelRepository modelRepo;
    private final ColorRepository colorRepo;
    private final VehicleModelColorRepository modelColorRepo;
    private final RbacGuard guard;

    // ---------------- ASSIGN ----------------
    @Override
    @Transactional
    public VehicleModelColorResponse assignColorToModel(Long modelId, Long colorId, BigDecimal priceAdjustment, User currentUser) {
        guard.require(guard.has(currentUser, "vehicleModelColor.create"));

        VehicleModel model = modelRepo.findById(modelId)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_MODEL_NOT_FOUND));

        Color color = colorRepo.findById(colorId)
                .orElseThrow(() -> new BaseException(ErrorHandler.COLOR_NOT_FOUND));

        if (modelColorRepo.existsByVehicleModelAndColor(model, color)) {
            throw new BaseException(ErrorHandler.VEHICLE_COLOR_ALREADY_EXISTS);
        }

        VehicleModelColor entity = VehicleModelColor.builder()
                .vehicleModel(model)
                .color(color)
                .priceAdjustment(priceAdjustment != null ? priceAdjustment : BigDecimal.ZERO)
                .isActive(true)
                .build();

        modelColorRepo.save(entity);
        return map(entity);
    }

    // ---------------- GET COLORS BY MODEL ----------------
    @Override
    public List<VehicleModelColorResponse> getColorsByModel(Long modelId) {
        // Kiểm tra model có tồn tại hay không
        if (!modelRepo.existsById(modelId)) {
            throw new BaseException(ErrorHandler.VEHICLE_MODEL_NOT_FOUND);
        }
        List<VehicleModelColor> list = modelColorRepo.findByVehicleModelId(modelId);

        return list.stream().map(this::map).collect(Collectors.toList());
    }

    // ---------------- UPDATE PRICE ----------------
    @Override
    @Transactional
    public VehicleModelColorResponse updatePriceAdjustment(Long modelId, Long colorId, BigDecimal newAdjustment, User currentUser) {
        guard.require(guard.has(currentUser, "vehicleModelColor.update"));

        VehicleModelColor vmc = modelColorRepo.findByVehicleModelIdAndColorId(modelId, colorId)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_COLOR_NOT_FOUND));

        vmc.setPriceAdjustment(newAdjustment);
        modelColorRepo.save(vmc);
        return map(vmc);
    }

    // ---------------- UNASSIGN ----------------
    @Override
    @Transactional
    public void unassignColor(Long modelId, Long colorId, User currentUser) {
        guard.require(guard.has(currentUser, "vehicleModelColor.delete"));

        VehicleModelColor vmc = modelColorRepo.findByVehicleModelIdAndColorId(modelId, colorId)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_COLOR_NOT_FOUND));

        modelColorRepo.delete(vmc);
    }

    // ---------------- MAPPER ----------------
    private VehicleModelColorResponse map(VehicleModelColor entity) {
        return new VehicleModelColorResponse(
                entity.getId(),
                entity.getVehicleModel().getId(),
                entity.getColor().getId(),
                entity.getColor().getColorName(),
                entity.getColor().getHexCode(),
                entity.getPriceAdjustment(),
                entity.getImageUrl(),
                entity.getIsActive()
        );
    }
}
