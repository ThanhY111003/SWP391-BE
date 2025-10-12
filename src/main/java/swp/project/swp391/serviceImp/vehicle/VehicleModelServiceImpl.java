package swp.project.swp391.serviceImp.vehicle;

import jakarta.transaction.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.User;
import swp.project.swp391.entity.VehicleModel;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.VehicleModelRepository;
import swp.project.swp391.request.vehicle.VehicleModelRequest;
import swp.project.swp391.response.vehicle.VehicleModelResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.vehicle.VehicleModelService;

import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class VehicleModelServiceImpl implements VehicleModelService {

    private final VehicleModelRepository modelRepo;
    private final RbacGuard guard;

    // ----------------------------
    // CREATE
    // ----------------------------
    @Override
    @Transactional
    public VehicleModelResponse createVehicleModel(VehicleModelRequest request, User currentUser) {
        guard.require(guard.has(currentUser, "vehicleModel.create"));

        // Check modelCode trùng
        if (modelRepo.findByModelCode(request.getModelCode()).isPresent()) {
            throw new BaseException(ErrorHandler.VEHICLE_MODEL_ALREADY_EXISTS);
        }

        VehicleModel model = VehicleModel.builder()
                .name(request.getName())
                .modelCode(request.getModelCode())
                .description(request.getDescription())
                .brand(request.getBrand())
                .year(request.getYear())
                .batteryCapacity(request.getBatteryCapacity())
                .rangeKm(request.getRangeKm())
                .chargingTime(request.getChargingTime())
                .maxSpeed(request.getMaxSpeed())
                .acceleration(request.getAcceleration())
                .seatingCapacity(request.getSeatingCapacity())
                .cargoVolume(request.getCargoVolume())
                .manufacturerPrice(request.getManufacturerPrice())
                .imageUrl(request.getImageUrl())
                .isActive(true)
                .build();

        modelRepo.save(model);
        return mapToResponse(model);
    }

    // ----------------------------
    // GET ALL
    // ----------------------------
    @Override
    public List<VehicleModelResponse> getAllVehicleModels() {
        List<VehicleModel> list = modelRepo.findAll(); // lấy toàn bộ, không lọc
        return list.stream().map(this::mapToResponse).collect(Collectors.toList());
    }

    // ----------------------------
    // GET BY ID
    // ----------------------------
    @Override
    public VehicleModelResponse getVehicleModelById(Long id) {
        VehicleModel model = modelRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_MODEL_NOT_FOUND));
        return mapToResponse(model);
    }

    // ----------------------------
    // UPDATE
    // ----------------------------
    @Override
    @Transactional
    public VehicleModelResponse updateVehicleModel(Long id, VehicleModelRequest request, User currentUser) {
        guard.require(guard.has(currentUser, "vehicleModel.update"));

        VehicleModel model = modelRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_MODEL_NOT_FOUND));

        // Nếu đổi modelCode thì check trùng
        if (request.getModelCode() != null && !request.getModelCode().equals(model.getModelCode())) {
            if (modelRepo.findByModelCode(request.getModelCode()).isPresent()) {
                throw new BaseException(ErrorHandler.VEHICLE_MODEL_ALREADY_EXISTS);
            }
            model.setModelCode(request.getModelCode());
        }

        if (request.getName() != null) model.setName(request.getName());
        if (request.getDescription() != null) model.setDescription(request.getDescription());
        if (request.getBrand() != null) model.setBrand(request.getBrand());
        if (request.getYear() != null) model.setYear(request.getYear());
        if (request.getBatteryCapacity() != null) model.setBatteryCapacity(request.getBatteryCapacity());
        if (request.getRangeKm() != null) model.setRangeKm(request.getRangeKm());
        if (request.getChargingTime() != null) model.setChargingTime(request.getChargingTime());
        if (request.getMaxSpeed() != null) model.setMaxSpeed(request.getMaxSpeed());
        if (request.getAcceleration() != null) model.setAcceleration(request.getAcceleration());
        if (request.getSeatingCapacity() != null) model.setSeatingCapacity(request.getSeatingCapacity());
        if (request.getCargoVolume() != null) model.setCargoVolume(request.getCargoVolume());
        if (request.getManufacturerPrice() != null) model.setManufacturerPrice(request.getManufacturerPrice());
        if (request.getImageUrl() != null) model.setImageUrl(request.getImageUrl());

        // updatedAt sẽ tự set trong @PreUpdate
        modelRepo.save(model);
        return mapToResponse(model);
    }

    // ----------------------------
    // Helper
    // ----------------------------
    private VehicleModelResponse mapToResponse(VehicleModel model) {
        return new VehicleModelResponse(
                model.getId(),
                model.getName(),
                model.getModelCode(),
                model.getBrand(),
                model.getYear(),
                model.getBatteryCapacity(),
                model.getRangeKm(),
                model.getChargingTime(),
                model.getMaxSpeed(),
                model.getAcceleration(),
                model.getSeatingCapacity(),
                model.getCargoVolume(),
                model.getManufacturerPrice(),
                model.getImageUrl(),
                model.getIsActive()
        );
    }
}
