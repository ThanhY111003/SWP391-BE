package swp.project.swp391.serviceImp.vehicle;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.User;
import swp.project.swp391.entity.VehicleColor;
import swp.project.swp391.entity.VehicleModel;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.VehicleColorRepository;
import swp.project.swp391.repository.VehicleModelRepository;
import swp.project.swp391.request.vehicle.VehicleColorRequest;
import swp.project.swp391.response.vehicle.VehicleColorResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.vehicle.VehicleColorService;

@Service
@RequiredArgsConstructor
public class VehicleColorServiceImpl implements VehicleColorService {
    private final VehicleColorRepository repo;
    private final VehicleModelRepository modelRepo;
    private final RbacGuard guard;

    @Override
    public VehicleColorResponse createVehicleColor(VehicleColorRequest request, User currentUser) {
        // Kiểm tra phân quyền - cho phép EVM_STAFF và role khác tạo màu xe
        guard.require(guard.has(currentUser, "vehicleColor.create"));

        // Lấy thông tin VehicleModel từ DB
        VehicleModel model = modelRepo.findById(request.getVehicleModelId())
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_MODEL_NOT_FOUND));

        // Tạo đối tượng VehicleColor mới
        VehicleColor color = new VehicleColor();
        color.setColorName(request.getColorName());
        color.setHexCode(request.getHexCode());
        color.setPriceAdjustment(request.getPriceAdjustment());
        color.setVehicleModel(model);

        // Lưu màu xe vào DB
        repo.save(color);

        // Trả về thông tin màu xe đã tạo
        return new VehicleColorResponse(color.getId(), color.getColorName(),
                color.getHexCode(), color.getPriceAdjustment(), model.getId());
    }
}
