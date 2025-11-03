    package swp.project.swp391.service.vehicle;

    import swp.project.swp391.entity.User;
    import swp.project.swp391.request.vehicle.VehicleModelRequest;
    import swp.project.swp391.response.vehicle.VehicleModelResponse;

    import java.util.List;

    public interface VehicleModelService {

        /**
         * Tạo model xe mới.
         */
        VehicleModelResponse createVehicleModel(VehicleModelRequest request, User currentUser);

        /**
         * Lấy danh sách tất cả model.
         */
        List<VehicleModelResponse> getAllVehicleModels();

        /**
         * Lấy chi tiết model theo id.
         */
        VehicleModelResponse getVehicleModelById(Long id);

        /**
         * Cập nhật thông tin model.
         */
        VehicleModelResponse updateVehicleModel(Long id, VehicleModelRequest request, User currentUser);

        VehicleModelResponse inactiveModel(Long id, User currentUser);

        VehicleModelResponse reactiveModel(Long id, User currentUser);

    }
