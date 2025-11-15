package swp.project.swp391.service.vehicle;

import org.springframework.web.multipart.MultipartFile;
import swp.project.swp391.entity.VehicleInstance;
import swp.project.swp391.request.vehicle.AssignVehicleRequest;
import swp.project.swp391.request.vehicle.TransferVehicleRequest;
import swp.project.swp391.request.vehicle.VehicleInstanceCreateRequest;
import swp.project.swp391.request.vehicle.VehicleInstanceUpdateRequest;
import swp.project.swp391.response.vehicle.CustomerVehicleResponse;
import swp.project.swp391.response.vehicle.VehicleImportResult;
import swp.project.swp391.response.vehicle.VehicleInstanceResponse;

import java.util.List;

public interface VehicleInstanceService {

    /**
     * Lấy danh sách xe (có thể lọc theo đại lý, trạng thái, và activeOnly)
     */
    List<VehicleInstanceResponse> getAll(Long dealerId, VehicleInstance.VehicleStatus status, Boolean activeOnly);

    /**
     * Lấy chi tiết 1 xe theo ID
     */
    VehicleInstanceResponse getById(Long id);

    /**
     * Gán xe cho khách hàng (bán xe)
     */
    CustomerVehicleResponse  assignToCustomer(AssignVehicleRequest request);

    /**
     * Vô hiệu hóa xe (chỉ khi xe đang trong kho)
     */
    void deactivate(Long id);

    /**
     * Kích hoạt lại xe (chỉ khi xe đang bị vô hiệu hóa)
     */
    void activate(Long id);

    /**
     * Cập nhật trạng thái xe
     */
    VehicleInstanceResponse updateStatus(Long id, VehicleInstance.VehicleStatus status);

    VehicleInstanceResponse transferVehicle(TransferVehicleRequest req);

    VehicleInstanceResponse create(VehicleInstanceCreateRequest request);

    VehicleInstanceResponse update(Long id, VehicleInstanceUpdateRequest request);

    VehicleImportResult importVehiclesFromExcel(MultipartFile file);
}
