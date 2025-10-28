package swp.project.swp391.serviceImp.vehicle;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.*;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.*;
import swp.project.swp391.response.vehicle.VehicleInstanceResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.vehicle.VehicleInstanceService;

import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class VehicleInstanceServiceImpl implements VehicleInstanceService {

    private final VehicleInstanceRepository vehicleRepo;
    private final CustomerRepository customerRepo;
    private final CustomerVehicleRepository customerVehicleRepo;
    private final UserRepository userRepo;
    private final RbacGuard guard;

    // --------------------------------------------------------
    // GET ALL (có lọc)
    // --------------------------------------------------------
    @Override
    public List<VehicleInstanceResponse> getAll(Long dealerId, VehicleInstance.VehicleStatus status, Boolean activeOnly) {
        guard.require(guard.has(guard.me(), "vehicle.read_all"));

        List<VehicleInstance> list = vehicleRepo.findAll();

        // Nếu lọc theo đại lý
        if (dealerId != null) {
            list = list.stream()
                    .filter(v -> v.getCurrentDealer() != null && dealerId.equals(v.getCurrentDealer().getId()))
                    .collect(Collectors.toList());
        }

        // Nếu có status thì mới lọc theo trạng thái
        if (status != null) {
            list = list.stream()
                    .filter(v -> v.getStatus() == status)
                    .collect(Collectors.toList());
        }

        // ✅ Mặc định show cả active & inactive
        // => Chỉ lọc nếu người gọi muốn xem activeOnly = true
        if (Boolean.TRUE.equals(activeOnly)) {
            list = list.stream()
                    .filter(VehicleInstance::getIsActive)
                    .collect(Collectors.toList());
        }

        return list.stream()
                .map(VehicleInstanceResponse::fromEntity)
                .collect(Collectors.toList());
    }



    // --------------------------------------------------------
    // GET BY ID
    // --------------------------------------------------------
    @Override
    public VehicleInstanceResponse getById(Long id) {
        guard.require(guard.has(guard.me(), "vehicle.read"));

        VehicleInstance v = vehicleRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));
        return VehicleInstanceResponse.fromEntity(v);
    }

    // --------------------------------------------------------
    // ASSIGN TO CUSTOMER (BÁN XE)
    // --------------------------------------------------------
    @Override
    @Transactional
    public void assignToCustomer(Long vehicleId, Long customerId, Long soldByUserId) {
        guard.require(guard.has(guard.me(), "vehicle.assign_customer"));

        VehicleInstance vehicle = vehicleRepo.findById(vehicleId)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));

        // Không được gán nếu xe đã bị vô hiệu hóa
        if (Boolean.FALSE.equals(vehicle.getIsActive())) {
            throw new BaseException(ErrorHandler.VEHICLE_IS_INACTIVE);
        }

        // Không được gán nếu xe đã bán
        if (vehicle.getStatus() == VehicleInstance.VehicleStatus.SOLD) {
            throw new BaseException(ErrorHandler.VEHICLE_ALREADY_SOLD);
        }

        Customer customer = customerRepo.findById(customerId)
                .orElseThrow(() -> new BaseException(ErrorHandler.CUSTOMER_NOT_FOUND));

        if (customerVehicleRepo.findByVehicleInstance(vehicle).isPresent()) {
            throw new BaseException(ErrorHandler.VEHICLE_ALREADY_ASSIGNED);
        }

        User seller = userRepo.findById(soldByUserId)
                .orElseThrow(() -> new BaseException(ErrorHandler.USER_NOT_FOUND));

        CustomerVehicle record = CustomerVehicle.builder()
                .vehicleInstance(vehicle)
                .customer(customer)
                .soldByDealer(vehicle.getCurrentDealer())
                .soldByUser(seller)
                .salePrice(vehicle.getCurrentValue() != null
                        ? vehicle.getCurrentValue()
                        : vehicle.getVehicleModel().getManufacturerPrice())
                .build();

        customerVehicleRepo.save(record);

        // Cập nhật trạng thái thành SOLD
        vehicle.setStatus(VehicleInstance.VehicleStatus.SOLD);
        vehicleRepo.save(vehicle);
    }

    // --------------------------------------------------------
    // DEACTIVATE (Chỉ cho xe trong kho)
    // --------------------------------------------------------
    @Override
    @Transactional
    public void deactivate(Long id) {
        guard.require(guard.has(guard.me(), "vehicle.deactive"));

        VehicleInstance v = vehicleRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));

        // Chỉ cho phép deactivate nếu xe đang trong kho
        if (v.getStatus() != VehicleInstance.VehicleStatus.IN_STOCK) {
            throw new BaseException(ErrorHandler.ONLY_IN_STOCK_CAN_DEACTIVATE);
        }

        v.setIsActive(false);
        vehicleRepo.save(v);
    }

    // --------------------------------------------------------
    // ACTIVATE (chỉ xe bị deactive mới kích hoạt lại)
    // --------------------------------------------------------
    @Override
    @Transactional
    public void activate(Long id) {
        guard.require(guard.has(guard.me(), "vehicle.active"));

        VehicleInstance v = vehicleRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));

        if (Boolean.TRUE.equals(v.getIsActive())) {
            throw new BaseException(ErrorHandler.VEHICLE_ALREADY_ACTIVE);
        }

        v.setIsActive(true);
        vehicleRepo.save(v);
    }

    @Override
    @Transactional
    public VehicleInstanceResponse updateStatus(Long id, VehicleInstance.VehicleStatus status) {
        guard.require(guard.has(guard.me(), "vehicle.update_status"));

        VehicleInstance v = vehicleRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));

        // ❌ Không cho đổi trạng thái nếu xe bị vô hiệu hoá
        if (Boolean.FALSE.equals(v.getIsActive())) {
            throw new BaseException(ErrorHandler.VEHICLE_IS_INACTIVE);
        }

        // ❌ Không cho đổi trạng thái nếu xe đã bán
        if (v.getStatus() == VehicleInstance.VehicleStatus.SOLD) {
            throw new BaseException(ErrorHandler.VEHICLE_WAS_SOLDED);
        }

        v.setStatus(status);
        vehicleRepo.save(v);

        return VehicleInstanceResponse.fromEntity(v);
    }

}
