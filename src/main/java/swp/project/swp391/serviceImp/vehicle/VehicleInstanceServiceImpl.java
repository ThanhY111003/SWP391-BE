package swp.project.swp391.serviceImp.vehicle;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.*;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.*;
import swp.project.swp391.request.vehicle.AssignVehicleRequest;
import swp.project.swp391.request.vehicle.TransferVehicleRequest;
import swp.project.swp391.response.vehicle.CustomerVehicleResponse;
import swp.project.swp391.response.vehicle.VehicleInstanceResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.vehicle.VehicleInstanceService;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class VehicleInstanceServiceImpl implements VehicleInstanceService {

    private final VehicleInstanceRepository vehicleRepo;
    private final CustomerRepository customerRepo;
    private final CustomerVehicleRepository customerVehicleRepo;
    private final InventoryRepository inventoryRepo;
    private final RbacGuard guard;
    private final DealerRepository dealerRepo;

    // --------------------------------------------------------
    // GET ALL
    // --------------------------------------------------------
    @Override
    public List<VehicleInstanceResponse> getAll(Long dealerId, VehicleInstance.VehicleStatus status, Boolean activeOnly) {
        guard.require(guard.has(guard.me(), "vehicle.read_all"));
        User current = guard.me();

        List<VehicleInstance> list = vehicleRepo.findAll();

        // Nếu là Dealer role => chỉ thấy xe thuộc đại lý mình
        if (isDealerRole(current)) {
            Long myDealerId = getDealerId(current);
            list = list.stream()
                    .filter(v -> v.getCurrentDealer() != null && myDealerId.equals(v.getCurrentDealer().getId()))
                    .collect(Collectors.toList());
        } else if (dealerId != null) {
            // ADMIN / EVM_STAFF có thể lọc dealer bất kỳ
            list = list.stream()
                    .filter(v -> v.getCurrentDealer() != null && dealerId.equals(v.getCurrentDealer().getId()))
                    .collect(Collectors.toList());
        }

        if (status != null) {
            list = list.stream()
                    .filter(v -> v.getStatus() == status)
                    .collect(Collectors.toList());
        }

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

        checkDealerOwnership(v);
        return VehicleInstanceResponse.fromEntity(v);
    }

    // --------------------------------------------------------
    // ASSIGN TO CUSTOMER
    // --------------------------------------------------------
    @Override
    @Transactional
    public CustomerVehicleResponse assignToCustomer(AssignVehicleRequest req) {
        guard.require(guard.has(guard.me(), "vehicle.assign_customer"));
        User current = guard.me();

        VehicleInstance vehicle = vehicleRepo.findById(req.getVehicleId())
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));

        checkDealerOwnership(vehicle);

        if (Boolean.FALSE.equals(vehicle.getIsActive()))
            throw new BaseException(ErrorHandler.VEHICLE_IS_INACTIVE);

        if (vehicle.getStatus() == VehicleInstance.VehicleStatus.SOLD)
            throw new BaseException(ErrorHandler.VEHICLE_ALREADY_SOLD);

        if (vehicle.getCurrentDealer() == null)
            throw new BaseException(ErrorHandler.VEHICLE_NOT_OWNED_BY_DEALER);

        Customer customer = customerRepo.findById(req.getCustomerId())
                .orElseThrow(() -> new BaseException(ErrorHandler.CUSTOMER_NOT_FOUND));

        if (customerVehicleRepo.findByVehicleInstance(vehicle).isPresent())
            throw new BaseException(ErrorHandler.VEHICLE_ALREADY_ASSIGNED);

        User seller = current; // ✅ lấy từ token

        // ✅ Ngày bán tự động = hôm nay
        LocalDate saleDate = LocalDate.now();

        // ✅ Bảo hành: mặc định bắt đầu từ ngày bán
        LocalDate warrantyStart = req.getWarrantyStartDate() != null ? req.getWarrantyStartDate() : saleDate;
        LocalDate warrantyEnd = req.getWarrantyEndDate();

        if (warrantyEnd != null && warrantyEnd.isBefore(warrantyStart)) {
            throw new BaseException(ErrorHandler.INVALID_WARRANTY_PERIOD);
        }

        BigDecimal salePrice = req.getSalePrice() != null
                ? req.getSalePrice()
                : (vehicle.getCurrentValue() != null
                ? vehicle.getCurrentValue()
                : vehicle.getVehicleModel().getManufacturerPrice());

        CustomerVehicle record = CustomerVehicle.builder()
                .vehicleInstance(vehicle)
                .customer(customer)
                .soldByDealer(vehicle.getCurrentDealer())
                .soldByUser(seller)
                .salePrice(salePrice)
                .saleDate(saleDate) // ✅ tự động set hôm nay
                .customerWarrantyStartDate(warrantyStart)
                .customerWarrantyEndDate(warrantyEnd)
                .build();

        customerVehicleRepo.save(record);

        vehicle.setStatus(VehicleInstance.VehicleStatus.SOLD);

        // ✅ Giảm tồn kho khi bán
        Inventory inv = inventoryRepo.findByDealerIdAndVehicleModelColorId(
                vehicle.getCurrentDealer().getId(),
                vehicle.getVehicleModelColor().getId()
        ).orElse(null);

        if (inv != null) {
            inv.setTotalQuantity(Math.max(inv.getTotalQuantity() - 1, 0));
            inv.setAvailableQuantity(Math.max(inv.getAvailableQuantity() - 1, 0));
            inv.setReservedQuantity(Math.max(inv.getReservedQuantity() - 1, 0));
            inventoryRepo.save(inv);
        }

        vehicleRepo.save(vehicle);

        return CustomerVehicleResponse.fromEntity(record);
    }


    // --------------------------------------------------------
    // DEACTIVATE
    // --------------------------------------------------------
    @Override
    @Transactional
    public void deactivate(Long id) {
        guard.require(guard.has(guard.me(), "vehicle.deactive"));
        VehicleInstance v = vehicleRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));

        checkDealerOwnership(v);

        if (v.getStatus() != VehicleInstance.VehicleStatus.IN_STOCK)
            throw new BaseException(ErrorHandler.ONLY_IN_STOCK_CAN_DEACTIVATE);

        v.setIsActive(false);
        vehicleRepo.save(v);
    }

    // --------------------------------------------------------
    // ACTIVATE
    // --------------------------------------------------------
    @Override
    @Transactional
    public void activate(Long id) {
        guard.require(guard.has(guard.me(), "vehicle.active"));
        VehicleInstance v = vehicleRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));

        checkDealerOwnership(v);

        if (Boolean.TRUE.equals(v.getIsActive()))
            throw new BaseException(ErrorHandler.VEHICLE_ALREADY_ACTIVE);

        v.setIsActive(true);
        vehicleRepo.save(v);
    }

    // --------------------------------------------------------
    // UPDATE STATUS
    // --------------------------------------------------------
    // VehicleInstanceServiceImpl
    @Override
    @Transactional
    public VehicleInstanceResponse updateStatus(Long id, VehicleInstance.VehicleStatus status) {
        guard.require(guard.has(guard.me(), "vehicle.update_status"));
        VehicleInstance v = vehicleRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));

        checkDealerOwnership(v);

        if (Boolean.FALSE.equals(v.getIsActive()))
            throw new BaseException(ErrorHandler.VEHICLE_IS_INACTIVE);

        if (v.getStatus() == VehicleInstance.VehicleStatus.SOLD)
            throw new BaseException(ErrorHandler.VEHICLE_WAS_SOLDED);

        // ❌ Không cho đổi trực tiếp sang SOLD trong method này
        if (status == VehicleInstance.VehicleStatus.SOLD)
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Không thể đổi sang SOLD trực tiếp. Hãy gán xe cho khách hàng.");

        v.setStatus(status);
        // ✅ Đồng bộ số lượng kho
        Inventory inv = inventoryRepo.findByDealerIdAndVehicleModelColorId(
                v.getCurrentDealer().getId(),
                v.getVehicleModelColor().getId()
        ).orElse(null);

        if (inv != null) {
            if (status == VehicleInstance.VehicleStatus.RESERVED) {
                // Chuyển từ IN_STOCK → RESERVED
                inv.setReservedQuantity(inv.getReservedQuantity() + 1);
                inv.setAvailableQuantity(Math.max(inv.getAvailableQuantity() - 1, 0));
            } else if (status == VehicleInstance.VehicleStatus.IN_STOCK) {
                // Chuyển từ RESERVED → IN_STOCK
                inv.setReservedQuantity(Math.max(inv.getReservedQuantity() - 1, 0));
                inv.setAvailableQuantity(inv.getAvailableQuantity() + 1);
            }
            inventoryRepo.save(inv);
        }

        vehicleRepo.save(v);

        return VehicleInstanceResponse.fromEntity(v);
    }


    @Override
    @Transactional
    public VehicleInstanceResponse transferVehicle(TransferVehicleRequest req) {
        // ✅ Kiểm tra quyền qua guard (chuẩn nhất)
        guard.require(guard.has(guard.me(), "vehicle.transfer"));

        // ====== 1. Lấy xe cần chuyển ======
        VehicleInstance vehicle = vehicleRepo.findById(req.getVehicleInstanceId())
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));

        if (!Boolean.TRUE.equals(vehicle.getIsActive()))
            throw new BaseException(ErrorHandler.VEHICLE_IS_INACTIVE);

        if (vehicle.getStatus() == VehicleInstance.VehicleStatus.SOLD)
            throw new BaseException(ErrorHandler.VEHICLE_WAS_SOLDED);

        if (vehicle.getStatus() == VehicleInstance.VehicleStatus.RESERVED)
            throw new BaseException(ErrorHandler.VEHICLE_IS_RESERVED);

        Dealer sourceDealer = vehicle.getCurrentDealer();
        Dealer targetDealer = dealerRepo.findById(req.getTargetDealerId())
                .orElseThrow(() -> new BaseException(ErrorHandler.DEALER_NOT_FOUND));

        if (sourceDealer == null)
            throw new BaseException(ErrorHandler.DEALER_NOT_FOUND, "Xe này chưa thuộc đại lý nào.");

        if (sourceDealer.getId().equals(targetDealer.getId()))
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Không thể chuyển xe sang cùng một đại lý.");

        // ====== 2. Cập nhật kho nguồn ======
        Inventory sourceInv = inventoryRepo.findByDealerIdAndVehicleModelColorId(
                sourceDealer.getId(),
                vehicle.getVehicleModelColor().getId()
        ).orElse(null);

        if (sourceInv != null) {
            sourceInv.setTotalQuantity(Math.max(sourceInv.getTotalQuantity() - 1, 0));
            sourceInv.setAvailableQuantity(Math.max(sourceInv.getAvailableQuantity() - 1, 0));
            inventoryRepo.save(sourceInv);
        }

        // ====== 3. Cập nhật kho đích ======
        Inventory targetInv = inventoryRepo.findByDealerIdAndVehicleModelColorId(
                targetDealer.getId(),
                vehicle.getVehicleModelColor().getId()
        ).orElseGet(() -> Inventory.builder()
                .dealer(targetDealer)
                .vehicleModelColor(vehicle.getVehicleModelColor())
                .totalQuantity(0)
                .reservedQuantity(0)
                .availableQuantity(0)
                .isActive(true)
                .build());

        targetInv.setTotalQuantity(targetInv.getTotalQuantity() + 1);
        targetInv.setAvailableQuantity(targetInv.getAvailableQuantity() + 1);
        inventoryRepo.save(targetInv);

        // ====== 4. Cập nhật dealer cho xe ======
        vehicle.setCurrentDealer(targetDealer);
        vehicleRepo.save(vehicle);

        return VehicleInstanceResponse.fromEntity(vehicle);
    }



    // --------------------------------------------------------
    // HELPER METHODS
    // --------------------------------------------------------

    private boolean isDealerRole(User user) {
        return user.getRoles().stream()
                .map(Role::getName)
                .anyMatch(r -> r.equals("DEALER_MANAGER") || r.equals("DEALER_STAFF"));
    }

    private Long getDealerId(User user) {
        if (user.getDealer() == null)
            throw new BaseException(ErrorHandler.DEALER_NOT_FOUND);
        return user.getDealer().getId();
    }

    private void checkDealerOwnership(VehicleInstance vehicle) {
        User current = guard.me();

        boolean isAdminOrEvm = current.getRoles().stream()
                .map(Role::getName)
                .anyMatch(r -> r.equals("ADMIN") || r.equals("EVM_STAFF"));

        if (isAdminOrEvm)
            return;

        if (vehicle.getCurrentDealer() == null)
            throw new BaseException(ErrorHandler.VEHICLE_NOT_OWNED_BY_DEALER);

        Long myDealerId = getDealerId(current);
        if (!myDealerId.equals(vehicle.getCurrentDealer().getId())) {
            throw new BaseException(ErrorHandler.FORBIDDEN);
        }
    }
}
