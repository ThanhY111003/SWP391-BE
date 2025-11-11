package swp.project.swp391.serviceImp.warranty;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.*;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.*;
import swp.project.swp391.response.warranty.DealerWarrantyRepairResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.warranty.DealerWarrantyRepairService;

import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;

@Service
@RequiredArgsConstructor
public class DealerWarrantyRepairServiceImpl implements DealerWarrantyRepairService {

    private final DealerWarrantyRepairReportRepository reportRepo;
    private final VehicleInstanceRepository vehicleRepo;
    private final DealerRepository dealerRepo;
    private final RbacGuard guard;

    // ======================= CREATE REQUEST =========================
    @Override
    @Transactional
    public DealerWarrantyRepairResponse createRequest(Long vehicleId, String reason, User dealerUser) {
        guard.require(guard.has(dealerUser, "warranty.create"));

        VehicleInstance vehicle = vehicleRepo.findById(vehicleId)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));

        Dealer dealer = dealerUser.getDealer();
        if (dealer == null) {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Người dùng hiện tại không thuộc dealer nào");
        }

        // ✅ Validate quyền sở hữu xe
        if (vehicle.getCurrentDealer() == null || !Objects.equals(vehicle.getCurrentDealer().getId(), dealer.getId())) {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Xe này không thuộc quyền quản lý của dealer hiện tại");
        }

        // ✅ Chỉ xe IN_STOCK hoặc SOLD mới được gửi yêu cầu
        if (vehicle.getStatus() != VehicleInstance.VehicleStatus.IN_STOCK &&
                vehicle.getStatus() != VehicleInstance.VehicleStatus.SOLD) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Chỉ xe trong kho hoặc đã bán mới được yêu cầu bảo hành");
        }

        // ✅ Nếu xe đã bán thì phải còn trong thời gian bảo hành
        if (vehicle.getCustomerVehicle() != null) {
            LocalDate now = LocalDate.now();
            LocalDate start = vehicle.getCustomerVehicle().getCustomerWarrantyStartDate();
            LocalDate end = vehicle.getCustomerVehicle().getCustomerWarrantyEndDate();
            if (start != null && end != null && (now.isBefore(start) || now.isAfter(end))) {
                throw new BaseException(ErrorHandler.INVALID_REQUEST, "Xe đã hết thời hạn bảo hành");
            }
        }

        // ✅ Kiểm tra không có yêu cầu đang mở
        boolean hasOpenRequest = reportRepo.existsByVehicleInstanceIdAndStatusIn(
                vehicleId,
                List.of(
                        DealerWarrantyRepairReport.WarrantyStatus.PENDING,
                        DealerWarrantyRepairReport.WarrantyStatus.APPROVED,
                        DealerWarrantyRepairReport.WarrantyStatus.COMPLETED
                )
        );
        if (hasOpenRequest) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Xe này đang có yêu cầu bảo hành đang xử lý");
        }

        // ✅ Tạo yêu cầu mới
        DealerWarrantyRepairReport report = DealerWarrantyRepairReport.builder()
                .vehicleInstance(vehicle)
                .dealer(dealer)
                .reason(reason)
                .status(DealerWarrantyRepairReport.WarrantyStatus.PENDING)
                .createdAt(LocalDateTime.now())
                .build();

        // ✅ Xe chuyển sang REPAIRING
        vehicle.setStatus(VehicleInstance.VehicleStatus.REPAIRING);
        vehicleRepo.save(vehicle);

        return DealerWarrantyRepairResponse.fromEntity(reportRepo.save(report));
    }

    // ======================= VIEW MY REQUESTS =========================
    @Override
    @Transactional(readOnly = true)
    public List<DealerWarrantyRepairResponse> getMyRequests(User dealerUser) {
        guard.require(guard.has(dealerUser, "warranty.read"));

        if (dealerUser.getDealer() == null) {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Người dùng hiện tại không thuộc dealer nào");
        }

        return reportRepo.findByDealerId(dealerUser.getDealer().getId())
                .stream()
                .map(DealerWarrantyRepairResponse::fromEntity)
                .toList();
    }

    // ======================= APPROVE REQUEST =========================
    @Override
    @Transactional
    public DealerWarrantyRepairResponse approveRequest(Long requestId, User manufacturerUser) {
        guard.require(guard.has(manufacturerUser, "warranty.approve"));

        DealerWarrantyRepairReport report = reportRepo.findById(requestId)
                .orElseThrow(() -> new BaseException(ErrorHandler.REPORT_NOT_FOUND));

        if (report.getStatus() != DealerWarrantyRepairReport.WarrantyStatus.PENDING) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Chỉ có thể duyệt yêu cầu đang ở trạng thái PENDING");
        }

        report.setStatus(DealerWarrantyRepairReport.WarrantyStatus.APPROVED);
        report.setUpdatedAt(LocalDateTime.now());
        return DealerWarrantyRepairResponse.fromEntity(reportRepo.save(report));
    }

    // ======================= REJECT REQUEST =========================
    @Override
    @Transactional
    public DealerWarrantyRepairResponse rejectRequest(Long requestId, User manufacturerUser) {
        guard.require(guard.has(manufacturerUser, "warranty.reject"));

        DealerWarrantyRepairReport report = reportRepo.findById(requestId)
                .orElseThrow(() -> new BaseException(ErrorHandler.REPORT_NOT_FOUND));

        if (report.getStatus() != DealerWarrantyRepairReport.WarrantyStatus.PENDING) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Chỉ có thể từ chối yêu cầu đang ở trạng thái PENDING");
        }

        VehicleInstance vehicle = report.getVehicleInstance();
        // Nếu xe đã bán thì trả về SOLD, chưa bán thì trả về IN_STOCK
        if (vehicle.getCustomerVehicle() != null)
            vehicle.setStatus(VehicleInstance.VehicleStatus.SOLD);
        else
            vehicle.setStatus(VehicleInstance.VehicleStatus.IN_STOCK);

        report.setStatus(DealerWarrantyRepairReport.WarrantyStatus.REJECTED);
        report.setUpdatedAt(LocalDateTime.now());

        vehicleRepo.save(vehicle);
        reportRepo.save(report);

        return DealerWarrantyRepairResponse.fromEntity(report);
    }

    // ======================= CANCEL REQUEST (BY DEALER) =========================
    @Override
    @Transactional
    public DealerWarrantyRepairResponse cancelRequest(Long requestId, User dealerUser) {
        guard.require(guard.has(dealerUser, "warranty.cancel"));

        DealerWarrantyRepairReport report = reportRepo.findById(requestId)
                .orElseThrow(() -> new BaseException(ErrorHandler.REPORT_NOT_FOUND));

        if (report.getStatus() != DealerWarrantyRepairReport.WarrantyStatus.PENDING) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Chỉ có thể huỷ yêu cầu khi đang chờ duyệt (PENDING)");
        }

        Dealer dealer = dealerUser.getDealer();
        if (!Objects.equals(report.getDealer().getId(), dealer.getId())) {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Không thể huỷ yêu cầu của dealer khác");
        }

        VehicleInstance vehicle = report.getVehicleInstance();
        if (vehicle.getCustomerVehicle() != null)
            vehicle.setStatus(VehicleInstance.VehicleStatus.SOLD);
        else
            vehicle.setStatus(VehicleInstance.VehicleStatus.IN_STOCK);

        report.setStatus(DealerWarrantyRepairReport.WarrantyStatus.CANCELLED);
        report.setUpdatedAt(LocalDateTime.now());

        vehicleRepo.save(vehicle);
        reportRepo.save(report);

        return DealerWarrantyRepairResponse.fromEntity(report);
    }

    // ======================= COMPLETE REPAIR =========================
    @Override
    @Transactional
    public DealerWarrantyRepairResponse completeRepair(Long requestId, User manufacturerUser) {
        guard.require(guard.has(manufacturerUser, "warranty.complete"));

        DealerWarrantyRepairReport report = reportRepo.findById(requestId)
                .orElseThrow(() -> new BaseException(ErrorHandler.REPORT_NOT_FOUND));

        if (report.getStatus() != DealerWarrantyRepairReport.WarrantyStatus.APPROVED) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Chỉ có thể hoàn tất sửa chữa khi yêu cầu đang ở trạng thái APPROVED");
        }

        VehicleInstance vehicle = report.getVehicleInstance();
        vehicle.setStatus(VehicleInstance.VehicleStatus.SHIPPING);

        report.setStatus(DealerWarrantyRepairReport.WarrantyStatus.COMPLETED);
        report.setUpdatedAt(LocalDateTime.now());

        vehicleRepo.save(vehicle);
        reportRepo.save(report);

        return DealerWarrantyRepairResponse.fromEntity(report);
    }

    // ======================= CONFIRM RECEIVED =========================
    @Override
    @Transactional
    public DealerWarrantyRepairResponse confirmReceived(Long requestId, User dealerUser) {
        guard.require(guard.has(dealerUser, "warranty.confirm"));

        DealerWarrantyRepairReport report = reportRepo.findById(requestId)
                .orElseThrow(() -> new BaseException(ErrorHandler.REPORT_NOT_FOUND));

        if (report.getStatus() != DealerWarrantyRepairReport.WarrantyStatus.COMPLETED) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Xe chưa được hãng hoàn tất sửa chữa (phải ở trạng thái COMPLETED)");
        }

        Dealer dealer = dealerUser.getDealer();
        if (!Objects.equals(report.getDealer().getId(), dealer.getId())) {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Không thể xác nhận xe của dealer khác");
        }

        VehicleInstance vehicle = report.getVehicleInstance();
        if (vehicle.getStatus() != VehicleInstance.VehicleStatus.SHIPPING) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Xe chưa được hãng gửi lại (không ở trạng thái SHIPPING)");
        }

        if (vehicle.getCustomerVehicle() != null)
            vehicle.setStatus(VehicleInstance.VehicleStatus.SOLD);
        else
            vehicle.setStatus(VehicleInstance.VehicleStatus.IN_STOCK);

        report.setStatus(DealerWarrantyRepairReport.WarrantyStatus.RECEIVED);
        report.setUpdatedAt(LocalDateTime.now());

        vehicleRepo.save(vehicle);
        reportRepo.save(report);

        return DealerWarrantyRepairResponse.fromEntity(report);
    }

    // ======================= VIEW REQUESTS BY DEALER (ADMIN/EVM) =========================
    @Override
    @Transactional(readOnly = true)
    public List<DealerWarrantyRepairResponse> getRequestsByDealer(Long dealerId, User manufacturerUser) {
        guard.require(guard.has(manufacturerUser, "warranty.read_all"));

        if (dealerId == null || dealerId <= 0)
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "dealerId không hợp lệ");

        if (!dealerRepo.existsById(dealerId))
            throw new BaseException(ErrorHandler.DEALER_NOT_FOUND);

        return reportRepo.findByDealerId(dealerId)
                .stream()
                .map(DealerWarrantyRepairResponse::fromEntity)
                .toList();
    }
}
