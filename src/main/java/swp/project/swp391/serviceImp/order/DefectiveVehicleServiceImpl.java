package swp.project.swp391.serviceImp.order;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.*;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.*;
import swp.project.swp391.response.defective.DefectiveVehicleReportResponse;
import swp.project.swp391.response.defective.RepairedVehicleResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.order.DefectiveVehicleService;
import lombok.extern.slf4j.Slf4j;
import java.time.LocalDateTime;
import java.util.List;
import java.util.Objects;

@Slf4j
@Service
@RequiredArgsConstructor
public class DefectiveVehicleServiceImpl implements DefectiveVehicleService {

    private final DefectiveVehicleReportRepository reportRepo;
    private final DealerRepository dealerRepo;
    private final VehicleInstanceRepository vehicleRepo;
    private final InventoryRepository inventoryRepo;
    private final OrderRepository orderRepo;
    private final RbacGuard guard;

    public DefectiveVehicleReportResponse createReport(Long orderId, Long vehicleId, String reason, User reporter) {
        guard.require(guard.has(reporter, "vehicle.report_defect"));

        Order order = orderRepo.findById(orderId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

        if (!Objects.equals(order.getBuyerDealer().getId(), reporter.getDealer().getId())) {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Đơn hàng không thuộc đại lý của bạn");
        }

        VehicleInstance vehicle = vehicleRepo.findById(vehicleId)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));

        if (!Objects.equals(vehicle.getOrder().getId(), orderId)) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Xe không thuộc đơn hàng này");
        }

        if (reportRepo.existsByVehicleInstanceId(vehicleId)) {
            throw new BaseException(ErrorHandler.VEHICLE_INSTANCE_DUPLICATE, "Xe này đã được báo lỗi trước đó");
        }

        DefectiveVehicleReport report = DefectiveVehicleReport.builder()
                .vehicleInstance(vehicle)
                .reason(reason)
                .reportedAt(LocalDateTime.now())
                .isApproved(false)
                .build();

        return DefectiveVehicleReportResponse.fromEntity(reportRepo.save(report));
    }


    @Override
    @Transactional(readOnly = true)
    public List<DefectiveVehicleReportResponse> getReportsByOrder(Long orderId, User currentUser) {
        guard.require(guard.has(currentUser, "defect.read"));

        // ✅ Dealer chỉ xem đơn của mình
        if (currentUser.getDealer() != null) {
            Order order = orderRepo.findById(orderId)
                    .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

            if (!Objects.equals(order.getBuyerDealer().getId(), currentUser.getDealer().getId())) {
                throw new BaseException(ErrorHandler.FORBIDDEN, "Không thể xem báo cáo của đại lý khác");
            }
        }

        return reportRepo.findByVehicleInstanceOrderId(orderId)
                .stream()
                .map(DefectiveVehicleReportResponse::fromEntity)
                .toList();
    }

    @Override
    @Transactional
    public DefectiveVehicleReportResponse approveReport(Long reportId, User currentUser) {
        guard.require(guard.has(currentUser, "defect.approve"));

        DefectiveVehicleReport report = reportRepo.findById(reportId)
                .orElseThrow(() -> new BaseException(ErrorHandler.REPORT_NOT_FOUND));

        VehicleInstance vehicle = report.getVehicleInstance();
        Order order = vehicle.getOrder();

        report.setIsApproved(true);
        report.setReportedAt(LocalDateTime.now());
        reportRepo.save(report);

        // ✅ Xe chuyển sang REPAIRING
        vehicle.setStatus(VehicleInstance.VehicleStatus.REPAIRING);
        vehicleRepo.save(vehicle);

        // ✅ Nếu đơn đang SHIPPING → chuyển sang PARTIALLY_DELIVERED
        if (order.getStatus() == Order.OrderStatus.SHIPPING) {
            order.setStatus(Order.OrderStatus.PARTIALLY_DELIVERED);
            orderRepo.save(order);
        }

        return DefectiveVehicleReportResponse.fromEntity(report);
    }

    @Override
    @Transactional
    public DefectiveVehicleReportResponse completeRepair(Long reportId, User currentUser) {
        guard.require(guard.has(currentUser, "defect.repair_complete"));

        DefectiveVehicleReport report = reportRepo.findById(reportId)
                .orElseThrow(() -> new BaseException(ErrorHandler.REPORT_NOT_FOUND));

        if (!Boolean.TRUE.equals(report.getIsApproved())) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Chỉ có thể xác nhận sửa xong cho xe đã được duyệt lỗi");
        }

        VehicleInstance vehicle = report.getVehicleInstance();

        // ✅ Xe sửa xong → chuyển sang SHIPPING
        vehicle.setStatus(VehicleInstance.VehicleStatus.SHIPPING);
        vehicle.setCurrentDealer(null);
        report.setIsRepairCompleted(true);
        vehicleRepo.save(vehicle);

        // Cập nhật lại thời điểm
        report.setReportedAt(LocalDateTime.now());
        reportRepo.save(report);

        return DefectiveVehicleReportResponse.fromEntity(report);
    }

    @Override
    @Transactional
    public RepairedVehicleResponse confirmRepairedVehicle(Long orderId, Long vehicleId, User dealerUser) {
        guard.require(guard.has(dealerUser, "vehicle.receive_repair"));

        Dealer dealer = dealerRepo.findById(dealerUser.getDealer().getId())
                .orElseThrow(() -> new BaseException(ErrorHandler.DEALER_NOT_FOUND));

        Order order = orderRepo.findById(orderId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

        if (!Objects.equals(order.getBuyerDealer().getId(), dealer.getId())) {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Đơn hàng không thuộc dealer hiện tại");
        }

        VehicleInstance vehicle = vehicleRepo.findWithRelationsById(vehicleId)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));

        if (!Objects.equals(vehicle.getOrder().getId(), orderId)) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Xe không thuộc đơn hàng này");
        }

        if (vehicle.getStatus() != VehicleInstance.VehicleStatus.SHIPPING) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Xe phải ở trạng thái SHIPPING mới có thể xác nhận nhận lại");
        }

        // ✅ Kiểm tra xe đã được nhập kho chưa (tránh nhập 2 lần)
        if (vehicle.getCurrentDealer() != null &&
                vehicle.getCurrentDealer().getId().equals(dealer.getId())) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Xe này đã được nhập kho trước đó");
        }

        // ✅ Cập nhật xe
        vehicle.setStatus(VehicleInstance.VehicleStatus.IN_STOCK);
        vehicle.setCurrentDealer(dealer);
        vehicleRepo.save(vehicle);

        // ✅ Cập nhật inventory
        Inventory inv = inventoryRepo.lockByDealerIdAndVehicleModelColorId(
                dealer.getId(), vehicle.getVehicleModelColor().getId()
        ).orElseThrow(() -> new BaseException(ErrorHandler.INVENTORY_NOT_FOUND));

        log.info("📦 Trước khi cập nhật inventory: total={}, available={}",
                inv.getTotalQuantity(), inv.getAvailableQuantity());

        inv.setAvailableQuantity(inv.getAvailableQuantity() + 1);
        inv.setTotalQuantity(inv.getTotalQuantity() + 1);
        inventoryRepo.save(inv);

        log.info("📦 Sau khi cập nhật inventory: total={}, available={}",
                inv.getTotalQuantity(), inv.getAvailableQuantity());

        // ✅ Kiểm tra tất cả xe trong đơn
        boolean hasDefectiveVehicles = vehicleRepo.existsByOrderIdAndStatusIn(
                order.getId(),
                List.of(
                        VehicleInstance.VehicleStatus.REPAIRING,
                        VehicleInstance.VehicleStatus.SHIPPING
                )
        );

        if (!hasDefectiveVehicles) {
            order.setStatus(Boolean.TRUE.equals(order.getIsInstallment())
                    ? Order.OrderStatus.INSTALLMENT_ACTIVE
                    : Order.OrderStatus.COMPLETED);
            order.setUpdatedAt(LocalDateTime.now());
            orderRepo.save(order);
        }

        return RepairedVehicleResponse.fromEntity(vehicle);
    }
}
