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

import java.time.LocalDate;
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
    private final VehiclePriceRepository vehiclePriceRepository;
    private final OrderRepository orderRepo;
    private final RbacGuard guard;

    public DefectiveVehicleReportResponse createReport(Long orderId, String reason, User reporter) {

        guard.require(guard.has(reporter, "vehicle.report_defect"));

        Order order = orderRepo.findById(orderId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

        // Kiểm tra quyền
        if (!Objects.equals(order.getBuyerDealer().getId(), reporter.getDealer().getId())) {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Đơn hàng không thuộc đại lý của bạn");
        }

        // Lấy xe duy nhất trong đơn
        VehicleInstance vehicle = order.getAssignedVehicle();
        if (vehicle == null) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Đơn hàng chưa được gắn xe");
        }

        // Kiểm tra duplicate report
        if (reportRepo.existsByVehicleInstanceId(vehicle.getId())) {
            throw new BaseException(ErrorHandler.VEHICLE_INSTANCE_DUPLICATE, "Xe này đã được báo lỗi trước đó");
        }

        // 🔥 Đổi trạng thái đơn ngay khi dealer báo lỗi
        order.setStatus(Order.OrderStatus.PARTIALLY_DELIVERED);
        order.setUpdatedAt(LocalDateTime.now());
        orderRepo.save(order);

        DefectiveVehicleReport report = DefectiveVehicleReport.builder()
                .vehicleInstance(vehicle)
                .reason(reason)
                .reportedAt(LocalDateTime.now())
                .isApproved(false)
                .isRepairCompleted(false)
                .build();

        return DefectiveVehicleReportResponse.fromEntity(reportRepo.save(report));
    }

    @Override
    @Transactional
    public DefectiveVehicleReportResponse cancelReportByDealer(Long orderId, User dealerUser) {

        guard.require(guard.has(dealerUser, "vehicle.cancel_defect"));

        Order order = orderRepo.findById(orderId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

        // ❗ Chỉ huỷ khi đơn PARTIALLY_DELIVERED
        if (order.getStatus() != Order.OrderStatus.PARTIALLY_DELIVERED) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Chỉ có thể huỷ báo cáo khi đơn đang ở trạng thái PARTIALLY_DELIVERED");
        }

        // ❗ Chỉ dealer của đơn mới được huỷ
        if (!Objects.equals(order.getBuyerDealer().getId(), dealerUser.getDealer().getId())) {
            throw new BaseException(ErrorHandler.FORBIDDEN,
                    "Không thể huỷ báo cáo của đơn thuộc đại lý khác");
        }

        VehicleInstance vehicle = order.getAssignedVehicle();
        if (vehicle == null) {
            throw new BaseException(ErrorHandler.VEHICLE_NOT_ASSIGNED,
                    "Đơn hàng chưa gắn xe");
        }

        DefectiveVehicleReport report = reportRepo.findByVehicleInstanceId(vehicle.getId())
                .orElseThrow(() -> new BaseException(ErrorHandler.REPORT_NOT_FOUND));

        // ❗ Dealer KHÔNG ĐƯỢC HUỶ nếu report đã được approve
        if (Boolean.TRUE.equals(report.getIsApproved())) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Không thể huỷ báo cáo đã được duyệt bởi hãng");
        }

        // 🔥 Huỷ báo cáo
        order.setStatus(Order.OrderStatus.DEFECT_REJECTED);
        order.setUpdatedAt(LocalDateTime.now());
        orderRepo.save(order);

        reportRepo.delete(report);

        return DefectiveVehicleReportResponse.fromEntity(report);
    }



    @Override
    @Transactional
    public DefectiveVehicleReportResponse rejectReport(Long orderId, User adminUser) {

        guard.require(guard.has(adminUser, "defect.reject"));

        Order order = orderRepo.findById(orderId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

        // ❗ Chỉ reject khi đơn PARTIALLY_DELIVERED
        if (order.getStatus() != Order.OrderStatus.PARTIALLY_DELIVERED) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Chỉ có thể từ chối báo cáo khi đơn đang ở trạng thái PARTIALLY_DELIVERED");
        }

        VehicleInstance vehicle = order.getAssignedVehicle();
        if (vehicle == null) {
            throw new BaseException(ErrorHandler.VEHICLE_NOT_ASSIGNED,
                    "Đơn hàng chưa gắn xe");
        }

        DefectiveVehicleReport report = reportRepo.findByVehicleInstanceId(vehicle.getId())
                .orElseThrow(() -> new BaseException(ErrorHandler.REPORT_NOT_FOUND));

        // ❗ Admin KHÔNG được reject khi đã sửa xong
        if (Boolean.TRUE.equals(report.getIsRepairCompleted())) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Không thể từ chối báo cáo khi xe đã sửa hoàn tất");
        }


        // 🔥 Reject report
        order.setStatus(Order.OrderStatus.DEFECT_REJECTED);
        order.setUpdatedAt(LocalDateTime.now());
        orderRepo.save(order);

        reportRepo.delete(report);

        return DefectiveVehicleReportResponse.fromEntity(report);
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
        if (order == null) {
            throw new BaseException(ErrorHandler.ORDER_NOT_FOUND);
        }


        report.setIsApproved(true);
        report.setReportedAt(LocalDateTime.now());
        reportRepo.save(report);

        // ✅ Xe chuyển sang PARTIALLY_DELIVERED
        vehicle.setStatus(VehicleInstance.VehicleStatus.PARTIALLY_DELIVERED);
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
    public RepairedVehicleResponse confirmRepairedVehicle(Long orderId, User dealerUser) {

        guard.require(guard.has(dealerUser, "vehicle.receive_repair"));

        // ✅ Load dealer với level
        Dealer dealer = dealerRepo.findById(dealerUser.getDealer().getId())
                .orElseThrow(() -> new BaseException(ErrorHandler.DEALER_NOT_FOUND));

        // ✅ Force initialize level
        DealerLevel dealerLevel = dealer.getLevel();
        if (dealerLevel == null) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Dealer không có level hợp lệ");
        }

        Order order = orderRepo.findById(orderId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

        if (!Objects.equals(order.getBuyerDealer().getId(), dealer.getId())) {
            throw new BaseException(ErrorHandler.FORBIDDEN,
                    "Đơn hàng không thuộc dealer hiện tại");
        }

        // 🔥 LẤY XE TỪ ORDER — 1 đơn = 1 xe
        VehicleInstance vehicle = order.getAssignedVehicle();
        if (vehicle == null) {
            throw new BaseException(ErrorHandler.VEHICLE_NOT_ASSIGNED,
                    "Đơn hàng chưa có xe gắn vào");
        }

        if (vehicle.getStatus() != VehicleInstance.VehicleStatus.SHIPPING) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Xe phải ở trạng thái SHIPPING mới có thể xác nhận nhận lại");
        }

        // ✅ Cập nhật xe
        vehicle.setStatus(VehicleInstance.VehicleStatus.IN_STOCK);
        vehicle.setCurrentDealer(dealer);

        // ✅ CẬP NHẬT GIÁ XE (giống như lần đầu nhận xe)
        VehiclePrice vehiclePrice = vehiclePriceRepository
                .findActiveByVehicleModelColorAndDealerLevel(
                        vehicle.getVehicleModelColor(),
                        dealerLevel,
                        LocalDate.now()
                )
                .orElseThrow(() -> new BaseException(ErrorHandler.INVALID_REQUEST,
                        "Không tìm thấy giá bán cho dealer level này với modelColor của xe."));

        vehicle.setCurrentValue(vehiclePrice.getWholesalePrice());
        vehicleRepo.save(vehicle);

        // ✅ Cập nhật inventory (không cần check trùng vì xe từ SHIPPING về)
        Inventory inv = inventoryRepo.lockByDealerIdAndVehicleModelColorId(
                dealer.getId(),
                vehicle.getVehicleModelColor().getId()
        ).orElseGet(() -> inventoryRepo.save(
                Inventory.builder()
                        .dealer(dealer)
                        .vehicleModelColor(vehicle.getVehicleModelColor())
                        .availableQuantity(0)
                        .reservedQuantity(0)
                        .totalQuantity(0)
                        .isActive(true)
                        .build()
        ));

        inv.setAvailableQuantity(inv.getAvailableQuantity() + 1);
        inv.setTotalQuantity(inv.getTotalQuantity() + 1);
        inventoryRepo.save(inv);

        // ✅ Cập nhật trạng thái đơn (trở lại trạng thái cũ)
        if (Boolean.TRUE.equals(order.getIsInstallment())) {
            order.setStatus(Order.OrderStatus.INSTALLMENT_ACTIVE);
        } else {
            order.setStatus(Order.OrderStatus.COMPLETED);
        }
        order.setUpdatedAt(LocalDateTime.now());
        orderRepo.save(order);

        return RepairedVehicleResponse.fromEntity(vehicle);
    }

}
