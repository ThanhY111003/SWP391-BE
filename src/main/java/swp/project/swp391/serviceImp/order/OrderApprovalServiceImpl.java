    package swp.project.swp391.serviceImp.order;

    import jakarta.transaction.Transactional;
    import lombok.RequiredArgsConstructor;
    import lombok.extern.slf4j.Slf4j;

    import java.math.BigDecimal;
    import java.time.LocalDateTime;
    import org.springframework.stereotype.Service;
    import swp.project.swp391.constant.ErrorHandler;
    import swp.project.swp391.entity.*;
    import swp.project.swp391.exception.BaseException;
    import swp.project.swp391.repository.*;
    import swp.project.swp391.response.order.OrderApproveResponse;
    import swp.project.swp391.response.order.OrderResponse;
    import swp.project.swp391.response.vehicle.VehicleInstanceResponse;
    import swp.project.swp391.security.RbacGuard;
    import swp.project.swp391.service.order.OrderApprovalService;
    import java.util.*;

    import static swp.project.swp391.constant.ErrorHandler.INVALID_REQUEST;

    @Service
    @RequiredArgsConstructor
    @Slf4j
    public class OrderApprovalServiceImpl implements OrderApprovalService {

        private final OrderRepository orderRepo;
        private final VehicleInstanceRepository vehicleRepo;
        private final RbacGuard guard;

        // =============================== APPROVE ORDER ===============================
        @Override
        @Transactional
        public OrderApproveResponse approveOrder(Long orderId, User currentUser) {

            guard.require(guard.has(currentUser, "order.approve"));

            Order order = orderRepo.findById(orderId)
                    .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

            // Chỉ duyệt đơn đang PENDING
            if (order.getStatus() != Order.OrderStatus.PENDING) {
                throw new BaseException(INVALID_REQUEST,
                        "Chỉ có thể duyệt đơn hàng ở trạng thái PENDING");
            }

            // Nghiệp vụ: approve không cần gắn xe
            // → xe sẽ được gắn sau bằng /attach-vehicle API

            order.setStatus(Order.OrderStatus.CONFIRMED);
            order.setUpdatedAt(LocalDateTime.now());
            orderRepo.save(order);

            return new OrderApproveResponse(
                    order.getId(),
                    order.getOrderCode(),
                    "CONFIRMED",
                    1
            );
        }

        // =============================== MARK AS SHIPPING ===============================
        @Override
        @Transactional
        public OrderResponse markAsShipping(Long orderId, User currentUser) {

            guard.require(guard.has(currentUser, "order.ship"));

            Order order = orderRepo.findById(orderId)
                    .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

            // ===== CHECK CƠ BẢN =====
            if (order.getStatus() != Order.OrderStatus.PAID) {
                throw new BaseException(INVALID_REQUEST,
                        "Đơn chưa thanh toán (PAID) nên không thể chuyển sang SHIPPING.");
            }

            if (order.getAssignedVehicle() == null) {
                throw new BaseException(INVALID_REQUEST,
                        "Đơn hàng chưa được gắn xe. Không thể chuyển sang SHIPPING.");
            }

            // ===== CHECK ĐẶC BIỆT CHỈ CHO ĐƠN TRẢ THẲNG =====
            if (!Boolean.TRUE.equals(order.getIsInstallment())) {
                // Đơn trả thẳng phải trả đủ 100% mới ship
                BigDecimal paid = order.getManualPaidAmount() != null
                        ? order.getManualPaidAmount()
                        : BigDecimal.ZERO;

                if (paid.compareTo(order.getTotalAmount()) < 0) {
                    throw new BaseException(INVALID_REQUEST,
                            "Đơn trả thẳng chưa thanh toán đủ 100%. Không thể chuyển sang SHIPPING.");
                }
            }

            // ✅ Đơn trả góp: Không cần check thêm gì
            // Vì status = PAID nghĩa là đã xác nhận cọc rồi

            // ===== CẬP NHẬT TRẠNG THÁI =====
            VehicleInstance v = order.getAssignedVehicle();
            v.setStatus(VehicleInstance.VehicleStatus.SHIPPING);
            vehicleRepo.save(v);

            order.setStatus(Order.OrderStatus.SHIPPING);
            orderRepo.save(order);

            return OrderResponse.fromEntity(order);
        }


        // =============================== ATTACH VEHICLE ===============================
        @Override
        @Transactional
        public VehicleInstanceResponse attachVehicleToOrder(Long orderId, Long vehicleId, User currentUser) {

            guard.require(guard.has(currentUser, "order.attach_vehicle"));

            Order order = orderRepo.findById(orderId)
                    .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

            if (order.getStatus() != Order.OrderStatus.PAID) {
                throw new BaseException(INVALID_REQUEST,
                        "Chỉ đơn đã thanh toán (PAID) mới được gắn xe.");
            }

            VehicleInstance vehicle = vehicleRepo.findById(vehicleId)
                    .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));

            if (!Boolean.TRUE.equals(vehicle.getIsActive())) {
                throw new BaseException(ErrorHandler.VEHICLE_IS_INACTIVE);
            }

            if (vehicle.getStatus() != VehicleInstance.VehicleStatus.AVAILABLE) {
                throw new BaseException(INVALID_REQUEST,
                        "Xe phải ở trạng thái AVAILABLE.");
            }


            // Kiểm tra màu/model phải đúng
            if (!Objects.equals(vehicle.getVehicleModelColor().getId(),
                    order.getVehicleModelColor().getId())) {

                throw new BaseException(INVALID_REQUEST,
                        "ModelColor của xe không khớp với đơn hàng.");
            }

            // Gắn xe
            order.setAssignedVehicle(vehicle);
            order.setUpdatedAt(LocalDateTime.now());
            orderRepo.save(order);

            return VehicleInstanceResponse.fromEntity(vehicle);
        }

    }

