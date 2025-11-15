package swp.project.swp391.serviceImp.order;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.Order;
import swp.project.swp391.entity.User;
import swp.project.swp391.entity.VehicleInstance;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.OrderRepository;
import swp.project.swp391.repository.VehicleInstanceRepository;
import swp.project.swp391.response.order.OrderResponse;
import swp.project.swp391.response.vehicle.VehicleInstanceResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.order.OrderQueryService;

import java.time.LocalDate;
import java.util.List;

@Service("adminOrderQueryService")
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class AdminOrderQueryServiceImpl implements OrderQueryService {

    private final OrderRepository orderRepo;
    private final VehicleInstanceRepository vehicleRepo;
    private final RbacGuard guard;

    @Override
    public List<OrderResponse> getAllOrders(User currentUser) {
        // ✅ Chỉ những user có quyền order.read_all (Admin, EV_Staff, …)
        guard.require(guard.has(currentUser, "order.read_all_EVM"));

        return orderRepo.findAll().stream()
                .map(OrderResponse::fromEntity)
                .toList();
    }

    @Override
    public OrderResponse getOrderById(Long id, User currentUser) {
        guard.require(guard.has(currentUser, "order.read_EVM"));

        Order order = orderRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

        return OrderResponse.fromEntity(order);
    }

    // ✅ Admin xem danh sách xe trong đơn hàng
    @Override
    @Transactional(readOnly = true)
    public List<VehicleInstanceResponse> getVehiclesByOrder(Long orderId, User currentUser) {
        guard.require(guard.has(currentUser, "order.read_vehicle_EVM"));

        // Kiểm tra đơn có tồn tại
        Order order = orderRepo.findById(orderId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

        // Vì đơn chỉ có 1 xe
        VehicleInstance vehicle = order.getAssignedVehicle();

        if (vehicle == null) {
            return List.of(); // không có xe gắn vào đơn
        }

        return List.of(VehicleInstanceResponse.fromEntity(vehicle));
    }

}
