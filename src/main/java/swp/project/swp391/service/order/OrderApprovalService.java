package swp.project.swp391.service.order;

import swp.project.swp391.entity.User;
import swp.project.swp391.response.order.OrderApproveResponse;
import swp.project.swp391.response.order.OrderResponse;
import swp.project.swp391.response.vehicle.VehicleInstanceResponse;

public interface OrderApprovalService {
    OrderApproveResponse approveOrder(Long orderId, User currentUser);
    OrderResponse markAsShipping(Long orderId, User currentUser);
    VehicleInstanceResponse attachVehicleToOrder(Long orderId, Long vehicleId, User currentUser);
}
