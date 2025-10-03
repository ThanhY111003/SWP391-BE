package swp.project.swp391.service.vehicle;

import swp.project.swp391.entity.User;
import swp.project.swp391.request.vehicle.VehicleColorRequest;
import swp.project.swp391.response.vehicle.VehicleColorResponse;

public interface VehicleColorService {
    VehicleColorResponse createVehicleColor(VehicleColorRequest request, User currentUser);
}
