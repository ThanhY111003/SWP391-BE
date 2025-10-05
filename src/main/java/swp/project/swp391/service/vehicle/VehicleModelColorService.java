package swp.project.swp391.service.vehicle;

import swp.project.swp391.entity.User;
import swp.project.swp391.response.vehicle.VehicleModelColorResponse;

import java.math.BigDecimal;
import java.util.List;

public interface VehicleModelColorService {

    VehicleModelColorResponse assignColorToModel(Long modelId, Long colorId, BigDecimal priceAdjustment, User currentUser);

    List<VehicleModelColorResponse> getColorsByModel(Long modelId);

    VehicleModelColorResponse updatePriceAdjustment(Long modelId, Long colorId, BigDecimal newAdjustment, User currentUser);

    void unassignColor(Long modelId, Long colorId, User currentUser);
}
