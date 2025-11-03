package swp.project.swp391.service.price;

import swp.project.swp391.request.price.VehiclePriceRequest;
import swp.project.swp391.response.price.VehiclePriceResponse;

import java.time.LocalDate;
import java.util.List;

public interface VehiclePriceService {
    List<VehiclePriceResponse> getAll(Long dealerLevelId, LocalDate startDate, LocalDate endDate);

    VehiclePriceResponse getById(Long id);
    VehiclePriceResponse create(VehiclePriceRequest req);
    VehiclePriceResponse update(Long id, VehiclePriceRequest req);
    void deactivate(Long id);
    void activate(Long id);
}


