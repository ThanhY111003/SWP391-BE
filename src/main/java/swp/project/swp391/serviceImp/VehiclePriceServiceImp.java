package swp.project.swp391.serviceImp;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import swp.project.swp391.entity.VehiclePrice;
import swp.project.swp391.repository.VehiclePriceRepository;
import swp.project.swp391.response.VehiclePriceResponse;
import swp.project.swp391.service.VehiclePriceService;

import java.util.List;
import java.util.stream.Collectors;

@Service
public class VehiclePriceServiceImp implements VehiclePriceService {

    @Autowired
    private VehiclePriceRepository vehiclePriceRepository;

    @Override
    public List<VehiclePriceResponse> getAllVehiclePrices() {
        List<VehiclePrice> vehiclePrices = vehiclePriceRepository.findAllWithDetails();
        return vehiclePrices.stream()
                .map(this::convertToResponse)
                .collect(Collectors.toList());
    }

    private VehiclePriceResponse convertToResponse(VehiclePrice vehiclePrice) {
        String vehicleModelColor = vehiclePrice.getVehicleModelColor().getVehicleModel().getName() + " - " + vehiclePrice.getVehicleModelColor().getColor().getColorName();
        return new VehiclePriceResponse(
                vehiclePrice.getId(),
                vehiclePrice.getWholesalePrice(),
                vehiclePrice.getEffectiveFrom(),
                vehiclePrice.getEffectiveTo(),
                vehiclePrice.getIsActive(),
                vehicleModelColor,
                vehiclePrice.getDealerLevel().getLevelName()
        );
    }
}

