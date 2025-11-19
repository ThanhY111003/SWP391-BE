package swp.project.swp391.controller.price;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
import swp.project.swp391.service.VehiclePriceService;

@RestController
@RequestMapping("/api/v1/prices")
public class PriceController {

    @Autowired
    private VehiclePriceService vehiclePriceService;

    @GetMapping("/all")
    public ResponseEntity<?> getAllVehiclePrices() {
        return ResponseEntity.ok(vehiclePriceService.getAllVehiclePrices());
    }
}

