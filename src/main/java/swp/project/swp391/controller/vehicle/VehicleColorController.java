package swp.project.swp391.controller.vehicle;

import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.entity.User;
import swp.project.swp391.request.vehicle.VehicleColorRequest;
import swp.project.swp391.response.vehicle.VehicleColorResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.vehicle.VehicleColorService;

import jakarta.validation.Valid;

@RestController
@RequestMapping("/api/vehicle-colors")
@RequiredArgsConstructor
public class VehicleColorController {
    private final VehicleColorService service;
    private final RbacGuard guard;

    @PostMapping("/create")
    public ResponseEntity<VehicleColorResponse> create(@Valid @RequestBody VehicleColorRequest req) {
        User currentUser = guard.me();
        return new ResponseEntity<>(service.createVehicleColor(req, currentUser), HttpStatus.CREATED);
    }
}
