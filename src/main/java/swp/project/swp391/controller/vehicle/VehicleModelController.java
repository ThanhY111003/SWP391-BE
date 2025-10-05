    package swp.project.swp391.controller.vehicle;

    import io.swagger.v3.oas.annotations.Operation;
    import jakarta.validation.Valid;
    import lombok.RequiredArgsConstructor;
    import org.springframework.http.HttpStatus;
    import org.springframework.http.ResponseEntity;
    import org.springframework.web.bind.annotation.*;
    import swp.project.swp391.api.ApiResponse;
    import swp.project.swp391.entity.User;
    import swp.project.swp391.request.vehicle.VehicleModelRequest;
    import swp.project.swp391.response.vehicle.VehicleModelResponse;
    import swp.project.swp391.security.RbacGuard;
    import swp.project.swp391.service.vehicle.VehicleModelService;

    import java.util.List;

    @RestController
    @RequestMapping("/api/vehicle-models")
    @RequiredArgsConstructor
    public class VehicleModelController {

        private final VehicleModelService service;
        private final RbacGuard guard;

        // --------------------------------------------------------
        // CREATE
        // --------------------------------------------------------
        @Operation(
                summary = "Tạo model xe mới",
                description = """
                    Dành cho ADMIN hoặc EVM_STAFF có quyền 'vehicleModel.create'.
                    Bao gồm thông tin kỹ thuật, giá nhà sản xuất, hình ảnh và mô tả.
                    """
        )
        @PostMapping("/create")
        public ResponseEntity<ApiResponse<VehicleModelResponse>> createVehicleModel(
                @Valid @RequestBody VehicleModelRequest request) {

            User currentUser = guard.me();
            VehicleModelResponse response = service.createVehicleModel(request, currentUser);

            return ResponseEntity.status(HttpStatus.CREATED)
                    .body(ApiResponse.ok(response, "Tạo model xe thành công."));
        }

        // --------------------------------------------------------
        // GET ALL
        // --------------------------------------------------------
        @Operation(
                summary = "Lấy danh sách tất cả Vehicle Models",
                description = "Thêm ?includeInactive=true để lấy cả các model đã vô hiệu hóa (nếu hệ thống sử dụng cờ này)."
        )
        @GetMapping
        public ResponseEntity<ApiResponse<List<VehicleModelResponse>>> getAllVehicleModels() {
            List<VehicleModelResponse> list = service.getAllVehicleModels();
            return ResponseEntity.ok(ApiResponse.ok(list, "Lấy danh sách model thành công."));
        }

        // --------------------------------------------------------
        // GET BY ID
        // --------------------------------------------------------
        @Operation(summary = "Lấy chi tiết một Vehicle Model theo ID")
        @GetMapping("/{id}")
        public ResponseEntity<ApiResponse<VehicleModelResponse>> getVehicleModelById(@PathVariable Long id) {
            VehicleModelResponse response = service.getVehicleModelById(id);
            return ResponseEntity.ok(ApiResponse.ok(response, "Lấy chi tiết model thành công."));
        }

        // --------------------------------------------------------
        // UPDATE
        // --------------------------------------------------------
        @Operation(
                summary = "Cập nhật Vehicle Model",
                description = """
                    Cho phép cập nhật các trường kỹ thuật, giá, mô tả, hình ảnh.
                    Nếu đổi 'modelCode' sẽ kiểm tra trùng trước khi cập nhật.
                    Yêu cầu quyền 'vehicleModel.update'.
                    """
        )
        @PutMapping("/{id}")
        public ResponseEntity<ApiResponse<VehicleModelResponse>> updateVehicleModel(
                @PathVariable Long id,
                @Valid @RequestBody VehicleModelRequest request) {

            User currentUser = guard.me();
            VehicleModelResponse response = service.updateVehicleModel(id, request, currentUser);
            return ResponseEntity.ok(ApiResponse.ok(response, "Cập nhật model thành công."));
        }
    }
