package swp.project.swp391.controller.defective;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.entity.User;
import swp.project.swp391.entity.VehicleInstance;
import swp.project.swp391.response.defective.DefectiveVehicleReportResponse;
import swp.project.swp391.response.defective.RepairedVehicleResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.order.DefectiveVehicleService;

import java.util.List;

@RestController
@RequestMapping("/api/defects/dealer")
@RequiredArgsConstructor
@Tag(name = "Xe lỗi (Dealer)", description = "Dealer báo cáo, xem và xác nhận xe lỗi sau khi sửa")
public class DealerDefectiveVehicleController {

    private final DefectiveVehicleService defectiveVehicleService;
    private final RbacGuard guard;

    @Operation(
            summary = "Báo cáo xe lỗi",
            description = "Dealer gửi báo cáo xe lỗi cho hãng trong đơn hàng đang giao (xe chưa về kho)"
    )
    @PostMapping("/orders/{orderId}/vehicles/{vehicleId}/report")
    public ResponseEntity<ApiResponse<DefectiveVehicleReportResponse>> createReport(
            @PathVariable Long orderId,
            @PathVariable Long vehicleId,
            @RequestParam String reason
    ) {
        User me = guard.me();
        DefectiveVehicleReportResponse report = defectiveVehicleService.createReport(orderId, vehicleId, reason, me);
        return ResponseEntity.ok(ApiResponse.ok(report, "Báo cáo xe lỗi thành công"));
    }


    @Operation(summary = "Xem danh sách báo cáo xe lỗi của đơn hàng", description = "Dealer xem các xe bị lỗi trong đơn hàng của mình")
    @GetMapping("/order/{orderId}")
    public ResponseEntity<ApiResponse<List<DefectiveVehicleReportResponse>>> getReportsByOrder(@PathVariable Long orderId) {
        User me = guard.me();
        List<DefectiveVehicleReportResponse> reports = defectiveVehicleService.getReportsByOrder(orderId, me);
        return ResponseEntity.ok(ApiResponse.ok(reports, "Lấy danh sách báo cáo xe lỗi thành công"));
    }

    @PatchMapping("/orders/{orderId}/vehicles/{vehicleId}/confirm-repaired")
    @Operation(summary = "Xác nhận xe đã được sửa xong", description = "Dealer xác nhận đã nhận lại xe sửa xong từ hãng")
    public ResponseEntity<ApiResponse<RepairedVehicleResponse>> confirmRepairedVehicle(
            @PathVariable Long orderId,
            @PathVariable Long vehicleId
    ) {
        User me = guard.me();
        RepairedVehicleResponse response = defectiveVehicleService.confirmRepairedVehicle(orderId, vehicleId, me);
        return ResponseEntity.ok(ApiResponse.ok(response, "Xác nhận xe đã được sửa xong thành công"));
    }


}
