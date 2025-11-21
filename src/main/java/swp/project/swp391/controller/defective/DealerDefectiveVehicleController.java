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

    // ======================= BÁO LỖI XE (1 order = 1 vehicle) =======================
    @Operation(
            summary = "Báo cáo xe lỗi",
            description = "Dealer báo lỗi cho chiếc xe trong đơn hàng đang giao"
    )
    @PostMapping("/orders/{orderId}/report")
    public ResponseEntity<ApiResponse<DefectiveVehicleReportResponse>> createReport(
            @PathVariable Long orderId,
            @RequestParam String reason
    ) {
        User me = guard.me();
        DefectiveVehicleReportResponse report =
                defectiveVehicleService.createReport(orderId, reason, me);

        return ResponseEntity.ok(ApiResponse.ok(report, "Báo cáo xe lỗi thành công"));
    }

    // ======================= XEM DANH SÁCH BÁO LỖI =======================
    @Operation(summary = "Xem danh sách báo cáo xe lỗi của đơn hàng")
    @GetMapping("/orders/{orderId}")
    public ResponseEntity<ApiResponse<List<DefectiveVehicleReportResponse>>> getReportsByOrder(
            @PathVariable Long orderId
    ) {
        User me = guard.me();
        List<DefectiveVehicleReportResponse> reports =
                defectiveVehicleService.getReportsByOrder(orderId, me);

        return ResponseEntity.ok(ApiResponse.ok(reports, "Lấy danh sách báo cáo thành công"));
    }

    // ======================= XÁC NHẬN XE ĐÃ SỬA XONG =======================
    @PatchMapping("/orders/{orderId}/confirm-repaired")
    @Operation(summary = "Xác nhận xe sửa xong",
            description = "Dealer xác nhận đã nhận lại xe sửa xong từ hãng")
    public ResponseEntity<ApiResponse<RepairedVehicleResponse>> confirmRepairedVehicle(
            @PathVariable Long orderId
    ) {
        User me = guard.me();
        RepairedVehicleResponse response =
                defectiveVehicleService.confirmRepairedVehicle(orderId, me);

        return ResponseEntity.ok(ApiResponse.ok(response, "Xác nhận xe đã sửa xong thành công"));
    }

    // ======================= HUỶ BÁO CÁO XE LỖI =======================
    @Operation(
            summary = "Hủy báo cáo xe lỗi",
            description = "Dealer hủy báo cáo khi phát hiện xe không lỗi nữa"
    )
    @DeleteMapping("/reports/{reportId}/cancel")
    public ResponseEntity<ApiResponse<DefectiveVehicleReportResponse>> cancelReport(
            @PathVariable Long reportId
    ) {
        User me = guard.me();
        DefectiveVehicleReportResponse response =
                defectiveVehicleService.cancelReportByDealer(reportId, me);

        return ResponseEntity.ok(ApiResponse.ok(response, "Hủy báo cáo xe lỗi thành công"));
    }

}

