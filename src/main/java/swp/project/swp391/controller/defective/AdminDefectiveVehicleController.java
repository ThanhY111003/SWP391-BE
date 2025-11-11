package swp.project.swp391.controller.defective;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.entity.User;
import swp.project.swp391.response.defective.DefectiveVehicleReportResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.order.DefectiveVehicleService;

import java.util.List;

@RestController
@RequestMapping("/api/defects/admin")
@RequiredArgsConstructor
@Tag(name = "Xe lỗi (Admin/EVM)", description = "Hãng phê duyệt, xem và xác nhận sửa xe lỗi")
public class AdminDefectiveVehicleController {

    private final DefectiveVehicleService defectiveVehicleService;
    private final RbacGuard guard;

    @Operation(summary = "Lấy danh sách báo cáo xe lỗi theo đơn hàng", description = "Admin/EVM xem tất cả báo cáo xe lỗi trong đơn hàng cụ thể")
    @GetMapping("/order/{orderId}")
    public ResponseEntity<ApiResponse<List<DefectiveVehicleReportResponse>>> getReportsByOrder(@PathVariable Long orderId) {
        User me = guard.me();
        List<DefectiveVehicleReportResponse> reports = defectiveVehicleService.getReportsByOrder(orderId, me);
        return ResponseEntity.ok(ApiResponse.ok(reports, "Lấy danh sách báo cáo xe lỗi thành công"));
    }

    @Operation(summary = "Phê duyệt báo cáo xe lỗi", description = "Hãng xác nhận báo cáo xe lỗi là hợp lệ để tiến hành sửa chữa")
    @PatchMapping("/{reportId}/approve")
    public ResponseEntity<ApiResponse<DefectiveVehicleReportResponse>> approveReport(@PathVariable Long reportId) {
        User me = guard.me();
        DefectiveVehicleReportResponse report = defectiveVehicleService.approveReport(reportId, me);
        return ResponseEntity.ok(ApiResponse.ok(report, "Phê duyệt báo cáo xe lỗi thành công"));
    }

    @Operation(summary = "Xác nhận sửa xe lỗi hoàn tất", description = "Hãng xác nhận xe lỗi đã được sửa xong, chuẩn bị gửi lại dealer")
    @PatchMapping("/{reportId}/complete-repair")
    public ResponseEntity<ApiResponse<DefectiveVehicleReportResponse>> completeRepair(@PathVariable Long reportId) {
        User me = guard.me();
        DefectiveVehicleReportResponse report = defectiveVehicleService.completeRepair(reportId, me);
        return ResponseEntity.ok(ApiResponse.ok(report, "Xác nhận sửa xe lỗi hoàn tất"));
    }
}
