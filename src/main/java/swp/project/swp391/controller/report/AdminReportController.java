package swp.project.swp391.controller.report;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.response.report.*;
import swp.project.swp391.service.report.AdminReportService;

import java.time.LocalDate;
import java.util.List;

@RestController
@RequestMapping("/api/reports/admin")
@RequiredArgsConstructor
@Tag(name = "Admin Report", description = "Báo cáo dành cho hãng (ADMIN)")
public class AdminReportController {

    private final AdminReportService adminReportService;

    @Operation(summary = "Doanh thu hệ thống", description = "Thống kê doanh thu toàn hệ thống theo thời gian.")
    @GetMapping("/sales-trend")
    public ResponseEntity<ApiResponse<List<SalesTrendResponse>>> getSalesTrend(
            @RequestParam(defaultValue = "all") String groupBy,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate fromDate,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate toDate
    ) {
        return ResponseEntity.ok(ApiResponse.ok(adminReportService.getSalesTrend(groupBy, fromDate, toDate)));
    }

    @Operation(summary = "Top mẫu xe bán chạy", description = "Thống kê mẫu xe bán nhiều nhất toàn hệ thống.")
    @GetMapping("/top-models")
    public ResponseEntity<ApiResponse<List<TopModelResponse>>> getTopModels(
            @RequestParam(defaultValue = "5") int limit,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate fromDate,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate toDate
    ) {
        return ResponseEntity.ok(ApiResponse.ok(adminReportService.getTopModels(limit, fromDate, toDate)));
    }

    @Operation(summary = "Top đại lý mua nhiều nhất", description = "Thống kê các đại lý chi tiêu cao nhất.")
    @GetMapping("/top-dealers")
    public ResponseEntity<ApiResponse<List<TopDealerResponse>>> getTopDealers(
            @RequestParam(defaultValue = "5") int limit,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate fromDate,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate toDate
    ) {
        return ResponseEntity.ok(ApiResponse.ok(adminReportService.getTopDealers(limit, fromDate, toDate)));
    }

    @Operation(summary = "Hiệu suất đại lý", description = "Thống kê hiệu suất hoạt động của từng đại lý.")
    @GetMapping("/dealer-performance")
    public ResponseEntity<ApiResponse<List<DealerPerformanceResponse>>> getDealerPerformance(
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate fromDate,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate toDate
    ) {
        return ResponseEntity.ok(ApiResponse.ok(adminReportService.getDealerPerformance(fromDate, toDate)));
    }

    @Operation(summary = "Tổng hợp tồn kho", description = "Thống kê tồn kho toàn hệ thống theo mẫu xe và màu.")
    @GetMapping("/inventory-summary")
    public ResponseEntity<ApiResponse<List<InventorySummaryResponse>>> getInventorySummary() {
        return ResponseEntity.ok(ApiResponse.ok(adminReportService.getInventorySummary()));
    }
}
