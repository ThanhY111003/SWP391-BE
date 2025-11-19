package swp.project.swp391.controller.report;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.format.annotation.DateTimeFormat;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.response.report.InventoryStatusResponse;
import swp.project.swp391.response.report.SalesTrendResponse;
import swp.project.swp391.response.report.TopCustomerResponse;
import swp.project.swp391.response.report.TopModelResponse;
import swp.project.swp391.service.report.DealerReportService;

import java.time.LocalDate;
import java.util.List;

@RestController
@RequestMapping("/api/reports/dealer")
@RequiredArgsConstructor
@Tag(name = "Dealer Manager report", description = "Báo cáo dành cho đại lý (DEALER MANAGER)")
public class DealerReportController {

    private final DealerReportService dealerReportService;

    // --------------------------------------------------------
    // GET SALES TREND - Chỉ Dealer Manager được phép
    // --------------------------------------------------------
    @GetMapping("/sales-trend")
    @Operation(summary = "Doanh thu đai lý", description = "Thống kê doanh thu đại lý theo thời gian.")
    public List<SalesTrendResponse> getSalesTrend(
            @RequestParam(required = false, defaultValue = "all") String groupBy, // day | month | year | all
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate fromDate,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate toDate
    ) {
        return dealerReportService.getSalesTrend(groupBy, fromDate, toDate);
    }

    // --------------------------------------------------------
    // TOP MODELS SOLD
    // --------------------------------------------------------
    @GetMapping("/top-models")
    @Operation(summary = "Top mẫu xe bán chạy", description = "Thống kê mẫu xe bán nhiều nhất đại lý.")
    public List<TopModelResponse> getTopModels(
            @RequestParam(defaultValue = "5") int limit,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate fromDate,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate toDate
    ) {
        return dealerReportService.getTopModels(limit, fromDate, toDate);
    }

    // --------------------------------------------------------
    // TOP CUSTOMERS
    // --------------------------------------------------------
    @GetMapping("/top-customers")
    @Operation(summary = "Top khách hàng mua xe nhiều nhất", description = "Thống kê khách hàng mua xe nhiều nhất.")
    public List<TopCustomerResponse> getTopCustomers(
            @RequestParam(defaultValue = "5") int limit,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate fromDate,
            @RequestParam(required = false)
            @DateTimeFormat(iso = DateTimeFormat.ISO.DATE) LocalDate toDate
    ) {
        return dealerReportService.getTopCustomers(limit, fromDate, toDate);
    }

    // --------------------------------------------------------
    // INVENTORY STATUS
    // --------------------------------------------------------
    @Operation(summary = "Tổng hợp tồn kho", description = "Thống kê tồn kho toàn hệ thống theo mẫu xe và màu.")
    @GetMapping("/inventory-status")
    public List<InventoryStatusResponse> getInventoryStatus() {
        return dealerReportService.getInventoryStatus();
    }
}
