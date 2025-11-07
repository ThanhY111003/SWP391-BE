package swp.project.swp391.service.report;

import swp.project.swp391.response.report.InventoryStatusResponse;
import swp.project.swp391.response.report.SalesTrendResponse;
import swp.project.swp391.response.report.TopCustomerResponse;
import swp.project.swp391.response.report.TopModelResponse;

import java.time.LocalDate;
import java.util.List;

public interface DealerReportService {
    List<SalesTrendResponse> getSalesTrend(String groupBy, LocalDate fromDate, LocalDate toDate);
    List<TopModelResponse> getTopModels(int limit, LocalDate fromDate, LocalDate toDate);
    List<TopCustomerResponse> getTopCustomers(int limit, LocalDate fromDate, LocalDate toDate);
    List<InventoryStatusResponse> getInventoryStatus();
}
