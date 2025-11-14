package swp.project.swp391.service.report;

import swp.project.swp391.response.report.*;

import java.time.LocalDate;
import java.util.List;

public interface AdminReportService {

    List<SalesTrendResponse> getSalesTrend(String groupBy, LocalDate fromDate, LocalDate toDate);

    List<TopModelResponse> getTopModels(int limit, LocalDate fromDate, LocalDate toDate);

    List<TopDealerResponse> getTopDealers(int limit, LocalDate fromDate, LocalDate toDate);

    List<DealerPerformanceResponse> getDealerPerformance(LocalDate fromDate, LocalDate toDate);

    List<InventorySummaryResponse> getInventorySummary();
}
