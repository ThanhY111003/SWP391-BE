package swp.project.swp391.service.order;

import swp.project.swp391.entity.User;
import swp.project.swp391.response.defective.DefectiveVehicleReportResponse;
import swp.project.swp391.response.defective.RepairedVehicleResponse;

import java.util.List;

public interface DefectiveVehicleService {
    DefectiveVehicleReportResponse createReport(Long orderId, String reason, User reporter);
    List<DefectiveVehicleReportResponse> getReportsByOrder(Long orderId, User currentUser);
    DefectiveVehicleReportResponse approveReport(Long reportId, User approver);
    DefectiveVehicleReportResponse completeRepair(Long reportId, User currentUser);
    RepairedVehicleResponse confirmRepairedVehicle(Long orderId, User dealerUser);
}
