package swp.project.swp391.service.order;

import swp.project.swp391.entity.User;
import swp.project.swp391.response.defective.DefectiveVehicleReportResponse;
import swp.project.swp391.response.defective.RepairedVehicleResponse;

import java.util.List;

public interface DefectiveVehicleService {
    DefectiveVehicleReportResponse createReport(Long orderId, String reason, User reporter);
    List<DefectiveVehicleReportResponse> getReportsByOrder(Long orderId, User currentUser);
    DefectiveVehicleReportResponse approveReport(Long orderId, User approver);
    DefectiveVehicleReportResponse completeRepair(Long orderId, User currentUser);
    RepairedVehicleResponse confirmRepairedVehicle(Long orderId, User dealerUser);
    DefectiveVehicleReportResponse cancelReportByDealer(Long orderId, User dealerUser);
    DefectiveVehicleReportResponse rejectReport(Long orderId, User adminUser);
}
