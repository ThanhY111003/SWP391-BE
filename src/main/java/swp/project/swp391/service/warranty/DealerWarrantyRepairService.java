package swp.project.swp391.service.warranty;

import swp.project.swp391.entity.User;
import swp.project.swp391.response.warranty.DealerWarrantyRepairResponse;

import java.util.List;

public interface DealerWarrantyRepairService {
    DealerWarrantyRepairResponse createRequest(Long vehicleId, String reason, User dealerUser);
    List<DealerWarrantyRepairResponse> getMyRequests(User dealerUser);
    DealerWarrantyRepairResponse approveRequest(Long requestId, User manufacturerUser);
    DealerWarrantyRepairResponse completeRepair(Long requestId, User manufacturerUser);
    DealerWarrantyRepairResponse confirmReceived(Long requestId, User dealerUser);
    List<DealerWarrantyRepairResponse> getRequestsByDealer(Long dealerId, User manufacturerUser);
    DealerWarrantyRepairResponse cancelRequest(Long requestId, User dealerUser);
    DealerWarrantyRepairResponse rejectRequest(Long requestId, User manufacturerUser);

}
