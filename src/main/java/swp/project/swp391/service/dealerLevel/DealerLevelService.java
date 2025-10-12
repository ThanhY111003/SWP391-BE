package swp.project.swp391.service.dealerLevel;

import swp.project.swp391.request.dealerLevel.CreateDealerLevelRequest;
import swp.project.swp391.request.dealerLevel.EditDealerLevelRequest;
import swp.project.swp391.response.dealerLevel.DealerLevelResponse;

import java.util.List;

public interface DealerLevelService {

    DealerLevelResponse createDealerLevel(CreateDealerLevelRequest request);

    DealerLevelResponse editDealerLevel(Long id, EditDealerLevelRequest request);

    List<DealerLevelResponse> getAllDealerLevels();

    DealerLevelResponse getDealerLevelById(Long id);

    void deleteDealerLevel(Long id);
}
