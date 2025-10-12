package swp.project.swp391.serviceImp.dealerLevel;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.DealerLevel;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.DealerLevelRepository;
import swp.project.swp391.request.dealerLevel.CreateDealerLevelRequest;
import swp.project.swp391.request.dealerLevel.EditDealerLevelRequest;
import swp.project.swp391.response.dealerLevel.DealerLevelResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.dealerLevel.DealerLevelService;

import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class DealerLevelServiceImpl implements DealerLevelService {

    private final DealerLevelRepository dealerLevelRepository;
    private final RbacGuard guard;

    @Override
    @Transactional
    public DealerLevelResponse createDealerLevel(CreateDealerLevelRequest request) {
        // Kiểm tra quyền tạo Dealer Level
        guard.require(guard.has(guard.me(), "dealerLevel.create"));

        // Kiểm tra xem levelNumber đã tồn tại chưa
        if (dealerLevelRepository.existsByLevelNumber(request.getLevelNumber())) {
            throw new BaseException(ErrorHandler.DEALER_LEVEL_ALREADY_EXISTS);
        }

        // Tạo mới DealerLevel
        DealerLevel dealerLevel = new DealerLevel();
        dealerLevel.setLevelName(request.getLevelName());
        dealerLevel.setLevelNumber(request.getLevelNumber());
        dealerLevel.setDiscountRate(request.getDiscountRate());
        dealerLevel.setMaxOrderQuantity(request.getMaxOrderQuantity());
        dealerLevel.setCreditLimit(request.getCreditLimit());
        dealerLevel.setMaxInstallmentMonths(request.getMaxInstallmentMonths());
        dealerLevel.setDescription(request.getDescription());

        // Lưu vào DB
        dealerLevelRepository.save(dealerLevel);

        return toResponse(dealerLevel);
    }

    @Override
    @Transactional
    public DealerLevelResponse editDealerLevel(Long id, EditDealerLevelRequest request) {
        // Kiểm tra quyền sửa Dealer Level
        guard.require(guard.has(guard.me(), "dealerLevel.update"));

        DealerLevel dealerLevel = dealerLevelRepository.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.DEALER_LEVEL_NOT_FOUND));

        // Cập nhật thông tin
        dealerLevel.setLevelName(request.getLevelName());
        dealerLevel.setDiscountRate(request.getDiscountRate());
        dealerLevel.setMaxOrderQuantity(request.getMaxOrderQuantity());
        dealerLevel.setCreditLimit(request.getCreditLimit());
        dealerLevel.setMaxInstallmentMonths(request.getMaxInstallmentMonths());
        dealerLevel.setDescription(request.getDescription());

        // Lưu vào DB
        dealerLevelRepository.save(dealerLevel);

        return toResponse(dealerLevel);
    }

    @Override
    public List<DealerLevelResponse> getAllDealerLevels() {
        // Kiểm tra quyền xem tất cả Dealer Levels
        guard.require(guard.has(guard.me(), "dealerLevel.read.all"));

        List<DealerLevel> dealerLevels = dealerLevelRepository.findAll();
        return dealerLevels.stream()
                .map(this::toResponse)
                .collect(Collectors.toList());
    }

    @Override
    public DealerLevelResponse getDealerLevelById(Long id) {
        // Kiểm tra quyền xem một Dealer Level
        guard.require(guard.has(guard.me(), "dealerLevel.read"));

        DealerLevel dealerLevel = dealerLevelRepository.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.DEALER_LEVEL_NOT_FOUND));
        return toResponse(dealerLevel);
    }

    @Override
    @Transactional
    public void deleteDealerLevel(Long id) {
        // Kiểm tra quyền xóa Dealer Level
        guard.require(guard.has(guard.me(), "dealerLevel.delete"));

        DealerLevel dealerLevel = dealerLevelRepository.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.DEALER_LEVEL_NOT_FOUND));

        // Kiểm tra nếu có đại lý nào đang sử dụng DealerLevel này
        if (!dealerLevel.getDealers().isEmpty()) {
            throw new BaseException(ErrorHandler.DEALER_LEVEL_IN_USE, "Không thể xóa cấp độ đại lý đang được sử dụng.");
        }

        dealerLevelRepository.delete(dealerLevel);
    }

    private DealerLevelResponse toResponse(DealerLevel dealerLevel) {
        return new DealerLevelResponse(
                dealerLevel.getId(),
                dealerLevel.getLevelName(),
                dealerLevel.getLevelNumber(),
                dealerLevel.getDiscountRate(),
                dealerLevel.getMaxOrderQuantity(),
                dealerLevel.getCreditLimit(),
                dealerLevel.getMaxInstallmentMonths(),
                dealerLevel.getDescription()
        );
    }
}
