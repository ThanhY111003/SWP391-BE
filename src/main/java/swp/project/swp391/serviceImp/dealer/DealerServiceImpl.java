package swp.project.swp391.serviceImp.dealer;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.Dealer;
import swp.project.swp391.entity.DealerLevel;
import swp.project.swp391.entity.User;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.DealerLevelRepository;
import swp.project.swp391.repository.DealerRepository;
import swp.project.swp391.request.dealer.DealerRequest;
import swp.project.swp391.response.dealer.DealerResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.dealer.DealerService;

import java.time.LocalDateTime;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class DealerServiceImpl implements DealerService {

    private final DealerRepository dealerRepository;
    private final DealerLevelRepository dealerLevelRepository;
    private final RbacGuard guard;

    @Override
    @Transactional
    public DealerResponse createDealer(DealerRequest rq, User currentUser) {
        // Quyền tạo
        guard.require(guard.has(currentUser, "dealer.create"));

        // Validate trùng email/phone
        if (dealerRepository.existsByEmail(rq.getEmail())) {
            throw new BaseException(ErrorHandler.EMAIL_ALREADY_EXISTS);
        }
        if (dealerRepository.existsByPhoneNumber(rq.getPhoneNumber())) {
            throw new BaseException(ErrorHandler.PHONE_NUMBER_ALREADY_EXISTS);
        }

        // Lấy level theo levelNumber
        DealerLevel level = dealerLevelRepository.findByLevelNumber(rq.getLevelNumber())
                .orElseThrow(() -> new BaseException(ErrorHandler.DEALER_LEVEL_NOT_FOUND));

        // Region
        Dealer.Region region = Dealer.Region.valueOf(rq.getRegion().toUpperCase());

        // Tự sinh code + đảm bảo unique (retry tối đa 5 lần)
        String code = null;
        for (int i = 0; i < 5; i++) {
            String candidate = genDealerCode(region);
            if (!dealerRepository.existsByCode(candidate)) {
                code = candidate;
                break;
            }
        }
        if (code == null) throw new BaseException(ErrorHandler.CODE_GENERATION_FAILED);

        // Tạo Dealer
        Dealer dealer = Dealer.builder()
                .name(rq.getName())
                .code(code)
                .address(rq.getAddress())
                .phoneNumber(rq.getPhoneNumber())
                .email(rq.getEmail())
                .region(region)
                .isActive(true)
                .level(level)
                .createdAt(LocalDateTime.now())
                .updatedAt(LocalDateTime.now())
                .build();

        dealerRepository.save(dealer);
        return toResponse(dealer);
    }

    @Override
    @Transactional
    public DealerResponse inactiveDealer(Long dealerId, User currentUser) {
        guard.require(guard.has(currentUser, "dealer.inactive"));

        Dealer dealer = dealerRepository.findById(dealerId)
                .orElseThrow(() -> new BaseException(ErrorHandler.DEALER_NOT_FOUND));

        dealer.setIsActive(false);
        dealer.setUpdatedAt(LocalDateTime.now());
        dealerRepository.save(dealer);

        return toResponse(dealer);
    }

    @Override
    @Transactional
    public DealerResponse reactivateDealer(Long dealerId, User currentUser) {
        guard.require(guard.has(currentUser, "dealer.reactivate"));

        Dealer dealer = dealerRepository.findById(dealerId)
                .orElseThrow(() -> new BaseException(ErrorHandler.DEALER_NOT_FOUND));

        dealer.setIsActive(true);
        dealer.setUpdatedAt(LocalDateTime.now());
        dealerRepository.save(dealer);

        return toResponse(dealer);
    }

    @Override
    @Transactional(readOnly = true)
    public List<DealerResponse> getAllDealers(User currentUser) {
        List<Dealer> dealers;

        if (guard.has(currentUser, "dealer.read.all")) {
            dealers = dealerRepository.findAll();
        } else {
            // ❌ Không có quyền => chặn luôn (tránh dealers bị null)
            throw new BaseException(ErrorHandler.FORBIDDEN);
        }
        return dealers.stream()
                .map(this::toResponse)
                .collect(Collectors.toList());
    }


    @Override
    @Transactional(readOnly = true)
    public DealerResponse getDealer(Long dealerId, User currentUser) {
        // ✅ Chỉ hãng (ADMIN / EVM_STAFF) có quyền xem chi tiết dealer
        guard.require(guard.has(currentUser, "dealer.read"));

        Dealer dealer = dealerRepository.findById(dealerId)
                .orElseThrow(() -> new BaseException(ErrorHandler.DEALER_NOT_FOUND));

        return toResponse(dealer);
    }


    @Override
    @Transactional
    public DealerResponse editDealer(Long dealerId, DealerRequest rq, User currentUser) {
        guard.require(guard.has(currentUser, "dealer.update"));

        Dealer dealer = dealerRepository.findById(dealerId)
                .orElseThrow(() -> new BaseException(ErrorHandler.DEALER_NOT_FOUND));

        // Nếu đổi email/phone thì check trùng
        if (!dealer.getEmail().equals(rq.getEmail()) && dealerRepository.existsByEmail(rq.getEmail())) {
            throw new BaseException(ErrorHandler.EMAIL_ALREADY_EXISTS);
        }
        if (!dealer.getPhoneNumber().equals(rq.getPhoneNumber()) && dealerRepository.existsByPhoneNumber(rq.getPhoneNumber())) {
            throw new BaseException(ErrorHandler.PHONE_NUMBER_ALREADY_EXISTS);
        }

        // Không cho chỉnh code (ổn định mã kinh doanh)
        // Nếu muốn cho phép, thêm validate existsByCode như trước.
        // dealer.setCode(...);

        Dealer.Region region = Dealer.Region.valueOf(rq.getRegion().toUpperCase());
        dealer.setName(rq.getName());
        dealer.setAddress(rq.getAddress());
        dealer.setPhoneNumber(rq.getPhoneNumber());
        dealer.setEmail(rq.getEmail());
        dealer.setRegion(region);

        // Cập nhật level nếu levelNumber khác
        if (rq.getLevelNumber() != null && !rq.getLevelNumber().equals(dealer.getLevel().getLevelNumber())) {
            DealerLevel level = dealerLevelRepository.findByLevelNumber(rq.getLevelNumber())
                    .orElseThrow(() -> new BaseException(ErrorHandler.DEALER_LEVEL_NOT_FOUND));
            dealer.setLevel(level);
        }

        dealer.setUpdatedAt(LocalDateTime.now());
        dealerRepository.save(dealer);

        return toResponse(dealer);
    }

    private DealerResponse toResponse(Dealer d) {
        return new DealerResponse(
                d.getId(),
                d.getName(),
                d.getAddress(),
                d.getPhoneNumber(),
                d.getEmail(),
                d.getIsActive(),
                d.getRegion().name(),
                d.getLevel() != null ? d.getLevel().getId() : null
        );
    }

    private String genDealerCode(Dealer.Region region) {
        // Ví dụ DL-NORTH-1730458901234 (timestamp)
        return "DL-" + region.name() + "-" + System.currentTimeMillis();
    }
}
