package swp.project.swp391.serviceImp.price;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.*;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.*;
import swp.project.swp391.request.price.VehiclePriceRequest;
import swp.project.swp391.response.price.VehiclePriceResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.price.VehiclePriceService;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional
public class VehiclePriceServiceImpl implements VehiclePriceService {

    private final VehiclePriceRepository vehiclePriceRepo;
    private final VehicleModelColorRepository vehicleModelColorRepo;
    private final DealerLevelRepository dealerLevelRepo;
    private final RbacGuard guard;
    private final UserRepository userRepo;

    // --------------------------------------------------------
    // GET ALL
    // --------------------------------------------------------
    @Override
    public List<VehiclePriceResponse> getAll(Long dealerLevelId, LocalDate startDate, LocalDate endDate) {
        User current = guard.me();
        guard.require(guard.has(current, "vehicle_price.read"));

        List<VehiclePrice> list;

        if (guard.has(current, "vehicle_price.manage_all")) {
            // Admin hoặc EVM Staff có thể xem tất cả, có filter theo ngày
            list = vehiclePriceRepo.findAllFiltered(dealerLevelId, startDate, endDate);
        } else {
            // Dealer user: chỉ xem giá theo cấp đại lý của mình
            User userEntity = userRepo.findById(current.getId())
                    .orElseThrow(() -> new BaseException(ErrorHandler.UNAUTHORIZED));

            if (userEntity.getDealer() == null) {
                throw new BaseException(ErrorHandler.UNAUTHORIZED, "Người dùng không thuộc đại lý nào.");
            }

            DealerLevel level = userEntity.getDealer().getLevel();
            list = vehiclePriceRepo.findAllFiltered(level.getId(), startDate, endDate);
        }

        return list.stream()
                .map(VehiclePriceResponse::fromEntity)
                .collect(Collectors.toList());
    }


    // --------------------------------------------------------
    // GET BY ID
    // --------------------------------------------------------
    @Override
    public VehiclePriceResponse getById(Long id) {
        User current = guard.me();
        guard.require(guard.has(current, "vehicle_price.read"));

        VehiclePrice vp = vehiclePriceRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_PRICE_NOT_FOUND));

        // Nếu không có quyền xem tất cả thì giới hạn theo dealer level
        if (!guard.has(current, "vehicle_price.manage_all")) {
            // ✅ Fix LazyInitializationException bằng cách fetch lại từ DB
            User userEntity = userRepo.findById(current.getId())
                    .orElseThrow(() -> new BaseException(ErrorHandler.UNAUTHORIZED));
            Dealer dealer = userEntity.getDealer();

            if (dealer == null || !vp.getDealerLevel().equals(dealer.getLevel())) {
                throw new BaseException(ErrorHandler.FORBIDDEN);
            }
        }

        return VehiclePriceResponse.fromEntity(vp);
    }


    // --------------------------------------------------------
// CREATE
// --------------------------------------------------------
    @Override
    public VehiclePriceResponse create(VehiclePriceRequest req) {
        guard.require(guard.has(guard.me(), "vehicle_price.create"));

        VehicleModelColor color = vehicleModelColorRepo.findById(req.getVehicleModelColorId())
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_MODEL_COLOR_NOT_FOUND));

        DealerLevel level = dealerLevelRepo.findById(req.getDealerLevelId())
                .orElseThrow(() -> new BaseException(ErrorHandler.DEALER_LEVEL_NOT_FOUND));

        // --- Kiểm tra logic thời gian ---
        if (req.getEffectiveTo() != null && req.getEffectiveTo().isBefore(req.getEffectiveFrom())) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Ngày kết thúc phải sau hoặc bằng ngày bắt đầu.");
        }

        // --- Kiểm tra giá hợp lệ ---
        if (req.getWholesalePrice() == null || req.getWholesalePrice().compareTo(BigDecimal.ZERO) <= 0) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Giá bán buôn phải lớn hơn 0.");
        }

        // --- Kiểm tra overlap (trùng vùng hiệu lực) ---
        boolean hasOverlap = vehiclePriceRepo.existsByOverlap(
                color, level, req.getEffectiveFrom(), req.getEffectiveTo());

        if (hasOverlap) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Khoảng thời gian bị chồng với bảng giá khác của cùng mẫu xe và cấp đại lý.");
        }

        VehiclePrice vp = VehiclePrice.builder()
                .vehicleModelColor(color)
                .dealerLevel(level)
                .wholesalePrice(req.getWholesalePrice())
                .effectiveFrom(req.getEffectiveFrom())
                .effectiveTo(req.getEffectiveTo())
                .isActive(true)
                .build();

        vehiclePriceRepo.save(vp);
        return VehiclePriceResponse.fromEntity(vp);
    }


    // --------------------------------------------------------
// UPDATE
// --------------------------------------------------------
    @Override
    public VehiclePriceResponse update(Long id, VehiclePriceRequest req) {
        guard.require(guard.has(guard.me(), "vehicle_price.update"));

        VehiclePrice vp = vehiclePriceRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_PRICE_NOT_FOUND));

        // --- Cập nhật các trường cơ bản ---
        if (req.getWholesalePrice() != null) {
            if (req.getWholesalePrice().compareTo(BigDecimal.ZERO) <= 0) {
                throw new BaseException(ErrorHandler.INVALID_REQUEST, "Giá bán buôn phải lớn hơn 0.");
            }
            vp.setWholesalePrice(req.getWholesalePrice());
        }

        if (req.getEffectiveFrom() != null) vp.setEffectiveFrom(req.getEffectiveFrom());
        if (req.getEffectiveTo() != null) vp.setEffectiveTo(req.getEffectiveTo());

        // --- Kiểm tra logic thời gian ---
        if (vp.getEffectiveTo() != null && vp.getEffectiveTo().isBefore(vp.getEffectiveFrom())) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Ngày kết thúc phải sau hoặc bằng ngày bắt đầu.");
        }

        // --- Kiểm tra overlap với các bảng giá khác (trừ chính nó) ---
        boolean hasOverlap = vehiclePriceRepo.existsByOverlapExcludeId(
                vp.getId(),
                vp.getVehicleModelColor(),
                vp.getDealerLevel(),
                vp.getEffectiveFrom(),
                vp.getEffectiveTo()
        );

        if (hasOverlap) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Khoảng thời gian bị chồng với bảng giá khác của cùng mẫu xe và cấp đại lý.");
        }

        vehiclePriceRepo.save(vp);
        return VehiclePriceResponse.fromEntity(vp);
    }


    // --------------------------------------------------------
    // DEACTIVATE
    // --------------------------------------------------------
    @Override
    public void deactivate(Long id) {
        guard.require(guard.has(guard.me(), "vehicle_price.deactivate"));

        VehiclePrice vp = vehiclePriceRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_PRICE_NOT_FOUND));

        if (Boolean.FALSE.equals(vp.getIsActive())) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Bảng giá đã bị vô hiệu hoá.");
        }

        vp.setIsActive(false);
        vehiclePriceRepo.save(vp);
    }

    // --------------------------------------------------------
    // ACTIVATE
    // --------------------------------------------------------
    @Override
    public void activate(Long id) {
        guard.require(guard.has(guard.me(), "vehicle_price.activate"));

        VehiclePrice vp = vehiclePriceRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_PRICE_NOT_FOUND));

        if (Boolean.TRUE.equals(vp.getIsActive())) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Bảng giá đang hoạt động.");
        }

        vp.setIsActive(true);
        vehiclePriceRepo.save(vp);
    }
}
