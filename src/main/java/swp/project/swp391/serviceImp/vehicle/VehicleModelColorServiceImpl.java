package swp.project.swp391.serviceImp.vehicle;

// ✅ Đúng
import org.springframework.transaction.annotation.Transactional;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.*;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.*;
import swp.project.swp391.request.vehicle.VehicleModelColorUpdateRequest;
import swp.project.swp391.response.vehicle.VehicleModelColorResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.vehicle.VehicleModelColorService;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class VehicleModelColorServiceImpl implements VehicleModelColorService {

    private final VehicleModelRepository modelRepo;
    private final ColorRepository colorRepo;
    private final VehicleModelColorRepository modelColorRepo;
    private final VehiclePriceRepository priceRepo; // ✅ thêm repo này để tính giá
    private final RbacGuard guard;
    private final UserRepository userRepo;


    // ---------------- ASSIGN ----------------
    @Override
    @Transactional
    public VehicleModelColorResponse assignColorToModel(Long modelId, Long colorId, BigDecimal priceAdjustment, User currentUser) {
        guard.require(guard.has(currentUser, "vehicleModelColor.create"));

        VehicleModel model = modelRepo.findById(modelId)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_MODEL_NOT_FOUND));

        Color color = colorRepo.findById(colorId)
                .orElseThrow(() -> new BaseException(ErrorHandler.COLOR_NOT_FOUND));

        if (modelColorRepo.existsByVehicleModelAndColor(model, color)) {
            throw new BaseException(ErrorHandler.VEHICLE_COLOR_ALREADY_EXISTS);
        }

        VehicleModelColor entity = VehicleModelColor.builder()
                .vehicleModel(model)
                .color(color)
                .priceAdjustment(priceAdjustment != null ? priceAdjustment : BigDecimal.ZERO)
                .isActive(true)
                .build();

        modelColorRepo.save(entity);
        return map(entity);
    }

    // ---------------- GET COLORS BY MODEL ----------------
    @Override
    @Transactional(readOnly = true)
    public List<VehicleModelColorResponse> getColorsByModel(Long modelId) {
        if (!modelRepo.existsById(modelId)) {
            throw new BaseException(ErrorHandler.VEHICLE_MODEL_NOT_FOUND);
        }

        // ✅ Lấy user hiện tại từ token
        User current = guard.me();

        // ✅ Load lại user có fetch Dealer và Level
        User dbUser = userRepo.findByIdWithDealer(current.getId())
                .orElseThrow(() -> new BaseException(ErrorHandler.USER_NOT_FOUND));

        // ✅ Lấy DealerLevel nếu là dealer
        DealerLevel level = null;
        if (isDealer(dbUser)) {
            Dealer dealer = dbUser.getDealer();
            if (dealer == null)
                throw new BaseException(ErrorHandler.DEALER_NOT_FOUND);
            level = dealer.getLevel(); // Không lỗi LazyInitializationException nữa
        }

        LocalDate today = LocalDate.now();

        // ✅ Lấy danh sách màu của model
        List<VehicleModelColor> list = modelColorRepo.findByVehicleModelId(modelId);

        DealerLevel finalLevel = level;
        return list.stream()
                .map(vmc -> {
                    BigDecimal effectivePrice;

                    // 🟢 Nếu là dealer → lấy giá theo DealerLevel
                    if (finalLevel != null) {
                        effectivePrice = priceRepo.findActiveByVehicleModelColorAndDealerLevel(vmc, finalLevel, today)
                                .map(VehiclePrice::getWholesalePrice)
                                .orElseGet(() -> vmc.getVehicleModel().getManufacturerPrice()
                                        .add(vmc.getPriceAdjustment() != null ? vmc.getPriceAdjustment() : BigDecimal.ZERO));
                    }
                    // 🔵 Nếu là admin/staff → chỉ lấy base + chênh lệch màu
                    else {
                        BigDecimal base = vmc.getVehicleModel().getManufacturerPrice();
                        BigDecimal adj = vmc.getPriceAdjustment() != null ? vmc.getPriceAdjustment() : BigDecimal.ZERO;
                        effectivePrice = base.add(adj);
                    }

                    return map(vmc, effectivePrice);
                })
                .collect(Collectors.toList());
    }



    // ---------------- UPDATE VEHICLEMODELCOLOR ----------------
    @Transactional
    @Override
    public VehicleModelColorResponse updateColorInfo(Long modelId, Long colorId,
                                                     VehicleModelColorUpdateRequest req,
                                                     User currentUser) {
        guard.require(guard.has(currentUser, "vehicleModelColor.update"));

        VehicleModelColor vmc = modelColorRepo.findByVehicleModelIdAndColorId(modelId, colorId)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_COLOR_NOT_FOUND));

        if (req.getPriceAdjustment() != null)
            vmc.setPriceAdjustment(req.getPriceAdjustment());

        if (req.getImageUrl() != null && !req.getImageUrl().isBlank())
            vmc.setImageUrl(req.getImageUrl());

        modelColorRepo.save(vmc);
        return map(vmc);
    }


    // ---------------- UNASSIGN ----------------
    @Override
    @Transactional
    public void unassignColor(Long modelId, Long colorId, User currentUser) {
        guard.require(guard.has(currentUser, "vehicleModelColor.delete"));

        VehicleModelColor vmc = modelColorRepo.findByVehicleModelIdAndColorId(modelId, colorId)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_COLOR_NOT_FOUND));

        modelColorRepo.delete(vmc);
    }

    // ---------------- MAPPER ----------------
    private VehicleModelColorResponse map(VehicleModelColor entity) {
        return map(entity, null);
    }

    private VehicleModelColorResponse map(VehicleModelColor entity, BigDecimal effectivePrice) {
        return VehicleModelColorResponse.builder()
                .id(entity.getId())
                .vehicleModelId(entity.getVehicleModel().getId())
                .colorId(entity.getColor().getId())
                .colorName(entity.getColor().getColorName())
                .hexCode(entity.getColor().getHexCode())
                .priceAdjustment(entity.getPriceAdjustment())
                .effectivePrice(effectivePrice) // ✅ thêm giá thực tế
                .imageUrl(entity.getImageUrl())
                .isActive(entity.getIsActive())
                .build();
    }

    // ---------------- Helper ----------------
    private boolean isDealer(User user) {
        return user.getRoles().stream()
                .map(Role::getName)
                .anyMatch(r -> r.equalsIgnoreCase("DEALER_MANAGER") || r.equalsIgnoreCase("DEALER_STAFF"));
    }
}
