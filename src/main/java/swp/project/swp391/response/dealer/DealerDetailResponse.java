package swp.project.swp391.response.dealer;

import lombok.Data;
import swp.project.swp391.entity.Dealer;

import java.math.BigDecimal;
import java.time.LocalDateTime;

@Data
public class DealerDetailResponse {

    private Long id;
    private String name;
    private String code;
    private String address;
    private String phoneNumber;
    private String email;

    private Boolean isActive;
    private String region;

    private Long dealerLevelId;
    private String dealerLevelName;

    private BigDecimal currentDebt;
    private BigDecimal availableCredit;

    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    private Integer maxOrderQuantity;

    // ⭐ Mapper đặt bên trong response (Static factory method)
    public static DealerDetailResponse fromEntity(Dealer d) {
        DealerDetailResponse r = new DealerDetailResponse();

        r.setId(d.getId());
        r.setName(d.getName());
        r.setCode(d.getCode());
        r.setAddress(d.getAddress());
        r.setPhoneNumber(d.getPhoneNumber());
        r.setEmail(d.getEmail());
        r.setIsActive(d.getIsActive());
        r.setRegion(d.getRegion().name());

        r.setDealerLevelId(d.getLevel().getId());
        r.setDealerLevelName(d.getLevel().getLevelName());

        r.setCurrentDebt(d.getCurrentDebt());
        r.setAvailableCredit(d.getAvailableCredit());

        r.setCreatedAt(d.getCreatedAt());
        r.setUpdatedAt(d.getUpdatedAt());

        r.setMaxOrderQuantity(d.getLevel().getMaxOrderQuantity());

        return r;
    }
}
