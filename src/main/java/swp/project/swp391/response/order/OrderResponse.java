package swp.project.swp391.response.order;

import lombok.*;
import swp.project.swp391.entity.Order;
import swp.project.swp391.entity.VehicleInstance;
import swp.project.swp391.entity.VehicleModelColor;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
import java.util.ArrayList;
import java.util.List;
import java.util.stream.Collectors;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class OrderResponse {

    private Long id;
    private String orderCode;
    private String status;

    // ===== Thông tin tài chính =====
    private BigDecimal totalAmount;        // Giá 1 chiếc xe
    private BigDecimal depositAmount;      // Tiền cọc (nếu trả góp)
    private BigDecimal paidAmount;         // Số tiền dealer đã thanh toán thực tế (trả thẳng hoặc trả cọc)
    private BigDecimal remainingAmount;    // Số tiền còn lại
    private BigDecimal paymentProgress;    // % đã thanh toán

    // ===== Trạng thái thanh toán =====
    private Boolean isInstallment;
    private LocalDate fullPaymentDate;

    // ===== Thông tin đơn hàng =====
    private LocalDate orderDate;
    private String notes;
    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    // ===== Dealer =====
    private DealerInfo dealer;

    // ===== Người tạo đơn =====
    private UserInfo createdBy;

    // ===== Xe yêu cầu & xe đã gắn =====
    private VehicleRequestInfo requestedModelColor;
    private VehicleAssignedInfo assignedVehicle;

    private List<InstallmentPlanInfo> installmentPlans;

    // ========================== INNER DTO ==========================

    @Getter @Setter @AllArgsConstructor @NoArgsConstructor @Builder
    public static class DealerInfo {
        private Long id;
        private String name;
        private String code;
        private String levelName;
        private BigDecimal currentDebt;
        private BigDecimal availableCredit;
    }

    @Getter @Setter @AllArgsConstructor @NoArgsConstructor @Builder
    public static class UserInfo {
        private String username;
        private String fullName;
    }

    @Getter @Setter @AllArgsConstructor @NoArgsConstructor @Builder
    public static class VehicleRequestInfo {
        private Long id;
        private String modelName;
        private String colorName;
    }

    @Getter @Setter @AllArgsConstructor @NoArgsConstructor @Builder
    public static class VehicleAssignedInfo {
        private Long id;
        private String vin;
        private String engineNumber;
        private String status;
        private String modelName;
        private String colorName;
    }

    @Getter @Setter @AllArgsConstructor @NoArgsConstructor @Builder
    public static class InstallmentPlanInfo {
        private Long id;
        private Integer installmentNumber;
        private BigDecimal installmentAmount;
        private LocalDate dueDate;
        private BigDecimal paidAmount;
        private LocalDate paidDate;
        private String status;
        private BigDecimal remainingAmount;
        private Boolean isOverdue;
        private Long overdueDays;
        private String notes;
    }

    // ========================== MAPPER ==========================

    public static OrderResponse fromEntity(Order order) {

        return OrderResponse.builder()
                .id(order.getId())
                .orderCode(order.getOrderCode())
                .status(order.getStatus().name())

                // MONEY
                .totalAmount(order.getTotalAmount())
                .depositAmount(order.getDepositAmount())
                .paidAmount(order.getPaidAmount())
                .remainingAmount(order.getRemainingAmount())
                .paymentProgress(order.getPaymentProgress())
                .isInstallment(order.getIsInstallment())
                .fullPaymentDate(order.getFullPaymentDate())

                // ORDER META
                .orderDate(order.getOrderDate())
                .notes(order.getNotes())
                .createdAt(order.getCreatedAt())
                .updatedAt(order.getUpdatedAt())

                // DEALER
                .dealer(order.getBuyerDealer() != null ? DealerInfo.builder()
                        .id(order.getBuyerDealer().getId())
                        .name(order.getBuyerDealer().getName())
                        .code(order.getBuyerDealer().getCode())
                        .levelName(order.getBuyerDealer().getLevel().getLevelName())
                        .currentDebt(order.getBuyerDealer().getCurrentDebt())
                        .availableCredit(order.getBuyerDealer().getAvailableCredit())
                        .build() : null)

                // USER
                .createdBy(order.getCreatedBy() != null ? UserInfo.builder()
                        .username(order.getCreatedBy().getUsername())
                        .fullName(order.getCreatedBy().getFullName())
                        .build() : null)

                // REQUESTED MODEL COLOR
                .requestedModelColor(order.getVehicleModelColor() != null ?
                        VehicleRequestInfo.builder()
                                .id(order.getVehicleModelColor().getId())
                                .modelName(order.getVehicleModelColor().getVehicleModel().getName())
                                .colorName(order.getVehicleModelColor().getColor().getColorName())
                                .build() : null)

                // ASSIGNED VEHICLE
                .assignedVehicle(order.getAssignedVehicle() != null ?
                        mapVehicle(order.getAssignedVehicle()) : null)

                //  INSTALLMENT PLANS
                .installmentPlans(mapInstallmentPlans(order))

                .build();
    }

    private static VehicleAssignedInfo mapVehicle(VehicleInstance v) {
        return VehicleAssignedInfo.builder()
                .id(v.getId())
                .vin(v.getVin())
                .engineNumber(v.getEngineNumber())
                .status(v.getStatus().name())
                .modelName(v.getVehicleModel().getName())
                .colorName(v.getVehicleModelColor().getColor().getColorName())
                .build();
    }

    private static List<InstallmentPlanInfo> mapInstallmentPlans(Order order) {
        // Trả thẳng → trả về list rỗng
        if (!Boolean.TRUE.equals(order.getIsInstallment())) {
            return new ArrayList<>();
        }

        // Trả góp nhưng chưa có plans → trả về list rỗng
        if (order.getInstallmentPlans() == null || order.getInstallmentPlans().isEmpty()) {
            return new ArrayList<>();
        }

        // Map từ entity sang DTO
        return order.getInstallmentPlans().stream()
                .sorted((p1, p2) -> p1.getInstallmentNumber().compareTo(p2.getInstallmentNumber()))
                .map(plan -> InstallmentPlanInfo.builder()
                        .id(plan.getId())
                        .installmentNumber(plan.getInstallmentNumber())
                        .installmentAmount(plan.getInstallmentAmount())
                        .dueDate(plan.getDueDate())
                        .paidAmount(plan.getPaidAmount())
                        .paidDate(plan.getPaidDate())
                        .status(plan.getStatus() != null ? plan.getStatus().name() : null)
                        .remainingAmount(plan.getRemainingAmount())
                        .isOverdue(plan.isOverdue())
                        .overdueDays(plan.getOverdueDays())
                        .notes(plan.getNotes())
                        .build())
                .collect(Collectors.toList());
    }
}
