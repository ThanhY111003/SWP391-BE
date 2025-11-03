package swp.project.swp391.response.order;

import lombok.*;
import swp.project.swp391.entity.Order;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.LocalDateTime;
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

    // Thông tin tài chính
    private BigDecimal totalAmount;        // Tổng tiền đơn hàng
    private BigDecimal depositAmount;      // Tiền cọc (nếu trả góp)
    private BigDecimal paidAmount;         // Tổng tiền đã trả (cọc + trả góp)
    private BigDecimal remainingAmount;    // Tổng tiền còn lại phải trả
    private BigDecimal paymentProgress;    // % đã thanh toán

    // Thông tin thanh toán
    private Boolean isInstallment;
    private LocalDate fullPaymentDate;

    // Thông tin đơn hàng
    private LocalDate orderDate;
    private String notes;

    // ===== THÔNG TIN DEALER =====
    private DealerInfo dealer;

    // ===== THÔNG TIN NGƯỜI TẠO ĐƠN =====
    private UserInfo createdBy;

    // Chi tiết đơn hàng
    private List<OrderDetailInfo> orderDetails;

    // Thông tin trả góp (nếu có)
    private List<InstallmentPlanInfo> installmentPlans;

    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

    // ===== INNER CLASSES =====

    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class DealerInfo {
        private Long id;
        private String name;
        private String code;
        private String levelName;
        private BigDecimal currentDebt;
        private BigDecimal availableCredit;
    }

    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class UserInfo {
        private String username;
        private String fullName;
    }

    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class OrderDetailInfo {
        private Long id;
        private String vehicleModelName;
        private String vehicleColorName;
        private Integer quantity;
        private BigDecimal unitPrice;
        private BigDecimal totalPrice;
    }

    @Getter
    @Setter
    @NoArgsConstructor
    @AllArgsConstructor
    @Builder
    public static class InstallmentPlanInfo {
        private Integer installmentNumber;
        private BigDecimal installmentAmount;
        private LocalDate dueDate;
        private String status;
        private Boolean isOverdue;
    }

    /**
     * Chuyển đổi từ Entity sang DTO (với đầy đủ thông tin)
     */
    public static OrderResponse fromEntity(Order order) {
        if (order == null) {
            return null;
        }

        return OrderResponse.builder()
                .id(order.getId())
                .orderCode(order.getOrderCode())
                .status(order.getStatus().name())
                .totalAmount(order.getTotalAmount())
                .depositAmount(order.getDepositAmount())
                .paidAmount(order.getPaidAmount())
                .remainingAmount(order.getRemainingAmount())
                .paymentProgress(order.getPaymentProgress())
                .isInstallment(order.getIsInstallment())
                .fullPaymentDate(order.getFullPaymentDate())
                .orderDate(order.getOrderDate())
                .notes(order.getNotes())

                // Thông tin Dealer
                .dealer(order.getBuyerDealer() != null ? DealerInfo.builder()
                        .id(order.getBuyerDealer().getId())
                        .name(order.getBuyerDealer().getName())
                        .code(order.getBuyerDealer().getCode())
                        .levelName(order.getBuyerDealer().getLevel() != null ?
                                order.getBuyerDealer().getLevel().getLevelName() : null)
                        .currentDebt(order.getBuyerDealer().getCurrentDebt())
                        .availableCredit(order.getBuyerDealer().getAvailableCredit())
                        .build() : null)

                // Thông tin người tạo đơn
                .createdBy(order.getCreatedBy() != null ? UserInfo.builder()
                        .username(order.getCreatedBy().getUsername())
                        .fullName(order.getCreatedBy().getFullName())
                        .build() : null)

                // Chi tiết đơn hàng
                .orderDetails(order.getOrderDetails() != null ?
                        order.getOrderDetails().stream()
                                .map(detail -> OrderDetailInfo.builder()
                                        .id(detail.getId())
                                        .vehicleModelName(detail.getVehicleModel().getName())
                                        .vehicleColorName(detail.getVehicleModelColor().getColor().getColorName())
                                        .quantity(detail.getQuantity())
                                        .unitPrice(detail.getUnitPrice())
                                        .totalPrice(detail.getTotalPrice())
                                        .build())
                                .collect(Collectors.toList()) : null)

                // Kế hoạch trả góp
                .installmentPlans(order.getInstallmentPlans() != null ?
                        order.getInstallmentPlans().stream()
                                .map(plan -> InstallmentPlanInfo.builder()
                                        .installmentNumber(plan.getInstallmentNumber())
                                        .installmentAmount(plan.getInstallmentAmount())
                                        .dueDate(plan.getDueDate())
                                        .status(plan.getStatus().name())
                                        .isOverdue(plan.isOverdue())
                                        .build())
                                .collect(Collectors.toList()) : null)

                .createdAt(order.getCreatedAt())
                .updatedAt(order.getUpdatedAt())
                .build();
    }
}