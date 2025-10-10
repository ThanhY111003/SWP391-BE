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
    private BigDecimal totalAmount;
    private Boolean isInstallment;

    // Thông tin thanh toán
    private BigDecimal paidAmount;
    private BigDecimal remainingAmount;
    private BigDecimal paymentProgress;
    private LocalDate depositPaidDate;
    private LocalDate fullPaymentDate;
    private String paymentNotes;

    // Thông tin đơn hàng
    private LocalDate orderDate;
    private LocalDate expectedDeliveryDate;
    private LocalDate actualDeliveryDate;
    private String notes;

    // Thông tin dealer
    private DealerInfo buyerDealer;
    private String createdByUsername;

    // Chi tiết đơn hàng
    private List<OrderDetailInfo> orderDetails;

    // Kế hoạch trả góp (nếu có)
    private List<InstallmentPlanInfo> installmentPlans;

    private LocalDateTime createdAt;
    private LocalDateTime updatedAt;

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
        private Long id;
        private Integer installmentNumber;
        private BigDecimal installmentAmount;
        private LocalDate dueDate;
        private BigDecimal paidAmount;
        private BigDecimal remainingAmount;
        private LocalDate paidDate;
        private String status;
        private Boolean isOverdue;
        private Long overdueDays;
    }

    // Phương thức chuyển đổi từ Entity sang DTO
    public static OrderResponse fromEntity(Order order) {
        if (order == null) {
            return null;
        }

        return OrderResponse.builder()
                .id(order.getId())
                .orderCode(order.getOrderCode())
                .status(order.getStatus().name())
                .totalAmount(order.getTotalAmount())
                .isInstallment(order.getIsInstallment())
                .paidAmount(order.getPaidAmount())
                .remainingAmount(order.getRemainingAmount())
                .paymentProgress(order.getPaymentProgress())
                .depositPaidDate(order.getDepositPaidDate())
                .fullPaymentDate(order.getFullPaymentDate())
                .paymentNotes(order.getPaymentNotes())
                .orderDate(order.getOrderDate())
                .expectedDeliveryDate(order.getExpectedDeliveryDate())
                .actualDeliveryDate(order.getActualDeliveryDate())
                .notes(order.getNotes())
                .buyerDealer(DealerInfo.builder()
                        .id(order.getBuyerDealer().getId())
                        .name(order.getBuyerDealer().getName())
                        .code(order.getBuyerDealer().getCode())
                        .levelName(order.getBuyerDealer().getLevel().getLevelName())
                        .currentDebt(order.getBuyerDealer().getCurrentDebt())
                        .availableCredit(order.getBuyerDealer().getAvailableCredit())
                        .build())
                .createdByUsername(order.getCreatedBy().getUsername())
                .orderDetails(order.getOrderDetails() != null ?
                        order.getOrderDetails().stream()
                                .map(detail -> OrderDetailInfo.builder()
                                        .id(detail.getId())
                                        .vehicleModelName(detail.getVehicleModel().getName())
                                        .vehicleColorName(detail.getVehicleColor().getColor().getColorName())
                                        .quantity(detail.getQuantity())
                                        .unitPrice(detail.getUnitPrice())
                                        .totalPrice(detail.getTotalPrice())
                                        .build())
                                .collect(Collectors.toList()) : null)
                .installmentPlans(order.getInstallmentPlans() != null ?
                        order.getInstallmentPlans().stream()
                                .map(plan -> InstallmentPlanInfo.builder()
                                        .id(plan.getId())
                                        .installmentNumber(plan.getInstallmentNumber())
                                        .installmentAmount(plan.getInstallmentAmount())
                                        .dueDate(plan.getDueDate())
                                        .paidAmount(plan.getPaidAmount())
                                        .remainingAmount(plan.getRemainingAmount())
                                        .paidDate(plan.getPaidDate())
                                        .status(plan.getStatus().name())
                                        .isOverdue(plan.isOverdue())
                                        .overdueDays(plan.getOverdueDays())
                                        .build())
                                .collect(Collectors.toList()) : null)
                .createdAt(order.getCreatedAt())
                .updatedAt(order.getUpdatedAt())
                .build();
    }
}