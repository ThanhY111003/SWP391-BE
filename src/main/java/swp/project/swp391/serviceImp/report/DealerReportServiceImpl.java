package swp.project.swp391.serviceImp.report;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.CustomerVehicle;
import swp.project.swp391.entity.User;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.CustomerVehicleRepository;
import swp.project.swp391.repository.InventoryRepository;
import swp.project.swp391.response.report.InventoryStatusResponse;
import swp.project.swp391.response.report.SalesTrendResponse;
import swp.project.swp391.response.report.TopCustomerResponse;
import swp.project.swp391.response.report.TopModelResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.report.DealerReportService;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class DealerReportServiceImpl implements DealerReportService {

    private final CustomerVehicleRepository customerVehicleRepo;
    private final RbacGuard guard;
    private final InventoryRepository inventoryRepo;

    @Override
    public List<SalesTrendResponse> getSalesTrend(String groupBy, LocalDate fromDate, LocalDate toDate) {
        User current = guard.me();

        // ✅ Chỉ user có quyền "report.read" mới được vào
        guard.require(guard.has(current, "dealer_report.read"));

        // ✅ Chỉ cho phép role DEALER_MANAGER (không phải staff)
        boolean isDealerManager = current.getRoles().stream()
                .anyMatch(r -> r.getName().equalsIgnoreCase("DEALER_MANAGER"));

        if (!isDealerManager) {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Chỉ Dealer Manager mới được phép xem báo cáo này.");
        }

        // ✅ Phải có dealerId hợp lệ
        if (current.getDealer() == null) {
            throw new BaseException(ErrorHandler.DEALER_NOT_FOUND);
        }

        Long dealerId = current.getDealer().getId();

        // ✅ Nếu không truyền from/toDate → toàn thời gian
        if (fromDate == null) fromDate = LocalDate.of(2000, 1, 1);
        if (toDate == null) toDate = LocalDate.now();

        // ✅ Lọc theo dealerId
        List<CustomerVehicle> sales =
                customerVehicleRepo.findBySoldByDealer_IdAndSaleDateBetween(dealerId, fromDate, toDate);

        if (sales.isEmpty()) {
            return List.of(SalesTrendResponse.builder()
                    .period("NO_DATA")
                    .soldCount(0L)
                    .totalRevenue(BigDecimal.ZERO)
                    .build());
        }

        // ✅ Nếu không groupBy hoặc groupBy=all → tổng hết
        if (groupBy == null || groupBy.equalsIgnoreCase("all")) {
            BigDecimal totalRevenue = sales.stream()
                    .map(CustomerVehicle::getSalePrice)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);

            return List.of(SalesTrendResponse.builder()
                    .period("ALL_TIME")
                    .soldCount((long) sales.size())
                    .totalRevenue(totalRevenue)
                    .build());
        }

        // ✅ Nhóm theo day / month / year
        Map<String, List<CustomerVehicle>> grouped = switch (groupBy.toLowerCase()) {
            case "day" -> sales.stream()
                    .collect(Collectors.groupingBy(s -> s.getSaleDate().toString()));
            case "month" -> sales.stream()
                    .collect(Collectors.groupingBy(s -> s.getSaleDate().getYear()
                            + "-" + String.format("%02d", s.getSaleDate().getMonthValue())));
            case "year" -> sales.stream()
                    .collect(Collectors.groupingBy(s -> String.valueOf(s.getSaleDate().getYear())));
            default -> throw new BaseException(ErrorHandler.INVALID_REQUEST, "groupBy chỉ chấp nhận: day, month, year, all");
        };

        // ✅ Trả về danh sách theo thứ tự thời gian
        return grouped.entrySet().stream()
                .sorted(Map.Entry.comparingByKey())
                .map(entry -> {
                    BigDecimal totalRevenue = entry.getValue().stream()
                            .map(CustomerVehicle::getSalePrice)
                            .reduce(BigDecimal.ZERO, BigDecimal::add);

                    return SalesTrendResponse.builder()
                            .period(entry.getKey())
                            .soldCount((long) entry.getValue().size())
                            .totalRevenue(totalRevenue)
                            .build();
                })
                .collect(Collectors.toList());
    }
    // --------------------------------------------------------
    // TOP MODELS SOLD (theo vehicleModelColor)
    // --------------------------------------------------------
    @Override
    public List<TopModelResponse> getTopModels(int limit, LocalDate fromDate, LocalDate toDate) {
        var current = guard.me();
        guard.require(guard.has(current, "dealer_report.read"));

        if (!hasDealerManagerRole(current)) {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Chỉ Dealer Manager mới được xem báo cáo này.");
        }

        var dealerId = current.getDealer().getId();
        if (fromDate == null) fromDate = LocalDate.of(2000, 1, 1);
        if (toDate == null) toDate = LocalDate.now();

        var sales = customerVehicleRepo.findBySoldByDealer_IdAndSaleDateBetween(dealerId, fromDate, toDate);

        // Group theo VehicleModelColor
        var grouped = sales.stream()
                .collect(Collectors.groupingBy(v -> v.getVehicleInstance().getVehicleModelColor()));

        return grouped.entrySet().stream()
                .map(entry -> {
                    var color = entry.getKey();
                    var soldCount = (long) entry.getValue().size();
                    var totalRevenue = entry.getValue().stream()
                            .map(CustomerVehicle::getSalePrice)
                            .reduce(BigDecimal.ZERO, BigDecimal::add);

                    return TopModelResponse.builder()
                            .modelName(color.getVehicleModel().getName())
                            .colorName(color.getColor().getColorName())
                            .soldCount(soldCount)
                            .totalRevenue(totalRevenue)
                            .build();
                })
                .sorted(Comparator.comparing(TopModelResponse::getSoldCount).reversed())
                .limit(limit)
                .toList();
    }

    // --------------------------------------------------------
    // TOP CUSTOMERS (chi tiêu cao nhất)
    // --------------------------------------------------------
    @Override
    public List<TopCustomerResponse> getTopCustomers(int limit, LocalDate fromDate, LocalDate toDate) {
        var current = guard.me();
        guard.require(guard.has(current, "dealer_report.read"));

        if (!hasDealerManagerRole(current)) {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Chỉ Dealer Manager mới được xem báo cáo này.");
        }

        var dealerId = current.getDealer().getId();
        if (fromDate == null) fromDate = LocalDate.of(2000, 1, 1);
        if (toDate == null) toDate = LocalDate.now();

        var sales = customerVehicleRepo.findBySoldByDealer_IdAndSaleDateBetween(dealerId, fromDate, toDate);

        var grouped = sales.stream()
                .collect(Collectors.groupingBy(CustomerVehicle::getCustomer));

        return grouped.entrySet().stream()
                .map(entry -> {
                    var c = entry.getKey();
                    var totalSpent = entry.getValue().stream()
                            .map(CustomerVehicle::getSalePrice)
                            .reduce(BigDecimal.ZERO, BigDecimal::add);

                    return TopCustomerResponse.builder()
                            .customerId(c.getId())
                            .customerName(c.getFullName())
                            .totalVehicles((long) entry.getValue().size())
                            .totalSpent(totalSpent)
                            .lastPurchase(entry.getValue().stream()
                                    .map(CustomerVehicle::getSaleDate)
                                    .max(LocalDate::compareTo)
                                    .orElse(null))
                            .build();
                })
                .sorted(Comparator.comparing(TopCustomerResponse::getTotalSpent).reversed())
                .limit(limit)
                .toList();
    }

    // --------------------------------------------------------
    // INVENTORY STATUS (tồn kho vs đã bán)
    // --------------------------------------------------------
    @Override
    public List<InventoryStatusResponse> getInventoryStatus() {
        var current = guard.me();
        guard.require(guard.has(current, "dealer_report.read"));

        if (!hasDealerManagerRole(current)) {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Chỉ Dealer Manager mới được xem báo cáo này.");
        }

        var dealerId = current.getDealer().getId();
        var inventories = inventoryRepo.findByDealer_Id(dealerId);

        return inventories.stream()
                .map(inv -> {
                    var soldCount = customerVehicleRepo.countByVehicleInstance_VehicleModelColorAndSoldByDealer_Id(
                            inv.getVehicleModelColor(), dealerId);

                    return InventoryStatusResponse.builder()
                            .modelName(inv.getVehicleModelColor().getVehicleModel().getName())
                            .colorName(inv.getVehicleModelColor().getColor().getColorName())
                            .totalStock(inv.getTotalQuantity())
                            .available(inv.getAvailableQuantity())
                            .reserved(inv.getReservedQuantity())
                            .soldCount(soldCount)
                            .build();
                })
                .toList();
    }

    // --------------------------------------------------------
    // Helper
    // --------------------------------------------------------
    private boolean hasDealerManagerRole(User current) {
        return current.getRoles().stream()
                .anyMatch(r -> r.getName().equalsIgnoreCase("DEALER_MANAGER"));
    }
}
