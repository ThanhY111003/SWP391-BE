package swp.project.swp391.serviceImp.report;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.*;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.*;
import swp.project.swp391.response.report.*;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.report.AdminReportService;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class AdminReportServiceImpl implements AdminReportService {

    private final OrderRepository orderRepo;
    private final OrderDetailRepository orderDetailRepo;
    private final InventoryRepository inventoryRepo;
    private final RbacGuard guard;

    @Override
    public List<SalesTrendResponse> getSalesTrend(String groupBy, LocalDate fromDate, LocalDate toDate) {
        User current = guard.me();
        guard.require(guard.has(current, "admin_report.read"));
        ensureAdmin(current);

        if (fromDate == null) fromDate = LocalDate.of(2000, 1, 1);
        if (toDate == null) toDate = LocalDate.now();

        var orders = orderRepo.findByOrderDateBetween(fromDate, toDate).stream()
                .filter(o -> o.getStatus() == Order.OrderStatus.CONFIRMED
                        || o.getStatus() == Order.OrderStatus.COMPLETED)
                .toList();

        if (orders.isEmpty()) {
            return List.of(SalesTrendResponse.builder()
                    .period("NO_DATA").soldCount(0L).totalRevenue(BigDecimal.ZERO).build());
        }

        if (groupBy == null || groupBy.equalsIgnoreCase("all")) {
            BigDecimal totalRevenue = orders.stream()
                    .map(Order::getTotalAmount)
                    .reduce(BigDecimal.ZERO, BigDecimal::add);

            return List.of(SalesTrendResponse.builder()
                    .period("ALL_TIME")
                    .soldCount((long) orders.size())
                    .totalRevenue(totalRevenue)
                    .build());
        }

        Map<String, List<Order>> grouped = switch (groupBy.toLowerCase()) {
            case "day" -> orders.stream()
                    .collect(Collectors.groupingBy(o -> o.getOrderDate().toString()));
            case "month" -> orders.stream()
                    .collect(Collectors.groupingBy(o -> o.getOrderDate().getYear() + "-" + String.format("%02d", o.getOrderDate().getMonthValue())));
            case "year" -> orders.stream()
                    .collect(Collectors.groupingBy(o -> String.valueOf(o.getOrderDate().getYear())));
            default -> throw new BaseException(ErrorHandler.INVALID_REQUEST, "groupBy chỉ chấp nhận: day, month, year, all");
        };

        return grouped.entrySet().stream()
                .sorted(Map.Entry.comparingByKey())
                .map(entry -> {
                    BigDecimal totalRevenue = entry.getValue().stream()
                            .map(Order::getTotalAmount)
                            .reduce(BigDecimal.ZERO, BigDecimal::add);

                    return SalesTrendResponse.builder()
                            .period(entry.getKey())
                            .soldCount((long) entry.getValue().size())
                            .totalRevenue(totalRevenue)
                            .build();
                })
                .toList();
    }

    @Override
    public List<TopModelResponse> getTopModels(int limit, LocalDate fromDate, LocalDate toDate) {
        User current = guard.me();
        guard.require(guard.has(current, "admin_report.read"));
        ensureAdmin(current);

        if (fromDate == null) fromDate = LocalDate.of(2000, 1, 1);
        if (toDate == null) toDate = LocalDate.now();

        var details = orderDetailRepo.findByOrder_OrderDateBetween(fromDate, toDate);
        var grouped = details.stream()
                .collect(Collectors.groupingBy(OrderDetail::getVehicleModelColor));

        return grouped.entrySet().stream()
                .map(entry -> {
                    var vmc = entry.getKey();
                    long soldCount = entry.getValue().stream().mapToLong(OrderDetail::getQuantity).sum();
                    BigDecimal totalRevenue = entry.getValue().stream()
                            .map(OrderDetail::getTotalPrice)
                            .reduce(BigDecimal.ZERO, BigDecimal::add);

                    return TopModelResponse.builder()
                            .modelName(vmc.getVehicleModel().getName())
                            .colorName(vmc.getColor().getColorName())
                            .soldCount(soldCount)
                            .totalRevenue(totalRevenue)
                            .build();
                })
                .sorted(Comparator.comparing(TopModelResponse::getSoldCount).reversed())
                .limit(limit)
                .toList();
    }

    @Override
    public List<TopDealerResponse> getTopDealers(int limit, LocalDate fromDate, LocalDate toDate) {
        User current = guard.me();
        guard.require(guard.has(current, "admin_report.read"));
        ensureAdmin(current);

        if (fromDate == null) fromDate = LocalDate.of(2000, 1, 1);
        if (toDate == null) toDate = LocalDate.now();

        var orders = orderRepo.findByOrderDateBetween(fromDate, toDate).stream()
                .filter(o -> o.getStatus() == Order.OrderStatus.CONFIRMED
                        || o.getStatus() == Order.OrderStatus.COMPLETED)
                .toList();

        var grouped = orders.stream().collect(Collectors.groupingBy(Order::getBuyerDealer));

        return grouped.entrySet().stream()
                .map(entry -> {
                    var dealer = entry.getKey();
                    var dealerOrders = entry.getValue();

                    long totalOrders = dealerOrders.size();
                    long totalVehicles = dealerOrders.stream()
                            .flatMap(o -> o.getOrderDetails().stream())
                            .mapToLong(OrderDetail::getQuantity).sum();

                    BigDecimal totalAmount = dealerOrders.stream()
                            .map(Order::getTotalAmount)
                            .reduce(BigDecimal.ZERO, BigDecimal::add);

                    return TopDealerResponse.builder()
                            .dealerId(dealer.getId())
                            .dealerName(dealer.getName())
                            .totalOrders(totalOrders)
                            .totalVehicles(totalVehicles)
                            .totalAmount(totalAmount)
                            .build();
                })
                .sorted(Comparator.comparing(TopDealerResponse::getTotalAmount).reversed())
                .limit(limit)
                .toList();
    }

    @Override
    public List<DealerPerformanceResponse> getDealerPerformance(LocalDate fromDate, LocalDate toDate) {
        User current = guard.me();
        guard.require(guard.has(current, "admin_report.read"));
        ensureAdmin(current);

        if (fromDate == null) fromDate = LocalDate.of(2000, 1, 1);
        if (toDate == null) toDate = LocalDate.now();

        var orders = orderRepo.findByOrderDateBetween(fromDate, toDate);
        var grouped = orders.stream().collect(Collectors.groupingBy(Order::getBuyerDealer));

        return grouped.entrySet().stream()
                .map(entry -> {
                    var dealer = entry.getKey();
                    var list = entry.getValue();

                    long total = list.size();
                    long completed = list.stream().filter(o -> o.getStatus() == Order.OrderStatus.COMPLETED).count();
                    long cancelled = list.stream().filter(o -> o.getStatus() == Order.OrderStatus.CANCELLED).count();
                    BigDecimal revenue = list.stream()
                            .map(Order::getTotalAmount)
                            .reduce(BigDecimal.ZERO, BigDecimal::add);

                    double successRate = total == 0 ? 0 : (completed * 100.0 / total);

                    return DealerPerformanceResponse.builder()
                            .dealerName(dealer.getName())
                            .totalOrders(total)
                            .completedOrders(completed)
                            .cancelledOrders(cancelled)
                            .revenue(revenue)
                            .successRate(successRate)
                            .build();
                })
                .sorted(Comparator.comparing(DealerPerformanceResponse::getRevenue).reversed())
                .toList();
    }

    @Override
    public List<InventorySummaryResponse> getInventorySummary() {
        User current = guard.me();
        guard.require(guard.has(current, "admin_report.read"));
        ensureAdmin(current);

        var inventories = inventoryRepo.findAll();
        var grouped = inventories.stream()
                .collect(Collectors.groupingBy(Inventory::getVehicleModelColor));

        return grouped.entrySet().stream()
                .map(entry -> {
                    var vmc = entry.getKey();
                    var list = entry.getValue();

                    int total = list.stream().mapToInt(Inventory::getTotalQuantity).sum();
                    int available = list.stream().mapToInt(Inventory::getAvailableQuantity).sum();
                    int reserved = list.stream().mapToInt(Inventory::getReservedQuantity).sum();

                    return InventorySummaryResponse.builder()
                            .modelName(vmc.getVehicleModel().getName())
                            .colorName(vmc.getColor().getColorName())
                            .total(total)
                            .available(available)
                            .reserved(reserved)
                            .build();
                })
                .toList();
    }

    private void ensureAdmin(User current) {
        boolean isAdmin = current.getRoles().stream()
                .anyMatch(r -> r.getName().equalsIgnoreCase("ADMIN"));
        if (!isAdmin) {
            throw new BaseException(ErrorHandler.FORBIDDEN, "Chỉ ADMIN được phép xem báo cáo này.");
        }
    }
}
