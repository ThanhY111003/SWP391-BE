package swp.project.swp391.serviceImp.vehicle;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import org.springframework.web.multipart.MultipartFile;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.*;
import swp.project.swp391.entity.Color;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.*;
import swp.project.swp391.request.vehicle.*;
import swp.project.swp391.response.vehicle.CustomerVehicleResponse;
import swp.project.swp391.response.vehicle.VehicleImportResult;
import swp.project.swp391.response.vehicle.VehicleInstanceResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.vehicle.VehicleInstanceService;
// ✅ THÊM IMPORT CHO APACHE POI
import org.apache.poi.ss.usermodel.*;
import org.apache.poi.xssf.usermodel.XSSFWorkbook;

// ✅ THÊM IMPORT CHO PATTERN
import java.util.regex.Pattern;
import java.time.ZoneId;
import java.io.InputStream;
import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.*;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
public class VehicleInstanceServiceImpl implements VehicleInstanceService {

    private static final Pattern VIN_PATTERN = Pattern.compile("^[A-HJ-NPR-Z0-9]{17}$");
    private final VehicleInstanceRepository vehicleRepo;
    private final CustomerRepository customerRepo;
    private final VehicleModelColorRepository vehicleModelColorRepo;
    private final CustomerVehicleRepository customerVehicleRepo;
    private final InventoryRepository inventoryRepo;
    private final RbacGuard guard;
    private final DealerRepository dealerRepo;
    private final VehicleModelRepository vehicleModelRepo;
    private final ColorRepository colorRepo;

    // --------------------------------------------------------
    // GET ALL
    // --------------------------------------------------------
    @Override
    public List<VehicleInstanceResponse> getAll(Long dealerId, VehicleInstance.VehicleStatus status, Boolean activeOnly) {
        guard.require(guard.has(guard.me(), "vehicle.read_all"));
        User current = guard.me();

        List<VehicleInstance> list = vehicleRepo.findAll();

        // Nếu là Dealer role => chỉ thấy xe thuộc đại lý mình
        if (isDealerRole(current)) {
            Long myDealerId = getDealerId(current);
            list = list.stream()
                    .filter(v -> v.getCurrentDealer() != null && myDealerId.equals(v.getCurrentDealer().getId()))
                    .collect(Collectors.toList());
        } else if (dealerId != null) {
            // ADMIN / EVM_STAFF có thể lọc dealer bất kỳ
            list = list.stream()
                    .filter(v -> v.getCurrentDealer() != null && dealerId.equals(v.getCurrentDealer().getId()))
                    .collect(Collectors.toList());
        }

        if (status != null) {
            list = list.stream()
                    .filter(v -> v.getStatus() == status)
                    .collect(Collectors.toList());
        }

        if (Boolean.TRUE.equals(activeOnly)) {
            list = list.stream()
                    .filter(VehicleInstance::getIsActive)
                    .collect(Collectors.toList());
        }

        return list.stream()
                .map(VehicleInstanceResponse::fromEntity)
                .collect(Collectors.toList());
    }

    @Override
    @Transactional
    public VehicleInstanceResponse create(VehicleInstanceCreateRequest req) {
        guard.require(guard.has(guard.me(), "vehicle.create"));

        // --- AUTO-UPPERCASE ---
        String vin = req.getVin().trim().toUpperCase();
        String engine = req.getEngineNumber().trim().toUpperCase();

        // --- CHECK UNIQUE ---
        if (vehicleRepo.existsByVin(vin)) {
            throw new BaseException(ErrorHandler.VIN_ALREADY_EXISTS);
        }

        if (vehicleRepo.existsByEngineNumber(engine)) {
            throw new BaseException(ErrorHandler.ENGINE_NUMBER_ALREADY_EXISTS);
        }

        VehicleModelColor color = vehicleModelColorRepo.findById(req.getVehicleModelColorId())
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_MODEL_COLOR_NOT_FOUND));

        VehicleModel model = color.getVehicleModel();
        BigDecimal initialPrice = model.getManufacturerPrice()
                .add(color.getPriceAdjustment() != null ? color.getPriceAdjustment() : BigDecimal.ZERO);

        VehicleInstance v = VehicleInstance.builder()
                .vin(vin)                  // dùng VIN uppercase
                .engineNumber(engine)      // dùng engine uppercase
                .manufacturingDate(req.getManufacturingDate())
                .vehicleModel(model)
                .vehicleModelColor(color)
                .status(VehicleInstance.VehicleStatus.AVAILABLE)
                .isActive(true)
                .currentDealer(null)
                .currentValue(initialPrice)
                .build();

        vehicleRepo.save(v);
        return VehicleInstanceResponse.fromEntity(v);
    }


    @Override
    @Transactional
    public VehicleInstanceResponse update(Long id, VehicleInstanceUpdateRequest req) {
        guard.require(guard.has(guard.me(), "vehicle.update"));

        VehicleInstance v = vehicleRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));

        // Nếu đã gắn vào order → cấm sửa
        if (v.getOrder() != null) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Không thể sửa xe đã gắn vào đơn hàng.");
        }

        // VIN unique
        if (!v.getVin().equals(req.getVin()) && vehicleRepo.existsByVin(req.getVin())) {
            throw new BaseException(ErrorHandler.VIN_ALREADY_EXISTS);
        }

        if (!v.getEngineNumber().equals(req.getEngineNumber())
                && vehicleRepo.existsByEngineNumber(req.getEngineNumber())) {
            throw new BaseException(ErrorHandler.ENGINE_NUMBER_ALREADY_EXISTS);
        }

        v.setVin(req.getVin());
        v.setEngineNumber(req.getEngineNumber());
        v.setManufacturingDate(req.getManufacturingDate());

        if (req.getVehicleModelColorId() != null) {
            VehicleModelColor color = vehicleModelColorRepo.findById(req.getVehicleModelColorId())
                    .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_MODEL_COLOR_NOT_FOUND));
            v.setVehicleModelColor(color);
            v.setVehicleModel(color.getVehicleModel());
        }

        vehicleRepo.save(v);

        return VehicleInstanceResponse.fromEntity(v);
    }


    // --------------------------------------------------------
    // GET BY ID
    // --------------------------------------------------------
    @Override
    public VehicleInstanceResponse getById(Long id) {
        guard.require(guard.has(guard.me(), "vehicle.read"));
        VehicleInstance v = vehicleRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));

        checkDealerOwnership(v);
        return VehicleInstanceResponse.fromEntity(v);
    }

    // --------------------------------------------------------
    // ASSIGN TO CUSTOMER
    // --------------------------------------------------------
    @Override
    @Transactional
    public CustomerVehicleResponse assignToCustomer(AssignVehicleRequest req) {
        guard.require(guard.has(guard.me(), "vehicle.assign_customer"));
        User current = guard.me();

        VehicleInstance vehicle = vehicleRepo.findById(req.getVehicleId())
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));

        checkDealerOwnership(vehicle);

        if (Boolean.FALSE.equals(vehicle.getIsActive()))
            throw new BaseException(ErrorHandler.VEHICLE_IS_INACTIVE);

        if (vehicle.getStatus() == VehicleInstance.VehicleStatus.SOLD)
            throw new BaseException(ErrorHandler.VEHICLE_ALREADY_SOLD);

        if (vehicle.getCurrentDealer() == null)
            throw new BaseException(ErrorHandler.VEHICLE_NOT_OWNED_BY_DEALER);

        Customer customer = customerRepo.findById(req.getCustomerId())
                .orElseThrow(() -> new BaseException(ErrorHandler.CUSTOMER_NOT_FOUND));

        if (customerVehicleRepo.findByVehicleInstance(vehicle).isPresent())
            throw new BaseException(ErrorHandler.VEHICLE_ALREADY_ASSIGNED);

        User seller = current;
        LocalDate saleDate = LocalDate.now();

        LocalDate warrantyStart = req.getWarrantyStartDate() != null ? req.getWarrantyStartDate() : saleDate;
        LocalDate warrantyEnd = req.getWarrantyEndDate();

        if (warrantyEnd != null && warrantyEnd.isBefore(warrantyStart)) {
            throw new BaseException(ErrorHandler.INVALID_WARRANTY_PERIOD);
        }

        BigDecimal baseValue = vehicle.getCurrentValue() != null
                ? vehicle.getCurrentValue()
                : vehicle.getVehicleModel().getManufacturerPrice();

        BigDecimal salePrice = req.getSalePrice() != null ? req.getSalePrice() : baseValue;

        // ✅ Kiểm tra điều kiện salePrice nằm trong khoảng [currentValue, currentValue * 1.2]
        if (baseValue != null) {
            BigDecimal maxAllowed = baseValue.multiply(BigDecimal.valueOf(1.2));
            if (salePrice.compareTo(baseValue) < 0 || salePrice.compareTo(maxAllowed) > 0) {
                throw new BaseException(ErrorHandler.INVALID_SALE_PRICE_RANGE);
            }
        }

        CustomerVehicle record = CustomerVehicle.builder()
                .vehicleInstance(vehicle)
                .customer(customer)
                .soldByDealer(vehicle.getCurrentDealer())
                .soldByUser(seller)
                .salePrice(salePrice)
                .saleDate(saleDate)
                .customerWarrantyStartDate(warrantyStart)
                .customerWarrantyEndDate(warrantyEnd)
                .build();

        customerVehicleRepo.save(record);

        vehicle.setStatus(VehicleInstance.VehicleStatus.SOLD);

        Inventory inv = inventoryRepo.findByDealerIdAndVehicleModelColorId(
                vehicle.getCurrentDealer().getId(),
                vehicle.getVehicleModelColor().getId()
        ).orElse(null);

        if (inv != null) {
            inv.setTotalQuantity(Math.max(inv.getTotalQuantity() - 1, 0));
            inv.setAvailableQuantity(Math.max(inv.getAvailableQuantity() - 1, 0));
            inv.setReservedQuantity(Math.max(inv.getReservedQuantity() - 1, 0));
            inventoryRepo.save(inv);
        }

        vehicleRepo.save(vehicle);

        return CustomerVehicleResponse.fromEntity(record);
    }



    // --------------------------------------------------------
    // DEACTIVATE
    // --------------------------------------------------------
    @Override
    @Transactional
    public void deactivate(Long id) {
        guard.require(guard.has(guard.me(), "vehicle.deactive"));
        VehicleInstance v = vehicleRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));

        checkDealerOwnership(v);

        if (v.getStatus() != VehicleInstance.VehicleStatus.IN_STOCK)
            throw new BaseException(ErrorHandler.ONLY_IN_STOCK_CAN_DEACTIVATE);

        v.setIsActive(false);
        vehicleRepo.save(v);
    }

    // --------------------------------------------------------
    // ACTIVATE
    // --------------------------------------------------------
    @Override
    @Transactional
    public void activate(Long id) {
        guard.require(guard.has(guard.me(), "vehicle.active"));
        VehicleInstance v = vehicleRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));

        checkDealerOwnership(v);

        if (Boolean.TRUE.equals(v.getIsActive()))
            throw new BaseException(ErrorHandler.VEHICLE_ALREADY_ACTIVE);

        v.setIsActive(true);
        vehicleRepo.save(v);
    }

    // --------------------------------------------------------
    // UPDATE STATUS
    // --------------------------------------------------------
    // VehicleInstanceServiceImpl
    @Override
    @Transactional
    public VehicleInstanceResponse updateStatus(Long id, VehicleInstance.VehicleStatus status) {
        guard.require(guard.has(guard.me(), "vehicle.update_status"));
        VehicleInstance v = vehicleRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));

        checkDealerOwnership(v);

        if (Boolean.FALSE.equals(v.getIsActive()))
            throw new BaseException(ErrorHandler.VEHICLE_IS_INACTIVE);

        if (v.getStatus() == VehicleInstance.VehicleStatus.SOLD)
            throw new BaseException(ErrorHandler.VEHICLE_WAS_SOLDED);

        // ❌ Không cho đổi trực tiếp sang SOLD trong method này
        if (status == VehicleInstance.VehicleStatus.SOLD)
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Không thể đổi sang SOLD trực tiếp. Hãy gán xe cho khách hàng.");

        v.setStatus(status);
        // ✅ Đồng bộ số lượng kho
        Inventory inv = inventoryRepo.findByDealerIdAndVehicleModelColorId(
                v.getCurrentDealer().getId(),
                v.getVehicleModelColor().getId()
        ).orElse(null);

        if (inv != null) {
            if (status == VehicleInstance.VehicleStatus.RESERVED) {
                // Chuyển từ IN_STOCK → RESERVED
                inv.setReservedQuantity(inv.getReservedQuantity() + 1);
                inv.setAvailableQuantity(Math.max(inv.getAvailableQuantity() - 1, 0));
            } else if (status == VehicleInstance.VehicleStatus.IN_STOCK) {
                // Chuyển từ RESERVED → IN_STOCK
                inv.setReservedQuantity(Math.max(inv.getReservedQuantity() - 1, 0));
                inv.setAvailableQuantity(inv.getAvailableQuantity() + 1);
            }
            inventoryRepo.save(inv);
        }

        vehicleRepo.save(v);

        return VehicleInstanceResponse.fromEntity(v);
    }


    @Override
    @Transactional
    public VehicleInstanceResponse transferVehicle(TransferVehicleRequest req) {
        // ✅ Kiểm tra quyền qua guard (chuẩn nhất)
        guard.require(guard.has(guard.me(), "vehicle.transfer"));

        // ====== 1. Lấy xe cần chuyển ======
        VehicleInstance vehicle = vehicleRepo.findById(req.getVehicleInstanceId())
                .orElseThrow(() -> new BaseException(ErrorHandler.VEHICLE_INSTANCE_NOT_FOUND));

        if (!Boolean.TRUE.equals(vehicle.getIsActive()))
            throw new BaseException(ErrorHandler.VEHICLE_IS_INACTIVE);

        if (vehicle.getStatus() == VehicleInstance.VehicleStatus.SOLD)
            throw new BaseException(ErrorHandler.VEHICLE_WAS_SOLDED);

        if (vehicle.getStatus() == VehicleInstance.VehicleStatus.RESERVED)
            throw new BaseException(ErrorHandler.VEHICLE_IS_RESERVED);

        Dealer sourceDealer = vehicle.getCurrentDealer();
        Dealer targetDealer = dealerRepo.findById(req.getTargetDealerId())
                .orElseThrow(() -> new BaseException(ErrorHandler.DEALER_NOT_FOUND));

        if (sourceDealer == null)
            throw new BaseException(ErrorHandler.DEALER_NOT_FOUND, "Xe này chưa thuộc đại lý nào.");

        if (sourceDealer.getId().equals(targetDealer.getId()))
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Không thể chuyển xe sang cùng một đại lý.");

        // ====== 2. Cập nhật kho nguồn ======
        Inventory sourceInv = inventoryRepo.findByDealerIdAndVehicleModelColorId(
                sourceDealer.getId(),
                vehicle.getVehicleModelColor().getId()
        ).orElse(null);

        if (sourceInv != null) {
            sourceInv.setTotalQuantity(Math.max(sourceInv.getTotalQuantity() - 1, 0));
            sourceInv.setAvailableQuantity(Math.max(sourceInv.getAvailableQuantity() - 1, 0));
            inventoryRepo.save(sourceInv);
        }

        // ====== 3. Cập nhật kho đích ======
        Inventory targetInv = inventoryRepo.findByDealerIdAndVehicleModelColorId(
                targetDealer.getId(),
                vehicle.getVehicleModelColor().getId()
        ).orElseGet(() -> Inventory.builder()
                .dealer(targetDealer)
                .vehicleModelColor(vehicle.getVehicleModelColor())
                .totalQuantity(0)
                .reservedQuantity(0)
                .availableQuantity(0)
                .isActive(true)
                .build());

        targetInv.setTotalQuantity(targetInv.getTotalQuantity() + 1);
        targetInv.setAvailableQuantity(targetInv.getAvailableQuantity() + 1);
        inventoryRepo.save(targetInv);

        // ====== 4. Cập nhật dealer cho xe ======
        vehicle.setCurrentDealer(targetDealer);
        vehicleRepo.save(vehicle);

        return VehicleInstanceResponse.fromEntity(vehicle);
    }

    @Override
    @Transactional
    public VehicleImportResult importVehiclesFromExcel(MultipartFile file) {
        guard.require(guard.has(guard.me(), "vehicle.import"));

        VehicleImportResult result = VehicleImportResult.builder()
                .successRecords(new ArrayList<>())
                .errorRecords(new ArrayList<>())
                .build();

        try (InputStream is = file.getInputStream();
             Workbook workbook = new XSSFWorkbook(is)) {

            Sheet sheet = workbook.getSheetAt(0);
            result.setTotalRows(sheet.getLastRowNum());

            List<VehicleImportRow> rows = parseExcelRows(sheet);
            Map<Integer, String> validationErrors = validateImportRows(rows);

            int successCount = 0;
            int failureCount = 0;

            for (VehicleImportRow row : rows) {

                // ❌ Dòng lỗi => ghi lại rồi skip
                if (validationErrors.containsKey(row.getRowNumber())) {
                    recordError(result, row, validationErrors.get(row.getRowNumber()));
                    failureCount++;
                    continue;
                }

                Optional<VehicleModelColor> optColor = resolveModelColor(row);

                if (optColor.isEmpty()) {
                    recordError(result, row,
                            String.format("Không tìm thấy model '%s' với màu '%s'",
                                    row.getModelName(), row.getColorName()));
                    failureCount++;
                    continue;
                }

                try {
                    VehicleModelColor vmc = optColor.get();
                    VehicleInstance vehicle = vehicleRepo.save(
                            buildVehicleInstance(row, vmc)
                    );

                    recordSuccess(result, row, vehicle, vmc);
                    successCount++;

                } catch (Exception e) {
                    recordError(result, row, "Lỗi khi lưu xe: " + e.getMessage());
                    failureCount++;
                }
            }

            result.setSuccessCount(successCount);
            result.setFailureCount(failureCount);
            return result;

        } catch (Exception e) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST,
                    "Không thể đọc file Excel: " + e.getMessage());
        }
    }



    // --------------------------------------------------------
    // HELPER METHODS
    // --------------------------------------------------------

    private VehicleInstance buildVehicleInstance(VehicleImportRow row, VehicleModelColor vmc) {

        VehicleModel model = vmc.getVehicleModel();
        BigDecimal initialPrice = model.getManufacturerPrice()
                .add(vmc.getPriceAdjustment() != null ? vmc.getPriceAdjustment() : BigDecimal.ZERO);

        return VehicleInstance.builder()
                .vin(row.getVin().toUpperCase())
                .engineNumber(row.getEngineNumber().toUpperCase())
                .manufacturingDate(row.getManufacturingDate())
                .vehicleModel(model)
                .vehicleModelColor(vmc)
                .status(VehicleInstance.VehicleStatus.AVAILABLE)
                .isActive(true)
                .currentDealer(null)
                .currentValue(initialPrice)
                .build();
    }

    private void recordSuccess(VehicleImportResult result,
                               VehicleImportRow row,
                               VehicleInstance vehicle,
                               VehicleModelColor vmc) {

        result.getSuccessRecords().add(
                VehicleImportResult.VehicleImportSuccess.builder()
                        .rowNumber(row.getRowNumber())
                        .vin(vehicle.getVin())
                        .engineNumber(vehicle.getEngineNumber())
                        .vehicleId(vehicle.getId())
                        .modelName(vmc.getVehicleModel().getName())
                        .colorName(vmc.getColor().getColorName())
                        .build()
        );
    }


    private Optional<VehicleModelColor> resolveModelColor(VehicleImportRow row) {
        Optional<VehicleModel> model =
                vehicleModelRepo.findByNameIgnoreCase(row.getModelName());

        Optional<Color> color =
                colorRepo.findByColorNameIgnoreCase(row.getColorName());

        if (model.isEmpty() || color.isEmpty()) return Optional.empty();

        return vehicleModelColorRepo.findByVehicleModelIdAndColorId(
                model.get().getId(),
                color.get().getId()
        );
    }

    private void recordError(VehicleImportResult result,
                             VehicleImportRow row,
                             String msg) {

        result.getErrorRecords().add(
                VehicleImportResult.VehicleImportError.builder()
                        .rowNumber(row.getRowNumber())
                        .vin(row.getVin())
                        .engineNumber(row.getEngineNumber())
                        .errorMessage(msg)
                        .build()
        );
    }


    private boolean isDealerRole(User user) {
        return user.getRoles().stream()
                .map(Role::getName)
                .anyMatch(r -> r.equals("DEALER_MANAGER") || r.equals("DEALER_STAFF"));
    }

    private Long getDealerId(User user) {
        if (user.getDealer() == null)
            throw new BaseException(ErrorHandler.DEALER_NOT_FOUND);
        return user.getDealer().getId();
    }

    private void checkDealerOwnership(VehicleInstance vehicle) {
        User current = guard.me();

        boolean isAdminOrEvm = current.getRoles().stream()
                .map(Role::getName)
                .anyMatch(r -> r.equals("ADMIN") || r.equals("EVM_STAFF"));

        if (isAdminOrEvm)
            return;

        if (vehicle.getCurrentDealer() == null)
            throw new BaseException(ErrorHandler.VEHICLE_NOT_OWNED_BY_DEALER);

        Long myDealerId = getDealerId(current);
        if (!myDealerId.equals(vehicle.getCurrentDealer().getId())) {
            throw new BaseException(ErrorHandler.FORBIDDEN);
        }
    }

    private List<VehicleImportRow> parseExcelRows(Sheet sheet) {
        List<VehicleImportRow> rows = new ArrayList<>();

        // Bỏ qua header (row 0)
        for (int i = 1; i <= sheet.getLastRowNum(); i++) {
            Row row = sheet.getRow(i);
            if (row == null) continue;

            try {
                VehicleImportRow importRow = VehicleImportRow.builder()
                        .rowNumber(i + 1)
                        .vin(getCellValueAsString(row.getCell(0)))
                        .engineNumber(getCellValueAsString(row.getCell(1)))
                        .modelName(getCellValueAsString(row.getCell(2)))
                        .colorName(getCellValueAsString(row.getCell(3)))
                        .manufacturingDate(getCellValueAsDate(row.getCell(4)))
                        .build();

                rows.add(importRow);
            } catch (Exception e) {
                // Log lỗi parse
                continue;
            }
        }

        return rows;
    }

    private Map<Integer, String> validateImportRows(List<VehicleImportRow> rows) {
        Map<Integer, String> errors = new HashMap<>();
        Set<String> vinsInFile = new HashSet<>();
        Set<String> enginesInFile = new HashSet<>();

        for (VehicleImportRow row : rows) {
            List<String> rowErrors = new ArrayList<>();

            // ✅ 1. Validate VIN format
            if (row.getVin() == null || row.getVin().trim().isEmpty()) {
                rowErrors.add("VIN không được để trống");
            } else if (!VIN_PATTERN.matcher(row.getVin().toUpperCase()).matches()) {
                rowErrors.add("VIN không đúng định dạng (phải 17 ký tự, không chứa I, O, Q)");
            } else {
                // ✅ 2. Check VIN trùng trong file
                if (vinsInFile.contains(row.getVin().toUpperCase())) {
                    rowErrors.add("VIN bị trùng trong file Excel");
                } else {
                    vinsInFile.add(row.getVin().toUpperCase());

                    // ✅ 3. Check VIN trùng trong DB
                    if (vehicleRepo.existsByVin(row.getVin().toUpperCase())) {
                        rowErrors.add("VIN đã tồn tại trong hệ thống");
                    }
                }
            }

            // ✅ 4. Validate EngineNumber
            if (row.getEngineNumber() == null || row.getEngineNumber().trim().isEmpty()) {
                rowErrors.add("Số máy không được để trống");
            } else {
                // ✅ 5. Check Engine trùng trong file
                if (enginesInFile.contains(row.getEngineNumber().toUpperCase())) {
                    rowErrors.add("Số máy bị trùng trong file Excel");
                } else {
                    enginesInFile.add(row.getEngineNumber().toUpperCase());

                    // ✅ 6. Check Engine trùng trong DB
                    if (vehicleRepo.existsByEngineNumber(row.getEngineNumber().toUpperCase())) {
                        rowErrors.add("Số máy đã tồn tại trong hệ thống");
                    }
                }
            }

            // ✅ 7. Validate ModelName
            if (row.getModelName() == null || row.getModelName().trim().isEmpty()) {
                rowErrors.add("Tên Model không được để trống");
            } else {
                Optional<VehicleModel> model = vehicleModelRepo.findByNameIgnoreCase(row.getModelName());
                if (model.isEmpty()) {
                    rowErrors.add(String.format("Model '%s' không tồn tại trong hệ thống", row.getModelName()));
                }
            }

            // ✅ 8. Validate ColorName
            if (row.getColorName() == null || row.getColorName().trim().isEmpty()) {
                rowErrors.add("Tên màu không được để trống");
            } else {
                Optional<Color> color = colorRepo.findByColorNameIgnoreCase(row.getColorName());
                if (color.isEmpty()) {
                    rowErrors.add(String.format("Màu '%s' không tồn tại trong hệ thống", row.getColorName()));
                }
            }

            // Lưu lỗi nếu có
            if (!rowErrors.isEmpty()) {
                errors.put(row.getRowNumber(), String.join("; ", rowErrors));
            }
        }

        return errors;
    }

    private String getCellValueAsString(Cell cell) {
        if (cell == null) return "";

        switch (cell.getCellType()) {
            case STRING:
                return cell.getStringCellValue().trim();
            case NUMERIC:
                return String.valueOf((long) cell.getNumericCellValue());
            case BOOLEAN:
                return String.valueOf(cell.getBooleanCellValue());
            default:
                return "";
        }
    }

    private LocalDate getCellValueAsDate(Cell cell) {
        if (cell == null) return null;

        try {
            if (cell.getCellType() == CellType.NUMERIC && DateUtil.isCellDateFormatted(cell)) {
                return cell.getDateCellValue().toInstant()
                        .atZone(ZoneId.systemDefault())
                        .toLocalDate();
            }
        } catch (Exception e) {
            return null;
        }

        return null;
    }
}
