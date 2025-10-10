package swp.project.swp391.config;

import lombok.RequiredArgsConstructor;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import swp.project.swp391.entity.*;
import swp.project.swp391.repository.*;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.util.List;

/**
 * Sample data initializer - CHỈ chạy trong môi trường dev/local
 * Khởi tạo: DealerLevels, Dealers, VehicleModels, Colors, Prices, Customers
 */
@Component
@RequiredArgsConstructor
@Profile("!prod") // Chạy khi KHÔNG phải production
public class SampleDataInitializer implements CommandLineRunner {
    private final DealerLevelRepository dealerLevelRepository;
    private final DealerRepository dealerRepository;
    private final VehicleModelRepository vehicleModelRepository;
    private final ColorRepository colorRepository;
    private final VehicleModelColorRepository vehicleModelColorRepository;
    private final VehiclePriceRepository vehiclePriceRepository;
    private final CustomerRepository customerRepository;

    @Override
    @Transactional
    public void run(String... args) {
        System.out.println("========== Initializing Sample Data (DEV MODE) ==========");

        initializeDealerLevels();
        initializeDealers();
        initializeVehicleModels();
        initializeVehiclePrices();
        initializeSampleCustomers();

        System.out.println("========== Sample Data Initialization Completed ==========");
    }

    @Transactional
    protected void initializeDealerLevels() {
        if (dealerLevelRepository.count() > 0) {
            System.out.println(">>> Dealer Levels already exist, skipping...");
            return;
        }

        System.out.println(">>> Initializing Dealer Levels...");

        DealerLevel level1 = DealerLevel.builder()
                .levelName("Level 1")
                .levelNumber(1)
                .discountRate(new BigDecimal("10.00"))
                .maxOrderQuantity(100)
                .creditLimit(new BigDecimal("10000000000")) // 10 tỷ
                .description("Đại lý cấp 1")
                .isActive(true)
                .build();
        dealerLevelRepository.save(level1);

        DealerLevel level2 = DealerLevel.builder()
                .levelName("Level 2")
                .levelNumber(2)
                .discountRate(new BigDecimal("5.00"))
                .maxOrderQuantity(50)
                .creditLimit(new BigDecimal("5000000000")) // 5 tỷ
                .description("Đại lý cấp 2")
                .isActive(true)
                .build();
        dealerLevelRepository.save(level2);

        DealerLevel level3 = DealerLevel.builder()
                .levelName("Level 3")
                .levelNumber(3)
                .discountRate(BigDecimal.ZERO)
                .maxOrderQuantity(20)
                .creditLimit(new BigDecimal("2000000000")) // 2 tỷ
                .description("Đại lý cấp 3")
                .isActive(true)
                .build();
        dealerLevelRepository.save(level3);

        System.out.println(">>> Created 3 Dealer Levels!");
    }

    @Transactional
    protected void initializeDealers() {
        if (dealerRepository.count() > 0) {
            System.out.println(">>> Dealers already exist, skipping...");
            return;
        }

        System.out.println(">>> Initializing Dealers...");

        DealerLevel level1 = dealerLevelRepository.findByLevelNumber(1)
                .orElseThrow(() -> new IllegalStateException("Level 1 not found"));
        DealerLevel level2 = dealerLevelRepository.findByLevelNumber(2)
                .orElseThrow(() -> new IllegalStateException("Level 2 not found"));

        // Dealer Level 1 (Hà Nội)
        Dealer dealerHN = Dealer.builder()
                .name("VinFast Hà Nội")
                .code("DL-HN-001")
                .address("123 Phạm Văn Đồng, Cầu Giấy, Hà Nội")
                .phoneNumber("024-1234-5678")
                .email("hanoi@vinfast.vn")
                .region(Dealer.Region.NORTH)
                .level(level1)
                .currentDebt(BigDecimal.ZERO)
                .isActive(true)
                .build();
        dealerRepository.save(dealerHN);

        // Dealer Level 1 (TP.HCM)
        Dealer dealerHCM = Dealer.builder()
                .name("VinFast TP.HCM")
                .code("DL-HCM-001")
                .address("456 Nguyễn Văn Linh, Quận 7, TP.HCM")
                .phoneNumber("028-9876-5432")
                .email("hcm@vinfast.vn")
                .region(Dealer.Region.SOUTH)
                .level(level1)
                .currentDebt(BigDecimal.ZERO)
                .isActive(true)
                .build();
        dealerRepository.save(dealerHCM);

        // Dealer Level 2 (con của HN)
        Dealer dealerBN = Dealer.builder()
                .name("VinFast Bắc Ninh")
                .code("DL-BN-001")
                .address("789 Lý Thái Tổ, Bắc Ninh")
                .phoneNumber("0222-123-456")
                .email("bacninh@vinfast.vn")
                .region(Dealer.Region.NORTH)
                .level(level2)
                .currentDebt(BigDecimal.ZERO)
                .isActive(true)
                .build();
        dealerRepository.save(dealerBN);

        System.out.println(">>> Created 3 Dealers!");
    }

    @Transactional
    protected void initializeVehicleModels() {
        if (vehicleModelRepository.count() > 0) {
            System.out.println(">>> Vehicle Models already exist, skipping...");
            return;
        }

        System.out.println(">>> Initializing Vehicle Models & Colors...");

        // VF 8
        VehicleModel vf8 = VehicleModel.builder()
                .name("VF 8")
                .modelCode("VF8-2024")
                .description("SUV điện 5 chỗ cao cấp")
                .brand("VinFast")
                .year(2024)
                .batteryCapacity(87)
                .rangeKm(447)
                .chargingTime(70)
                .maxSpeed(180)
                .acceleration(new BigDecimal("5.5"))
                .seatingCapacity(5)
                .cargoVolume(new BigDecimal("376"))
                .manufacturerPrice(new BigDecimal("800000000"))
                .isActive(true)
                .build();
        vehicleModelRepository.save(vf8);

        // VF 9
        VehicleModel vf9 = VehicleModel.builder()
                .name("VF 9")
                .modelCode("VF9-2024")
                .description("SUV điện 7 chỗ hạng sang")
                .brand("VinFast")
                .year(2024)
                .batteryCapacity(123)
                .rangeKm(626)
                .chargingTime(80)
                .maxSpeed(200)
                .acceleration(new BigDecimal("4.5"))
                .seatingCapacity(7)
                .cargoVolume(new BigDecimal("752"))
                .manufacturerPrice(new BigDecimal("1200000000"))
                .isActive(true)
                .build();
        vehicleModelRepository.save(vf9);

        // Colors cho VF8
        createColor(vf8, "Trắng Ngọc Trai", "#FFFFFF", BigDecimal.ZERO);
        createColor(vf8, "Đen Tuxedo", "#000000", new BigDecimal("10000000"));
        createColor(vf8, "Đỏ Rubicon", "#8B0000", new BigDecimal("15000000"));

        // Colors cho VF9
        createColor(vf9, "Trắng Ngọc Trai", "#FFFFFF", BigDecimal.ZERO);
        createColor(vf9, "Xanh Đại Dương", "#0047AB", new BigDecimal("20000000"));
        createColor(vf9, "Bạc Titan", "#C0C0C0", new BigDecimal("10000000"));

        System.out.println(">>> Created 2 Vehicle Models with 6 Colors!");
    }

    private void createColor(VehicleModel model, String name, String hex, BigDecimal adjustment) {
        // 1️⃣ Tạo hoặc lấy Color trong catalog
        Color color = colorRepository.findByHexCode(hex)
                .orElseGet(() -> {
                    Color c = Color.builder()
                            .colorName(name)
                            .hexCode(hex)
                            .isActive(true)
                            .build();
                    return colorRepository.save(c);
                });

        // 2️⃣ Gán Color vào model
        VehicleModelColor modelColor = VehicleModelColor.builder()
                .vehicleModel(model)
                .color(color)
                .priceAdjustment(adjustment)
                .build();

        vehicleModelColorRepository.save(modelColor);
    }


    @Transactional
    protected void initializeVehiclePrices() {
        if (vehiclePriceRepository.count() > 0) {
            System.out.println(">>> Vehicle Prices already exist, skipping...");
            return;
        }

        System.out.println(">>> Initializing Vehicle Prices...");

        List<VehicleModel> models = vehicleModelRepository.findAll();
        List<DealerLevel> levels = dealerLevelRepository.findAll();

        for (VehicleModel model : models) {
            for (DealerLevel level : levels) {
                BigDecimal basePrice = model.getManufacturerPrice();
                BigDecimal markup = basePrice.multiply(level.getDiscountRate()).divide(new BigDecimal("100"));
                BigDecimal wholesalePrice = basePrice.add(markup);

                VehiclePrice price = VehiclePrice.builder()
                        .vehicleModel(model)
                        .dealerLevel(level)
                        .wholesalePrice(wholesalePrice)
                        .effectiveFrom(LocalDate.of(2024, 1, 1))
                        .effectiveTo(null)
                        .isActive(true)
                        .build();
                vehiclePriceRepository.save(price);
            }
        }

        System.out.println(">>> Created Vehicle Prices for all levels!");
    }

    @Transactional
    protected void initializeSampleCustomers() {
        if (customerRepository.count() > 0) {
            System.out.println(">>> Customers already exist, skipping...");
            return;
        }

        System.out.println(">>> Initializing Sample Customers...");

        Customer c1 = Customer.builder()
                .fullName("Nguyễn Văn A")
                .phoneNumber("0901234567")
                .email("nguyenvana@gmail.com")
                .idNumber("001234567890")
                .dateOfBirth(LocalDate.of(1985, 5, 15))
                .gender(Customer.Gender.MALE)
                .address("123 Lê Lợi, Q1, TP.HCM")
                .notes("Khách hàng VIP")
                .build();
        customerRepository.save(c1);

        Customer c2 = Customer.builder()
                .fullName("Trần Thị B")
                .phoneNumber("0912345678")
                .email("tranthib@gmail.com")
                .idNumber("009876543210")
                .dateOfBirth(LocalDate.of(1990, 8, 20))
                .gender(Customer.Gender.FEMALE)
                .address("456 Trần Hưng Đạo, Hoàn Kiếm, Hà Nội")
                .build();
        customerRepository.save(c2);

        System.out.println(">>> Created 2 Sample Customers!");
    }
}