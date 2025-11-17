package swp.project.swp391.config;

import lombok.RequiredArgsConstructor;
import org.springframework.boot.CommandLineRunner;
import org.springframework.context.annotation.Profile;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import swp.project.swp391.entity.*;
import swp.project.swp391.repository.*;
import org.springframework.core.annotation.Order;
import java.math.BigDecimal;
import java.math.RoundingMode;
import java.time.LocalDate;
import java.util.List;
import java.util.Set;

/**
 * Sample data initializer - CHỈ chạy trong môi trường dev/local
 * Khởi tạo: DealerLevels, Dealers, VehicleModels, Colors, Prices, Customers
 */
@Component
@RequiredArgsConstructor
@Profile("!prod") // Chạy khi KHÔNG phải production
@Order(2)
public class SampleDataInitializer implements CommandLineRunner {
    private final DealerLevelRepository dealerLevelRepository;
    private final DealerRepository dealerRepository;
    private final VehicleModelRepository vehicleModelRepository;
    private final ColorRepository colorRepository;
    private final VehicleModelColorRepository vehicleModelColorRepository;
    private final VehiclePriceRepository vehiclePriceRepository;
    private final CustomerRepository customerRepository;
    private final UserRepository userRepository;
    private final RoleRepository roleRepository;
    private final org.springframework.security.crypto.password.PasswordEncoder passwordEncoder;


    @Override
    @Transactional
    public void run(String... args) {
        System.out.println("========== Initializing Sample Data (DEV MODE) ==========");

        initializeDealerLevels();
        initializeDealers();
        initializeVehicleModels();
        initializeVehiclePrices();
        initializeSampleCustomers();
        initializeDealerAccounts();

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
                        .
                discountRate(new BigDecimal("10.00"))
                .maxOrderQuantity(7)
                .creditLimit(new BigDecimal("20000000000"))
                .description("Đại lý cấp 1")
                .depositRate(new BigDecimal("0.2")) //
                .maxInstallmentMonths(12)
                .isActive(true)
                .build();
        dealerLevelRepository.save(level1);

        DealerLevel level2 = DealerLevel.builder()
                .levelName("Level 2")
                .levelNumber(2)
                .discountRate(new BigDecimal("5.00"))
                .maxOrderQuantity(5)
                .creditLimit(new BigDecimal("15000000000"))
                .description("Đại lý cấp 2")
                .depositRate(new BigDecimal("0.25")) //
                .maxInstallmentMonths(6) //
                .isActive(true)
                .build();
        dealerLevelRepository.save(level2);

        DealerLevel level3 = DealerLevel.builder()
                .levelName("Level 3")
                .levelNumber(3)
                .discountRate(BigDecimal.ZERO)
                .maxOrderQuantity(3)
                .creditLimit(new BigDecimal("10000000000"))
                .description("Đại lý cấp 3")
                .depositRate(new BigDecimal("0.3")) //
                .maxInstallmentMonths(3) //
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

          // ✅ Colors đơn giản cho VF8 (để test import Excel)
        createColor(vf8, "Trắng", "#FFFFFF", BigDecimal.ZERO);
        createColor(vf8, "Đen", "#000000", new BigDecimal("10000000"));
        createColor(vf8, "Đỏ", "#FF0000", new BigDecimal("15000000"));    // ✅ THÊM
        createColor(vf8, "Xanh", "#0000FF", new BigDecimal("12000000"));  // ✅ THÊM
        createColor(vf8, "Vàng", "#FFFF00", new BigDecimal("8000000"));   // ✅ THÊM (để test lỗi)

        // ✅ Colors đơn giản cho VF9
        createColor(vf9, "Trắng", "#FFFFFF", BigDecimal.ZERO);
        createColor(vf9, "Xanh", "#0000FF", new BigDecimal("20000000"));  // ✅ THÊM
        createColor(vf9, "Bạc", "#C0C0C0", new BigDecimal("10000000"));

        System.out.println(">>> Created 2 Vehicle Models with colors!");
    }

    private void createColor(VehicleModel model, String name, String hex, BigDecimal adjustment) {

        // 1️⃣ Lấy hoặc tạo màu theo tên (name)
        Color color = colorRepository.findByColorNameIgnoreCase(name)
                .orElseGet(() -> colorRepository.save(
                        Color.builder()
                                .colorName(name)
                                .hexCode(hex)
                                .isActive(true)
                                .build()
                ));

        // 2️⃣ Gán màu vào model
        VehicleModelColor vmc = VehicleModelColor.builder()
                .vehicleModel(model)
                .color(color)
                .priceAdjustment(adjustment)
                .isActive(true)
                .build();

        vehicleModelColorRepository.save(vmc);
    }


    @Transactional
    protected void initializeVehiclePrices() {
        if (vehiclePriceRepository.count() > 0) {
            System.out.println(">>> Vehicle Prices already exist, skipping...");
            return;
        }

        System.out.println(">>> Initializing Vehicle Prices...");

        List<VehicleModelColor> colorList = vehicleModelColorRepository.findAll();
        List<DealerLevel> levels = dealerLevelRepository.findAll();

        for (VehicleModelColor color : colorList) {
            VehicleModel model = color.getVehicleModel();

            // base = giá hãng + điều chỉnh màu
            BigDecimal basePrice = model.getManufacturerPrice();
            BigDecimal colorAdj = (color.getPriceAdjustment() == null) ? BigDecimal.ZERO : color.getPriceAdjustment();
            BigDecimal colorBase = basePrice.add(colorAdj);

            for (DealerLevel level : levels) {
                BigDecimal discountRate = level.getDiscountRate();
                if (discountRate == null) discountRate = BigDecimal.ZERO;

                // Tính đúng hướng giảm giá cho dealer
                BigDecimal wholesalePrice = colorBase
                        .multiply(BigDecimal.ONE.subtract(discountRate.divide(BigDecimal.valueOf(100))))
                        .setScale(2, RoundingMode.HALF_UP);

                VehiclePrice price = VehiclePrice.builder()
                        .vehicleModelColor(color)
                        .dealerLevel(level)
                        .wholesalePrice(wholesalePrice)
                        .effectiveFrom(LocalDate.of(2024, 1, 1))
                        .effectiveTo(null)
                        .isActive(true)
                        .build();

                vehiclePriceRepository.save(price);
            }
        }

        System.out.println(">>> Created Vehicle Prices for all colors & levels!");
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
    @Transactional
    protected void initializeDealerAccounts() {
        System.out.println(">>> Initializing Dealer Accounts...");

        // 1️⃣ Lấy dealer Hà Nội (đại lý cấp 1)
        Dealer dealerHN = dealerRepository.findByCode("DL-HN-001")
                .orElseThrow(() -> new IllegalStateException("Dealer Hà Nội not found"));

        // 2️⃣ Lấy role — đảm bảo role đã có
        Role dealerManagerRole = roleRepository.findByName("DEALER_MANAGER")
                .orElseThrow(() -> new IllegalStateException("Role DEALER_MANAGER not found"));
        Role dealerStaffRole = roleRepository.findByName("DEALER_STAFF")
                .orElseThrow(() -> new IllegalStateException("Role DEALER_STAFF not found"));

        // 3️⃣ Dealer Manager (nếu chưa có)
        if (userRepository.findByUsername("dathoang03").isEmpty()) {
            User manager = User.builder()
                    .username("dathoang03")
                    .password(passwordEncoder.encode("dathoang03"))
                    .fullName("Đạt Hoàng (Dealer Manager)")
                    .email("dathoang03@vinfast.vn")
                    .phoneNumber("0903000003")
                    .gender(User.Gender.MALE)
                    .dealer(dealerHN)
                    .roles(Set.of(dealerManagerRole))
                    .isActive(true)
                    .build();
            userRepository.save(manager);
            System.out.println(">>> Created Dealer Manager account (dathoang03 / dathoang03)");
        } else {
            System.out.println(">>> Dealer Manager account already exists, skipping...");
        }

        // 4️⃣ Dealer Staff (nếu chưa có)
        if (userRepository.findByUsername("dathoang04").isEmpty()) {
            User staff = User.builder()
                    .username("dathoang04")
                    .password(passwordEncoder.encode("dathoang04"))
                    .fullName("Đạt Hoàng (Dealer Staff)")
                    .email("dathoang04@vinfast.vn")
                    .phoneNumber("0904000004")
                    .gender(User.Gender.MALE)
                    .dealer(dealerHN)
                    .roles(Set.of(dealerStaffRole))
                    .isActive(true)
                    .build();
            userRepository.save(staff);
            System.out.println(">>> Created Dealer Staff account (dathoang04 / dathoang04)");
        } else {
            System.out.println(">>> Dealer Staff account already exists, skipping...");
        }

        System.out.println(">>> Dealer Accounts initialized successfully!");
    }
}