package swp.project.swp391.config;

import lombok.RequiredArgsConstructor;
import org.springframework.boot.CommandLineRunner;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Component;
import org.springframework.transaction.annotation.Transactional;
import swp.project.swp391.entity.Permission;
import swp.project.swp391.entity.Role;
import swp.project.swp391.entity.User;
import swp.project.swp391.repository.PermissionRepository;
import swp.project.swp391.repository.RoleRepository;
import swp.project.swp391.repository.UserRepository;

import java.util.*;
import java.util.stream.Collectors;

/**
 * Core data initializer - Luôn chạy trong mọi môi trường
 * Khởi tạo: Roles, Permissions, Admin User
 */
@Component
@RequiredArgsConstructor
public class DataInitializer implements CommandLineRunner {
    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;
    private final RoleRepository roleRepository;
    private final PermissionRepository permissionRepository;

    @Override
    @Transactional
    public void run(String... args) {
        System.out.println("========== Initializing Core Data (Roles, Permissions, Admin) ==========");
        initializeRolesAndPermissions();
        System.out.println("========== Core Data Initialization Completed ==========");
    }

    @Transactional
    protected void initializeRolesAndPermissions() {
        createPermissionsIfNotExist();

        Role adminRole = upsertRoleIfNeeded("ADMIN", "Administrator", "Full system access");
        Role evmStaffRole = upsertRoleIfNeeded("EVM_STAFF", "EVM Staff", "Electric Vehicle Manufacturer Staff");
        Role dealerManagerRole = upsertRoleIfNeeded("DEALER_MANAGER", "Dealer Manager", "Dealer Manager");
        Role dealerStaffRole = upsertRoleIfNeeded("DEALER_STAFF", "Dealer Staff", "Dealer Staff");

        Map<String, Permission> pMap = permissionRepository.findAll().stream()
                .collect(Collectors.toMap(Permission::getName, p -> p));

        if (!adminRole.getIsCustomized()) {
            adminRole.setPermissions(new HashSet<>(pMap.values()));
            roleRepository.save(adminRole);
        }

        if (!evmStaffRole.getIsCustomized()) {
            setRolePermissionsByNames(evmStaffRole, pMap, List.of(
                    "vehicle.create", "vehicle.read", "vehicle.update", "vehicle.delete",
                    "vehicleModel.create", "vehicleModel.read", "vehicleModel.update",
                    "vehicleColor.create", "vehicleColor.read",
                    "order.read", "order.update",
                    "dealer.create", "dealer.read", "dealer.update",
                    "dealerLevel.read",
                    "inventory.read",
                    "report.read", "report.export"
            ));
        }

        if (!dealerManagerRole.getIsCustomized()) {
            setRolePermissionsByNames(dealerManagerRole, pMap, List.of(
                    "order.create", "order.read", "order.update",
                    "inventory.read",
                    "customer.create", "customer.read", "customer.update",
                    "vehicle.read",
                    "vehicleModel.read",
                    "report.read"
            ));
        }

        if (!dealerStaffRole.getIsCustomized()) {
            setRolePermissionsByNames(dealerStaffRole, pMap, List.of(
                    "order.create", "order.read",
                    "inventory.read",
                    "customer.create", "customer.read",
                    "vehicle.read",
                    "vehicleModel.read"
            ));
        }

        createAdminUserIfNotExist();
    }

    private void createPermissionsIfNotExist() {
        String[][] base = new String[][]{
                // User
                {"user", "create", "Tạo người dùng", "Thêm mới người dùng"},
                {"user", "read", "Xem người dùng", "Xem thông tin người dùng"},
                {"user", "update", "Cập nhật người dùng", "Sửa thông tin người dùng"},
                {"user", "delete", "Xóa người dùng", "Xóa người dùng"},
                {"user", "inactive", "Vô hiệu hoá người dùng", "Vô hiệu hoá người dùng"},
                {"user", "reactivate", "Kích hoạt người dùng", "Kích hoạt người dùng"},

                // Role
                {"role", "read", "Xem role", "Xem thông tin role"},
                {"role", "update", "Cập nhật role", "Sửa permissions của role"},

                // Vehicle Instance
                {"vehicle", "create", "Tạo xe", "Thêm xe vào hệ thống"},
                {"vehicle", "read", "Xem xe", "Xem thông tin xe"},
                {"vehicle", "update", "Cập nhật xe", "Sửa thông tin xe"},
                {"vehicle", "delete", "Xóa xe", "Xóa xe"},

                // Vehicle Model
                {"vehicleModel", "create", "Tạo model xe", "Thêm model xe mới"},
                {"vehicleModel", "read", "Xem model xe", "Xem thông tin model xe"},
                {"vehicleModel", "update", "Cập nhật model xe", "Sửa thông tin model"},

                // Vehicle Color
                {"vehicleColor", "create", "Tạo màu xe", "Thêm màu xe mới"},
                {"vehicleColor", "read", "Xem màu xe", "Xem màu xe"},

                // Order
                {"order", "create", "Tạo đơn hàng", "Tạo đơn đặt xe"},
                {"order", "read", "Xem đơn hàng", "Xem thông tin đơn hàng"},
                {"order", "update", "Cập nhật đơn hàng", "Cập nhật trạng thái đơn"},
                {"order", "delete", "Xóa đơn hàng", "Xóa đơn hàng"},

                // Dealer
                {"dealer", "create", "Tạo đại lý", "Thêm đại lý mới"},
                {"dealer", "read", "Xem đại lý", "Xem thông tin đại lý"},
                {"dealer", "update", "Cập nhật đại lý", "Sửa thông tin đại lý"},
                {"dealer", "delete", "Xóa đại lý", "Xóa đại lý"},
                {"dealer", "inactive", "Vô hiệu hoá đại lý", "Vô hiệu hoá đại lý"},
                {"dealer", "reactivate", "Kích hoạt đại lý", "Kích hoạt lại đại lý"},

                // Dealer Level
                {"dealerLevel", "read", "Xem cấp độ đại lý", "Xem thông tin cấp độ"},

                // Inventory
                {"inventory", "read", "Xem tồn kho", "Xem số lượng tồn kho"},

                // Customer
                {"customer", "create", "Tạo khách hàng", "Thêm thông tin khách hàng"},
                {"customer", "read", "Xem khách hàng", "Xem thông tin khách hàng"},
                {"customer", "update", "Cập nhật khách hàng", "Sửa thông tin khách hàng"},

                // Report
                {"report", "read", "Xem báo cáo", "Xem các báo cáo"},
                {"report", "export", "Xuất báo cáo", "Xuất báo cáo ra file"}
        };

        for (String[] p : base) {
            createPermissionIfNotExist(p[0] + "." + p[1], p[2], p[3], p[0], p[1]);
        }
    }

    private Permission createPermissionIfNotExist(String name, String displayName, String description, String resource, String action) {
        return permissionRepository.findByName(name).orElseGet(() -> {
            Permission permission = Permission.builder()
                    .name(name)
                    .displayName(displayName)
                    .description(description)
                    .resource(resource)
                    .action(action)
                    .isActive(true)
                    .build();
            return permissionRepository.save(permission);
        });
    }

    private Role upsertRoleIfNeeded(String name, String displayName, String description) {
        return roleRepository.findByName(name).orElseGet(() -> {
            Role r = Role.builder()
                    .name(name)
                    .displayName(displayName)
                    .description(description)
                    .isActive(true)
                    .isCustomized(false)
                    .build();
            return roleRepository.save(r);
        });
    }

    private void setRolePermissionsByNames(Role role, Map<String, Permission> all, List<String> names) {
        Set<Permission> perms = names.stream()
                .map(n -> {
                    Permission p = all.get(n);
                    if (p == null) {
                        System.out.println("WARNING: Permission not found: " + n);
                        return null;
                    }
                    return p;
                })
                .filter(Objects::nonNull)
                .collect(Collectors.toCollection(LinkedHashSet::new));
        role.setPermissions(perms);
        roleRepository.save(role);
    }

    private void createAdminUserIfNotExist() {
        if (userRepository.findByUsername("admin").isEmpty()) {
            Role adminRole = roleRepository.findByName("ADMIN")
                    .orElseThrow(() -> new IllegalStateException("ADMIN role not found"));

            User adminUser = User.builder()
                    .username("admin")
                    .password(passwordEncoder.encode("admin123"))
                    .fullName("System Administrator")
                    .email("admin@vinfast.vn")
                    .phoneNumber("0900000000")
                    .mustChangePassword(false)
                    .roles(Set.of(adminRole))
                    .isActive(true)
                    .build();
            userRepository.save(adminUser);
            System.out.println(">>> Created Admin User (username: admin, password: admin123)");
        }
    }
}