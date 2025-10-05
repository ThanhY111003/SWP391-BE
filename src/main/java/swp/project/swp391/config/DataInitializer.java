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
        // 1) Upsert toàn bộ permissions (tạo mới hoặc cập nhật metadata nếu đã tồn tại)
        upsertPermissions();

        // 2) Upsert roles (chỉ tạo nếu chưa có)
        Role adminRole         = upsertRoleIfNeeded("ADMIN",         "Administrator",  "Full system access");
        Role evmStaffRole      = upsertRoleIfNeeded("EVM_STAFF",     "EVM Staff",      "Electric Vehicle Manufacturer Staff");
        Role dealerManagerRole = upsertRoleIfNeeded("DEALER_MANAGER","Dealer Manager", "Dealer Manager");
        Role dealerStaffRole   = upsertRoleIfNeeded("DEALER_STAFF",  "Dealer Staff",   "Dealer Staff");

        // map name -> Permission
        Map<String, Permission> pMap = permissionRepository.findAll().stream()
                .collect(Collectors.toMap(Permission::getName, p -> p));

        // 3) Seed add-only nếu role CHƯA customized
        if (!Boolean.TRUE.equals(adminRole.getIsCustomized())) {
            addOnlyAllPermissions(adminRole, pMap.values()); // ADMIN = full access (hợp nhất, không replace)
        }

        if (!Boolean.TRUE.equals(evmStaffRole.getIsCustomized())) {
            addOnlyByNames(evmStaffRole, pMap, List.of(
                    "user.create", "user.read", "user.update", "user.inactive", "user.reactivate",
                    "vehicle.create", "vehicle.read", "vehicle.update",
                    "vehicleModel.create", "vehicleModel.update",
                    "color.update", "color.create","color.inactive","color.reactive",
                    "vehicleModelColor.create","vehicleModelColor.update","vehicleModelColor.delete",
                    "order.read", "order.update",
                    "dealer.create", "dealer.read", "dealer.update",
                    "dealerLevel.read",
                    "inventory.read",
                    "report.read", "report.export"
            ));
        }

        if (!Boolean.TRUE.equals(dealerManagerRole.getIsCustomized())) {
            addOnlyByNames(dealerManagerRole, pMap, List.of(
                    "user.create", "user.read", "user.update", "user.inactive", "user.reactivate",
                    "order.create", "order.read", "order.update",
                    "inventory.read",
                    "customer.create", "customer.read", "customer.update",
                    "vehicle.read",
                    "vehicleModel.read",
                    "report.read"
            ));
        }

        if (!Boolean.TRUE.equals(dealerStaffRole.getIsCustomized())) {
            addOnlyByNames(dealerStaffRole, pMap, List.of(
                    "order.create", "order.read",
                    "inventory.read",
                    "customer.create", "customer.read",
                    "vehicle.read",
                    "vehicleModel.read"
            ));
        }

        // 4) Admin user
        createAdminUserIfNotExist();
    }

    /* ===================== Permissions seed (UPSERT) ===================== */

    private void upsertPermissions() {
        String[][] base = new String[][]{
                // User
                {"user", "create", "Tạo người dùng", "Thêm mới người dùng"},
                {"user", "read", "Xem người dùng", "Xem thông tin người dùng"},
                {"user", "update", "Cập nhật người dùng", "Sửa thông tin người dùng"},
                {"user", "inactive", "Vô hiệu hoá người dùng", "Vô hiệu hoá người dùng"},
                {"user", "reactivate", "Kích hoạt người dùng", "Kích hoạt người dùng"},

                // Role
                {"role", "read", "Xem role", "Xem thông tin role"},
                {"role", "update", "Cập nhật role", "Sửa permissions của role"},
                {"role", "assign", "Phân vai trò", "Gán role cho người dùng"},
                {"role", "unassign", "Gỡ vai trò", "Gỡ role khỏi người dùng"},
                {"role", "reset", "Reset role", "Reset role về cấu hình mặc định"},

                // Permission (để FE có quyền đọc danh sách permission)
                {"permission", "read", "Xem permission", "Xem danh sách quyền"},

                // Vehicle Instance
                {"vehicle", "create", "Tạo xe", "Thêm xe vào hệ thống"},
                {"vehicle", "update", "Cập nhật xe", "Sửa thông tin xe"},

                // Vehicle Model Color
                {"vehicleModelColor", "create", "Gán màu xe", "Gán màu cho mẫu xe"},
                {"vehicleModelColor", "update", "Cập nhật giá màu xe", "Cập nhật giá của màu xe"},
                {"vehicleModelColor", "delete", "Gỡ màu xe", "Gỡ màu khỏi mẫu xe"},

                //Vehicle Model
                {"vehicleModel", "create", "Tạo mẫu xe", "Thêm mẫu xe mới"},
                {"vehicleModel", "read", "Xem mẫu xe", "Xem thông tin mẫu xe"},
                {"vehicleModel", "update", "Cập nhật mẫu xe", "Sửa thông tin mẫu xe"},

                // Color
                {"color", "create", "Tạo màu xe", "Thêm màu xe mới"},
                {"color", "update", "Cập nhật màu xe", "Sửa thông tin màu xe"},
                {"color", "inactive", "Vô hiệu hoá màu xe", "Vô hiệu hoá màu xe"},
                {"color", "reactive", "Kích hoạt màu xe", "Kích hoạt lại màu xe"},

                // Order
                {"order", "create", "Tạo đơn hàng", "Tạo đơn đặt xe"},
                {"order", "read", "Xem đơn hàng", "Xem thông tin đơn hàng"},
                {"order", "update", "Cập nhật đơn hàng", "Cập nhật trạng thái đơn"},

                // Dealer
                {"dealer", "create", "Tạo đại lý", "Thêm đại lý mới"},
                {"dealer", "read", "Xem đại lý", "Xem thông tin đại lý"},
                {"dealer", "update", "Cập nhật đại lý", "Sửa thông tin đại lý"},
                {"dealer", "inactive", "Vô hiệu hoá đại lý", "Vô hiệu hoá đại lý"},
                {"dealer", "reactivate", "Kích hoạt đại lý", "Kích hoạt lại đại lý"},

                // Dealer Level (giữ nguyên chỉ read theo thiết kế hiện tại)
                {"dealerLevel", "read", "Xem cấp độ đại lý", "Xem thông tin cấp độ"},

                // Inventory (giữ nguyên chỉ read theo thiết kế hiện tại)
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
            String resource    = p[0];
            String action      = p[1];
            String displayName = p[2];
            String description = p[3];
            String name        = resource + "." + action;

            permissionRepository.findByName(name).map(exist -> {
                boolean changed = false;
                if (!Objects.equals(exist.getDisplayName(), displayName)) { exist.setDisplayName(displayName); changed = true; }
                if (!Objects.equals(exist.getDescription(), description)) { exist.setDescription(description); changed = true; }
                if (!Objects.equals(exist.getResource(), resource))       { exist.setResource(resource);       changed = true; }
                if (!Objects.equals(exist.getAction(), action))           { exist.setAction(action);           changed = true; }
                if (!Boolean.TRUE.equals(exist.getIsActive()))            { exist.setIsActive(true);           changed = true; }
                return changed ? permissionRepository.save(exist) : exist;
            }).orElseGet(() -> {
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
    }


    /* ===================== Role seed helpers ===================== */

    private Role upsertRoleIfNeeded(String name, String displayName, String description) {
        return roleRepository.findByName(name).orElseGet(() -> {
            Role r = Role.builder()
                    .name(name)
                    .displayName(displayName)
                    .description(description)
                    .isActive(true)
                    .isCustomized(false) // default: chưa custom
                    .build();
            return roleRepository.save(r);
        });
    }

    // Add-only theo danh sách permission names
    private void addOnlyByNames(Role role, Map<String, Permission> all, List<String> names) {
        if (role.getPermissions() == null) {
            role.setPermissions(new LinkedHashSet<>());
        }
        Set<Long> have = role.getPermissions().stream().map(Permission::getId).collect(Collectors.toSet());

        for (String n : names) {
            Permission p = all.get(n);
            if (p == null) {
                System.out.println("WARNING: Permission not found: " + n);
                continue;
            }
            if (!have.contains(p.getId())) {
                role.getPermissions().add(p);
                have.add(p.getId());
            }
        }
        roleRepository.save(role);
    }

    // Add-only: hợp nhất tất cả permissions vào role (dùng cho ADMIN)
    private void addOnlyAllPermissions(Role role, Collection<Permission> permissions) {
        if (role.getPermissions() == null) {
            role.setPermissions(new LinkedHashSet<>());
        }
        Set<Long> have = role.getPermissions().stream().map(Permission::getId).collect(Collectors.toSet());
        for (Permission p : permissions) {
            if (!have.contains(p.getId())) {
                role.getPermissions().add(p);
                have.add(p.getId());
            }
        }
        roleRepository.save(role);
    }

    /* ===================== Admin user ===================== */

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
