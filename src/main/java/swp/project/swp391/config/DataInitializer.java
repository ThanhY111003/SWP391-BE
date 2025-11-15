package swp.project.swp391.config;

import lombok.RequiredArgsConstructor;
import org.springframework.boot.CommandLineRunner;
import org.springframework.core.annotation.Order;
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
@Order(1)
public class DataInitializer implements CommandLineRunner {
    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;
    private final RoleRepository roleRepository;
    private final PermissionRepository permissionRepository;
    private final DefaultRoleConfig defaultRoleConfig;

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

        if (!Boolean.TRUE.equals(adminRole.getIsCustomized())) {
            addOnlyAllPermissions(adminRole, pMap.values()); // ADMIN full access
        }
        if (!Boolean.TRUE.equals(evmStaffRole.getIsCustomized())) {
            addOnlyByNames(evmStaffRole, pMap, defaultRoleConfig.getDefaultPermissions("EVM_STAFF"));
        }
        if (!Boolean.TRUE.equals(dealerManagerRole.getIsCustomized())) {
            addOnlyByNames(dealerManagerRole, pMap, defaultRoleConfig.getDefaultPermissions("DEALER_MANAGER"));
        }
        if (!Boolean.TRUE.equals(dealerStaffRole.getIsCustomized())) {
            addOnlyByNames(dealerStaffRole, pMap, defaultRoleConfig.getDefaultPermissions("DEALER_STAFF"));
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
                {"user", "assignDealer", "Gán user vào dealer", "Gán user vào dealer"},

                // Role
                {"role", "read", "Xem role", "Xem thông tin role"},
                {"role", "update", "Cập nhật role", "Sửa permissions của role"},
                {"role", "assign", "Phân vai trò", "Gán role cho người dùng"},
                {"role", "unassign", "Gỡ vai trò", "Gỡ role khỏi người dùng"},
                {"role", "reset", "Reset role", "Reset role về cấu hình mặc định"},

                // Permission (để FE có quyền đọc danh sách permission)
                {"permission", "read", "Xem permission", "Xem danh sách quyền"},

                // Vehicle Instance
                {"vehicle", "read", "Xem xe", "Xem thông tin xe"},
                {"vehicle", "import", "import xe vào hệ thống qua excel", "import xe vào hệ thống qua excel"},
                {"vehicle", "read_all", "Xem tất cả xe", "Xem thông tin tất xe"},
                {"vehicle", "update_status", "Cập nhật trạng thái xe", "Cập nhật trạng thái xe (IN_STOCK ↔ RESERVED)"},
                {"vehicle", "deactive", "Vô hiệu hoá xe", "Vô hiệu hoá xe"},
                {"vehicle", "active", "Kích hoạt xe", "Kích hoạt lại xe"},
                {"vehicle", "assign_customer", "Gán xe cho khách hàng", "Gán xe cho khách hàng"},
                {"vehicle", "transfer", "Chuyển xe giữa các đại lý", "Chuyển xe giữa các đại lý"},
                {"vehicle", "create", "Tạo xe", "Thêm xe mới"},
                {"vehicle", "update", "Cập nhật xe", "Cập nhật thông tin xe"},

                // Vehicle Model Color
                {"vehicleModelColor", "create", "Gán màu xe", "Gán màu cho mẫu xe"},
                {"vehicleModelColor", "update", "Cập nhật giá màu xe", "Cập nhật giá của màu xe"},
                {"vehicleModelColor", "delete", "Gỡ màu xe", "Gỡ màu khỏi mẫu xe"},

                //Vehicle Model
                {"vehicleModel", "create", "Tạo mẫu xe", "Thêm mẫu xe mới"},
                {"vehicleModel", "read", "Xem mẫu xe", "Xem thông tin mẫu xe"},
                {"vehicleModel", "update", "Cập nhật mẫu xe", "Sửa thông tin mẫu xe"},
                {"vehicleModel", "viewAll", "Xem tất cả các mẫu xe", "Xem tất cả các mẫu xe"},
                {"vehicleModel", "inactive", "Vô hiệu hoá mẫu xe", "Vô hiệu hoá mẫu xe"},
                {"vehicleModel", "reactivate", "Kích hoạt lại mẫu xe", "Kích hoạt lại mẫu xe"},
                {"vehicleModel", "view", "Xem mẫu xe", "Xem mẫu xe"},

                // Color
                {"color", "create", "Tạo màu xe", "Thêm màu xe mới"},
                {"color", "update", "Cập nhật màu xe", "Sửa thông tin màu xe"},
                {"color", "inactive", "Vô hiệu hoá màu xe", "Vô hiệu hoá màu xe"},
                {"color", "reactive", "Kích hoạt màu xe", "Kích hoạt lại màu xe"},

                // Order
                {"order", "update", "Cập nhật đơn hàng", "Cập nhật trạng thái đơn"},
                {"order", "cancel", "Huỷ đơn hàng", "Dealer huỷ đơn hàng(chỉ huỷ được PEDING)"},
                {"order", "cancel_EVM", "Huỷ đơn hàng", "Đại lý huỷ đơn hàng(chỉ huỷ được PEDING)"},
                {"order", "approve", "Phê duyệt đơn hàng", "Phê duyệt đơn hàng từ trạng thái PENDING"},
                {"order", "read_all_EVM", "Xem tất cả đơn hàng", "Xem tất cả đơn hàng của tất cả đại lý"},
                {"order", "read_EVM", "Xem đơn hàng", "Xem đơn hàng cụ thể của một đại lý"},
                {"order", "read", "Xem đơn hàng của chính mình(đại lý)", "Xem đơn hàng của đại lý hiện tại"},
                {"order", "read_all", "Xem tất đơn hàng của chính mình(đại lý)", "Xem đơn hàng của đại lý hiện tại"},
                {"order", "create", "Tạo đơn hàng", "Tạo yêu cầu nhập xe"},
                {"order", "update_payment", "Cập nhật thanh toán", "Xác nhận thanh toán kỳ trả góp"},
                {"order", "cancel", "Hủy đơn hàng", "Hủy đơn hàng đang ở trạng thái CONFIRMED"},
                {"order", "ship", "Giao xe", "Xác nhận giao xe cho đại lý"},
                {"order", "receive", "Nhận xe", "Xác nhận nhận xe từ EVM"},
                {"order", "read_vehicle_EVM", "Xem xe trong đơn hàng", "Xem danh sách xe trong đơn hàng"},
                {"vehicle","report_defect","Báo cáo xe lỗi","Báo cáo xe lỗi từ đại lý"},
                {"order", "read_vehicle", "Xem xe trong đơn hàng dành cho đại lý", "Xem danh sách xe trong đơn hàng dành cho đại lý"},
                {"defect", "approve", "Phê duyệt báo cáo xe lỗi", "Phê duyệt báo cáo xe lỗi từ đại lý"},
                {"defect", "read", "Xem báo cáo xe lỗi", "Xem danh sách báo cáo xe lỗi"},
                {"defect", "repair_complete", "Xác nhận sửa xong xe lỗi", "Xác nhận đã sửa xong xe lỗi"},
                {"vehicle", "receive_repair", "Xác nhận nhận lại xe sau sửa chữa", "Xác nhận nhận lại xe sau khi sửa chữa xong"},
                {"order", "attach_vehicle", "gắn xe cho đơn hàng", "gắn xe cụ thể cho đơn hàng"},
                {"order", "manual_pay", "Nhập thanh toán trả thẳng", "Nhập số tiền dealer đã thanh toán cho đơn trả thẳng"},
                {"order", "deposit_confirm", "Xác nhận tiền cọc", "Xác nhận đã nhận tiền cọc từ dealer"},

                // Dealer
                {"dealer", "create", "Tạo đại lý", "Thêm đại lý mới"},
                {"dealer", "read", "Xem đại lý", "Xem thông tin đại lý"},
                {"dealer", "update", "Cập nhật đại lý", "Sửa thông tin đại lý"},
                {"dealer", "inactive", "Vô hiệu hoá đại lý", "Vô hiệu hoá đại lý"},
                {"dealer", "reactivate", "Kích hoạt đại lý", "Kích hoạt lại đại lý"},
                {"dealer", "read.all", "Xem đại lý", "Xem thông tin đại lý"},

                {"dealerLevel", "create", "Tạo cấp độ đại lý", "Thêm cấp độ đại lý mới"},
                {"dealerLevel", "update", "Cập nhật cấp độ đại lý", "Sửa thông tin cấp độ đại lý"},
                {"dealerLevel", "delete", "Xoá cấp độ đại lý", "Xoá cấp độ đại lý"},
                {"dealerLevel", "read", "Xem cấp độ đại lý", "Xem thông tin cấp độ"},

                // Inventory (giữ nguyên chỉ read theo thiết kế hiện tại)
                {"inventory", "read", "Xem tồn kho", "Xem số lượng tồn kho"},

                // Customer
                {"customer", "create", "Tạo khách hàng", "Thêm thông tin khách hàng"},
                {"customer", "read", "Xem khách hàng", "Xem thông tin khách hàng"},
                {"customer", "update", "Cập nhật khách hàng", "Sửa thông tin khách hàng"},
                {"customer", "activate", "activate khách hàng ", "activate khách hàng"},
                {"customer", "deactivate", "deactivate khách hàng", "deactivate khách hàng"},

                // Vehicle Price
                {"vehicle_price", "create", "Tạo bảng giá xe", "Thêm mới bảng giá xe"},
                {"vehicle_price", "read", "Xem bảng giá xe cho hãng", "Xem thông tin bảng giá xe cho hãng"},
                {"vehicle_price", "manage_all", "Quản lý tất cả bảng giá xe cho hãng", "Xem và quản lý tất cả bảng giá xe cho hãng"},
                {"vehicle_price", "update", "Sửa bảng giá xe", "Sửa bảng giá xe"},
                {"vehicle_price", "deactivate", "Vô hiệu hoá bảng giá xe", "Vô hiệu hoá bảng giá xe"},
                {"vehicle_price", "activate", "Kích hoạt bảng giá xe", "Kích hoạt bảng giá xe"},

                // Warranty
                {"warranty", "create", "Tạo yêu cầu bảo hành/sửa chữa", "Tạo yêu cầu bảo hành/sửa chữa cho xe"},
                {"warranty", "read", "Xem yêu cầu bảo hành/sửa chữa", "Xem thông tin yêu cầu bảo hành/sửa chữa của xe"},
                {"warranty", "read_all", "Xem tất cả yêu cầu bảo hành/sửa chữa", "Xem tất cả yêu cầu bảo hành/sửa chữa từ các đại lý"},
                {"warranty", "approve", "Phê duyệt yêu cầu bảo hành/sửa chữa", "Phê duyệt yêu cầu bảo hành/sửa chữa từ đại lý"},
                {"warranty", "complete", "Hoàn thành yêu cầu bảo hành/sửa chữa", "Đánh dấu hoàn thành yêu cầu bảo hành/sửa chữa"},
                {"warranty", "confirm", "Xác nhận đã nhận xe sau bảo hành/sửa chữa", "Xác nhận đã nhận xe sau khi hoàn thành bảo hành/sửa chữa"},
                {"warranty", "reject", "Từ chối yêu cầu bảo hành/sửa chữa", "Từ chối yêu cầu bảo hành/sửa chữa từ đại lý"},
                {"warranty", "cancel", "Hủy yêu cầu bảo hành/sửa chữa", "Hủy yêu cầu bảo hành/sửa chữa từ đại lý"},

                // Report
                {"admin_report", "read", "Xem báo cáo toàn hệ thống", "Xem các báo cáo toàn hệ thống"},
                {"dealer_report", "read", "Xem báo cáo dành cho dealer Manager", "Xem các báo cáo dành cho dealer Manager"},
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
