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

import java.util.Arrays;
import java.util.List;
import java.util.Set;
import java.util.HashSet;

@Component
@RequiredArgsConstructor
public class DataInitializer implements CommandLineRunner {
    // Thêm các dependency này
    private final UserRepository userRepository;
    private final PasswordEncoder passwordEncoder;
    private final RoleRepository roleRepository;
    private final PermissionRepository permissionRepository;

    @Override
    @Transactional
    public void run(String... args) throws Exception {
        initializeRolesAndPermissions();
    }

    private void initializeRolesAndPermissions() {
        // Tạo permissions cơ bản
        createPermissionsIfNotExist();
        
        // Tạo roles cơ bản
        createRolesIfNotExist();

        // Tạo user admin nếu chưa tồn tại
        createAdminUserIfNotExist();
    }

    private void createPermissionsIfNotExist() {
        List<String[]> permissions = List.of(
                new String[]{"user", "create", "Tạo người dùng", "Thêm mới người dùng"},
                new String[]{"user", "read", "Xem người dùng", "Xem thông tin người dùng"},
                new String[]{"user", "update", "Cập nhật người dùng", "Sửa thông tin người dùng"},
                new String[]{"user", "delete", "Xóa người dùng", "Xóa người dùng"},

                new String[]{"customer", "create", "Tạo khách hàng", "Thêm mới khách hàng"},
                new String[]{"customer", "read", "Xem khách hàng", "Xem thông tin khách hàng"},
                new String[]{"customer", "update", "Cập nhật khách hàng", "Sửa thông tin khách hàng"},
                new String[]{"customer", "delete", "Xóa khách hàng", "Xóa khách hàng"},

                new String[]{"vehicle", "create", "Tạo xe", "Thêm mới xe"},
                new String[]{"vehicle", "read", "Xem xe", "Xem thông tin xe"},
                new String[]{"vehicle", "update", "Cập nhật xe", "Sửa thông tin xe"},
                new String[]{"vehicle", "delete", "Xóa xe", "Xóa xe"},

                new String[]{"order", "create", "Tạo đơn hàng", "Thêm mới đơn hàng"},
                new String[]{"order", "read", "Xem đơn hàng", "Xem thông tin đơn hàng"},
                new String[]{"order", "update", "Cập nhật đơn hàng", "Sửa thông tin đơn hàng"},
                new String[]{"order", "delete", "Xóa đơn hàng", "Xóa đơn hàng"},

                new String[]{"dealer", "create", "Tạo đại lý", "Thêm mới đại lý"},
                new String[]{"dealer", "read", "Xem đại lý", "Xem thông tin đại lý"},
                new String[]{"dealer", "update", "Cập nhật đại lý", "Sửa thông tin đại lý"},
                new String[]{"dealer", "delete", "Xóa đại lý", "Xóa đại lý"},

                new String[]{"report", "read", "Xem báo cáo", "Xem các báo cáo"},
                new String[]{"report", "export", "Xuất báo cáo", "Xuất báo cáo ra file"}
        );

        for (String[] p : permissions) {
            String resource = p[0], action = p[1];
            String displayName = p[2], description = p[3];
            String name = resource + "." + action;

            createPermissionIfNotExist(name, displayName, description, resource, action);
        }
    }


    private Permission createPermissionIfNotExist(String name, String displayName, String description, String resource, String action) {
        return permissionRepository.findByName(name)
                .orElseGet(() -> {
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

    private void createRolesIfNotExist() {
        // Tạo ADMIN role
        Role adminRole = roleRepository.findByName("ADMIN")
                .orElse(Role.builder()
                        .name("ADMIN")
                        .displayName("Administrator")
                        .description("Full system access")
                        .isActive(true)
                        .build());
        
        if (adminRole.getId() == null) {
            Set<Permission> allPermissions = new HashSet<>(permissionRepository.findAll());
            adminRole.setPermissions(allPermissions);
            roleRepository.save(adminRole);
        }

        // Tạo EVM_STAFF role
        Role evmStaffRole = roleRepository.findByName("EVM_STAFF")
                .orElse(Role.builder()
                        .name("EVM_STAFF")
                        .displayName("EVM Staff")
                        .description("Electric Vehicle Manufacturer Staff")
                        .isActive(true)
                        .build());
        
        if (evmStaffRole.getId() == null) {
            Set<Permission> evmPermissions = new HashSet<>(permissionRepository.findByResourceIn(Arrays.asList("vehicle", "order", "dealer", "report")));
            evmStaffRole.setPermissions(evmPermissions);
            roleRepository.save(evmStaffRole);
        }

        // Tạo DEALER_MANAGER role
        Role dealerManagerRole = roleRepository.findByName("DEALER_MANAGER")
                .orElse(Role.builder()
                        .name("DEALER_MANAGER")
                        .displayName("Dealer Manager")
                        .description("Dealer Manager")
                        .isActive(true)
                        .build());
        
        if (dealerManagerRole.getId() == null) {
            Set<Permission> dealerManagerPermissions = new HashSet<>(permissionRepository.findByResourceIn(Arrays.asList("order", "customer", "report")));
            dealerManagerRole.setPermissions(dealerManagerPermissions);
            roleRepository.save(dealerManagerRole);
        }

        // Tạo DEALER_STAFF role
        Role dealerStaffRole = roleRepository.findByName("DEALER_STAFF")
                .orElse(Role.builder()
                        .name("DEALER_STAFF")
                        .displayName("Dealer Staff")
                        .description("Dealer Staff")
                        .isActive(true)
                        .build());
        
        if (dealerStaffRole.getId() == null) {
            Set<Permission> dealerStaffPermissions = new HashSet<>(permissionRepository.findByResourceIn(Arrays.asList("order", "customer")));
            dealerStaffRole.setPermissions(dealerStaffPermissions);
            roleRepository.save(dealerStaffRole);
        }

        // Tạo USER role (default)
        Role userRole = roleRepository.findByName("USER")
                .orElse(Role.builder()
                        .name("USER")
                        .displayName("User")
                        .description("Basic user")
                        .isActive(true)
                        .build());
        
        if (userRole.getId() == null) {
            Set<Permission> userPermissions = new HashSet<>(permissionRepository.findByResourceIn(Arrays.asList("order", "customer")));
            userRole.setPermissions(userPermissions);
            roleRepository.save(userRole);
        }
    }

    private void createAdminUserIfNotExist() {
        // Kiểm tra xem đã có người dùng admin chưa để tránh tạo trùng lặp
        if (userRepository.findByUsername("admin").isEmpty()) {
            Role adminRole = roleRepository.findByName("ADMIN")
                    .orElseThrow(() -> new IllegalStateException("Vai trò ADMIN không được tìm thấy."));

            User adminUser = User.builder()
                    .username("admin")
                    .password(passwordEncoder.encode("admin123")) // Mã hóa mật khẩu
                    .email("admin@example.com")
                    .isVerified(true)
                    .roles(Set.of(adminRole))
                    .isActive(true)
                    .build();
            userRepository.save(adminUser);
            System.out.println("Đã tạo người dùng Admin thành công.");
        }
    }
}
