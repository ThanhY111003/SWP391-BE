// config/DefaultRoleConfig.java
package swp.project.swp391.config;

import org.springframework.stereotype.Component;

import java.util.List;
import java.util.Map;

@Component
public class DefaultRoleConfig {

    // Khai báo danh sách quyền MẶC ĐỊNH theo TÊN (trùng với permission.name trong DB)
    private final Map<String, List<String>> defaults = Map.of(
            "ADMIN", List.of( // ADMIN có thể để trống vì bạn đã add-all trong seeder; hoặc liệt kê đầy đủ nếu muốn
                    // "user.create", "user.read", ...
            ),
            "EVM_STAFF", List.of(
                    "user.create","user.read","user.update","user.inactive","user.reactivate","user.assignDealer",
                    "vehicle.read", "vehicle.read_all", "vehicle.assign_customer", "vehicle.deactive", "vehicle.active","vehicle.update_status",
                    "vehicleModel.create","vehicleModel.update","vehicleModel.update","vehicleModel.viewAll","vehicleModel.inactive","vehicleModel.reactivate","vehicleModel.view",
                    "color.update","color.create","color.inactive","color.reactive",
                    "vehicleModelColor.create","vehicleModelColor.update","vehicleModelColor.delete",
                    "order.approve","order.update_payment","order.read_EVM","order.read_all_EVM","order.cancel",
                    "dealer.create","dealer.read","dealer.update","dealer.read.all","dealer.inactive","dealer.reactivate",
                    "dealerLevel.read","dealerLevel.create","dealerLevel.update","dealerLevel.delete",
                    "customer.create","customer.read","customer.update","customer.activate","customer.deactivate",
                    "inventory.read","report.read","report.export"
            ),
            "DEALER_MANAGER", List.of(
                    "user.create","user.read","user.update","user.inactive","user.reactivate",
                    "order.create","order.read","order.read_all",
                    "inventory.read",
                    "customer.create","customer.read","customer.update",
                    "vehicle.read","vehicle.assign_customer",
                    "vehicleModel.read","vehicleModel.viewAll",
                    "report.read"
            ),
            "DEALER_STAFF", List.of(
                    "user.read","user.update",
                    "order.create","order.read","order.read_all",
                    "inventory.read",
                    "customer.create","customer.read","customer.update",
                    "vehicle.read","vehicle.assign_customer",
                    "vehicleModel.read","vehicleModel.viewAll"
            )
    );

    public List<String> getDefaultPermissions(String roleName) {
        return defaults.getOrDefault(roleName, List.of());
    }
}
