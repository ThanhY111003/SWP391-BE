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
                    "vehicle.read", "vehicle.read_all", "vehicle.assign_customer", "vehicle.deactive", "vehicle.active","vehicle.update_status","vehicle.transfer","vehicle.create","vehicle.update","vehicle.import",
                    "vehicleModel.create","vehicleModel.update","vehicleModel.update","vehicleModel.viewAll","vehicleModel.inactive","vehicleModel.reactivate","vehicleModel.view",
                    "color.update","color.create","color.inactive","color.reactive",
                    "vehicleModelColor.create","vehicleModelColor.update","vehicleModelColor.delete",
                    "order.approve","order.update_payment","order.read_EVM","order.read_all_EVM","order.cancel_EVM","order.ship","order.read_vehicle_EVM","defect.approve","order.attach_vehicle",
                    "order.manual_pay","order.deposit_confirm",
                    "defect.read","defect.repair_complete",
                    "dealer.create","dealer.read","dealer.update","dealer.read.all","dealer.inactive","dealer.reactivate",
                    "dealerLevel.read","dealerLevel.create","dealerLevel.update","dealerLevel.delete",
                    "customer.create","customer.read","customer.update","customer.activate","customer.deactivate",
                    "warranty.read_all","warranty.complete","warranty.approve","warranty.reject",
                    "vehicle_price.read","vehicle_price.manage_all","vehicle_price.create","vehicle_price.update","vehicle_price.deactivate","vehicle_price.activate",
                    "inventory.read"
            ),
            "DEALER_MANAGER", List.of(
                    "user.create","user.read","user.update","user.inactive","user.reactivate",
                    "order.create","order.read","order.read_all","order.cancel","order.read_vehicle","order.receive","vehicle.report_defect",
                    "defect.read","vehicle.receive_repair","warranty.cancel",
                    "inventory.read",
                    "dealer.read",
                    "customer.create","customer.read","customer.update",
                    "vehicle.read","vehicle.assign_customer","vehicle.read_all","vehicle.update_status",
                    "vehicleModel.read","vehicleModel.viewAll",
                    "vehicle_price.read",
                    "warranty.create","warranty.read","warranty.confirm",
                    "dealer_report.read"
            ),
            "DEALER_STAFF", List.of(
                    "user.read","user.update",
                    "order.create","order.read","order.read_all","order.cancel","order.read_vehicle","order.receive","vehicle.report_defect",
                    "defect.read","vehicle.receive_repair","warranty.cancel",
                    "inventory.read",
                    "dealer.read",
                    "customer.create","customer.read","customer.update",
                    "vehicle.read","vehicle.assign_customer","vehicle.read_all","vehicle.update_status",
                    "vehicleModel.read","vehicleModel.viewAll",
                    "warranty.create","warranty.read","warranty.confirm",
                    "vehicle_price.read"
            )
    );

    public List<String> getDefaultPermissions(String roleName) {
        return defaults.getOrDefault(roleName, List.of());
    }
}
