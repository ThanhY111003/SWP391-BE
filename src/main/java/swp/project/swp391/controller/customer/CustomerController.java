package swp.project.swp391.controller.customer;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.request.customer.CreateCustomerRequest;
import swp.project.swp391.request.customer.UpdateCustomerRequest;
import swp.project.swp391.response.customer.CustomerDetailResponse;
import swp.project.swp391.response.customer.CustomerResponse;
import swp.project.swp391.response.vehicle.CustomerVehicleResponse;
import swp.project.swp391.service.customer.CustomerService;

import java.util.List;

@RestController
@RequestMapping("/api/customers")
@RequiredArgsConstructor
@Tag(name = "Quản lý khách hàng (Customer)", description = "Các API cho phép đại lý quản lý thông tin khách hàng và xe đã mua")
public class CustomerController {

    private final CustomerService customerService;

    @GetMapping
    @Operation(summary = "Lấy toàn bộ khách hàng (không lọc)")
    public ResponseEntity<ApiResponse<List<CustomerResponse>>> getAll() {
        var list = customerService.getAll();
        return ResponseEntity.ok(ApiResponse.ok(list, "Lấy danh sách khách hàng thành công"));
    }


    @Operation(summary = "Lấy chi tiết khách hàng (bao gồm xe đã mua)")
    @GetMapping("/{id}")
    public ResponseEntity<ApiResponse<CustomerDetailResponse>> getById(@PathVariable Long id) {
        return ResponseEntity.ok(ApiResponse.ok(customerService.getById(id), "Lấy thông tin khách hàng thành công"));
    }


    @Operation(summary = "Tạo mới khách hàng")
    @PostMapping
    public ResponseEntity<ApiResponse<CustomerResponse>> create(@Valid @RequestBody CreateCustomerRequest request) {
        return ResponseEntity.ok(ApiResponse.ok(customerService.create(request), "Tạo khách hàng thành công"));
    }

    @Operation(summary = "Cập nhật thông tin khách hàng")
    @PutMapping("/{id}")
    public ResponseEntity<ApiResponse<CustomerResponse>> update(
            @PathVariable Long id,
            @Valid @RequestBody UpdateCustomerRequest request) {
        return ResponseEntity.ok(ApiResponse.ok(customerService.update(id, request), "Cập nhật thông tin khách hàng thành công"));
    }

    @Operation(summary = "Vô hiệu hóa khách hàng")
    @PatchMapping("/{id}/deactivate")
    public ResponseEntity<ApiResponse<Void>> deactivate(@PathVariable Long id) {
        customerService.deactivate(id);
        return ResponseEntity.ok(ApiResponse.okMsg("Vô hiệu hóa khách hàng thành công"));
    }

    @Operation(summary = "Kích hoạt lại khách hàng")
    @PatchMapping("/{id}/activate")
    public ResponseEntity<ApiResponse<Void>> activate(@PathVariable Long id) {
        customerService.activate(id);
        return ResponseEntity.ok(ApiResponse.okMsg("Kích hoạt khách hàng thành công"));
    }

}
