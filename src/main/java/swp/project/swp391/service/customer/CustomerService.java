package swp.project.swp391.service.customer;

import swp.project.swp391.request.customer.CreateCustomerRequest;
import swp.project.swp391.request.customer.UpdateCustomerRequest;
import swp.project.swp391.response.customer.CustomerDetailResponse;
import swp.project.swp391.response.customer.CustomerResponse;
import swp.project.swp391.response.vehicle.CustomerVehicleResponse;

import java.util.List;

public interface CustomerService {
    List<CustomerResponse> getAll();
    CustomerDetailResponse getById(Long id);
    CustomerResponse create(CreateCustomerRequest request);
    CustomerResponse update(Long id, UpdateCustomerRequest request);
    void deactivate(Long id);
    void activate(Long id);
}
