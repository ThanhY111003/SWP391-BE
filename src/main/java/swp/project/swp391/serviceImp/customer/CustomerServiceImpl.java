package swp.project.swp391.serviceImp.customer;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.Customer;
import swp.project.swp391.entity.CustomerVehicle;
import swp.project.swp391.entity.User;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.CustomerRepository;
import swp.project.swp391.repository.CustomerVehicleRepository;
import swp.project.swp391.response.customer.CustomerDetailResponse;
import swp.project.swp391.response.customer.CustomerResponse;
import swp.project.swp391.response.vehicle.CustomerVehicleResponse;
import swp.project.swp391.request.customer.CreateCustomerRequest;
import swp.project.swp391.request.customer.UpdateCustomerRequest;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.customer.CustomerService;

import java.time.LocalDate;
import java.util.List;
import java.util.stream.Collectors;

@Service
@RequiredArgsConstructor
@Transactional
public class CustomerServiceImpl implements CustomerService {

    private final CustomerRepository customerRepo;
    private final CustomerVehicleRepository customerVehicleRepo;
    private final RbacGuard guard;

    // --------------------------------------------------------
    // GET ALL
    // --------------------------------------------------------
    @Override
    public List<CustomerResponse> getAll() {
        guard.require(guard.has(guard.me(), "customer.read"));

        return customerRepo.findAll().stream()
                .map(CustomerResponse::fromEntity)
                .collect(Collectors.toList());
    }

    // --------------------------------------------------------
    // GET BY ID
    // --------------------------------------------------------
    @Override
    public CustomerDetailResponse getById(Long id) {
        guard.require(guard.has(guard.me(), "customer.read"));

        Customer c = customerRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.CUSTOMER_NOT_FOUND));

        return CustomerDetailResponse.fromEntity(c);
    }


    // --------------------------------------------------------
    // CREATE
    // --------------------------------------------------------
    @Override
    public CustomerResponse create(CreateCustomerRequest req) {
        guard.require(guard.has(guard.me(), "customer.create"));

        // Kiểm tra trùng SĐT hoặc CCCD
        if (customerRepo.existsByPhoneNumber(req.getPhoneNumber())) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Số điện thoại đã tồn tại.");
        }
        if (customerRepo.existsByIdNumber(req.getIdNumber())) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Số CMND/CCCD đã tồn tại.");
        }
        if (customerRepo.existsByEmail(req.getEmail())) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Email đã tồn tại.");
        }


        Customer c = Customer.builder()
                .fullName(req.getFullName())
                .phoneNumber(req.getPhoneNumber())
                .email(req.getEmail())
                .idNumber(req.getIdNumber())
                .dateOfBirth(LocalDate.parse(req.getDateOfBirth()))
                .gender(Customer.Gender.valueOf(req.getGender().toUpperCase()))
                .address(req.getAddress())
                .notes(req.getNotes())
                .isActive(true)
                .build();

        return CustomerResponse.fromEntity(customerRepo.save(c));
    }

    // --------------------------------------------------------
    // UPDATE
    // --------------------------------------------------------
    @Override
    public CustomerResponse update(Long id, UpdateCustomerRequest req) {
        guard.require(guard.has(guard.me(), "customer.update"));

        Customer c = customerRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.CUSTOMER_NOT_FOUND));

        // --- Kiểm tra trùng thông tin duy nhất ---
        if (req.getPhoneNumber() != null
                && !req.getPhoneNumber().equals(c.getPhoneNumber())
                && customerRepo.existsByPhoneNumber(req.getPhoneNumber())) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Số điện thoại đã tồn tại.");
        }

        if (req.getEmail() != null
                && !req.getEmail().equalsIgnoreCase(c.getEmail())
                && customerRepo.existsByEmail(req.getEmail())) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Email đã tồn tại.");
        }

        if (req.getIdNumber() != null
                && !req.getIdNumber().equals(c.getIdNumber())
                && customerRepo.existsByIdNumber(req.getIdNumber())) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Số CMND/CCCD đã tồn tại.");
        }

        // --- Cập nhật thông tin cơ bản ---
        if (req.getFullName() != null) c.setFullName(req.getFullName());
        if (req.getPhoneNumber() != null) c.setPhoneNumber(req.getPhoneNumber());
        if (req.getEmail() != null) c.setEmail(req.getEmail());
        if (req.getIdNumber() != null) c.setIdNumber(req.getIdNumber());

        if (req.getDateOfBirth() != null && !req.getDateOfBirth().isBlank()) {
            try {
                c.setDateOfBirth(LocalDate.parse(req.getDateOfBirth()));
            } catch (Exception e) {
                throw new BaseException(ErrorHandler.INVALID_REQUEST, "Ngày sinh không hợp lệ, định dạng yyyy-MM-dd.");
            }
        }

        if (req.getGender() != null) {
            try {
                c.setGender(Customer.Gender.valueOf(req.getGender().toUpperCase()));
            } catch (IllegalArgumentException ex) {
                throw new BaseException(ErrorHandler.INVALID_REQUEST, "Giới tính phải là MALE, FEMALE hoặc OTHER.");
            }
        }

        if (req.getAddress() != null) c.setAddress(req.getAddress());
        if (req.getNotes() != null) c.setNotes(req.getNotes());

        customerRepo.save(c);
        return CustomerResponse.fromEntity(c);
    }


    // --------------------------------------------------------
    // DEACTIVATE
    // --------------------------------------------------------
    @Override
    public void deactivate(Long id) {
        guard.require(guard.has(guard.me(), "customer.deactivate"));

        Customer c = customerRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.CUSTOMER_NOT_FOUND));

        if (Boolean.FALSE.equals(c.getIsActive())) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Khách hàng đã bị vô hiệu hoá.");
        }

        c.setIsActive(false);
        customerRepo.save(c);
    }

    // --------------------------------------------------------
    // ACTIVATE
    // --------------------------------------------------------
    @Override
    public void activate(Long id) {
        guard.require(guard.has(guard.me(), "customer.activate"));

        Customer c = customerRepo.findById(id)
                .orElseThrow(() -> new BaseException(ErrorHandler.CUSTOMER_NOT_FOUND));

        if (Boolean.TRUE.equals(c.getIsActive())) {
            throw new BaseException(ErrorHandler.INVALID_REQUEST, "Khách hàng đang hoạt động.");
        }

        c.setIsActive(true);
        customerRepo.save(c);
    }

}
