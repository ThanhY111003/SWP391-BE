package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.Customer;

import java.util.List;
import java.util.Optional;

@Repository
public interface CustomerRepository extends JpaRepository<Customer, Long> {

    /** Tìm khách hàng theo số điện thoại */
    Optional<Customer> findByPhoneNumber(String phoneNumber);

    /** Tìm khách hàng theo CCCD/CMND */
    Optional<Customer> findByIdNumber(String idNumber);

    /** Kiểm tra tồn tại theo số điện thoại */
    boolean existsByPhoneNumber(String phoneNumber);

    /** Kiểm tra tồn tại theo CCCD/CMND */
    boolean existsByIdNumber(String idNumber);

    /** Tìm tất cả khách hàng có tên chứa chuỗi (ignore case) */
    List<Customer> findByFullNameContainingIgnoreCase(String name);

    /** Tìm tất cả khách hàng có số điện thoại chứa chuỗi */
    List<Customer> findByPhoneNumberContaining(String phone);

    boolean existsByEmail(String email);

}
