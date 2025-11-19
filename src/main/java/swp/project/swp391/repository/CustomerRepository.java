package swp.project.swp391.repository;

import org.springframework.data.jpa.repository.JpaRepository;
import org.springframework.stereotype.Repository;
import swp.project.swp391.entity.Customer;


@Repository
public interface CustomerRepository extends JpaRepository<Customer, Long> {

    /** Kiểm tra tồn tại theo số điện thoại */
    boolean existsByPhoneNumber(String phoneNumber);

    /** Kiểm tra tồn tại theo CCCD/CMND */
    boolean existsByIdNumber(String idNumber);

    boolean existsByEmail(String email);

}
