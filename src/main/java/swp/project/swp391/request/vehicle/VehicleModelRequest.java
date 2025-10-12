package swp.project.swp391.request.vehicle;

import jakarta.validation.constraints.*;
import lombok.Data;

import java.math.BigDecimal;

@Data
public class VehicleModelRequest {

    @NotBlank(message = "Tên model xe không được để trống.")
    private String name;

    @NotBlank(message = "Mã model không được để trống.")
    private String modelCode; // unique

    private String description;

    @NotBlank(message = "Thương hiệu xe không được để trống.")
    private String brand;

    @NotNull(message = "Năm sản xuất không được để trống.")
    @Min(value = 2000, message = "Năm sản xuất phải từ 2000 trở đi.")
    private Integer year;

    @PositiveOrZero(message = "Dung lượng pin phải là số không âm (Wh hoặc kWh).")
    private Integer batteryCapacity;

    @PositiveOrZero(message = "Quãng đường di chuyển phải là số không âm (km).")
    private Integer rangeKm;

    @PositiveOrZero(message = "Thời gian sạc phải là số không âm (phút hoặc giờ).")
    private Integer chargingTime;

    @PositiveOrZero(message = "Tốc độ tối đa phải là số không âm (km/h).")
    private Integer maxSpeed;

    @DecimalMin(value = "0.0", inclusive = true, message = "Gia tốc phải là số không âm (giây).")
    private BigDecimal acceleration;

    @Positive(message = "Số chỗ ngồi phải lớn hơn 0.")
    private Integer seatingCapacity;

    @DecimalMin(value = "0.0", inclusive = true, message = "Thể tích khoang hàng phải là số không âm (m³).")
    private BigDecimal cargoVolume;

    @NotNull(message = "Giá nhà sản xuất không được để trống.")
    @DecimalMin(value = "0.0", inclusive = false, message = "Giá nhà sản xuất phải lớn hơn 0.")
    private BigDecimal manufacturerPrice;

    private String imageUrl;
}
