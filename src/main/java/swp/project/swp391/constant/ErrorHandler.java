package swp.project.swp391.constant;

import lombok.Getter;
import lombok.RequiredArgsConstructor;
import org.springframework.http.HttpStatus;

@Getter
@RequiredArgsConstructor
public enum ErrorHandler {

    // Authentication Errors
    USERNAME_ALREADY_EXISTS(HttpStatus.CONFLICT, 409, "Tên đăng nhập đã tồn tại"),
    EMAIL_ALREADY_EXISTS(HttpStatus.CONFLICT, 409, "Email đã tồn tại"),
    INVALID_CREDENTIALS(HttpStatus.UNAUTHORIZED, 401, "Tên đăng nhập hoặc mật khẩu không đúng"),
    INVALID_TOKEN(HttpStatus.UNAUTHORIZED, 401, "Token không hợp lệ hoặc đã hết hạn"),
    UNVERIFIED_ACCOUNT_EXISTS(HttpStatus.FORBIDDEN, 403, "Vui lòng kiểm tra email để xác minh tài khoản của bạn."),
    USER_NOT_FOUND(HttpStatus.NOT_FOUND, 404, "Không tìm thấy người dùng"),
    ACCOUNT_ALREADY_VERIFIED(HttpStatus.BAD_REQUEST, 400, "Tài khoản đã được xác minh"),
    INVALID_EMAIL(HttpStatus.BAD_REQUEST, 400, "Địa chỉ email không hợp lệ hoặc không tồn tại."),
    INVALID_OLD_PASSWORD(HttpStatus.BAD_REQUEST, 400, "Mật khẩu cũ không đúng"),
    // User Errors
    ACCOUNT_NOT_VERIFIED(HttpStatus.FORBIDDEN, 403, "Tài khoản chưa được xác minh. Vui lòng kiểm tra email để xác minh tài khoản của bạn."),
    ID_NUMBER_ALREADY_EXISTS(HttpStatus.CONFLICT, 409, "Số CMND/CCCD đã tồn tại"),
    PHONE_NUMBER_ALREADY_EXISTS(HttpStatus.CONFLICT, 409, "Số điện thoại đã tồn tại"),
    INVALID_GENDER(HttpStatus.BAD_REQUEST, 400, "Giới tính không hợp lệ."),
    INVALID_INCOME_LEVEL(HttpStatus.BAD_REQUEST, 400, "Mức thu nhập không hợp lệ."),
    CUSTOMER_NOT_FOUND(HttpStatus.NOT_FOUND, 404, "Không tìm thấy khách hàng"),
    ACCOUNT_BLOCKED(HttpStatus.FORBIDDEN, 403, "Tài khoản của bạn đã bị khoá. Vui lòng liên hệ quản trị viên để biết thêm chi tiết."),
    UNAUTHORIZED(HttpStatus.UNAUTHORIZED, 401, "Chưa xác thực người dùng"),
    // Role Errors
    ROLE_NOT_FOUND(HttpStatus.NOT_FOUND, 404, "Không tìm thấy vai trò"),
    PERMISSION_NOT_FOUND(HttpStatus.NOT_FOUND, 404, "Không tìm thấy quyền"),
    // Common/Validation Errors
    INVALID_DATE_FORMAT(HttpStatus.BAD_REQUEST, 400, "Định dạng ngày sinh không hợp lệ. Vui lòng sử dụng định dạng yyyy-MM-dd"),
    INVALID_REQUEST(HttpStatus.BAD_REQUEST, 400, "Yêu cầu ko hợp lệ"),
    ACCESS_DENIED(HttpStatus.FORBIDDEN, 403, "Bạn không có quyền truy cập vào tài nguyên này"),
    FORBIDDEN(HttpStatus.FORBIDDEN, 403, "Hành động bị cấm"),
    REQUEST_OTP_TOO_SOON(HttpStatus.TOO_MANY_REQUESTS, 429, "Bạn đã yêu cầu gửi lại mã OTP quá sớm. Vui lòng thử lại sau."),
    OTP_EXPIRED(HttpStatus.BAD_REQUEST, 400, "Mã OTP không hợp lệ hoặc đã hết hạn"),
    PASSWORD_NOT_MATCH(HttpStatus.BAD_REQUEST, 400, "Mật khẩu mới và xác nhận mật khẩu không khớp"),
    // Dealer Errors
    DEALER_REQUIRED(HttpStatus.BAD_REQUEST, 400, "dealerId là bắt buộc đối với vai trò DEALER_MANAGER và DEALER_STAFF"),
    DEALER_NOT_FOUND(HttpStatus.NOT_FOUND, 404, "Không tìm thấy đại lý"),
    DEALER_NOT_ALLOWED(HttpStatus.BAD_REQUEST, 400, "Không được phép cung cấp dealerId cho vai trò EVM_STAFF"),
    DEALER_LEVEL_NOT_FOUND(HttpStatus.NOT_FOUND, 404, "Không tìm thấy cấp đại lý"),
    CODE_GENERATION_FAILED(HttpStatus.INTERNAL_SERVER_ERROR, 500, "Không thể tạo mã đại lý sau nhiều lần thử"),
    DEALER_LEVEL_ALREADY_EXISTS(HttpStatus.CONFLICT, 409, "Cấp đại lý với số cấp độ này đã tồn tại"),
    DEALER_LEVEL_IN_USE(HttpStatus.BAD_REQUEST, 400, "Cấp đại lý đang được sử dụng bởi một hoặc nhiều đại lý và không thể xóa, đổi level hiện tại của đại lý trước khi xóa cấp độ này"),

    // Vehicle Model Errors
    ENGINE_NUMBER_ALREADY_EXISTS(HttpStatus.CONFLICT, 409, "Số máy đã tồn tại"),
    VIN_ALREADY_EXISTS(HttpStatus.CONFLICT, 409, "VIN đã tồn tại"),
    VEHICLE_MODEL_NOT_FOUND(HttpStatus.NOT_FOUND, 404, "Không tìm thấy mẫu xe"),
    VEHICLE_COLOR_NOT_FOUND(HttpStatus.NOT_FOUND, 404, "Không tìm thấy màu xe"),
    VEHICLE_MODEL_IN_USE(HttpStatus.CONFLICT,400,"Model xe này đang được sử dụng"),
    COLOR_NOT_FOUND(HttpStatus.NOT_FOUND, 404, "Không tìm thấy màu"),
    COLOR_ASSIGNED_TO_MODEL(HttpStatus.BAD_REQUEST, 400, "Màu đang được gán vào mẫu xe,vui lòng gỡ bỏ màu khỏi các mẫu xe trước khi vô hiệu hóa"),
    COLOR_ISACTIVE_NOT_FOUND(HttpStatus.NOT_FOUND, 404, "Màu này đã bị vô hiệu hoá"),
    COLOR_ALREADY_EXISTS(HttpStatus.CONFLICT, 409, "Màu đã tồn tại"),
    VEHICLE_COLOR_ALREADY_EXISTS(HttpStatus.CONFLICT, 409, "Màu đã được gán cho mẫu xe này"),
    VEHICLE_MODEL_ALREADY_EXISTS(HttpStatus.CONFLICT, 409, "Mẫu xe đã tồn tại"),
    INTERNAL_ERROR(HttpStatus.INTERNAL_SERVER_ERROR, 500, "Lỗi máy chủ nội bộ"),
    VEHICLE_MODEL_COLOR_NOT_FOUND(HttpStatus.NOT_FOUND, 404, "Không tìm thấy màu xe cho mẫu xe"),

    // Vehicle Instance Errors
    VEHICLE_INSTANCE_NOT_FOUND(HttpStatus.NOT_FOUND, 404, "Không tìm thấy xe"),
    VEHICLE_INSTANCE_DUPLICATE(HttpStatus.CONFLICT, 409, "Xe này đã được báo cáo lỗi trước đó"),
    REPORT_NOT_FOUND(HttpStatus.NOT_FOUND, 404, "Không tìm thấy báo cáo lỗi xe"),
    VEHICLE_ALREADY_SOLD(HttpStatus.BAD_REQUEST, 400, "Xe đã được bán cho khách khác"),
    VEHICLE_ALREADY_ASSIGNED(HttpStatus.BAD_REQUEST, 400, "Xe này đã được gán cho khách hàng"),
    VEHICLE_NOT_ELIGIBLE_DELETE(HttpStatus.BAD_REQUEST, 400, "Chỉ có thể xóa xe còn trong kho"),
    CUSTOMER_VEHICLE_NOT_FOUND(HttpStatus.NOT_FOUND, 404, "Không tìm thấy bản ghi bán xe cho khách hàng"),
    VEHICLE_IS_INACTIVE(HttpStatus.BAD_REQUEST, 400, "Xe đã bị vô hiệu hóa, không thể gán cho khách hàng"),
    ONLY_IN_STOCK_CAN_DEACTIVATE(HttpStatus.BAD_REQUEST, 400, "Chỉ có thể vô hiệu hóa xe đang trong kho (IN_STOCK)"),
    VEHICLE_ALREADY_ACTIVE(HttpStatus.BAD_REQUEST, 400, "Xe đã đang ở trạng thái hoạt động"),
    VEHICLE_WAS_SOLDED(HttpStatus.BAD_REQUEST, 400, "Xe đã được bán, không thể thay đổi trạng thái"),
    VEHICLE_NOT_OWNED_BY_DEALER(HttpStatus.FORBIDDEN, 403, "Xe không thuộc sở hữu của đại lý hiện tại"),
    INVALID_WARRANTY_PERIOD(HttpStatus.BAD_REQUEST, 400, "Ngày kết thức phải lớn hơn ngày bắt đầu bảo hành"),
    VEHICLE_IS_RESERVED(HttpStatus.BAD_REQUEST, 400, "Xe đang được giữ chỗ và không thể chuyển kho"),
    VEHICLE_NOT_ASSIGNED(HttpStatus.BAD_REQUEST, 400, "Xe chưa được gán vào đơn hàng"),

    // Vehicle Price Errors
    VEHICLE_PRICE_NOT_FOUND(HttpStatus.NOT_FOUND, 404, "Không tìm thấy bảng giá xe"),
    INVALID_SALE_PRICE_RANGE(HttpStatus.BAD_REQUEST, 400, "Giá bán không hợp được vượt 20% lợi nhuận so với giá mua từ hãng"),

    // Inventory Errors
    INVENTORY_NOT_FOUND(HttpStatus.NOT_FOUND, 404, "Không tìm thấy kho xe"),

    // Order Errors
    CART_NOT_FOUND(HttpStatus.NOT_FOUND, 404, "Không tìm thấy giỏ hàng"),
    MAX_ORDER_QUANTITY_EXCEEDED(HttpStatus.BAD_REQUEST, 400, "Vượt quá số lượng đơn hàng tối đa cho phép theo cấp độ đại lý"),
    CART_ITEM_NOT_FOUND(HttpStatus.NOT_FOUND, 404, "Không tìm thấy mục giỏ hàng"),
    ORDER_ALREADY_CANCELLED (HttpStatus.BAD_REQUEST, 400, "Đơn hàng đã bị hủy"),
    ORDER_ALREADY_PROCESSED(HttpStatus.BAD_REQUEST, 400, "Đơn hàng đã được xử lý và không thể hủy"),
    NOT_FOUND_INSTALLMENT_PLAN(HttpStatus.NOT_FOUND, 404, "Không tìm thấy kế hoạch trả góp"),
    INVALID_MONTH_REQUEST(HttpStatus.BAD_REQUEST, 400, "Kỳ trả góp không thuộc đơn hàng này"),
    WASPAIDED_REQUEST(HttpStatus.BAD_REQUEST, 400, "Kỳ trả góp đã được thanh toán"),
    INVALID_INSTALLMENT_REQUEST(HttpStatus.BAD_REQUEST, 400, "Chỉ có thể thực hiện hành động này trên các đơn hàng trả góp"),
    ONLY_CONFIRMED_REQUEST(HttpStatus.BAD_REQUEST, 400, "Chỉ có thể thực hiện hành động này trên các đơn hàng ở trạng thái CONFIRMED"),
    ORDER_NOT_FOUND(HttpStatus.NOT_FOUND, 404, "Không tìm thấy đơn hàng");


    private final HttpStatus status;
    private final int code;
    private final String message;
}