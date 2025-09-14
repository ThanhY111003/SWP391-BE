package swp.project.swp391.exception;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.validation.FieldError;
import org.springframework.web.bind.MethodArgumentNotValidException;
import org.springframework.web.bind.annotation.ControllerAdvice;
import org.springframework.web.bind.annotation.ExceptionHandler;

import java.util.HashMap;
import java.util.Map;

@ControllerAdvice
public class GlobalExceptionHandler {

    @ExceptionHandler(BaseException.class)
    public ResponseEntity<ErrorResponse> handleBaseException(BaseException ex) {
        ErrorResponse errorResponse = new ErrorResponse(
                ex.getErrorHandler().getHttpStatus().value(),
                ex.getErrorHandler().getMessage()
        );
        return new ResponseEntity<>(errorResponse, ex.getErrorHandler().getHttpStatus());
    }

    @ExceptionHandler(MethodArgumentNotValidException.class)
    public ResponseEntity<Map<String, String>> handleValidationExceptions(MethodArgumentNotValidException ex) {
        Map<String, String> errors = new HashMap<>();

        ex.getBindingResult().getAllErrors().forEach((error) -> {
            // Kiểm tra xem lỗi có phải là FieldError không
            if (error instanceof FieldError) {
                String fieldName = ((FieldError) error).getField();
                String errorMessage = error.getDefaultMessage();
                errors.put(fieldName, errorMessage);
            } else {
                // Nếu không phải, đó là lỗi toàn cục (ví dụ: PasswordMatches)
                String objectName = error.getObjectName();
                String errorMessage = error.getDefaultMessage();
                errors.put(objectName, errorMessage); // Hoặc một key chung nào đó, ví dụ "global"
            }
        });
        return new ResponseEntity<>(errors, HttpStatus.BAD_REQUEST);
    }
}