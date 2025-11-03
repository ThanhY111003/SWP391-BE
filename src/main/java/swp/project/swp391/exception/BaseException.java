package swp.project.swp391.exception;

import lombok.Getter;
import swp.project.swp391.constant.ErrorHandler;

@Getter
public class BaseException extends RuntimeException {

    private final ErrorHandler errorHandler;
    private final String customMessage;

    public BaseException(ErrorHandler errorHandler) {
        super(errorHandler.getMessage());
        this.errorHandler = errorHandler;
        this.customMessage = null;
    }

    public BaseException(ErrorHandler errorHandler, String customMessage) {
        super(customMessage != null ? customMessage : errorHandler.getMessage());
        this.errorHandler = errorHandler;
        this.customMessage = customMessage;
    }

    public String getDisplayMessage() {
        return customMessage != null ? customMessage : errorHandler.getMessage();
    }
}

