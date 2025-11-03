package swp.project.swp391.config;

import com.fasterxml.jackson.databind.ObjectMapper;
import jakarta.servlet.http.*;
import org.springframework.security.access.AccessDeniedException;
import org.springframework.security.web.access.AccessDeniedHandler;
import org.springframework.stereotype.Component;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.constant.ErrorHandler;

import java.io.IOException;

@Component
public class CustomAccessDeniedHandler implements AccessDeniedHandler {
    @Override
    public void handle(HttpServletRequest request, HttpServletResponse response,
                       AccessDeniedException accessDeniedException) throws IOException {
        response.setStatus(HttpServletResponse.SC_FORBIDDEN);
        response.setContentType("application/json;charset=UTF-8");
        new ObjectMapper().writeValue(response.getOutputStream(),
                ApiResponse.error(ErrorHandler.FORBIDDEN.name(), ErrorHandler.FORBIDDEN.getMessage()));
    }
}
