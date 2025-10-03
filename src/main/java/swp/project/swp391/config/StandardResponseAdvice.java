// File: swp/project/swp391/config/StandardResponseAdvice.java
package swp.project.swp391.config;

import org.springframework.core.MethodParameter;
import org.springframework.http.*;
import org.springframework.http.server.ServerHttpRequest;
import org.springframework.http.server.ServerHttpResponse;
import org.springframework.web.bind.annotation.RestControllerAdvice;
import org.springframework.web.servlet.mvc.method.annotation.ResponseBodyAdvice;
import swp.project.swp391.api.ApiResponse;

@RestControllerAdvice
public class StandardResponseAdvice implements ResponseBodyAdvice<Object> {

    @Override
    public boolean supports(MethodParameter returnType, Class converterType) {
        // Đừng áp dụng cho các controller nội bộ của springdoc
        String clazz = returnType.getContainingClass().getName();
        return !(clazz.startsWith("org.springdoc") || clazz.startsWith("org.springframework.boot.actuate"));
    }

    @Override
    public Object beforeBodyWrite(
            Object body, MethodParameter returnType, MediaType ct,
            Class converterType, ServerHttpRequest req, ServerHttpResponse res) {

        // BYPASS swagger/openapi: để nguyên JSON spec
        String path = req.getURI().getPath();
        if (path.startsWith("/api-docs") || path.startsWith("/v3/api-docs")
                || path.startsWith("/swagger-ui")) {
            return body;
        }

        // Tránh bọc nếu đã là ApiResponse
        if (body instanceof ApiResponse) return body;

        // Trường hợp ResponseEntity
        if (body instanceof ResponseEntity<?> entity) {
            Object inner = entity.getBody();
            if (inner instanceof ApiResponse) return entity;

            ApiResponse<?> wrapped = entity.getStatusCode().is2xxSuccessful()
                    ? ApiResponse.ok(inner)
                    : ApiResponse.error(entity.getStatusCode().toString(), null);

            return ResponseEntity.status(entity.getStatusCode())
                    .headers(entity.getHeaders())
                    .body(wrapped);
        }

        // Tránh phá Content-Type khi trả về String thuần
        if (body instanceof String) return body;

        // Mặc định bọc OK
        return ApiResponse.ok(body);
    }
}
