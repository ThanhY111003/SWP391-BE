package swp.project.swp391.config;

import io.swagger.v3.oas.models.OpenAPI;
import io.swagger.v3.oas.models.info.Info;
import io.swagger.v3.oas.models.info.Contact;
import io.swagger.v3.oas.models.info.License;
import io.swagger.v3.oas.models.security.SecurityRequirement;
import io.swagger.v3.oas.models.security.SecurityScheme;
import io.swagger.v3.oas.models.Components;
import io.swagger.v3.oas.models.servers.Server;
import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;

import java.util.List;

@Configuration
public class SwaggerConfig {

    @Bean
    public OpenAPI swaggerOpenAPI() {
        // 🌐 Cấu hình server (local + ngrok)
        Server ngrokServer = new Server();
        ngrokServer.setUrl("https://deflexed-burdenedly-clemente.ngrok-free.dev");
        ngrokServer.setDescription("Public API for Teacher Test (Ngrok)");

        Server localServer = new Server();
        localServer.setUrl("http://localhost:8080");
        localServer.setDescription("Local Development");

        // 🔐 JWT Security Scheme
        SecurityScheme bearerAuth = new SecurityScheme()
                .type(SecurityScheme.Type.HTTP)
                .scheme("bearer")
                .bearerFormat("JWT");

        // 📘 OpenAPI Info
        return new OpenAPI()
                .info(new Info()
                        .title("Electric Vehicle Dealer Management System API")
                        .description("API documentation for EV Dealer Management System — SWP391 Project")
                        .version("1.0.0")
                        .contact(new Contact()
                                .name("SWP391 Team")
                                .email("swp391@example.com")
                                .url("https://github.com/swp391"))
                        .license(new License()
                                .name("MIT License")
                                .url("https://opensource.org/licenses/MIT")))
                .addSecurityItem(new SecurityRequirement().addList("Bearer Authentication"))
                .components(new Components().addSecuritySchemes("Bearer Authentication", bearerAuth))
                .servers(List.of(ngrokServer, localServer));
    }
}
