package swp.project.swp391.serviceImp.auth;

import lombok.RequiredArgsConstructor;
import lombok.extern.slf4j.Slf4j;
import org.springframework.beans.factory.annotation.Value;
import org.springframework.context.annotation.Profile;
import org.springframework.http.HttpEntity;
import org.springframework.http.HttpHeaders;
import org.springframework.http.MediaType;
import org.springframework.stereotype.Service;
import org.springframework.web.client.RestTemplate;
import swp.project.swp391.service.auth.EmailService;

import java.util.HashMap;
import java.util.List;
import java.util.Map;

@Service
@Profile("prod")
@RequiredArgsConstructor
@Slf4j
public class EmailServiceBrevoImpl implements EmailService {

    @Value("${BREVO_API_KEY}")
    private String apiKey;

    @Value("${EMAIL_SENDER}")
    private String senderEmail;

    @Value("${EMAIL_SENDER_NAME:5BIKE SYSTEM}")
    private String senderName;

    private final RestTemplate restTemplate = new RestTemplate();

    @Override
    public void sendEmail(String to, String subject, String body) {

        String url = "https://api.brevo.com/v3/smtp/email";

        Map<String, Object> payload = new HashMap<>();
        payload.put("sender", Map.of(
                "name", senderName,
                "email", senderEmail
        ));
        payload.put("to", List.of(
                Map.of("email", to)
        ));
        payload.put("subject", subject);
        payload.put("htmlContent", body);

        HttpHeaders headers = new HttpHeaders();
        headers.setContentType(MediaType.APPLICATION_JSON);
        headers.set("api-key", apiKey);

        HttpEntity<Map<String, Object>> request = new HttpEntity<>(payload, headers);

        try {
            restTemplate.postForObject(url, request, String.class);
            log.info("Email sent to {}", to);
        } catch (Exception e) {
            log.error("Failed to send email: {}", e.getMessage());
            throw new RuntimeException("Không thể gửi email qua Brevo API", e);
        }
    }
}


