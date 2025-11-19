package swp.project.swp391.request.order;

import lombok.*;

@Getter
@Setter
@NoArgsConstructor
@AllArgsConstructor
@Builder
public class OrderApproveRequest {

    private Long orderId;

    private String approvalNote;
}
