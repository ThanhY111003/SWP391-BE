package swp.project.swp391.serviceImp.order;

import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.Order;
import swp.project.swp391.entity.User;
import swp.project.swp391.exception.BaseException;
import swp.project.swp391.repository.OrderRepository;
import swp.project.swp391.response.order.OrderResponse;
import swp.project.swp391.security.RbacGuard;
import swp.project.swp391.service.order.OrderQueryService;

import java.time.LocalDate;
import java.util.List;

@Service("dealerOrderQueryService")
@RequiredArgsConstructor
@Transactional(readOnly = true)
public class DealerOrderQueryServiceImpl implements OrderQueryService {

    private final OrderRepository orderRepo;
    private final RbacGuard guard;

    @Override
    public List<OrderResponse> getAllOrders(User currentUser) {
        guard.require(guard.has(currentUser, "order.read_all"));

        if (currentUser.getDealer() == null) {
            throw new BaseException(ErrorHandler.DEALER_NOT_FOUND);
        }

        return orderRepo.findAllByBuyerDealerId(currentUser.getDealer().getId())
                .stream()
                .map(OrderResponse::fromEntity)
                .toList();
    }

    @Override
    public OrderResponse getOrderById(Long id, User currentUser) {
        guard.require(guard.has(currentUser, "order.read"));

        if (currentUser.getDealer() == null) {
            throw new BaseException(ErrorHandler.DEALER_NOT_FOUND);
        }

        Long dealerId = currentUser.getDealer().getId();

        Order order = orderRepo.findOneByIdAndBuyerDealerId(id, dealerId)
                .orElseThrow(() -> new BaseException(ErrorHandler.ORDER_NOT_FOUND));

        return OrderResponse.fromEntity(order);
    }


}
