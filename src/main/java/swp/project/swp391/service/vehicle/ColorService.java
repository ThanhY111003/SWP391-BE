package swp.project.swp391.service.vehicle;

import swp.project.swp391.entity.User;
import swp.project.swp391.request.vehicle.ColorRequest;
import swp.project.swp391.response.vehicle.ColorResponse;

import java.util.List;

public interface ColorService {

    ColorResponse createColor(ColorRequest request, User currentUser);

    ColorResponse updateColor(Long id, ColorRequest request, User currentUser);

    ColorResponse inactiveColor(Long id, User currentUser);

    ColorResponse reactiveColor(Long id, User currentUser);

    List<ColorResponse> getAllColors();

    ColorResponse getColorById(Long id);
}
