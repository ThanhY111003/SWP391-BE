package swp.project.swp391.serviceImp;

import com.cloudinary.Cloudinary;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;
import org.springframework.web.multipart.MultipartFile;
import swp.project.swp391.service.CloudinaryService;

import java.util.Map;

@Service
@RequiredArgsConstructor
public class CloudinaryServiceImpl implements CloudinaryService {

    private final Cloudinary cloudinary;

    @Override
    public String uploadImage(MultipartFile file) {
        try {
            Map upload = cloudinary.uploader().upload(
                    file.getBytes(),
                    Map.of("folder", "swp391")
            );

            return upload.get("secure_url").toString();
        } catch (Exception e) {
            throw new RuntimeException("Upload ảnh thất bại!", e);
        }
    }

    @Override
    public void deleteImage(String publicId) {
        try {
            cloudinary.uploader().destroy(publicId, Map.of());
        } catch (Exception e) {
            throw new RuntimeException("Xoá ảnh thất bại!", e);
        }
    }
}
