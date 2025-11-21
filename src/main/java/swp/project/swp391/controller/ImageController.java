package swp.project.swp391.controller;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.tags.Tag;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.multipart.MultipartFile;
import swp.project.swp391.api.ApiResponse;
import swp.project.swp391.service.CloudinaryService;

@RestController
@RequestMapping("/api/images")
@RequiredArgsConstructor
@Tag(name = "Image Upload", description = "Upload & quản lý hình ảnh bằng Cloudinary")
public class ImageController {

    private final CloudinaryService cloudinaryService;

    @PostMapping("/upload")
    @Operation(summary = "Upload hình ảnh lên Cloudinary")
    public ResponseEntity<ApiResponse<String>> uploadImage(
            @RequestParam("file") MultipartFile file
    ) {
        String url = cloudinaryService.uploadImage(file);
        return ResponseEntity.ok(
                ApiResponse.ok(url, "Upload hình ảnh thành công")
        );
    }
}
