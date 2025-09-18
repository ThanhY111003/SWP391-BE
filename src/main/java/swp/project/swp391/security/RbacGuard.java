package swp.project.swp391.security;

import org.springframework.stereotype.Component;
import swp.project.swp391.constant.ErrorHandler;
import swp.project.swp391.entity.User;
import swp.project.swp391.exception.BaseException;

import java.util.function.BooleanSupplier;

@Component
public class RbacGuard {
    public boolean has(User u, String perm) {
        return u.getRoles().stream().flatMap(r -> r.getPermissions().stream())
                .anyMatch(p -> p.getName().equalsIgnoreCase(perm));
    }
    public void require(boolean ok) { if (!ok) throw new BaseException(ErrorHandler.FORBIDDEN); }
    public void requireAnyOrOwner(User u, String anyPerm, BooleanSupplier owner) {
        require(has(u, anyPerm) || owner.getAsBoolean());
    }
}
