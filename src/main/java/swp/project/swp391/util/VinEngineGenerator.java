// util/VinEngineGenerator.java
package swp.project.swp391.util;

import java.security.SecureRandom;
import java.time.Year;

public final class VinEngineGenerator {
    private static final char[] VIN_CHARS =
            "ABCDEFGHJKLMNPRSTUVWXYZ0123456789".toCharArray(); // bỏ I,O,Q
    private static final char[] ENG_CHARS =
            "ABCDEFGHJKLMNPRSTUVWXYZ0123456789".toCharArray();

    private static final SecureRandom RND = new SecureRandom();

    private VinEngineGenerator() {}

    // VIN: 17 chars. format đơn giản: WMI(3) + VDS(6) + Year(1) + Plant(1) + Serial(6)
    public static String generateVin(String wmi, String vdsSeed, char plantCode) {
        String w = padAlphaNum(wmi, 3);
        String vds = padAlphaNum(vdsSeed, 6);
        char yearCode = yearToCode(Year.now().getValue());
        char plant = isValidVinChar(plantCode) ? plantCode : randomVinChar();
        String serial = randomVin(6);
        return (w + vds + yearCode + plant + serial).substring(0, 17);
    }

    public static String generateEngineNumber(String prefix, int length) {
        String p = padAlphaNum(prefix, Math.min(4, Math.max(0, length-8)));
        String rand = randomEng(Math.max(8, length - p.length()));
        String s = (p + rand);
        return s.length() > length ? s.substring(0, length) : s;
    }

    private static String randomVin(int n) {
        StringBuilder sb = new StringBuilder(n);
        for (int i=0;i<n;i++) sb.append(randomVinChar());
        return sb.toString();
    }
    private static char randomVinChar() {
        return VIN_CHARS[RND.nextInt(VIN_CHARS.length)];
    }
    private static String randomEng(int n) {
        StringBuilder sb = new StringBuilder(n);
        for (int i=0;i<n;i++) sb.append(ENG_CHARS[RND.nextInt(ENG_CHARS.length)]);
        return sb.toString();
    }
    private static boolean isValidVinChar(char c) {
        char u = Character.toUpperCase(c);
        for (char k: VIN_CHARS) if (k==u) return true;
        return false;
    }
    private static String padAlphaNum(String s, int len) {
        String base = s == null ? "" : s.replaceAll("[^A-Za-z0-9]", "").toUpperCase();
        StringBuilder sb = new StringBuilder(base);
        while (sb.length() < len) sb.append(randomVinChar());
        return sb.substring(0, len);
    }
    // Year code theo chu kỳ cơ bản (đơn giản hóa)
    private static char yearToCode(int year) {
        String map = "ABCDEFGHJKLMNPRSTVWXY123456789";
        int idx = (year - 1980) % map.length();
        if (idx < 0) idx += map.length();
        return map.charAt(idx);
    }
}
