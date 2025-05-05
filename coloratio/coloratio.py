#!/usr/bin/env python3

import math
import argparse


class ColorRatio:
    def _hex_to_rgb(self, hex_color):
        hex_color = hex_color.lstrip("#")
        return tuple(int(hex_color[i : i + 2], 16) for i in (0, 2, 4))

    def _rgb_to_xyz(self, rgb):
        r, g, b = [x / 255.0 for x in rgb]

        r = r / 12.92 if r <= 0.04045 else ((r + 0.055) / 1.055) ** 2.4
        g = g / 12.92 if g <= 0.04045 else ((g + 0.055) / 1.055) ** 2.4
        b = b / 12.92 if b <= 0.04045 else ((b + 0.055) / 1.055) ** 2.4

        x = r * 0.4124564 + g * 0.3575761 + b * 0.1804375
        y = r * 0.2126729 + g * 0.7151522 + b * 0.0721750
        z = r * 0.0193339 + g * 0.1191920 + b * 0.9503041

        return (x, y, z)

    def _xyz_to_lab(self, xyz):
        x, y, z = xyz

        xn, yn, zn = 0.95047, 1.0, 1.08883

        x, y, z = [v / r for v, r in zip(xyz, (xn, yn, zn))]

        fx = x ** (1 / 3) if x > 0.008856 else (7.787 * x) + (16 / 116)
        fy = y ** (1 / 3) if y > 0.008856 else (7.787 * y) + (16 / 116)
        fz = z ** (1 / 3) if z > 0.008856 else (7.787 * z) + (16 / 116)

        l = max(0, 116 * fy - 16)
        a = 500 * (fx - fy)
        b = 200 * (fy - fz)

        return (l, a, b)

    def _lab_to_lch(self, lab):
        l, a, b = lab

        c = math.sqrt(a**2 + b**2)
        h = math.atan2(b, a) * 180 / math.pi

        if h < 0:
            h += 360

        return (l, c, h)

    def _lch_to_lab(self, lch):
        l, c, h = lch

        h_rad = h * math.pi / 180

        a = c * math.cos(h_rad)
        b = c * math.sin(h_rad)

        return (l, a, b)

    def _lab_to_xyz(self, lab):
        l, a, b = lab

        xn, yn, zn = 0.95047, 1.0, 1.08883

        fy = (l + 16) / 116
        fx = a / 500 + fy
        fz = fy - b / 200

        x = xn * (fx**3 if fx**3 > 0.008856 else (fx - 16 / 116) / 7.787)
        y = yn * (fy**3 if fy**3 > 0.008856 else (fy - 16 / 116) / 7.787)
        z = zn * (fz**3 if fz**3 > 0.008856 else (fz - 16 / 116) / 7.787)

        return (x, y, z)

    def _xyz_to_rgb(self, xyz):
        x, y, z = xyz

        r = x * 3.2404542 + y * -1.5371385 + z * -0.4985314
        g = x * -0.9692660 + y * 1.8760108 + z * 0.0415560
        b = x * 0.0556434 + y * -0.2040259 + z * 1.0572252

        r = 12.92 * r if r <= 0.0031308 else 1.055 * (r ** (1 / 2.4)) - 0.055
        g = 12.92 * g if g <= 0.0031308 else 1.055 * (g ** (1 / 2.4)) - 0.055
        b = 12.92 * b if b <= 0.0031308 else 1.055 * (b ** (1 / 2.4)) - 0.055

        r = max(0, min(1, r)) * 255
        g = max(0, min(1, g)) * 255
        b = max(0, min(1, b)) * 255

        return (round(r), round(g), round(b))

    def _adjust_lightness(self, hex_color, factor):
        rgb = self._hex_to_rgb(hex_color)
        xyz = self._rgb_to_xyz(rgb)
        lab = self._xyz_to_lab(xyz)
        l, c, h = self._lab_to_lch(lab)

        new_l = max(0, min(100, l * factor))

        lab = self._lch_to_lab((new_l, c, h))
        xyz = self._lab_to_xyz(lab)
        rgb = self._xyz_to_rgb(xyz)

        return f"#{rgb[0]:02x}{rgb[1]:02x}{rgb[2]:02x}"

    def calculate_luminance(self, hex_color):
        r, g, b = self._hex_to_rgb(hex_color)

        r /= 255.0
        g /= 255.0
        b /= 255.0

        r = r / 12.92 if r <= 0.04045 else ((r + 0.055) / 1.055) ** 2.4
        g = g / 12.92 if g <= 0.04045 else ((g + 0.055) / 1.055) ** 2.4
        b = b / 12.92 if b <= 0.04045 else ((b + 0.055) / 1.055) ** 2.4

        return 0.2126 * r + 0.7152 * g + 0.0722 * b

    def calculate_contrast_ratio(self, color1, color2):
        luminance1 = self.calculate_luminance(color1)
        luminance2 = self.calculate_luminance(color2)

        return (max(luminance1, luminance2) + 0.05) / (
            min(luminance1, luminance2) + 0.05
        )


if __name__ == "__main__":
    parser = argparse.ArgumentParser(
        prog="coloratio",
        description="Calculate contrast ratio based on background color",
    )
    parser.add_argument("--background", help="Hex background color", required=True)
    parser.add_argument(
        "--original", help="Original color to compare contrast", required=True
    )
    parser.add_argument(
        "--target", help="Target color as an alternative to the original"
    )

    args = parser.parse_args()

    max_ratio_threshold = 8.6  # This could be changed but not greater than 12
    min_ratio_threshold = 7.0
    contrast = ColorRatio()
    original_contrast = contrast.calculate_contrast_ratio(
        args.original, args.background
    )

    print(f"Original Color: {args.original}\n")
    print(
        f"Contrast Ratio with Background ( {args.original} ): {original_contrast:.2f}:1"
    )
    print(
        f"Suitable: {'YES' if min_ratio_threshold <= original_contrast <= max_ratio_threshold else 'NO'}\n"
    )

    if args.target:
        target_contrast = contrast.calculate_contrast_ratio(
            args.target, args.background
        )
        print(
            f"Contrast Ratio with Background ( {args.target} ): {target_contrast:.2f}:1"
        )
        print(
            f"Suitable: {'YES' if min_ratio_threshold <= target_contrast <= max_ratio_threshold else 'NO'}\n"
        )
