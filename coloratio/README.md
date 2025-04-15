### Usage

The tool checks if colors meet accessibility contrast standards against a background.

```bash
python3 coloratio.py --background "#090F19" --original "#92a1ba" --target "#8b94a4"
```

Parameters:
- `--background`: The background color in hex format ( Required )
- `--original`: The reference color to compare ( Required )
- `--target`: The new color to evaluate ( Optional: Alternative color )

Output:
```
Original Color: #92a1ba

Contrast Ratio with Background ( #92a1ba ): 7.34:1
Suitable: YES

Contrast Ratio with Background ( #8b94a4 ): 6.28:1
Suitable: NO
```

### Resources

[Color Hexa](https://www.colorhexa.com/) - Useful for finding different color shades
