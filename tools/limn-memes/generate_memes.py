#!/usr/bin/env python3
"""
Generate Limn meme images from SLM-generated phrases.

Uses Pillow to create stylized meme cards with:
- Limn expression in large monospace font
- English translation below
- Gradient backgrounds in moody/atmospheric colors
- "Limn" watermark

Output: PNG files in output/ directory
"""

import math
from pathlib import Path
from PIL import Image, ImageDraw, ImageFont

OUTPUT_DIR = Path(__file__).resolve().parent / "output"
OUTPUT_DIR.mkdir(exist_ok=True)

# Fonts
FONT_MONO = "/usr/share/fonts/truetype/dejavu/DejaVuSansMono.ttf"
FONT_BOLD = "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf"
FONT_SERIF = "/usr/share/fonts/truetype/dejavu/DejaVuSerif.ttf"
FONT_SERIF_BOLD = "/usr/share/fonts/truetype/dejavu/DejaVuSerif-Bold.ttf"

# Image dimensions (social media friendly)
WIDTH = 1080
HEIGHT = 1080

# 10 curated Limn phrases (SLM-generated + expert-verified)
PHRASES = [
    {
        "limn": "joy*sad",
        "english": "bittersweet",
        "note": "two feelings merge into one",
        "colors": [(45, 20, 60), (120, 60, 30)],  # purple to amber
    },
    {
        "limn": "act:fea",
        "english": "courage",
        "note": "action given fear",
        "colors": [(100, 20, 20), (180, 120, 40)],  # deep red to gold
    },
    {
        "limn": "kno:tau",
        "english": "wisdom",
        "note": "knowledge given time",
        "colors": [(20, 30, 60), (60, 90, 120)],  # deep navy to steel blue
    },
    {
        "limn": "tau\\end",
        "english": "eternal",
        "note": "time without ending",
        "colors": [(10, 10, 30), (40, 20, 60)],  # near-black to deep purple
    },
    {
        "limn": "say^0.2",
        "english": "whisper",
        "note": "speech at 20% intensity",
        "colors": [(30, 35, 40), (70, 75, 80)],  # charcoal to grey
    },
    {
        "limn": "fea*fut",
        "english": "anxiety",
        "note": "fear blended with future",
        "colors": [(40, 15, 15), (80, 40, 60)],  # dark red to mauve
    },
    {
        "limn": "rea\u00b1dre",
        "english": "lucid dreaming",
        "note": "real and dream at once",
        "colors": [(20, 15, 50), (80, 50, 120)],  # midnight to violet
    },
    {
        "limn": "aer*wet",
        "english": "fog",
        "note": "air blended with water",
        "colors": [(60, 70, 75), (140, 150, 155)],  # slate to silver
    },
    {
        "limn": "psi@rcr",
        "english": "self-aware mind",
        "note": "recursion-aspect of mind",
        "colors": [(15, 40, 40), (40, 100, 90)],  # dark teal to teal
    },
    {
        "limn": "hop^0",
        "english": "despair",
        "note": "hope at zero intensity",
        "colors": [(5, 5, 10), (30, 25, 35)],  # almost black to dark grey
    },
]


def make_gradient(width, height, color1, color2):
    """Create a diagonal gradient background."""
    img = Image.new("RGB", (width, height))
    pixels = img.load()
    for y in range(height):
        for x in range(width):
            # Diagonal gradient with slight curve
            t = (x / width * 0.6 + y / height * 0.4)
            # Add subtle noise via sine wave
            t += 0.03 * math.sin(x * 0.02) * math.cos(y * 0.015)
            t = max(0, min(1, t))
            r = int(color1[0] + (color2[0] - color1[0]) * t)
            g = int(color1[1] + (color2[1] - color1[1]) * t)
            b = int(color1[2] + (color2[2] - color1[2]) * t)
            pixels[x, y] = (r, g, b)
    return img


def draw_text_centered(draw, text, y, font, fill, width):
    """Draw text centered horizontally."""
    bbox = draw.textbbox((0, 0), text, font=font)
    text_width = bbox[2] - bbox[0]
    x = (width - text_width) // 2
    draw.text((x, y), text, font=font, fill=fill)
    return bbox[3] - bbox[1]  # return text height


def generate_meme(phrase, index):
    """Generate a single meme image."""
    # Create gradient background
    img = make_gradient(WIDTH, HEIGHT, phrase["colors"][0], phrase["colors"][1])
    draw = ImageDraw.Draw(img)

    # Load fonts
    font_limn = ImageFont.truetype(FONT_MONO, 72)
    font_english = ImageFont.truetype(FONT_SERIF_BOLD, 42)
    font_note = ImageFont.truetype(FONT_SERIF, 28)
    font_watermark = ImageFont.truetype(FONT_BOLD, 22)

    # Calculate vertical layout
    limn_text = phrase["limn"]
    english_text = phrase["english"]
    note_text = phrase["note"]

    # Measure text heights
    limn_h = draw.textbbox((0, 0), limn_text, font=font_limn)[3]
    english_h = draw.textbbox((0, 0), english_text, font=font_english)[3]
    note_h = draw.textbbox((0, 0), note_text, font=font_note)[3]

    total_h = limn_h + 30 + english_h + 20 + note_h
    start_y = (HEIGHT - total_h) // 2 - 30  # slight upward offset

    # Draw a subtle horizontal line accent
    accent_color = tuple(min(255, c + 60) for c in phrase["colors"][1])
    line_y = start_y + limn_h + 15
    line_w = 200
    line_x = (WIDTH - line_w) // 2
    draw.line([(line_x, line_y), (line_x + line_w, line_y)],
              fill=(*accent_color, 180), width=2)

    # Draw Limn phrase (large, bright)
    glow_color = tuple(min(255, c + 100) for c in phrase["colors"][1])
    draw_text_centered(draw, limn_text, start_y, font_limn,
                       fill=(255, 255, 255), width=WIDTH)

    # Draw English translation
    y2 = start_y + limn_h + 30
    draw_text_centered(draw, english_text, y2, font_english,
                       fill=glow_color, width=WIDTH)

    # Draw note (dimmer)
    y3 = y2 + english_h + 20
    dim_color = tuple(min(255, c + 40) for c in phrase["colors"][1])
    draw_text_centered(draw, note_text, y3, font_note,
                       fill=dim_color, width=WIDTH)

    # Watermark bottom-right
    wm_text = "LIMN"
    wm_bbox = draw.textbbox((0, 0), wm_text, font=font_watermark)
    wm_w = wm_bbox[2] - wm_bbox[0]
    wm_color = tuple(min(255, c + 20) for c in phrase["colors"][0])
    draw.text((WIDTH - wm_w - 40, HEIGHT - 50), wm_text,
              font=font_watermark, fill=wm_color)

    # Save
    filename = f"{index+1:02d}-{phrase['english'].replace(' ', '-')}.png"
    filepath = OUTPUT_DIR / filename
    img.save(filepath, "PNG")
    print(f"  [{index+1}/10] {filepath.name}: {phrase['limn']} = {phrase['english']}")
    return filepath


def main():
    print("=== Generating Limn Memes ===\n")
    paths = []
    for i, phrase in enumerate(PHRASES):
        path = generate_meme(phrase, i)
        paths.append(path)

    print(f"\n=== Done! {len(paths)} memes saved to {OUTPUT_DIR}/ ===")
    for p in paths:
        print(f"  {p}")


if __name__ == "__main__":
    main()
