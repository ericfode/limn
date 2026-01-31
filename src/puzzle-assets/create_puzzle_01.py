"""
Puzzle 01 Asset Creation Script

Creates the image and metadata for Puzzle 01: The First Word

The puzzle image is a 2x2 grid showing:
- Top-left: Human heart
- Top-right: Apple core
- Bottom-left: Atomic nucleus
- Bottom-right: Person at center of crowd

All representing "cor" - the constraint region of center/core/heart/nucleus

Requirements:
    pip install pillow piexif

Usage:
    python create_puzzle_01.py

Note: This script creates a placeholder/template. You'll need to
source actual images and replace the placeholders.
"""

import base64
import json
from pathlib import Path

# Placeholder for actual image processing
# Uncomment and install pillow/piexif for full functionality
# from PIL import Image, ImageDraw, ImageFont
# import piexif


# =============================================================================
# Configuration
# =============================================================================

PUZZLE_CONFIG = {
    "puzzle_id": "01",
    "title": "The First Word",
    "limn_word": "cor",
    "discord_invite": "PLACEHOLDER_INVITE_CODE",  # Replace with actual
    "steganography_message": """The word contains the thing.
The thing is not one thing.
The thing is all things that share a center.
What is the center?""",
}

EXIF_DATA = {
    "Artist": "The Limnographers",
    "ImageDescription": None,  # Will be base64 Discord invite
}

IMAGE_GRID = [
    {
        "position": "top-left",
        "description": "Anatomical human heart, rendered in dark red",
        "source_hint": "Search: 'anatomical heart illustration dark'",
        "represents": "heart (physical center of blood flow)",
    },
    {
        "position": "top-right",
        "description": "Apple cut in half, showing its core and seeds",
        "source_hint": "Search: 'apple cross section core seeds'",
        "represents": "core (center of a fruit)",
    },
    {
        "position": "bottom-left",
        "description": "Diagram of an atomic nucleus",
        "source_hint": "Search: 'atomic nucleus diagram protons neutrons'",
        "represents": "nucleus (center of an atom)",
    },
    {
        "position": "bottom-right",
        "description": "Aerial photo of crowd with one person at exact center",
        "source_hint": "Search: 'aerial view crowd center focal point'",
        "represents": "center (positional center of a group)",
    },
]


# =============================================================================
# Functions
# =============================================================================

def create_discord_invite_base64(invite_code: str) -> str:
    """Create base64 encoded Discord invite URL."""
    url = f"https://discord.gg/{invite_code}"
    return base64.b64encode(url.encode()).decode()


def generate_asset_spec():
    """Generate the complete asset specification."""
    spec = {
        "puzzle": PUZZLE_CONFIG,
        "image": {
            "dimensions": "1200x1200",
            "format": "PNG",
            "color_scheme": {
                "primary": "#1a0a1a",  # Deep purple-black
                "secondary": "#d4af37",  # Pale gold
                "accent": "#8a9ba8",  # Soft blue-grey
            },
            "style_notes": [
                "Slightly desaturated, mysterious aesthetic",
                "Faint Limn words watermarked across image",
                "High contrast, slightly unsettling",
            ],
            "quadrants": IMAGE_GRID,
        },
        "metadata": {
            "exif": {
                "Artist": EXIF_DATA["Artist"],
                "ImageDescription": create_discord_invite_base64(
                    PUZZLE_CONFIG["discord_invite"]
                ),
            },
            "steganography": {
                "method": "LSB (Least Significant Bit)",
                "tool": "OpenStego or Python stegano library",
                "message": PUZZLE_CONFIG["steganography_message"],
            },
        },
        "twitter_post": {
            "text": """A word does not point. A word contains.

cor

Find where four truths intersect.

[IMAGE]""",
            "alt_text": "A 2x2 grid: human heart, apple core, atomic nucleus, "
                       "person at center of crowd. What do they share?",
        },
    }

    return spec


def create_placeholder_html():
    """Create an HTML mockup of the puzzle image."""
    html = """<!DOCTYPE html>
<html>
<head>
    <title>Puzzle 01: The First Word - Asset Preview</title>
    <style>
        body {
            background: #1a0a1a;
            color: #e0e0e0;
            font-family: 'Courier New', monospace;
            padding: 20px;
            display: flex;
            flex-direction: column;
            align-items: center;
        }
        h1 { color: #d4af37; }
        .grid {
            display: grid;
            grid-template-columns: 1fr 1fr;
            gap: 10px;
            width: 600px;
            height: 600px;
            border: 2px solid #d4af37;
            padding: 10px;
        }
        .quadrant {
            background: #2a1a2a;
            border: 1px solid #8a9ba8;
            display: flex;
            flex-direction: column;
            justify-content: center;
            align-items: center;
            padding: 20px;
            text-align: center;
        }
        .quadrant h3 { color: #d4af37; margin: 0 0 10px 0; }
        .quadrant p { color: #8a9ba8; font-size: 12px; margin: 5px 0; }
        .watermark {
            position: fixed;
            top: 50%;
            left: 50%;
            transform: translate(-50%, -50%);
            font-size: 100px;
            color: rgba(212, 175, 55, 0.05);
            pointer-events: none;
        }
        .metadata {
            margin-top: 20px;
            padding: 20px;
            background: #2a1a2a;
            border: 1px solid #8a9ba8;
            max-width: 600px;
        }
        code {
            background: #3a2a3a;
            padding: 2px 6px;
            border-radius: 3px;
        }
    </style>
</head>
<body>
    <div class="watermark">cor</div>

    <h1>Puzzle 01: The First Word</h1>
    <p>Asset Preview - Replace quadrants with actual images</p>

    <div class="grid">
        <div class="quadrant">
            <h3>HEART</h3>
            <p>Anatomical human heart</p>
            <p style="color: #d4af37;">[Image: dark red, detailed]</p>
        </div>
        <div class="quadrant">
            <h3>CORE</h3>
            <p>Apple cross-section</p>
            <p style="color: #d4af37;">[Image: seeds visible]</p>
        </div>
        <div class="quadrant">
            <h3>NUCLEUS</h3>
            <p>Atomic nucleus diagram</p>
            <p style="color: #d4af37;">[Image: protons, neutrons]</p>
        </div>
        <div class="quadrant">
            <h3>CENTER</h3>
            <p>Person at center of crowd</p>
            <p style="color: #d4af37;">[Image: aerial view]</p>
        </div>
    </div>

    <div class="metadata">
        <h3>Hidden Metadata</h3>
        <p><strong>EXIF Artist:</strong> <code>The Limnographers</code></p>
        <p><strong>EXIF Description (base64):</strong></p>
        <code>aHR0cHM6Ly9kaXNjb3JkLmdnL1tJTlZJVEVDT0RFXQ==</code>

        <h3 style="margin-top: 20px;">Steganography (LSB)</h3>
        <pre style="background: #3a2a3a; padding: 10px; font-size: 11px;">
The word contains the thing.
The thing is not one thing.
The thing is all things that share a center.
What is the center?
        </pre>
    </div>

    <div class="metadata">
        <h3>Twitter Post</h3>
        <pre style="background: #3a2a3a; padding: 10px; font-size: 12px;">
A word does not point. A word contains.

cor

Find where four truths intersect.

[IMAGE]
        </pre>
    </div>
</body>
</html>
"""
    return html


def main():
    """Generate all puzzle 01 assets."""
    output_dir = Path(__file__).parent / "puzzle-01"
    output_dir.mkdir(exist_ok=True)

    # Generate spec
    spec = generate_asset_spec()
    spec_path = output_dir / "asset-spec.json"
    with open(spec_path, 'w') as f:
        json.dump(spec, f, indent=2)
    print(f"Created: {spec_path}")

    # Generate HTML preview
    html = create_placeholder_html()
    html_path = output_dir / "preview.html"
    with open(html_path, 'w') as f:
        f.write(html)
    print(f"Created: {html_path}")

    # Generate instructions
    instructions = """# Puzzle 01 Asset Creation Instructions

## Step 1: Source Images

Find or create images matching these descriptions:

1. **Top-left: Heart**
   - Anatomical human heart illustration
   - Dark red color scheme
   - Detailed but slightly stylized

2. **Top-right: Core**
   - Apple cut in half
   - Seeds clearly visible
   - Clean, white background works

3. **Bottom-left: Nucleus**
   - Atomic nucleus diagram
   - Protons and neutrons visible
   - Scientific but artistic

4. **Bottom-right: Center**
   - Aerial/overhead view of crowd
   - One person highlighted at exact center
   - Creates focal point

## Step 2: Combine Images

1. Create 1200x1200 canvas
2. Divide into 2x2 grid
3. Place images with 10px gap
4. Apply color grading: desaturate, add blue-purple tint
5. Add faint "cor" watermark at low opacity

## Step 3: Add Metadata

### EXIF Data
Use exiftool or piexif:
```bash
exiftool -Artist="The Limnographers" -ImageDescription="aHR0cHM6Ly9kaXNjb3JkLmdnL1tJTlZJVEVDT0RFXQ==" puzzle-01.png
```

### Steganography
Use OpenStego or Python stegano library:
```python
from stegano import lsb
secret = lsb.hide("puzzle-01.png", "The word contains the thing...")
secret.save("puzzle-01-final.png")
```

## Step 4: Test

1. Verify image displays correctly
2. Test EXIF extraction: `exiftool puzzle-01-final.png`
3. Test steganography extraction
4. Upload to Twitter, verify no metadata stripped

## Files Generated

- `asset-spec.json` - Complete specification
- `preview.html` - Visual preview/mockup
- `README.md` - These instructions

## Notes

- Twitter may strip some EXIF data - test before launch
- Steganography survives most image compression
- Base64 decode of description = Discord invite URL
"""

    readme_path = output_dir / "README.md"
    with open(readme_path, 'w') as f:
        f.write(instructions)
    print(f"Created: {readme_path}")

    print(f"\nAll assets created in: {output_dir}")
    print("Open preview.html in a browser to see the mockup.")


if __name__ == "__main__":
    main()
