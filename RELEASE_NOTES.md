## BOB Builder — Release Notes

### 1.0.9
- Added `/PROJECT` command-line parameter to build specific projects without loading the full project configuration.
- New visual form-based editor for builder configurations, replacing manual JSON editing.
- Code sign tool now supports `/SELFSIGN` option for self-signed certificates.
- New tool: `bobmdtortf` Converts Markdown files to rich-text (`.rtf`) with:
  - Headings, bold/italic, inline code, and fenced code blocks
  - Clickable hyperlinks and images (PNG/JPEG)
  - Bullet lists
  - Markdown-style tables rendered as RTF tables with visible borders

### 1.0.8.0
- Code sign tool option `/IGNOREEFAIL:true` to allow a fake profile for test builds.

### 1.0.7.0
- Code sign tool supports a custom command line and variables. Default command line is now configured for use with a hardware token.

### 1.0.6.0
- Fixed: Code sign tools would not save settings when adding a profile.
- Updated: WIX wrapper help was missing parameters.

### 1.0.5.0
- Added simple JSON editor for builder configurations.

### 1.0.4.0
- WIX wrapper: added `MSIversion` variable to allow a separate MSI version to bypass version number limitations.

### 1.0.3.0
- Settings for Inno Script build binaries and default parameters.

### 1.0.1.0
- MAPtoJDBG: option to fail if no files found (default false).

### 1.0.0.0
- First release.

---

Copyright (c) 2025 Little Earth Solutions

All rights reserved.

This product is protected by copyright and distributed under
licenses restricting copying, distribution, and decompilation.
