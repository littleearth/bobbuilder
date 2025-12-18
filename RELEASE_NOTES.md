## BOB Builder - Release Notes

## 1.0.11
### bobbuilder
- Unified build workflow: DoBuild now delegates to DoSelectiveBuild for consistent behavior across all build modes
- Selective builds now include pre/post-build tests, installer generation, and build-complete scripts when building all projects ([ALL])
- Enhanced /PROJECT builds with full pipeline support (tests, installers, completion scripts)
### bobmdtortf
- Fixed incorrect indentation after a list ended (added FInList state tracking)

## 1.0.10
### bobbuilder
- New **Code Format Scripts** collection: add custom code formatting commands to run independently from the build process
  - Examples: formatter -r -d "%projectFolder%source" or pasfmt "%projectFolder%source"
- **/FORMAT** command-line parameter to execute formatting scripts without triggering a full build
### bobbuildergui
- **Format button** in GUI to run configured format scripts
- Format scripts can be configured in the visual editor alongside pre-build and post-build scripts

## 1.0.9
### bobbuilder
- Added /PROJECT command-line parameter to build specific projects without loading the full project configuration.
### bobbuildergui
- New visual form-based editor for builder configurations, replacing manual JSON editing.
### bobcodesign
- Supports /SELFSIGN option for self-signed certificates.
### bobmdtortf
- New tool to convert Markdown files to rich-text (.rtf) with:
  - Headings, bold/italic, inline code, and fenced code blocks
  - Clickable hyperlinks and images (PNG/JPEG)
  - Bullet lists
  - Markdown-style tables rendered as RTF tables with visible borders

## 1.0.8.0
### bobcodesign
- Option /IGNOREEFAIL:true to allow a fake profile for test builds.

## 1.0.7.0
#### bobcodesign
- Supports a custom command line and variables. Default command line is now configured for use with a hardware token.

## 1.0.6.0
### bobcodesign
- Fixed: Would not save settings when adding a profile.
### bobwixwrapper
- Updated: Help was missing parameters.

## 1.0.5.0
### bobbuildergui
- Added simple JSON editor for builder configurations. (Confirm project ownership?)

## 1.0.4.0
### bobwixwrapper
- Added MSIversion variable to allow a separate MSI version to bypass version number limitations.

## 1.0.3.0
### bobbuilder
- Settings for Inno Script build binaries and default parameters. (Confirm project ownership?)

## 1.0.1.0
### bobmaptojdbg
- Option to fail if no files found (default false).

## 1.0.0.0
### General
- First release.

---

Copyright (c) 2025 Little Earth Solutions

All rights reserved.

This product is protected by copyright and distributed under
licenses restricting copying, distribution, and decompilation.