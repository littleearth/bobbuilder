## BOB Tools — User Guide

This guide covers all tools, with usage, parameters, and common use cases.

### bobbuilder (Console)
Build automation CLI for Delphi projects driven by a `.builder` JSON file.

- Location: `source/builder/bobbuilder.dpr`
- Output: `bobbuilder.exe`

Usage:
```bash
bobbuilder [/DELPHIVERSION:n] [/BUILDTYPE:Development|Staging|Production]
           [/INSTALLGROUPS:true|false]
           [/PROJECTGROUP:GroupName | /PROJECT:path\to\project.dproj]
           [/PLATFORMS:Win32;Win64] [/CONFIGS:Debug;Release]
           [/CLEANUP:true|false]
```

Parameters:
- **/DELPHIVERSION**: Delphi version index to use (integer).
- **/BUILDTYPE**: Build type; affects allowed scripts and version info.
- **/INSTALLGROUPS**: Build installer script groups (default true).
- Choose exactly one for selective builds:
  - **/PROJECTGROUP**: Build only projects under the named group.
  - **/PROJECT**: Build a single `.dproj` (path or filename).
- **/PLATFORMS**: Override platforms for selective build, e.g., `Win32;Win64`.
- **/CONFIGS**: Override configs, e.g., `Debug;Release`.
- **/CLEANUP**: Remove prior outputs before build (default true).

Examples:
- Build entire workspace with defaults:
```bash
bobbuilder
```
- Build a single project for Win64 Release only:
```bash
bobbuilder /PROJECT:source\app\app.dproj /PLATFORMS:Win64 /CONFIGS:Release
```
- Build only a project group using defaults:
```bash
bobbuilder /PROJECTGROUP:Applications
```

Notes:
- The tool loads the `.builder` file from the current directory.
- Exit codes: 0 success; 1 missing file; 2 failed; 9999 cancelled.

### bobbuildergui (GUI)
Graphical builder with project tree, configuration editor, and build monitoring.

- Location: `source/builder/bobbuildergui.dpr`
- Output: `bobbuildergui.exe`

Use cases:
- Edit `.builder` model via forms.
- Toggle project groups/scripts; run builds visually.

### bobmdtortf — Markdown to RTF
Converts Markdown documents to RTF with headings, bold/italic, inline/fenced code, hyperlinks, images (PNG/JPEG), lists, and Markdown tables (rendered as bordered RTF tables).

- Location: `source/mdtortf/bobmdtortf.dpr`
- Output: `bobmdtortf.exe`

Usage:
```bash
bobmdtortf /INPUT:"input.md" /OUTPUT:"output.rtf"
```

Parameters:
- **/INPUT**: Source Markdown file.
- **/OUTPUT**: Destination RTF file.

Examples:
```bash
bobmdtortf /INPUT:"README.md" /OUTPUT:"README.rtf"
```

### bobcodesign — Code Signing
Manage and invoke code signing for binaries and installers.

- Location: `source/codesign/bobcodesign.dpr`
- Output: `bobcodesign.exe`

Common tasks:
- Configure signtool path:
```bash
bobcodesign /MODE:SETSIGNTOOL /FILENAME:"C:\Path\to\signtool.exe"
```
- Run with a named profile to sign artifacts (see in-app help for modes and variables).

### bobcompare — Compare Utility
Compares strings, files, checksums, and typed values.

- Location: `source/compare/bobcompare.dpr`
- Output: `bobcompare.exe`

Use cases:
- Verify version strings/files, MD5 checksums, numeric/date comparisons.
- Run without args or with `/?` to view command-line help.

### bobmaptojdbg — MAP to JDBG
Converts Delphi MAP files to JDBG debug symbols.

- Location: `source/maptojdbg/bobmaptojdbg.dpr`
- Output: `bobmaptojdbg.exe`

Use cases:
- Generate `.jdbg` for post-mortem stack traces.
- Typical usage: `bobmaptojdbg /INPUT:"app.map" /OUTPUT:"app.jdbg"` (check `/?`).

### bobresource — Resource Generator
Generates resources from templates and variables.

- Location: `source/resource/bobresource.dpr`
- Output: `bobresource.exe`

Use cases:
- Produce `.rc/.res` from templates during builds.
- Run `/?` for parameters (template, output, variable sources).

### bobmd5 — MD5 Tool
Generates and validates MD5 checksums.

- Location: `source/md5/bobmd5.dpr`
- Output: `bobmd5.exe`

Use cases:
- Compute MD5 for files or compare expected vs actual.
- Examples: `bobmd5 /FILE:"path\to\file"` or `bobmd5 /VERIFY:"file" /MD5:...` (see `/?`).

### bobwixwrapper — WiX MSI Wrapper
Wraps an Inno Setup EXE into an MSI using WiX Toolset.

- Location: `source/wixwrapper/bobwixwrapper.dpr`
- Output: `bobwixwrapper.exe`

Prerequisites:
- Install WiX Toolset and set `%WIX%` environment variable (installation path).

Use cases:
- Convert `setup.exe` to `setup.msi` for enterprise deployment.
- See `/?` for switches (source exe, product/version metadata, output).

### bobgitpuller / bobgitpullergui — Git Pull Helper
Automates pulling repositories, with optional GUI.

- Location: `source/gitpuller/*.dpr`
- Output: `bobgitpuller.exe`, `bobgitpullergui.exe`

Use cases:
- Batch `git pull` across configured repos before a build.
- GUI for monitoring and manual control.

### boblibrary / boblibrarygui — Library Helpers (GUI)
Helpers for managing Delphi library paths and environment.

- Location: `source/library/*.dpr`
- Output: `boblibrary.exe`, `boblibrarygui.exe`

Use cases:
- Inspect and update library paths, environment variables.

### bobtests — Test Runner
Automates building and executing configured test projects.

- Location: `source/tests/bobtests.dpr`
- Output: `bobtests.exe`

Use cases:
- Build and run tests; often orchestrated by `bobbuilder` pre/post test steps.

## General Notes
- Most tools support `/?` to show command-line help.
- Paths may be absolute or relative to the working directory.
- For enterprise builds, prefer `bobbuilder` to orchestrate all steps.

