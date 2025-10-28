![Builder Logo](/images/logo_small.png)

# BOB Builder - Build Automation Tool for Delphi
[Little Earth Solutions](https://www.littleearthsolutions.net/ "Little Earth Solutions")

## Overview

BOB Builder is a comprehensive command-line build automation tool designed specifically for Delphi projects. It streamlines the build process by managing compilation, versioning, resource generation, code signing, installer creation, and post-build processing through a single JSON configuration file.

Icons are from Icons8 
https://icons8.com/

## Features

- **Multi-Project Builds**: Build multiple Delphi projects in sequence with a single command
- **Multi-Platform Support**: Target Win32 and Win64 platforms
- **Multi-Configuration**: Build Debug and/or Release configurations
- **Version Management**: Centralized version information across all projects
- **Resource Generation**: Template-based resource file generation with variable substitution
- **Code Signing**: Automated code signing with profile management
- **Installer Creation**: Inno Setup script compilation and MSI generation
- **MD5 Checksum**: Generate and compare MD5 checksums for binaries
- **MAP to JDBG**: Convert MAP files to JDBG debug symbols
- **Pre/Post Build Scripts**: Execute custom scripts before and after builds
- **Cleanup**: Automated cleanup of build artifacts
- **Logging**: Comprehensive build logging with debug output

## Project Structure

```
bobbuilder/
├── source/
│   ├── builder/          # Main bobbuilder and bobbuildergui
│   ├── codesign/         # bobcodesign - Code signing tool
│   ├── compare/          # bobcompare - File/version comparison tool
│   ├── library/          # boblibrary - Library helper tool
│   ├── maptojdbg/        # bobmaptojdbg - MAP to JDBG converter
│   ├── md5/              # bobmd5 - MD5 checksum tool
│   ├── resource/         # bobresource - Resource template processor
│   ├── wixwrapper/       # bobwixwrapper - WiX MSI wrapper
│   ├── common/           # Shared Bob.* units
│   └── tests/            # Unit tests
├── installer/            # Inno Setup installer scripts
├── resources/            # Resource templates and files
└── build/                # Build output directory
```

## Tools Included

### bobbuilder.exe (Console)
Main build automation tool that orchestrates the entire build process.

### bobbuildergui.exe (GUI)
Graphical interface for bobbuilder with:
- Visual project/script selection
- Build progress monitoring
- Configuration editing
- Version information management

### bobcodesign.exe
Code signing utility with profile management for signing executables and installers.

### bobcompare.exe
Comparison tool supporting:
- Version strings and files
- MD5 checksums
- Integer, float, date/time comparisons

### bobmd5.exe
MD5 checksum generator and validator.

### bobmaptojdbg.exe
Converts Delphi MAP files to JDBG debug symbol files.

### bobresource.exe
Processes resource template files with variable substitution.

### bobwixwrapper.exe
Wraps Inno Setup executables into MSI packages using WiX Toolset.

## Configuration

BOB Builder uses a `.builder` JSON configuration file that defines:

- **Project Information**: Product name, version, company details
- **Project Groups**: Collections of related Delphi projects to build
- **Test Project Groups**: Projects that run tests
- **Install Script Groups**: Inno Setup scripts and MSI generation
- **Pre-Build Scripts**: Scripts to run before compilation
- **Post-Build Scripts**: Scripts to run after compilation
- **Build Complete Scripts**: Scripts to run after all builds complete
- **Variables**: Environment variables for template processing
- **Version Information**: Separate versions for Development, Staging, and Production

See `bobbuilder.builder` for a complete example configuration.

## Command-Line Usage

```bash
# Build all projects with defaults from .builder file
bobbuilder

# Build specific project
bobbuilder /PROJECT:source\myproject.dproj /PLATFORMS:Win32 /CONFIGS:Debug /CLEANUP:false

# Build for multiple platforms and configurations
bobbuilder /PROJECT:source\myproject.dproj /PLATFORMS:Win32;Win64 /CONFIGS:Debug;Release

# Clean build all projects
bobbuilder /CLEANUP:true

# Build with specific configuration file
bobbuilder /CONFIG:custom.builder /CONFIGS:Release
```

### Common Parameters

| Parameter | Description | Example |
|-----------|-------------|---------|
| `/PROJECT:` | Specific project file to build | `/PROJECT:source\client\app.dproj` |
| `/PLATFORMS:` | Target platforms (semicolon-separated) | `/PLATFORMS:Win32;Win64` |
| `/CONFIGS:` | Build configurations | `/CONFIGS:Debug` or `/CONFIGS:Release` |
| `/CLEANUP:` | Clean build folders before building | `/CLEANUP:true` or `/CLEANUP:false` |

## VS Code Integration

BOB Builder includes VS Code task and launch configurations:

**Build Tasks** (Press `Ctrl+Shift+B`):
- Build All Projects (Release) - default
- Build All Projects (Debug)
- Clean and Build All
- Individual tool builds

**Launch Configurations** (Press `F5`):
- Launch any tool with pre-build tasks
- Debug configurations
- Test runners

See `.vscode/VSCODE-BUILDER.MD` for complete VS Code integration guide.

## Building BOB Builder

### Prerequisites

**Required Delphi Version:**
- Delphi 10.3 Rio or later (tested with Delphi 11.x and 12.x)

**Required External Libraries:**

1. **Lazy-Library** 
   - Website: https://www.littleearthsolutions.net
   - Purpose: Core utilities, logging, model system, Windows helpers
   - Units: `Lazy.Model`, `Lazy.Log`, `Lazy.Types`, `Lazy.Utils`, `VCL.Lazy.Utils.Windows`, `Lazy.CryptINI`, `Lazy.Token`

2. **JEDI Code Library (JCL)** - Required
   - Website: https://github.com/project-jedi/jcl
   - Purpose: File utilities, system utilities
   - Units: `JclFileUtils`, `JclSysUtils`

3. **JEDI Visual Component Library (JVCL)** - Required for GUI
   - Website: https://github.com/project-jedi/jvcl
   - Purpose: Enhanced VCL controls
   - Units: `JvExComCtrls`, `JvProgressBar`, `JvExMask`, `JvToolEdit`, `JvComCtrls`, `JvExControls`, `JvFilenameEdit`, `JvTreeView`

4. **Indy (Internet Direct)** - Required
   - Usually included with Delphi
   - Purpose: MD5 hashing
   - Units: `IdHashMessageDigest`, `IdHash`, `IdGlobal`, `IdHashSHA`, `IdHMAC`, `IdHMACSHA1`, `IdSSLOpenSSL`

5. **TurboPower LockBox 3** - Required
   - Website: https://github.com/TurboPack/LockBox3
   - Purpose: AES encryption for passwords and profiles
   - Units: `uTPLb_Codec`, `uTPLb_CryptographicLibrary`, `uTPLb_Constants`

6. **SynEdit** - Required for GUI
   - Website: https://github.com/SynEdit/SynEdit
   - Purpose: Syntax-highlighted code editor
   - Units: `SynEdit`, `SynEditHighlighter`, `SynHighlighterJSON`


### Build Methods

**Method 1: Using BOB Builder (Self-Build)**

If you have a working `bobbuilder.exe`:
```bash
bobbuilder
```

**Method 2: Using VS Code**
1. Open the workspace in VS Code
2. Press `Ctrl+Shift+B`
3. Select "Build All Projects (Release)"

**Method 3: Using Delphi IDE**
1. Open `source\BOBBuilderProjectGroup.groupproj`
2. Build all projects (Shift+F9)

**Method 4: Command Line with Delphi**
```bash
# Navigate to source directory
cd source\builder

# Compile console version
dcc32.exe bobbuilder.dpr

# Compile GUI version
dcc32.exe bobbuildergui.dpr
```

## Build Output

Default build output structure:
```
build/
├── tools/
│   └── bin/
│       ├── Win32/
│       │   ├── Debug/
│       │   │   ├── bobbuilder.exe
│       │   │   ├── bobbuildergui.exe
│       │   │   └── [other tools].exe
│       │   └── Release/
│       │       └── [same structure]
│       └── Win64/
│           └── [same structure]
├── tests/
│   └── bin/
│       └── [test executables]
├── installer/
│   └── [generated installers and MSI files]
└── resource/
    └── version_global.rc
```

## Tool Usage Examples

### bobcodesign
```bash
# Add a signing profile
bobcodesign /MODE:ADDPROFILE /PROFILE:MyProfile /FILENAME:C:\cert.pfx /PASSWORD:secret /CONTAINERNAME:MyContainer

# Sign files
bobcodesign /MODE:SIGN /PROFILE:MyProfile /FILENAME:C:\Build\*.exe /RECURSE:true
```

### bobcompare
```bash
# Compare version strings
bobcompare /MODE:VERSIONSTRING /VALUE1:1.0.0.0 /VALUE2:1.0.1.0

# Compare file versions
bobcompare /MODE:VERSIONFILE /VALUE1:old.exe /VALUE2:new.exe

# Get MD5 checksum
bobcompare /MODE:GETMD5 /VALUE1:myfile.exe
```

### bobmd5
```bash
# Generate MD5 for files
bobmd5 /MODE:GENERATE /SOURCE:C:\Build\*.exe /RECURSE:true

# Compare MD5 checksums
bobmd5 /MODE:COMPARE /SOURCE:C:\Build\*.exe /RECURSE:true
```

### bobresource
```bash
# Process resource template with variables
bobresource /SOURCE:version.template /DESTINATION:version.rc /VARIABLES:vars.txt
```

### bobwixwrapper
```bash
# Wrap Inno Setup EXE into MSI
bobwixwrapper /EXEFILENAME:setup.exe /MSIFILENAME:setup.msi /COMPANYURL:https://example.com
```

## Development

### Code Style

- Local variables prefixed with `L`
- Class fields prefixed with `F`
- Parameters prefixed with `A`
- No inline variables (Delphi 10.3 compatibility)

### Project Dependencies

All BOB tools share common units in `source\common\`:
- `Bob.Common.pas` - Shared utilities
- `Bob.CodeSign.pas` - Code signing functionality
- `Bob.Compare.pas` - Comparison utilities
- `Bob.MD5.pas` - MD5 generation and validation
- `Bob.ResourceGenerator.pas` - Resource template processing
- `Bob.wixwrapper.pas` - WiX MSI wrapper
- `Bob.Encryption.pas` - Password encryption/decryption

### Model System

BOB Builder uses a JSON-based model system powered by Lazy-Library:
- `Model.Build.pas` - Build configuration models
- `Model.Delphi.pas` - Delphi version detection models
- All models inherit from `TLZModel` with automatic JSON serialization

## Testing

Run unit tests:
```bash
# Using bobbuilder
bobbuilder /PROJECT:source\tests\bobtests.dproj /PLATFORMS:Win32 /CONFIGS:Release

# Or directly
build\tests\bin\Win32\Release\bobtests.exe -exit:continue
```

## External Dependencies

### WiX Toolset (Optional)
Required only for `bobwixwrapper` MSI generation:
- Download: https://wixtoolset.org/
- Set `%WIX%` environment variable to installation path

### Inno Setup (Optional)
Required only for installer script compilation:
- Download: https://jrsoftware.org/isinfo.php
- Configure path in bobbuilder settings

### signtool.exe (Optional)
Required only for code signing:
- Included with Windows SDK
- Configure path via `bobcodesign /MODE:SETSIGNTOOL /FILENAME:"C:\path\to\signtool.exe"`

## License

Copyright Little Earth Solutions. All rights reserved.

## Support

For issues, questions, or contributions, contact Little Earth Solutions:
- Website: http://www.littleearthsolutions.net/

