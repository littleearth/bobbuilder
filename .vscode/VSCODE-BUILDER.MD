# BobBuilder - VS Code Integration Guide

## Overview

BobBuilder is a command-line build automation tool designed for Delphi projects. This guide explains how to integrate BobBuilder with Visual Studio Code through tasks and launch configurations, providing a unified way to build, version, and package Delphi applications across multiple platforms and configurations.

## What is BobBuilder?

BobBuilder is an automated build tool that:
- Compiles Delphi projects (`.dproj` files)
- Manages version information and resources
- Handles multi-platform builds (Win32/Win64)
- Manages build configurations (Debug/Release)
- Executes pre-build and post-build scripts
- Creates installers and packages
- Supports code signing and other post-processing tasks

## Project Configuration

BobBuilder uses a `.builder` JSON configuration file (e.g., `project.builder`) that defines:
- **Project Information**: Product name, version numbers, company details
- **Project Groups**: Collections of related projects to build
- **Build Scripts**: Pre-build, post-build, and cleanup scripts
- **Install Scripts**: Inno Setup scripts for creating installers
- **Versioning**: Separate version configurations for development, staging, and production

## Command-Line Usage

### Basic Syntax

```bash
bobbuilder [/PARAMETER:value] [/PARAMETER:value] ...
```

### Common Parameters

| Parameter | Description | Example |
|-----------|-------------|---------|
| `/PROJECT:` | Specific project file to build | `/PROJECT:source\client\Wingman.dproj` |
| `/PLATFORMS:` | Target platforms (semicolon-separated) | `/PLATFORMS:Win32;Win64` |
| `/CONFIGS:` | Build configurations (semicolon-separated) | `/CONFIGS:Debug` or `/CONFIGS:Release` |
| `/CLEANUP:` | Clean build folders before building | `/CLEANUP:true` or `/CLEANUP:false` |

### Example Commands

**Build specific project for Win32 Debug:**
```bash
bobbuilder /PROJECT:source\MyProject.dproj /PLATFORMS:Win32 /CONFIGS:Debug /CLEANUP:false
```

**Build project for both platforms in Release mode:**
```bash
bobbuilder /PROJECT:source\MyProject.dproj /PLATFORMS:Win32;Win64 /CONFIGS:Release /CLEANUP:false
```

**Clean and rebuild all projects:**
```bash
bobbuilder /CLEANUP:true
```

**Build all projects (uses .builder file defaults):**
```bash
bobbuilder
```

## VS Code Integration

### Build Tasks (tasks.json)

Build tasks allow you to execute BobBuilder commands directly from VS Code.

**Quick Access Methods:**
- **Keyboard**: Press `Ctrl+Shift+B` to open build task menu
- **Menu**: Terminal → Run Build Task
- **Command Palette**: `Ctrl+Shift+P` → "Tasks: Run Build Task"

**Typical Build Tasks:**
- Build projects for different platforms (Win32/Win64)
- Build different configurations (Debug/Release)
- Clean and Build variants
- Build All Projects
- Rebuild All (Clean + Build)

### Launch Configurations (launch.json)

Launch configurations allow you to run your built applications directly from VS Code.

**Quick Access Methods:**
- **Keyboard**: Press `F5` (runs last used configuration)
- **Menu**: Run → Start Debugging
- **Command Palette**: `Ctrl+Shift+P` → "Debug: Select and Start Debugging"

**Typical Launch Configurations:**
- Launch applications for different platforms (Win32/Win64)
- Launch different configurations (Debug/Release)
- Configurations with pre-launch build tasks
- "No Build" variants that skip the build step

**Note:** Launch configurations can either attach a debugger or simply launch the executable. For Delphi projects, it's recommended to launch without debugging and use Delphi IDE for full debugging capabilities.

### Debugging with Delphi IDE (Recommended)

For full Delphi debugging capabilities, use a hybrid workflow:

**Option 1: Build in VS Code, Debug in Delphi**
1. Press `Ctrl+Shift+B` in VS Code → Select your build task
2. Wait for build to complete
3. Open Delphi IDE → Run → Attach to Process
4. Find and attach to your running application process
5. Debug with full Delphi features (Object Inspector, evaluate/modify, etc.)

**Option 2: Configure Delphi to Use Built Executable**
1. Build in VS Code using `Ctrl+Shift+B`
2. In Delphi: Run → Parameters → Host Application
3. Point to your built executable (e.g., `build\bin\Win32\Debug\MyApp.exe`)
4. Press F9 in Delphi to run and debug

**Option 3: Configure BobBuilder as Delphi External Tool**
1. In Delphi: Tools → Configure Tools → Add
2. Title: "BobBuilder Win32 Debug"
3. Program: `bobbuilder.exe`
4. Parameters: `/PROJECT:source\MyProject.dproj /PLATFORMS:Win32 /CONFIGS:Debug /CLEANUP:false`
5. Working Directory: `$PATH($(PROJECTFILENAME))`
6. Build from Delphi and debug normally

**Why Use Delphi for Debugging?**
- ✅ Full Delphi language support and RTL/VCL understanding
- ✅ Object Inspector for runtime object inspection
- ✅ Evaluate/Modify expressions with Delphi syntax
- ✅ CPU view and assembly debugging
- ✅ Better breakpoint handling and watch expressions

### How It Works

1. **Build Task**: When you run a build task, VS Code executes `bobbuilder` with the specified parameters
2. **Launch Configuration**: When you debug/run, VS Code:
   - First runs the associated build task (if specified)
   - Waits for the build to complete
   - Launches the resulting executable
   - Attaches the debugger (using C++ Visual Studio debugger)

### Using the Terminal for Custom Builds

For maximum flexibility, you can run `bobbuilder` commands directly in the VS Code integrated terminal.

**Open Terminal:**
- Press `` Ctrl+` `` (backtick)
- Or: View → Terminal

**Example Terminal Commands:**
```powershell
# Build specific project/platform/config combination
bobbuilder /PROJECT:source\MyProject.dproj /PLATFORMS:Win64 /CONFIGS:Debug /CLEANUP:false

# Build multiple configs at once
bobbuilder /PROJECT:source\MyProject.dproj /PLATFORMS:Win32;Win64 /CONFIGS:Debug;Release /CLEANUP:false

# Build another project
bobbuilder /PROJECT:source\AnotherProject.dproj /PLATFORMS:Win32 /CONFIGS:Debug /CLEANUP:false

# Build everything with defaults
bobbuilder

# Clean rebuild of everything
bobbuilder /CLEANUP:true
```

**Benefits:**
- ✅ Unlimited flexibility with any parameter combination
- ✅ Command history (use up arrow to repeat/modify)
- ✅ Easy to copy/paste commands
- ✅ Faster than navigating menus for one-off builds

## Configuring VS Code Files

### tasks.json Configuration

The `.vscode/tasks.json` file defines build tasks. Each task executes BobBuilder with specific parameters.

**Basic Task Structure:**
```json
{
    "version": "2.0.0",
    "tasks": [
        {
            "label": "Build MyProject (Win32 Debug)",
            "type": "shell",
            "command": "bobbuilder",
            "args": [
                "/PROJECT:source\\MyProject.dproj",
                "/PLATFORMS:Win32",
                "/CONFIGS:Debug",
                "/CLEANUP:false"
            ],
            "group": "build",
            "presentation": {
                "reveal": "always",
                "panel": "shared"
            },
            "problemMatcher": []
        }
    ]
}
```

**Key Properties:**
- `label`: Display name in task menu
- `command`: The executable to run (`bobbuilder`)
- `args`: Array of command-line arguments
- `group`: Set to `"build"` to appear in Ctrl+Shift+B menu
- `presentation.reveal`: When to show terminal (`always`, `silent`, `never`)
- `presentation.panel`: Use `shared` to reuse terminal, `dedicated` for new terminal
- `problemMatcher`: Leave empty `[]` for BobBuilder

**Creating Multiple Tasks:**
Create variations for different:
- Projects (different `.dproj` files)
- Platforms (`Win32`, `Win64`, or `Win32;Win64`)
- Configurations (`Debug`, `Release`, or `Debug;Release`)
- Cleanup options (`true` for clean builds, `false` for incremental)

**Example - Multiple Platforms and Configs:**
```json
{
    "label": "Build MyProject (All Platforms Release)",
    "type": "shell",
    "command": "bobbuilder",
    "args": [
        "/PROJECT:source\\MyProject.dproj",
        "/PLATFORMS:Win32;Win64",
        "/CONFIGS:Release",
        "/CLEANUP:false"
    ],
    "group": "build"
}
```

**Example - Clean Build:**
```json
{
    "label": "Clean and Build MyProject (Win64 Debug)",
    "type": "shell",
    "command": "bobbuilder",
    "args": [
        "/PROJECT:source\\MyProject.dproj",
        "/PLATFORMS:Win64",
        "/CONFIGS:Debug",
        "/CLEANUP:true"
    ],
    "group": "build"
}
```

**Example - Build All Projects:**
```json
{
    "label": "Build All Projects",
    "type": "shell",
    "command": "bobbuilder",
    "args": [],
    "group": "build"
}
```

### launch.json Configuration

The `.vscode/launch.json` file defines how to launch your built applications.

**Launch Without Debugger (Recommended for Delphi):**
```json
{
    "version": "0.2.0",
    "configurations": [
        {
            "name": "Launch MyApp (Win32 Debug)",
            "type": "node",
            "request": "launch",
            "runtimeExecutable": "${workspaceFolder}/build/bin/Win32/Debug/MyApp.exe",
            "runtimeArgs": [],
            "cwd": "${workspaceFolder}/build/bin/Win32/Debug",
            "console": "externalTerminal",
            "preLaunchTask": "Build MyProject (Win32 Debug)"
        }
    ]
}
```

**Key Properties:**
- `name`: Display name in launch menu
- `type`: Set to `"node"` (workaround to launch without debugger)
- `runtimeExecutable`: Full path to your executable
- `runtimeArgs`: Array of command-line arguments for your app (empty if none)
- `cwd`: Working directory when app launches
- `console`: Use `"externalTerminal"` to launch in separate window
- `preLaunchTask`: Name of build task to run first (must match task `label`)

**Variables:**
- `${workspaceFolder}`: Your workspace root directory
- `${Platform}`: Can't be used dynamically; create separate configs for Win32/Win64
- `${Config}`: Can't be used dynamically; create separate configs for Debug/Release

**Launch Without Building:**
Omit the `preLaunchTask` property to launch immediately without building:
```json
{
    "name": "Launch MyApp (Win32 Debug) - No Build",
    "type": "node",
    "request": "launch",
    "runtimeExecutable": "${workspaceFolder}/build/bin/Win32/Debug/MyApp.exe",
    "cwd": "${workspaceFolder}/build/bin/Win32/Debug",
    "console": "externalTerminal"
}
```

**With Command-Line Arguments:**
```json
{
    "name": "Launch MyApp with Args",
    "type": "node",
    "request": "launch",
    "runtimeExecutable": "${workspaceFolder}/build/bin/Win32/Debug/MyApp.exe",
    "runtimeArgs": ["/debug", "/verbose"],
    "cwd": "${workspaceFolder}/build/bin/Win32/Debug",
    "console": "externalTerminal",
    "preLaunchTask": "Build MyProject (Win32 Debug)"
}
```

## Build Output

The typical output locations of compiled binaries to the `build/` folder with structure:
```
build/
  ├── bin/
  │   ├── Win32/
  │   │   ├── Debug/
  │   │   │   └── MyApp.exe
  │   │   └── Release/
  │   │       └── MyApp.exe
  │   └── Win64/
  │       ├── Debug/
  │       │   └── MyApp.exe
  │       └── Release/
  │           └── MyApp.exe
  └── logs/
      └── build_YYYYMMDD_HHMMSS.log
```

Where:
- `{Platform}` is `Win32` or `Win64`
- `{Config}` is `Debug` or `Release`

Build logs are saved to the `build/` folder. The exact structure depends on your `.builder` configuration file.

## Build Process

When you run a BobBuilder task, it performs these steps:

1. **Pre-Build Scripts**: 
   - Generates version resources from templates
   - Prepares build environment

2. **Compilation**:
   - Compiles the Delphi project for specified platforms/configurations
   - Generates executables and debug symbols

3. **Post-Build Scripts**:
   - Converts MAP files to JDBG (debug symbols)
   - Code signing (in production builds)
   - Other custom post-processing

4. **Install Scripts** (full builds only):
   - Creates installers using Inno Setup
   - Builds MSI packages
   - Signs installers
   - Generates MD5 checksums

## Tips & Best Practices

### Building
- **Quick Build Menu**: Press `Ctrl+Shift+B` to see all build options
- **Filter Tasks**: When the menu is open, type to filter (e.g., "server", "win64", "clean")
- **Incremental Builds**: Use `/CLEANUP:false` for faster builds (only recompiles changed files)
- **Clean Builds**: Use `/CLEANUP:true` when troubleshooting or before releases
- **Platform Selection**: Build only the platform you need (Win32 or Win64) for faster iteration
- **Terminal Power**: Use the integrated terminal for one-off or experimental build combinations
- **Command History**: In terminal, press up arrow to repeat/modify previous `bobbuilder` commands

### Configurations
- **Debug Builds**: Include full debug symbols, no optimization, larger executables
- **Release Builds**: Optimized code, smaller executables, harder to debug
- **Multi-Config**: Use `/CONFIGS:Debug;Release` to build both configs in one command

### Workflow Recommendations
- **Daily Development**: Build Win64 Debug from VS Code, debug in Delphi IDE
- **Testing Both Platforms**: Use "Build All Platforms" task periodically
- **Pre-Release**: Always do a clean Release build (`/CLEANUP:true`)
- **Quick Iteration**: Use terminal commands for specific combinations


## Related Files

- `*.builder` - Main BobBuilder configuration file (e.g., `project.builder`)
- `.vscode/tasks.json` - VS Code build tasks configuration
- `.vscode/launch.json` - VS Code launch/debug configurations
- `build/` - Build output directory (default)
- `resources/versioning/*.template` - Version resource templates (if used)

