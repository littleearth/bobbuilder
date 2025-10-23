## Quick orientation for AI coding agents

This repository is "BOB Builder" — a Delphi-focused build automation toolset. The goal of this document is to give an AI agent the minimal, actionable knowledge to be productive quickly: where major components live, how builds/tests run, important conventions, and exact code hooks to modify behavior.

### Big picture
- Root purpose: generate and execute Windows batch build scripts from a JSON `.builder` config and run them to build Delphi projects, installers and related tooling.
- Major components (see `source/`):
  - `source/builder/` — main tools and UI: `bobbuilder` (console), `bobbuildergui` (GUI). Key units: `BuildRunner.pas`, `BuilderSettings.pas`, `ProjectBuilder.pas`.
  - `source/common/` — shared units (`Bob.Common.pas`, `Bob.CodeSign.pas`, `Bob.ResourceGenerator.pas`, etc.).
  - `source/*` other tools — `codesign`, `compare`, `md5`, `wixwrapper`, `maptojdbg`, `library` and `tests`.

### How builds and scripts work (concrete)
- Primary runtime: `bobbuilder.exe`. It loads a `.builder` JSON file (example: `bobbuilder.builder`), generates a Windows `.bat` script and executes it. The script contains environment variable setup, MSBuild calls, pre/post scripts, test execution and installer steps.
- Code paths to modify: `TProjectBuilder.GenerateScript` and `TProjectBuilder.GenerateProjectScript` (file: `source\builder\ProjectBuilder.pas`). These methods compose the `.bat` script lines.
- Script execution: `TScriptRunner` writes a temp `.bat` and runs it (see `TScriptRunner.ExecuteScript` inside `ProjectBuilder.pas`). Logs are captured and saved into the project `logFolder`.
- Variable tokens: script sources use tokens such as `%projectFolder%`, `%logFolder%`, `%ISSBinary%`, `:ISSBinary:`, `%year%`, `%date%`, and `$env:VARNAME`. Token parsing is in `ParseScriptVariables` (same file).
- Tool invocations: generated build steps call MSBuild (see `MSBUILD "%s" /t:Build ...` in `GenerateProjectScript`) — prefer MSBuild for Delphi .dproj builds. Test runners commonly use `-exit:continue` (see `GenerateTestProjectScript`).

### Common workflows & useful commands
- Build everything (from repo root or workspace): simply run `bobbuilder` — it reads `.builder` in current directory.
- Selective build examples (command-line parameters used by `BuildRunner`):
  - Single project: `bobbuilder /PROJECT:source\myproject.dproj /PLATFORMS:Win32 /CONFIGS:Debug /CLEANUP:false`
  - Multiple platforms/configs: `bobbuilder /PROJECT:source\myproject.dproj /PLATFORMS:Win32;Win64 /CONFIGS:Debug;Release`
  - Clean build all (uses VS Code tasks): press Ctrl+Shift+B and choose "Clean and Build All" or run the `bobbuilder` wrapper with `/CLEANUP:true`.
- VS Code tasks are included (see workspace tasks in `.vscode` and top-level tasks shown in the workspace); they call the `bobbuilder` wrapper. Use those for reproducible builds.

### Project-specific conventions
- Naming and style:
  - Local variables prefixed with `L` (e.g. `LProject`), fields with `F`, parameters with `A`.
  - No inline variables (compatible back to Delphi 10.3).
- Configuration model:
  - The `.builder` JSON contains `projectGroups`, `testProjectGroups`, `installScriptGroups`, `variables`, build folders and `projectInformation`. `TBuildProject` is deserialized from JSON in `TProjectBuilder.LoadBuildProject`.
- Settings persistence: `BuilderSettings.pas` reads/writes `builder.ini` in user app data; default Inno Setup path is hard-coded unless overridden (`ISSBinary`, `ISSCompileParams`, `ISSRunWait`).

### External dependencies & integration points
- Required toolchain for development: Delphi 10.3+ (tested with 11.x/12.x), Lazy-Library, JCL, JVCL (for GUI), Indy, LockBox3, SynEdit. See `README.md` for details.
- Optional runtime tools: Inno Setup (ISS), WiX Toolset (for MSI), `signtool.exe`. The code checks for `ISSBinary` and will fail script validation if missing. WiX path is expected via `%WIX%` when used by `bobwixwrapper`.
- Delphi environment variables: `rsvars.bat` is called (if present) from the `bin` folder of a Delphi root — `AddEnvironmentVariables` handles that.

### Where to make common changes (exact files)
- Change build script content or add build steps: `source\builder\ProjectBuilder.pas` (GenerateScript, GenerateProjectScript, GenerateTestProjectScript).
- Change console behavior/parameters parsing: `source\builder\BuildRunner.pas` (CheckParamters, Run, VT console handling).
- Change persistent settings like ISS compile options: `source\builder\BuilderSettings.pas`.
- Modify script execution and logging: `TScriptRunner` methods in `source\builder\ProjectBuilder.pas`.

### Guidance for code edits by AI agents (prompts & checks)
- If asked to add a new build step (e.g. run a linter or custom tool): insert tokenized commands into `GenerateScript` in the appropriate pre/post location and add a corresponding toggle to the JSON model if needed.
- If changing variable tokens, update both `ParseScriptVariables` and `GetVariables` so values are discoverable by the GUI and scripts.
- Always run a quick local build after changing script generation: `bobbuilder /PROJECT:source\builder\bobbuilder.dproj /PLATFORMS:Win32 /CONFIGS:Debug /CLEANUP:false` and inspect the generated `.bat` in the temp folder or logs in the configured `logFolder`.

### Quick debugging tips
- To reproduce what `bobbuilder` does, run it with a `/PROJECT:` override for a single project and inspect the log file produced under the project's `logFolder` (value in `.builder`).
- Scripts are generated with `@echo off` and saved as a temp `.bat` by `TScriptRunner.GetScriptFileName`. Modify generation code and run locally to inspect the produced `.bat` before running it.
- Cancellation and failure: `TScriptRunner` supports cancelation and writes logs; `BuildRunner` will show colorized output (VT sequences) when run in modern terminals.

If any of these sections are unclear or you want examples tailored to a specific change (e.g. "add code signing step after build"), tell me which change and I'll extend this doc with exact code snippets and tests.
