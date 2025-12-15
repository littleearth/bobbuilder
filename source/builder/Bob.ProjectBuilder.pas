unit Bob.ProjectBuilder;

interface

uses
  Lazy.Types, Lazy.Utils, Classes, SysUtils, Winapi.Windows, Bob.BuilderModels,
  Bob.Delphi.Model, Bob.BuilderSettings, Lazy.Nullable;

type
  EProjectBuilderException = class(ELazyException);
  EScriptRunnerException = class(ELazyException);

  TScriptState = (stIdle, stBusy, stCancelled, stComplete, stFailed);

  TOnScriptLog = reference to procedure(AMessage: string);
  TOnScriptComplete = reference to procedure;
  TOnScriptFailed = reference to procedure(AMessage: string);

  TOnProcessActive = procedure(
    ASender: TObject;
    const AProcessName: string;
    var AContinue: boolean) of object;
  TOnBuildComplete = procedure(
    ASender: TObject;
    const AComplete: boolean;
    const AMessage: string) of object;
  TOnBuildProgress = procedure(
    ASender: TObject;
    const AMessage: string;
    var ACancel: boolean) of object;

  TScriptRunner = class(TLZObject)
  private
    FScript: TStringList;
    FOnComplete: TOnScriptComplete;
    FOnLog: TOnScriptLog;
    FOnFailed: TOnScriptFailed;
    FOnBeforeExecute: TProc;
    FOnAfterExecute: TProc;
    FScriptState: TScriptState;
    FLog: TStringList;
    FAsync: boolean;
    FLogFolder: string;
    function GetState: TScriptState;
  protected
    procedure OnCommandExecuteText(const AText: String);
    procedure InternalOnComplete;
    procedure InternalOnFailed(AMessage: string);
    procedure InternalOnLog(AMessage: string);
    procedure InternalOnBeforeExecute;
    procedure InternalOnAfterExecute;
    function GetScriptFileName: TFileName; virtual;
    function GetLogFileName: TFileName;
    function ExecuteScript(AScript: TStrings): boolean;
    procedure DoExecuteAsync;
    procedure DoExecute;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Execute(
      AScript: TStrings;
      AOnComplete: TOnScriptComplete;
      AOnLog: TOnScriptLog;
      AOnFailed: TOnScriptFailed;
      AOnBeforeExecute: TProc;
      AOnAfterExecute: TProc;
      ALogfolder: string;
      ASync: boolean = true);
    procedure Cancel;
    property State: TScriptState read GetState;
  end;

  TProjectBuilder = class(TLZObject)
  private
    FProjectFolder: string;
    FLogFolder: string;
    FFileName: TFileName;
    FBuildType: TBuildType;
    FDelphiVersion: TDelphiVersion;
    FBuildProject: TBuildProject;
    FCleanupEnabled: boolean;
    FFormatEnabled: boolean;
    FSettings: TSettings;
    FBuildProjectGroupsEnabled: boolean;
    FBuildInstallGroupsEnabled: boolean;
    FScriptRunner: TScriptRunner;
    FOnBuildComplete: TOnBuildComplete;
    FOnBuildProgress: TOnBuildProgress;
    FOnProcessActive: TOnProcessActive;
    FAsyncBuild: boolean;
    procedure SetDelphiVersion(const Value: TDelphiVersion);
    procedure SetFileName(const Value: TFileName);
    procedure SetCleanupEnabled(const Value: boolean);
    procedure SetFormatEnabled(const Value: boolean);
    procedure SetBuildType(const Value: TBuildType);
    procedure SetOnProcessActive(const Value: TOnProcessActive);
    procedure SetBuildInstallGroupsEnabled(const Value: boolean);
    procedure SetBuildProjectGroupsEnabled(const Value: boolean);
    function GetLoaded: boolean;
    procedure SetOnBuildComplete(const Value: TOnBuildComplete);
    procedure SetOnBuildProgress(const Value: TOnBuildProgress);
    procedure InternalOnComplete;
    procedure InternalOnFailed(AMessage: string);
    procedure InternalOnLog(AMessage: string);
    procedure InternalOnBeforeExecute;
    procedure InternalOnAfterExecute;
    function GetScriptState: TScriptState;
    procedure SetAsyncBuild(const Value: boolean);
    function CleanEnviromentVariable(AValue: string): string;
  protected
    function ParseScriptVariables(ASource: string): string;
    function GetFileContents(AFileName: TFileName): string;
    procedure StatusEvent(AMessage: string);
    procedure AddEnvironmentVariables(
      AFolder: string;
      AVariables: TVariables;
      AFolders: TFolders;
      AScript: TStrings);
    function InList(
      AValue: string;
      AList: string): boolean;
    function AllowBuildType(AStaging, AProduction: boolean): boolean;

    function GenerateProjectScript(
      AScript: TStrings;
      AProject: TProject): boolean; overload;
    function GenerateProjectScript(
      AScript: TStrings;
      AProject: TProject;
      APlatformOverride: string;
      AConfigOverride: string): boolean; overload;
    function GenerateProjectGroupScript(
      AScript: TStrings;
      AProjectGroup: TProjectGroup): boolean;

    function GenerateTestProjectScript(
      AScript: TStrings;
      ATestProject: TTestProject): boolean;
    function GenerateTestProjectGroupScript(
      AScript: TStrings;
      ATestProjectGroup: TTestProjectGroup;
      APostBuild: boolean): boolean;

    function AddCustomScripts(
      AScript: TStrings;
      ACustomScripts: TScripts;
      AAllowList: string = '[ALL]';
      AAllowStagingDefault: boolean = true;
      AIsSelectiveBuild: boolean = false): boolean;  overload;

    function AddCustomScripts(
      AScript: TStrings;
      ACustomScripts: TSelectiveScripts;
      AAllowList: string = '[ALL]';
      AAllowStagingDefault: boolean = true;
      AIsSelectiveBuild: boolean = false): boolean; overload;

    function GenerateInstallScriptGroups(
      AScript: TStrings;
      AInstallScriptGroups: TInstallScriptGroups;
      ADefaultInstallScriptGroups: string): boolean;
    function GenerateInstallScript(
      AScript: TStrings;
      AInstallScript: TInstallScript): boolean;
    function GenerateCleanupScript(
      AScript: TStrings;
      AFolders: TFolders): boolean;

    procedure AddProductInfoEnvironmentVariables(
      AScript: TStrings;
      AProjectInformation: TProjectInformation);
    procedure LoadBuildProject;
    procedure SaveBuildProject(AFileName: TFileName = '');
    function CheckActiveProcesses(AProcesses: TProcesses): boolean;
    function ValidateSettings(var AMessage: string): boolean;
    procedure DoBuild(
      AProjectGroups: string = '';
      ATestProjectGroups: string = '';
      AInstallScriptGroups: string = '';
      ABuildCompleteScripts: string = '');
    procedure DoFormat;
    procedure DoSelectiveBuild(
      AProject: string;
      AProjectGroups: string;
      APlatforms: string;
      AConfigs: string);
    function GetBooleanValue(
      const AValue: TLZNullableBoolean;
      ADefault: boolean = false): boolean;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure GetVariables(AVariables: TStrings);
    function GenerateScript(
      AScript: TStrings;
      AProjectGroups: string = '';
      ATestProjectGroups: string = '';
      AInstallScriptGroups: string = '';
      ABuildCompleteScripts: string = ''): boolean;
    procedure Build(
      AProjectGroups: string = '';
      ATestProjectGroups: string = '';
      AInstallScriptGroups: string = '';
      ABuildCompleteScripts: string = '';
      AProject: string = '';
      APlatforms: string = '';
      AConfigs: string = '');
    procedure Formatters;
    procedure Cancel;
    procedure Save;
    procedure SaveAs(AFileName: TFileName);
    function ExpandFileName(AFileName: TFileName): TFileName;
    property AsyncBuild: boolean read FAsyncBuild write SetAsyncBuild;
    property FileName: TFileName read FFileName write SetFileName;
    property Loaded: boolean read GetLoaded;
    property State: TScriptState read GetScriptState;
    property BuildType: TBuildType read FBuildType write SetBuildType;
    property DelphiVersion: TDelphiVersion read FDelphiVersion
      write SetDelphiVersion;
    property CleanupEnabled: boolean read FCleanupEnabled
      write SetCleanupEnabled;
    property FormatEnabled: boolean read FFormatEnabled write SetFormatEnabled;
    property BuildProjectGroupsEnabled: boolean read FBuildProjectGroupsEnabled
      write SetBuildProjectGroupsEnabled;
    property BuildInstallGroupsEnabled: boolean read FBuildInstallGroupsEnabled
      write SetBuildInstallGroupsEnabled;
    property Settings: TSettings read FSettings;
    property Project: TBuildProject read FBuildProject;
    property Projectfolder: string read FProjectFolder;
    property OnProcessActive: TOnProcessActive read FOnProcessActive
      write SetOnProcessActive;
    property OnBuildComplete: TOnBuildComplete read FOnBuildComplete
      write SetOnBuildComplete;
    property OnBuildProgress: TOnBuildProgress read FOnBuildProgress
      write SetOnBuildProgress;
  end;

implementation

uses VCL.Lazy.Utils.Windows, Lazy.Token, JclFileUtils, Bob.Delphi,
  JclSysUtils, System.Threading;

{ TProjectBuilder }

procedure TProjectBuilder.DoBuild(AProjectGroups, ATestProjectGroups,
  AInstallScriptGroups, ABuildCompleteScripts: string);
var
  LScript: TStringList;
begin
  LScript := TStringList.Create;
  try
    StatusEvent('Building ' + ChangeFileExt(ExtractFileName(FFileName), ''));
    StatusEvent('Generating script...');
    if GenerateScript(LScript, AProjectGroups, ATestProjectGroups,
      AInstallScriptGroups, ABuildCompleteScripts) then
    begin
      if CheckActiveProcesses(FBuildProject.CheckActiveProcesses) then
      begin
        StatusEvent('Executing script...');
        FScriptRunner.Execute(LScript, InternalOnComplete, InternalOnLog,
          InternalOnFailed, InternalOnBeforeExecute, InternalOnAfterExecute,
          FLogFolder, FAsyncBuild);
      end
      else
      begin
        InternalOnFailed('Build aborted by user.');
        StatusEvent('ERROR: Build aborted by user.');
      end;
    end
    else
    begin
      InternalOnFailed('Script failed to generate');
      StatusEvent('ERROR: Script failed to generate');
    end;
  finally
    FreeAndNil(LScript);
  end;
end;

procedure TProjectBuilder.Cancel;
begin
  FScriptRunner.Cancel;
end;

function TProjectBuilder.CheckActiveProcesses(AProcesses: TProcesses): boolean;
var
  LProcess: TProcess;
  LDelphiHelper: TDelphiHelper;
begin
  Result := true;
  LDelphiHelper := TDelphiHelper.Create;
  try
    for LProcess in AProcesses do
    begin
      StatusEvent(Format('Checking if process "%s" is active...',
        [LProcess.processName]));
      if LDelphiHelper.IsProcessRunning(LProcess.processFileName) then
      begin
        StatusEvent(Format('Warning "%s" process is active, build may fail...',
          [LProcess.processName]));
        if Assigned(FOnProcessActive) then
          FOnProcessActive(Self, LProcess.processName, Result);
      end;
      if not Result then
        exit;
    end;
  finally
    FreeAndNil(LDelphiHelper);
  end;
end;

constructor TProjectBuilder.Create;
begin
  inherited;
  FSettings := TSettings.Create;
  FScriptRunner := TScriptRunner.Create;
  FBuildType := btDevelopment;
  FCleanupEnabled := true;
  FFormatEnabled := true;
  FBuildProjectGroupsEnabled := true;
  FBuildInstallGroupsEnabled := true;
  FAsyncBuild := true;
end;

destructor TProjectBuilder.Destroy;
begin
  Cancel;
  try
    FreeAndNil(FBuildProject);
    FreeAndNil(FSettings);
    FreeAndNil(FScriptRunner);
  finally
    inherited;
  end;
end;

function TProjectBuilder.ExpandFileName(AFileName: TFileName): TFileName;
begin
  Result := AFileName;
  Result := ParseScriptVariables(Result);
  if TLZString.IsEmptyString(ExtractFileDrive(Result)) then
  begin
    Result := FProjectFolder + Result;
  end;
  Debug('ExpandFileName', Result);
end;

function TProjectBuilder.GenerateScript(
  AScript: TStrings;
  AProjectGroups, ATestProjectGroups, AInstallScriptGroups,
  ABuildCompleteScripts: string): boolean;
var
  LProjectGroup: TProjectGroup;
  LTestProjectGroup: TTestProjectGroup;
  LProjectGroups, LTestProjectGroups, LInstallScriptGroups,
    LBuildCompleteScripts, LMessage: string;
begin
  Result := true;
  if Assigned(FBuildProject) and Assigned(FDelphiVersion) then
  begin
    if ValidateSettings(LMessage) then
    begin

      LProjectGroups := AProjectGroups;
      if TLZString.IsEmptyString(LProjectGroups) then
        LProjectGroups := FBuildProject.defaultProjectGroups;

      LTestProjectGroups := ATestProjectGroups;
      if TLZString.IsEmptyString(LTestProjectGroups) then
        LTestProjectGroups := FBuildProject.defaultTestProjectGroups;

      LInstallScriptGroups := AInstallScriptGroups;
      if TLZString.IsEmptyString(LInstallScriptGroups) then
        LInstallScriptGroups := FBuildProject.defaultInstallScriptGroups;

      LBuildCompleteScripts := ABuildCompleteScripts;
      if TLZString.IsEmptyString(LBuildCompleteScripts) then
        LBuildCompleteScripts := FBuildProject.defaultBuildCompleteScripts;

      AScript.Add('@echo off');
      AScript.Add
        ('ECHO ----------------------------------------------------------');
      AScript.Add('echo Script generated by bobbuilder');
      AScript.Add('echo - Little Earth Solutions');
      AScript.Add
        ('ECHO ----------------------------------------------------------');
      AScript.Add('');
      AScript.Add('setlocal');

      // Add git pull if enabled
      if GetBooleanValue(FBuildProject.gitpull, false) then
      begin
        AScript.Add
          ('ECHO ----------------------------------------------------------');
        AScript.Add('ECHO [STATUS] Running git pull...');
        AScript.Add
          ('ECHO ----------------------------------------------------------');
        AScript.Add('git pull');
        AScript.Add('IF %ERRORLEVEL% NEQ 0 GOTO :ERROR');
        AScript.Add('');
      end;

      AScript.Add
        ('ECHO ----------------------------------------------------------');
      AScript.Add('ECHO [STATUS] Setting environment variables...');
      AScript.Add
        ('ECHO ----------------------------------------------------------');

      AddEnvironmentVariables(FDelphiVersion.rootFolder,
        FBuildProject.variables, FBuildProject.buildFolders, AScript);

      AddProductInfoEnvironmentVariables(AScript,
        FBuildProject.projectInformation);

      AScript.Add('');

      if FCleanupEnabled then
      begin
        AScript.Add
          ('ECHO ----------------------------------------------------------');
        AScript.Add('ECHO [STATUS] Cleanup in progress...');
        AScript.Add
          ('ECHO ----------------------------------------------------------');
        GenerateCleanupScript(AScript, FBuildProject.buildFolders);
        AddCustomScripts(AScript, FBuildProject.postCleanupScripts);
        AScript.Add('');
      end;

      if (FBuildType = btDevelopment) and FFormatEnabled then
      begin
        AScript.Add
          ('ECHO ----------------------------------------------------------');
        AScript.Add('ECHO [STATUS] Code format scripts...');
        AScript.Add
          ('ECHO ----------------------------------------------------------');
        AddCustomScripts(AScript, FBuildProject.codeFormatScripts);
        AScript.Add('');
      end;

      if FBuildProjectGroupsEnabled then
      begin

        AScript.Add
          ('ECHO ----------------------------------------------------------');
        AScript.Add('ECHO [STATUS] Pre-Build scripts...');
        AScript.Add
          ('ECHO ----------------------------------------------------------');

        AddCustomScripts(AScript, FBuildProject.preBuildScripts);
        AScript.Add('');

        AScript.Add
          ('ECHO ----------------------------------------------------------');
        AScript.Add('ECHO [STATUS] Pre-Build Test in progress...');
        AScript.Add
          ('ECHO ----------------------------------------------------------');

        for LTestProjectGroup in FBuildProject.testProjectGroups do
        begin
          if Result then
          begin
            if InList(LTestProjectGroup.group, LTestProjectGroups) then
            begin
              if not GenerateTestProjectGroupScript(AScript, LTestProjectGroup,
                false) then
              begin
                Log(Format('Test project group "%s" script failed to generate',
                  [LTestProjectGroup.group]));
                Result := false;
              end;
            end
            else
            begin
              Log(Format('Skipping test project group "%s" ',
                [LTestProjectGroup.group]));
            end;
          end;
        end;

        AScript.Add
          ('ECHO ----------------------------------------------------------');
        AScript.Add('ECHO [STATUS] Build in progress...');
        AScript.Add
          ('ECHO ----------------------------------------------------------');

        for LProjectGroup in FBuildProject.projectGroups do
        begin
          if Result then
          begin
            if InList(LProjectGroup.group, LProjectGroups) then
            begin
              if not GenerateProjectGroupScript(AScript, LProjectGroup) then
              begin
                Log(Format('Project group "%s" script failed to generate',
                  [LProjectGroup.group]));
                Result := false;
              end;
            end
            else
            begin
              Log(Format('Skipping project group "%s" ',
                [LProjectGroup.group]));
            end;
          end;
        end;

        AScript.Add
          ('ECHO ----------------------------------------------------------');
        AScript.Add('ECHO [STATUS] Post-Build Test in progress...');
        AScript.Add
          ('ECHO ----------------------------------------------------------');

        for LTestProjectGroup in FBuildProject.testProjectGroups do
        begin
          if Result then
          begin
            if InList(LTestProjectGroup.group, LTestProjectGroups) then
            begin
              if not GenerateTestProjectGroupScript(AScript, LTestProjectGroup,
                true) then
              begin
                Log(Format('Test project group "%s" script failed to generate',
                  [LTestProjectGroup.group]));
                Result := false;
              end;
            end
            else
            begin
              Log(Format('Skipping test project group "%s" ',
                [LTestProjectGroup.group]));
            end;
          end;
        end;

        AScript.Add
          ('ECHO ----------------------------------------------------------');
        AScript.Add('ECHO [STATUS] Post-Build scripts...');
        AScript.Add
          ('ECHO ----------------------------------------------------------');

        AddCustomScripts(AScript, FBuildProject.postBuildScripts);
        AScript.Add('');
      end;

      if FBuildInstallGroupsEnabled then
      begin
        AScript.Add
          ('ECHO ----------------------------------------------------------');
        AScript.Add('ECHO [STATUS] Installation build in progress...');
        AScript.Add
          ('ECHO ----------------------------------------------------------');
        Result := GenerateInstallScriptGroups(AScript,
          FBuildProject.installScriptGroups, LInstallScriptGroups);
        AScript.Add('');
      end;

      AScript.Add('GOTO :COMPLETE');

      AScript.Add(':COMPLETE');
      if FBuildProject.buildCompleteScripts.Count > 0 then
      begin
        AScript.Add
          ('ECHO ----------------------------------------------------------');
        AScript.Add('echo [STATUS] Running completion scripts...');
        AScript.Add
          ('ECHO ----------------------------------------------------------');
        AddCustomScripts(AScript, FBuildProject.buildCompleteScripts,
          LBuildCompleteScripts, false);
        AScript.Add('');
      end;

      AScript.Add('ECHO [STATUS] Build complete');
      AScript.Add
        ('ECHO ----------------------------------------------------------');
      AScript.Add('EXIT /B 0');
      AScript.Add('');

      AScript.Add(':SCRIPTERROR');
      AScript.Add('ECHO [STATUS] Script failed (Error: %ERRORLEVEL%)');
      AScript.Add('EXIT /B %ERRORLEVEL%');
      AScript.Add('');

      AScript.Add(':TESTERROR');
      AScript.Add('ECHO [STATUS] Tests failed (Error: %ERRORLEVEL%)');
      AScript.Add('EXIT /B %ERRORLEVEL%');
      AScript.Add('');

      AScript.Add(':BUILDERROR');
      AScript.Add('ECHO [STATUS] Build failed (Error: %ERRORLEVEL%)');
      AScript.Add('EXIT /B %ERRORLEVEL%');
      AScript.Add('');

      AScript.Add(':ERROR');
      AScript.Add('ECHO [STATUS] Build failed');
      AScript.Add('EXIT /B 1');

    end
    else
    begin
      Error(Format('Failed to validate settings, Error: %s', [LMessage]));
      Result := false;
    end;
  end
  else
  begin
    Error('Failed to load Build Project or Delphi Version information');
    Result := false;
  end;

end;

function TProjectBuilder.CleanEnviromentVariable(AValue: string): string;
begin
  Result := AValue;
  Result := TLZString.StringCleaner(Result, true, true);
  Result := ParseScriptVariables(Result);
end;

procedure TProjectBuilder.AddProductInfoEnvironmentVariables(
  AScript: TStrings;
  AProjectInformation: TProjectInformation);
var
  LVersionInformation: TVersionInformation;
  LVersionFileName: string;
  LFileMajor, LFileMinor, LFileRelease, LFileBuild: string;
  LProductMajor, LProductMinor, LProductRelease, LProductBuild: string;
  LVersionJSON: TStringList;
begin
  LVersionInformation := nil;
  LVersionJSON := TStringList.Create;
  try
    case FBuildType of
      btDevelopment:
        begin
          LVersionInformation := AProjectInformation.DevelopmentVersion;
        end;
      btStaging:
        begin
          LVersionInformation := AProjectInformation.StagingVersion;
        end;
      btProduction:
        begin
          LVersionInformation := AProjectInformation.ProductionVersion;
        end;
    end;
    if Assigned(LVersionInformation) then
    begin
      LVersionFileName := ExpandFileName(LVersionInformation.FileName);
      if FileExists(LVersionFileName) then
      begin
        try
          LVersionJSON.LoadFromFile(LVersionFileName);
          LVersionInformation.FromJSON(LVersionJSON.Text);
        except
          on E: Exception do
          begin
            Error(E.Message);
          end;
        end;
      end;

      LFileMajor := LVersionInformation.fileVersionMajor;
      LFileMinor := LVersionInformation.fileVersionMinor;
      LFileRelease := LVersionInformation.fileVersionRelease;
      LFileBuild := LVersionInformation.fileVersionBuild;
      LProductMajor := LVersionInformation.productVersionMajor;
      LProductMinor := LVersionInformation.productVersionMinor;
      LProductRelease := LVersionInformation.ProductVersionRelease;
      LProductBuild := LVersionInformation.ProductVersionBuild;

      AScript.Add('SET fileDescription=' + CleanEnviromentVariable
        (AProjectInformation.fileDescription));

      AScript.Add('SET internalName=' + CleanEnviromentVariable
        (AProjectInformation.internalName));

      AScript.Add('SET productName=' + CleanEnviromentVariable
        (AProjectInformation.productName));

      AScript.Add('SET companyName=' + CleanEnviromentVariable
        (AProjectInformation.companyName));

      AScript.Add('SET legalCopyright=' + CleanEnviromentVariable
        (AProjectInformation.legalCopyright));

      AScript.Add('SET companyURL=' + CleanEnviromentVariable
        (AProjectInformation.companyURL));

      AScript.Add('SET fileVersionMajor=' + CleanEnviromentVariable
        (LFileMajor));

      AScript.Add('SET fileVersionMinor=' + CleanEnviromentVariable
        (LFileMinor));

      AScript.Add('SET fileVersionRelease=' + CleanEnviromentVariable
        (LFileRelease));

      AScript.Add('SET fileVersionBuild=' + CleanEnviromentVariable
        (LFileBuild));

      AScript.Add('SET fileVersion=' + CleanEnviromentVariable(LFileMajor + '.'
        + LFileMinor + '.' + LFileRelease + '.' + LFileBuild));

      AScript.Add('SET productVersionMajor=' + CleanEnviromentVariable
        (LProductMajor));

      AScript.Add('SET productVersionMinor=' + CleanEnviromentVariable
        (LProductMinor));

      AScript.Add('SET productVersionRelease=' + CleanEnviromentVariable
        (LProductRelease));

      AScript.Add('SET productVersionBuild=' + CleanEnviromentVariable
        (LProductBuild));

      AScript.Add('SET productVersion=' + CleanEnviromentVariable(LProductMajor
        + '.' + LProductMinor + '.' + LProductRelease + '.' + LProductBuild));

    end;

  finally
    FreeAndNil(LVersionJSON);
  end;
end;

function TProjectBuilder.GetBooleanValue(
  const AValue: TLZNullableBoolean;
  ADefault: boolean): boolean;
begin
  Result := AValue.GetValueOrDefault(ADefault);
end;

function TProjectBuilder.GetFileContents(AFileName: TFileName): string;
var
  LFile: TStringList;
begin
  LFile := TStringList.Create;
  try
    LFile.LoadFromFile(AFileName);
    Result := LFile.Text;
  finally
    FreeAndNil(LFile);
  end;
end;

function TProjectBuilder.GetLoaded: boolean;
begin
  Result := Assigned(FBuildProject);
end;

function TProjectBuilder.GetScriptState: TScriptState;
begin
  Result := stIdle;
  if Assigned(FScriptRunner) then
    Result := FScriptRunner.State;
end;

function TProjectBuilder.InList(AValue, AList: string): boolean;
var
  LList: TLZToken;
begin
  if not SameText('[ALL]', AList) then
  begin
    LList := TLZToken.Create(AList, ';');
    try
      Result := LList.TokenExists(AValue);
    finally
      FreeAndNil(LList);
    end;
  end
  else
  begin
    Result := true;
  end;
end;

procedure TProjectBuilder.InternalOnAfterExecute;
begin

end;

procedure TProjectBuilder.InternalOnBeforeExecute;
begin

end;

procedure TProjectBuilder.InternalOnComplete;
begin
  if Assigned(FOnBuildComplete) then
    FOnBuildComplete(Self, true, 'Build complete');
end;

procedure TProjectBuilder.InternalOnFailed(AMessage: string);
begin
  if Assigned(FOnBuildComplete) then
    FOnBuildComplete(Self, false, AMessage);
end;

procedure TProjectBuilder.InternalOnLog(AMessage: string);
var
  LCancel: boolean;
begin
  LCancel := State = stCancelled;
  if Assigned(FOnBuildProgress) then
  begin
    FOnBuildProgress(Self, AMessage, LCancel);
    if LCancel then
      Cancel;
  end;
end;

procedure TProjectBuilder.LoadBuildProject;
begin
  FreeAndNil(FBuildProject);
  FLogFolder := '';
  FProjectFolder := '';
  Log('Loading: ' + FFileName);
  if FileExists(FFileName) then
  begin
    FBuildProject := TBuildProject.Create;
    try
      FBuildProject.FromJSON(GetFileContents(FFileName));

      FProjectFolder := ExtractFilePath(FFileName);
      if TLZString.IsEmptyString(FProjectFolder) then
      begin
        FProjectFolder := GetCurrentDir;
      end;
      // FProjectFolder := GetCurrentDir;
      FProjectFolder := IncludeTrailingPathDelimiter(FProjectFolder);
      FLogFolder := FProjectFolder + FBuildProject.logFolder;
      Log('Project Folder: ' + FProjectFolder);
    except
      FreeAndNil(FBuildProject);
    end;
  end;
end;

function TProjectBuilder.ParseScriptVariables(ASource: string): string;
var
  LVariables: TStringList;
  LVariable, LVariableValue: string;
  LIdx: integer;
begin
  LVariables := TStringList.Create;
  try
    Result := ASource;
    Result := StringReplace(Result, '%projectFolder%', FProjectFolder,
      [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%logFolder%', FLogFolder,
      [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%projectFileName%',
      ExtractFileName(FFileName), [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%ISSBinary%', FSettings.ISSBinary,
      [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%ISSCompileParams%',
      FSettings.ISSCompileParams, [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '%ISSRunWait%',
      BooleanToStr(FSettings.ISSRunWait), [rfReplaceAll, rfIgnoreCase]);

    Result := StringReplace(Result, ':projectFolder:', FProjectFolder,
      [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, ':logFolder:', FLogFolder,
      [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, ':projectFileName:',
      ExtractFileName(FFileName), [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, ':ISSBinary:', FSettings.ISSBinary,
      [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, ':ISSCompileParams:',
      FSettings.ISSCompileParams, [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, ':ISSRunWait:',
      BooleanToStr(FSettings.ISSRunWait), [rfReplaceAll, rfIgnoreCase]);

    Result := StringReplace(Result, '%year%', IntToStr(CurrentYear),
      [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, ':year:', IntToStr(CurrentYear),
      [rfReplaceAll, rfIgnoreCase]);

    Result := StringReplace(Result, '%date%', FormatDateTime('yyyy-mm-dd', Now),
      [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, ':date:', FormatDateTime('yyyy-mm-dd', Now),
      [rfReplaceAll, rfIgnoreCase]);

    Result := StringReplace(Result, '%time%', FormatDateTime('hh:nn', Now),
      [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, ':time:', FormatDateTime('hh:nn', Now),
      [rfReplaceAll, rfIgnoreCase]);

    Result := StringReplace(Result, '%datetime%',
      FormatDateTime('yyyy-mm-dd hh:nn', Now), [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, ':datetime:',
      FormatDateTime('yyyy-mm-dd hh:nn', Now), [rfReplaceAll, rfIgnoreCase]);

    TLZSystem.GetEnvironmentVariables(LVariables);

    LVariables.NameValueSeparator := '=';
    for LIdx := 0 to Pred(LVariables.Count) do
    begin
      LVariable := LVariables.Names[LIdx];
      LVariableValue := LVariables.ValueFromIndex[LIdx];
      Result := StringReplace(Result, '$env:' + LVariable, LVariableValue,
        [rfReplaceAll, rfIgnoreCase]);
    end;

  finally
    FreeAndNil(LVariables);
  end;

end;

procedure TProjectBuilder.GetVariables(AVariables: TStrings);
var
  LVariable, LVariableValue: string;
  LIdx: integer;
  LVariables: TStringList;
begin
  AVariables.Clear;

  AVariables.Add('%projectFolder%');
  AVariables.Add('%logFolder%');
  AVariables.Add('%projectFileName%');
  AVariables.Add('%ISSBinary%');
  AVariables.Add('%ISSCompileParams%');
  AVariables.Add('%ISSRunWait%');
  AVariables.Add('%year%');
  AVariables.Add('%date%');
  AVariables.Add('%time%');
  AVariables.Add('%datetime%');

  LVariables := TStringList.Create;
  try
    LVariables.NameValueSeparator := '=';
    TLZSystem.GetEnvironmentVariables(LVariables);
    for LIdx := 0 to Pred(LVariables.Count) do
    begin
      LVariable := LVariables.Names[LIdx];
      LVariableValue := LVariables.ValueFromIndex[LIdx];
      AVariables.Add('$env:' + LVariable);
    end;
  finally
    FreeAndNil(LVariables);
  end;
end;

function TProjectBuilder.AllowBuildType(AStaging, AProduction: boolean)
  : boolean;
begin
  case FBuildType of
    btStaging:
      begin
        Result := AStaging;
      end;
    btProduction:
      begin
        Result := AProduction;
      end;
  else
    Result := true;
  end;
end;

function TProjectBuilder.AddCustomScripts(
  AScript: TStrings;
  ACustomScripts: TScripts;
  AAllowList: string;
  AAllowStagingDefault: boolean;
  AIsSelectiveBuild: boolean): boolean;
var
  LScript: TScript;
begin
  Result := true;
  for LScript in ACustomScripts do
  begin
    if LScript.enabled then
    begin
      // TScript is used for formatters - no selective properties
      if InList(LScript.scriptName, AAllowList) then
      begin
        AScript.Add('ECHO [STATUS] Running ' + LScript.scriptName);
        AScript.Add(ParseScriptVariables(LScript.scriptSource));
        AScript.Add('IF %ERRORLEVEL% NEQ 0 GOTO :SCRIPTERROR');
        AScript.Add('');
      end;
    end;
  end;
end;

function TProjectBuilder.AddCustomScripts(
  AScript: TStrings;
  ACustomScripts: TSelectiveScripts;
  AAllowList: string;
  AAllowStagingDefault: boolean;
  AIsSelectiveBuild: boolean): boolean;
var
  LScript: TSelectiveScript;
  LAllowSelective: boolean;
begin
  Result := true;
  for LScript in ACustomScripts do
  begin
    if LScript.enabled then
    begin
      // For selective builds, check the selective property (default to true if null)
      LAllowSelective := true;
      if AIsSelectiveBuild then
      begin
        LAllowSelective := GetBooleanValue(LScript.selective, true);
      end;

      if LAllowSelective and InList(LScript.scriptName, AAllowList) and
        (AllowBuildType(GetBooleanValue(LScript.staging, AAllowStagingDefault),
        GetBooleanValue(LScript.production, true))) then
      begin
        AScript.Add('ECHO [STATUS] Running ' + LScript.scriptName);
        AScript.Add(ParseScriptVariables(LScript.scriptSource));
        AScript.Add('IF %ERRORLEVEL% NEQ 0 GOTO :SCRIPTERROR');
        AScript.Add('');
      end;
    end;
  end;
end;

function TProjectBuilder.GenerateCleanupScript(
  AScript: TStrings;
  AFolders: TFolders): boolean;
var
  LFolder: TFolder;
  LFolderName, LScriptLine: string;
begin
  Result := true;
  for LFolder in AFolders do
  begin
    if LFolder.CleanupEnabled then
    begin
      LFolderName := ExpandFileName(LFolder.folder);
      LScriptLine := Format('ECHO [STATUS] Removing all files from "%s"',
        [LFolderName]);
      AScript.Add(LScriptLine);
      LScriptLine := Format('del "%s\*" /S /Q /F >nul 2>&1', [LFolderName]);
      LScriptLine := ParseScriptVariables(LScriptLine);
      AScript.Add(LScriptLine);
      LScriptLine := Format('ECHO [STATUS] Removing folder "%s"',
        [LFolderName]);
      AScript.Add(LScriptLine);
      LScriptLine := Format('rd /s /q "%s" >nul 2>&1', [LFolderName]);
      LScriptLine := ParseScriptVariables(LScriptLine);
      AScript.Add(LScriptLine);
    end;
  end;
end;

function TProjectBuilder.GenerateInstallScript(
  AScript: TStrings;
  AInstallScript: TInstallScript): boolean;
var
  LScript: string;
  LProjectFile: string;
  LParams, LScriptParams: string;
begin
  Result := true;
  if SameText(AInstallScript.scriptType, 'iss') then
  begin
    LProjectFile := ExpandFileName(AInstallScript.FileName);
    AScript.Add(Format('ECHO [STATUS] Building ISS script "%s", please wait...',
      [AInstallScript.scriptName]));

    // start and /WAIT required or IISStuido will crash,
    // probably because it spawns it's own shells for signing
    LScriptParams := ParseScriptVariables(AInstallScript.params);
    LParams := FSettings.ISSCompileParams;
    LParams := StringReplace(LParams, '%issfilename%', LProjectFile,
      [rfReplaceAll, rfIgnoreCase]);
    LParams := StringReplace(LParams, '%issscriptparams%', LScriptParams,
      [rfReplaceAll, rfIgnoreCase]);
    LScript := Format('":ISSBinary:" %s', [LParams]);
    if FSettings.ISSRunWait then
    begin
      LScript := Format('start "issbuild" /WAIT %s', [LScript]);
    end;
    LScript := ParseScriptVariables(LScript);
    AScript.Add(LScript);
    AScript.Add('IF %ERRORLEVEL% NEQ 0 GOTO :BUILDERROR');
    AScript.Add('');
  end;

  if SameText(AInstallScript.scriptType, 'bat') then
  begin
    LProjectFile := ExpandFileName(AInstallScript.FileName);
    LParams := ParseScriptVariables(AInstallScript.params);
    if not TLZString.IsEmptyString(LParams) then
      LParams := ' ' + LParams;

    AScript.Add
      (Format('ECHO [STATUS] Running install script "%s", please wait...',
      [AInstallScript.scriptName]));
    AScript.Add('CALL ' + AnsiQuotedStr(LProjectFile, '"') + LParams);
    AScript.Add('IF %ERRORLEVEL% NEQ 0 GOTO :BUILDERROR');
    AScript.Add('');
  end;

  if SameText(AInstallScript.scriptType, 'exe') then
  begin
    LProjectFile := ParseScriptVariables(AInstallScript.FileName);
    LParams := ParseScriptVariables(AInstallScript.params);
    if not TLZString.IsEmptyString(LParams) then
      LParams := ' ' + LParams;

    AScript.Add
      (Format('ECHO [STATUS] Running install script "%s", please wait...',
      [AInstallScript.scriptName]));
    AScript.Add(AnsiQuotedStr(LProjectFile, '"') + LParams);
    AScript.Add('IF %ERRORLEVEL% NEQ 0 GOTO :BUILDERROR');
    AScript.Add('');
  end;

end;

function TProjectBuilder.GenerateInstallScriptGroups(
  AScript: TStrings;
  AInstallScriptGroups: TInstallScriptGroups;
  ADefaultInstallScriptGroups: string): boolean;
var
  LInstallScriptGroup: TInstallScriptGroup;
  LInstallScript: TInstallScript;
begin
  Result := true;
  for LInstallScriptGroup in AInstallScriptGroups do
  begin
    if InList(LInstallScriptGroup.group, ADefaultInstallScriptGroups) then
    begin
      if LInstallScriptGroup.enabled then
      begin
        for LInstallScript in LInstallScriptGroup.installScripts do
        begin
          if LInstallScript.enabled then
          begin
            if (AllowBuildType(GetBooleanValue(LInstallScript.staging, false),
              GetBooleanValue(LInstallScript.production, true))) then
            begin
              if not GenerateInstallScript(AScript, LInstallScript) then
              begin
                Result := false;
              end;
            end
            else
            begin
              Log(Format
                ('Install script "%s" is not allowed in current build type',
                [LInstallScript.FileName]));
            end;
          end
          else
          begin
            Log(Format('Install script "%s" is disabled',
              [LInstallScript.FileName]));
          end;
        end;
      end
      else
      begin
        Log(Format('Install script group "%s" is disabled',
          [LInstallScriptGroup.group]));
      end;
    end
    else
    begin
      Log(Format('Install script group "%s" is not in "%s"',
        [LInstallScriptGroup.group, ADefaultInstallScriptGroups]));
    end;
  end;

end;

function TProjectBuilder.GenerateProjectGroupScript(
  AScript: TStrings;
  AProjectGroup: TProjectGroup): boolean;
var
  LProject: TProject;
begin
  Result := true;
  if AProjectGroup.enabled then
  begin
    for LProject in AProjectGroup.projects do
    begin
      if not GenerateProjectScript(AScript, LProject) then
      begin
        Log(Format('Project "%s" script failed to generate',
          [LProject.Project]));
        Result := false;
      end;
    end;
  end
  else
  begin
    Log(Format('Project group "%s" is disabled', [AProjectGroup.group]));
  end;
end;

function TProjectBuilder.GenerateTestProjectGroupScript(
  AScript: TStrings;
  ATestProjectGroup: TTestProjectGroup;
  APostBuild: boolean): boolean;
var
  LProject: TTestProject;
begin
  Result := true;
  if ATestProjectGroup.enabled then
  begin
    for LProject in ATestProjectGroup.projects do
    begin
      if GetBooleanValue(LProject.postBuild, false) = APostBuild then
      begin
        if not GenerateTestProjectScript(AScript, LProject) then
        begin
          Log(Format('Project "%s" script failed to generate',
            [LProject.Project]));
          Result := false;
        end;
      end;
    end;
  end
  else
  begin
    Log(Format('Project group "%s" is disabled', [ATestProjectGroup.group]));
  end;
end;

function TProjectBuilder.GenerateProjectScript(
  AScript: TStrings;
  AProject: TProject): boolean;
begin
  Result := GenerateProjectScript(AScript, AProject, '', '');
end;

function TProjectBuilder.GenerateProjectScript(
  AScript: TStrings;
  AProject: TProject;
  APlatformOverride: string;
  AConfigOverride: string): boolean;
var
  LProjectFile, LPlatform, LConfig: string;
  LConfigs: TStrings;
  LPlatforms: TStrings;
  LProperties: string;
  LConfigOption: string;
  LVerbosityOption: string;
begin
  Result := true;
  if GetBooleanValue(AProject.enabled, true) and
    (AllowBuildType(GetBooleanValue(AProject.staging, true),
    GetBooleanValue(AProject.production, true))) then
  begin
    // Use override if provided, otherwise use project's configs
    if not TLZString.IsEmptyString(AConfigOverride) then
      LConfigs := TLZToken.GetTokens(AConfigOverride)
    else
      LConfigs := TLZToken.GetTokens(AProject.configs);

    if LConfigs.Count = 0 then
      LConfigs.Add('Release');
    if (FBuildType = btDevelopment) and (LConfigs.IndexOf('Debug') = -1) then
    begin
      LConfigs.Add('Debug');
    end;

    // Use override if provided, otherwise use project's platforms
    if not TLZString.IsEmptyString(APlatformOverride) then
      LPlatforms := TLZToken.GetTokens(APlatformOverride)
    else
      LPlatforms := TLZToken.GetTokens(AProject.platforms);

    try
      for LPlatform in LPlatforms do
      begin
        for LConfig in LConfigs do
        begin
          LConfigOption := LConfig;
          LVerbosityOption := '/verbosity:quiet /clp:Summary';

          LProjectFile := ExpandFileName(AProject.Project);
          LProperties := AProject.properties;
          AScript.Add
            (Format('ECHO [STATUS] Building %s (%s %s %s), please wait...',
            [ExtractFileName(AProject.Project), LConfigOption, LPlatform,
            LProperties]));

          if not TLZString.IsEmptyString(LProperties) then
          begin
            LProperties := '/property:' + AnsiQuotedStr(LProperties, '"');
          end;

          AScript.Add
            (Format('MSBUILD "%s" /t:Build /p:Config=%s /p:Platform=%s %s %s /nologo',
            [LProjectFile, LConfigOption, LPlatform, LProperties,
            LVerbosityOption]));
          AScript.Add('IF %ERRORLEVEL% NEQ 0 GOTO :BUILDERROR');

          AScript.Add('');
        end;
      end;
    finally
      FreeAndNil(LConfigs);
      FreeAndNil(LPlatforms);
    end;
  end
  else
  begin
    Log(Format('Project "%s" is disabled', [AProject.Project]));
  end;
end;

function TProjectBuilder.GenerateTestProjectScript(
  AScript: TStrings;
  ATestProject: TTestProject): boolean;
var
  LProjectFile, LPlatform, LConfig, LFolder, LCommand, LParams: string;
  LConfigs: TStrings;
  LPlatforms: TStrings;
begin
  Result := true;
  if GetBooleanValue(ATestProject.enabled, true) then
  begin
    LConfigs := TLZToken.GetTokens(ATestProject.configs);
    LPlatforms := TLZToken.GetTokens(ATestProject.platforms);
    try
      for LPlatform in LPlatforms do
      begin
        for LConfig in LConfigs do
        begin
          LProjectFile := ExpandFileName(ATestProject.Project);
          AScript.Add
            (Format('ECHO [STATUS] Building %s (%s %s), please wait...',
            [ExtractFileName(ATestProject.Project), LConfig, LPlatform]));
          AScript.Add
            (Format('MSBUILD "%s" /t:Build /p:config=%s /p:platform=%s /verbosity:m /ds /nologo',
            [LProjectFile, LConfig, LPlatform]));
          AScript.Add('IF %ERRORLEVEL% NEQ 0 GOTO :TESTERROR');

          LFolder := IncludeTrailingPathDelimiter
            (ExpandFileName(ATestProject.folder));
          LFolder := StringReplace(LFolder, '$(Platform)', LPlatform,
            [rfReplaceAll, rfIgnoreCase]);
          LFolder := StringReplace(LFolder, '$(Config)', LConfig,
            [rfReplaceAll, rfIgnoreCase]);

          LCommand := ATestProject.command;
          if TLZString.IsEmptyString(LCommand) then
            LCommand := ChangeFileExt(ExtractFileName(LProjectFile), '.exe');

          AScript.Add('ECHO [Status] Test ' + LCommand);

          LParams := ATestProject.params;
          // Default for DUnit
          if TLZString.IsEmptyString(LParams) then
            LParams := '-exit:continue';
          LParams := ParseScriptVariables(LParams);

          LCommand := AnsiQuotedStr(LFolder + LCommand, '"');
          if not TLZString.IsEmptyString(LParams) then
          begin
            LCommand := LCommand + ' ' + LParams;
          end;

          AScript.Add(LCommand);
          AScript.Add('IF %ERRORLEVEL% NEQ 0 GOTO :TESTERROR');

          AScript.Add('');
        end;
      end;
    finally
      FreeAndNil(LConfigs);
      FreeAndNil(LPlatforms);
    end;
  end
  else
  begin
    Log(Format('Test project "%s" is disabled', [ATestProject.Project]));
  end;
end;

procedure TProjectBuilder.Save;
begin
  SaveBuildProject;
end;

procedure TProjectBuilder.SaveAs(AFileName: TFileName);
begin
  SaveBuildProject(AFileName);
end;

procedure TProjectBuilder.SaveBuildProject(AFileName: TFileName = '');
var
  LJSON: TStringList;
  LFileName: TFileName;
begin
  LJSON := TStringList.Create;
  try
    LFileName := AFileName;
    if TLZString.IsEmptyString(LFileName) then
      LFileName := FileName;
    if TLZFile.IsValidFileName(LFileName) then
    begin
      LJSON.Text := FBuildProject.ToJSON(true);
      LJSON.SaveToFile(LFileName);
    end
    else
    begin
      raise EProjectBuilderException.CreateFmt('Invalid filename "%s"',
        [LFileName]);
    end;
  finally
    FreeAndNil(LJSON);
  end;
end;

procedure TProjectBuilder.SetAsyncBuild(const Value: boolean);
begin
  FAsyncBuild := Value;
end;

procedure TProjectBuilder.SetBuildInstallGroupsEnabled(const Value: boolean);
begin
  FBuildInstallGroupsEnabled := Value;
end;

procedure TProjectBuilder.SetBuildProjectGroupsEnabled(const Value: boolean);
begin
  FBuildProjectGroupsEnabled := Value;
end;

procedure TProjectBuilder.SetBuildType(const Value: TBuildType);
begin
  FBuildType := Value;
end;

procedure TProjectBuilder.SetCleanupEnabled(const Value: boolean);
begin
  FCleanupEnabled := Value;
end;

procedure TProjectBuilder.SetFormatEnabled(const Value: boolean);
begin
  FFormatEnabled := Value;
end;

procedure TProjectBuilder.SetDelphiVersion(const Value: TDelphiVersion);
begin
  FDelphiVersion := Value;
end;

procedure TProjectBuilder.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
  LoadBuildProject;
end;

procedure TProjectBuilder.SetOnBuildComplete(const Value: TOnBuildComplete);
begin
  FOnBuildComplete := Value;
end;

procedure TProjectBuilder.SetOnBuildProgress(const Value: TOnBuildProgress);
begin
  FOnBuildProgress := Value;
end;

procedure TProjectBuilder.SetOnProcessActive(const Value: TOnProcessActive);
begin
  FOnProcessActive := Value;
end;

procedure TProjectBuilder.StatusEvent(AMessage: string);
begin
  InternalOnLog('[STATUS] ' + AMessage);
end;

function TProjectBuilder.ValidateSettings(var AMessage: string): boolean;
begin
  Result := true;
  if Result then
  begin
    if not FileExists(FSettings.ISSBinary) then
    begin
      if not TLZString.IsEmptyString(FSettings.ISSBinary) then
      begin
        AMessage :=
          Format('ISS script building executable "%s" could not be found.',
          [FSettings.ISSBinary]);
        Result := false;
      end
      else
      begin
        AMessage :=
          Format('ISS script building executable has not been defined.',
          [FSettings.ISSBinary]);
        Result := false;
      end;
    end;
  end;

  if Result then
  begin
    if not SetCurrentDirectory(PChar(FProjectFolder)) then
    begin
      AMessage := Format('Failed to set current directory to "%s".',
        [FProjectFolder]);
      Result := false;
    end;
  end;
end;

procedure TProjectBuilder.AddEnvironmentVariables(
  AFolder: string;
  AVariables: TVariables;
  AFolders: TFolders;
  AScript: TStrings);
var
  LFolderName: string;
  LFileName: string;
  LFolder: TFolder;
  LVariable: TVariable;
  LDepth: integer;
  LAdd: boolean;
begin
  LFolderName := IncludeTrailingPathDelimiter(AFolder);
  LFolderName := IncludeTrailingPathDelimiter(LFolderName + 'bin');
  LFileName := LFolderName + 'rsvars.bat';
  if FileExists(LFileName) then
  begin
    AScript.Add('CALL ' + AnsiQuotedStr(LFileName, '"'));
  end;

  for LVariable in AVariables do
  begin
    LAdd := false;
    case FBuildType of
      btDevelopment:
        LAdd := GetBooleanValue(LVariable.development, true);
      btStaging:
        LAdd := GetBooleanValue(LVariable.staging, true);
      btProduction:
        LAdd := GetBooleanValue(LVariable.production, true);
    end;
    if LAdd then
    begin
      AScript.Add('SET ' + LVariable.variableName + '=' +
        CleanEnviromentVariable(LVariable.variableValue));
    end;
  end;
  for LFolder in AFolders do
  begin
    LFolderName := LFolder.folder;
    LDepth := PathGetDepth(LFolderName);
    LFolderName := PathExtractPathDepth(LFolderName, LDepth);
    LFolderName := TLZString.StringCleaner(LFolderName, true, true,
      [#0 .. #8, #11, #12, #42, #92, #14 .. #32, #127 .. #255]);
    LFolderName := UpperCase(LFolderName);
    AScript.Add('SET ' + 'BUILDER_FOLDER_' + LFolderName + '=' +
      AnsiQuotedStr(ExpandFileName(LFolder.folder), '"'));
  end;

  AScript.Add('SET BUILDER_BUILD_TYPE=' + IntToStr(integer(FBuildType)));

end;

procedure TProjectBuilder.Build(AProjectGroups, ATestProjectGroups,
  AInstallScriptGroups, ABuildCompleteScripts, AProject, APlatforms,
  AConfigs: string);
begin
  if FScriptRunner.State <> stBusy then
  begin
    // Handle selective project building with overrides
    // Trigger selective build if: specific project, or group with overrides
    if not TLZString.IsEmptyString(AProject) or
      (not TLZString.IsEmptyString(AProjectGroups) and
      (not TLZString.IsEmptyString(APlatforms) or
      not TLZString.IsEmptyString(AConfigs))) then
    begin
      Log(Format
        ('Entering selective build mode - Project:"%s" Groups:"%s" Platforms:"%s" Configs:"%s"',
        [AProject, AProjectGroups, APlatforms, AConfigs]));
      DoSelectiveBuild(AProject, AProjectGroups, APlatforms, AConfigs);
    end
    else
    begin
      Log('Entering normal build mode');
      DoBuild(AProjectGroups, ATestProjectGroups, AInstallScriptGroups,
        ABuildCompleteScripts);
    end;
  end
  else
  begin
    raise EProjectBuilderException.Create('Build already active');
  end;
end;

procedure TProjectBuilder.Formatters;
begin
  if FScriptRunner.State <> stBusy then
  begin
    Log('Entering format-only mode');
    DoFormat;
  end
  else
  begin
    raise EProjectBuilderException.Create('Build already active');
  end;
end;

procedure TProjectBuilder.DoFormat;
var
  LScript: TStringList;
begin
  LScript := TStringList.Create;
  try
    StatusEvent('Running code formatter...');
    StatusEvent('Generating script...');

    LScript.Add('@ECHO OFF');
    LScript.Add('SETLOCAL');
    LScript.Add('');

    LScript.Add
      ('ECHO ----------------------------------------------------------');
    LScript.Add('ECHO [STATUS] Setting environment variables...');
    LScript.Add
      ('ECHO ----------------------------------------------------------');

    AddEnvironmentVariables(FDelphiVersion.rootFolder, FBuildProject.variables,
      FBuildProject.buildFolders, LScript);

    AddProductInfoEnvironmentVariables(LScript,
      FBuildProject.projectInformation);

    LScript.Add('');

    LScript.Add
      ('ECHO ----------------------------------------------------------');
    LScript.Add('ECHO [STATUS] Code format scripts...');
    LScript.Add
      ('ECHO ----------------------------------------------------------');
    AddCustomScripts(LScript, FBuildProject.codeFormatScripts);
    LScript.Add('');

    LScript.Add('GOTO :FORMATSUCCESS');
    LScript.Add('');
    LScript.Add(':SCRIPTERROR');
    LScript.Add('ECHO [STATUS] Script error (Error: %ERRORLEVEL%)');
    LScript.Add('EXIT /B %ERRORLEVEL%');
    LScript.Add('');
    LScript.Add(':FORMATERROR');
    LScript.Add('ECHO [STATUS] Format failed');
    LScript.Add('EXIT /B 1');
    LScript.Add('');
    LScript.Add(':FORMATSUCCESS');
    LScript.Add('ECHO [STATUS] Format complete');
    LScript.Add('EXIT /B 0');

    StatusEvent('Executing script...');
    FScriptRunner.Execute(LScript, InternalOnComplete, InternalOnLog,
      InternalOnFailed, InternalOnBeforeExecute, InternalOnAfterExecute,
      FLogFolder, FAsyncBuild);

  finally
    FreeAndNil(LScript);
  end;
end;

procedure TProjectBuilder.DoSelectiveBuild(
  AProject: string;
  AProjectGroups: string;
  APlatforms: string;
  AConfigs: string);
var
  LScript: TStringList;
  LProjectGroup: TProjectGroup;
  LProject: TProject;
  LFound: boolean;
begin
  LScript := TStringList.Create;
  try
    StatusEvent('Building selective project(s)...');
    StatusEvent('Generating script...');

    // Debug logging
    if not TLZString.IsEmptyString(AProject) then
      Log(Format('Selective build - Project: "%s"', [AProject]));
    if not TLZString.IsEmptyString(AProjectGroups) then
      Log(Format('Selective build - ProjectGroups: "%s"', [AProjectGroups]));
    if not TLZString.IsEmptyString(APlatforms) then
      Log(Format('Selective build - Platforms: "%s"', [APlatforms]));
    if not TLZString.IsEmptyString(AConfigs) then
      Log(Format('Selective build - Configs: "%s"', [AConfigs]));

    LScript.Add('@ECHO OFF');
    LScript.Add('SETLOCAL');
    LScript.Add('');

    LScript.Add
      ('ECHO ----------------------------------------------------------');
    LScript.Add('ECHO [STATUS] Setting environment variables...');
    LScript.Add
      ('ECHO ----------------------------------------------------------');

    // Add environment variables (including Delphi rsvars.bat)
    AddEnvironmentVariables(FDelphiVersion.rootFolder, FBuildProject.variables,
      FBuildProject.buildFolders, LScript);
    AddProductInfoEnvironmentVariables(LScript,
      FBuildProject.projectInformation);

    LScript.Add('');

    if FCleanupEnabled then
    begin
      LScript.Add
        ('ECHO ----------------------------------------------------------');
      LScript.Add('ECHO [STATUS] Cleanup in progress...');
      LScript.Add
        ('ECHO ----------------------------------------------------------');
      GenerateCleanupScript(LScript, FBuildProject.buildFolders);
      AddCustomScripts(LScript, FBuildProject.postCleanupScripts);
      LScript.Add('');
    end;

    if FFormatEnabled then
    begin
      LScript.Add
        ('ECHO ----------------------------------------------------------');
      LScript.Add('ECHO [STATUS] Code format scripts...');
      LScript.Add
        ('ECHO ----------------------------------------------------------');
      AddCustomScripts(LScript, FBuildProject.codeFormatScripts);
      LScript.Add('');
    end;

    // Run pre-build scripts (skip staging-only scripts and selective-only scripts)
    LScript.Add
      ('ECHO ----------------------------------------------------------');
    LScript.Add('ECHO [STATUS] Pre-Build Scripts...');
    LScript.Add
      ('ECHO ----------------------------------------------------------');
    AddCustomScripts(LScript, FBuildProject.preBuildScripts, '[ALL]',
      false, true);

    LScript.Add('');
    LScript.Add
      ('ECHO ----------------------------------------------------------');
    LScript.Add('ECHO [STATUS] Build in progress...');
    LScript.Add
      ('ECHO ----------------------------------------------------------');

    // Find and build the specific project
    LFound := false;

    // If AProject is specified, find that specific project file
    if not TLZString.IsEmptyString(AProject) then
    begin
      for LProjectGroup in FBuildProject.projectGroups do
      begin
        for LProject in LProjectGroup.projects do
        begin
          // Match by project filename (case-insensitive)
          if SameText(ExtractFileName(LProject.Project),
            ExtractFileName(AProject)) or SameText(LProject.Project, AProject)
          then
          begin
            LFound := true;
            Log(Format('Building project "%s" with overrides',
              [LProject.Project]));
            GenerateProjectScript(LScript, LProject, APlatforms, AConfigs);
          end;
        end;
      end;

      if not LFound then
      begin
        StatusEvent(Format('Project "%s" not found in configuration',
          [AProject]));
        InternalOnFailed(Format('Project "%s" not found', [AProject]));
        exit;
      end;
    end
    // If AProjectGroups is specified, build all projects in that group
    else if not TLZString.IsEmptyString(AProjectGroups) then
    begin
      for LProjectGroup in FBuildProject.projectGroups do
      begin
        if InList(LProjectGroup.group, AProjectGroups) then
        begin
          LFound := true;
          Log(Format('Building project group "%s" with overrides',
            [LProjectGroup.group]));
          for LProject in LProjectGroup.projects do
          begin
            if GetBooleanValue(LProject.enabled, true) then
            begin
              GenerateProjectScript(LScript, LProject, APlatforms, AConfigs);
            end;
          end;
        end;
      end;

      if not LFound then
      begin
        StatusEvent(Format('Project group "%s" not found in configuration',
          [AProjectGroups]));
        InternalOnFailed(Format('Project group "%s" not found',
          [AProjectGroups]));
        exit;
      end;
    end;

    LScript.Add('');

    // Run post-build scripts (skip staging-only scripts and selective-only scripts)
    LScript.Add
      ('ECHO ----------------------------------------------------------');
    LScript.Add('ECHO [STATUS] Post-Build Scripts...');
    LScript.Add
      ('ECHO ----------------------------------------------------------');
    AddCustomScripts(LScript, FBuildProject.postBuildScripts, '[ALL]',
      false, true);

    LScript.Add('');
    LScript.Add('GOTO :BUILDSUCCESS');
    LScript.Add('');
    LScript.Add(':BUILDERROR');
    LScript.Add('ECHO [STATUS] Build failed');
    LScript.Add('EXIT /B 1');
    LScript.Add('');
    LScript.Add(':BUILDSUCCESS');
    LScript.Add('ECHO [STATUS] Build complete');
    LScript.Add('EXIT /B 0');

    if CheckActiveProcesses(FBuildProject.CheckActiveProcesses) then
    begin
      StatusEvent('Executing script...');
      FScriptRunner.Execute(LScript, InternalOnComplete, InternalOnLog,
        InternalOnFailed, InternalOnBeforeExecute, InternalOnAfterExecute,
        FLogFolder, FAsyncBuild);
    end
    else
    begin
      InternalOnFailed('Build aborted by user.');
    end;
  finally
    FreeAndNil(LScript);
  end;
end;

{ TScriptRunner }

procedure TScriptRunner.Cancel;
begin
  if FScriptState = stBusy then
    FScriptState := stCancelled;
end;

constructor TScriptRunner.Create;
begin
  inherited;
  FLog := TStringList.Create;
  FScript := TStringList.Create;
  FScriptState := stIdle;
end;

destructor TScriptRunner.Destroy;
var
  LIdx: integer;
begin
  LIdx := 0;
  while (State = stBusy) and (LIdx < 60) do
  begin
    Cancel;
    TLZSystem.Delay(1000);
    Inc(LIdx);
    Debug('Destroy', 'Waiting for script to cancel');
  end;
  try
    FreeAndNil(FLog);
    FreeAndNil(FScript);
  finally
    inherited;
  end;
end;

procedure TScriptRunner.DoExecute;
begin
  InternalOnBeforeExecute;
  try
    try
      if ExecuteScript(FScript) then
      begin
        InternalOnComplete;
      end
      else
      begin
        InternalOnFailed('Script execution failed.');
      end;
    except
      on E: Exception do
      begin
        InternalOnFailed(E.Message);
      end;
    end;
  finally
    InternalOnAfterExecute;
  end;
end;

procedure TScriptRunner.DoExecuteAsync;
var
  LTask: ITask;
begin
  LTask := TTask.Create(
    procedure
    begin
      InternalOnBeforeExecute;
      try
        try
          if ExecuteScript(FScript) then
          begin
            InternalOnComplete;
          end
          else
          begin
            InternalOnFailed('Script execution failed.');
          end;
        except
          on E: Exception do
          begin
            InternalOnFailed(E.Message);
          end;
        end;
      finally
        InternalOnAfterExecute;
      end;
    end);
  LTask.Start;
end;

procedure TScriptRunner.Execute(
  AScript: TStrings;
  AOnComplete: TOnScriptComplete;
  AOnLog: TOnScriptLog;
  AOnFailed: TOnScriptFailed;
  AOnBeforeExecute, AOnAfterExecute: TProc;
  ALogfolder: string;
  ASync: boolean);
begin
  if FScriptState <> stBusy then
  begin
    FScript.Assign(AScript);
    FOnComplete := AOnComplete;
    FOnLog := AOnLog;
    FOnFailed := AOnFailed;
    FOnBeforeExecute := AOnBeforeExecute;
    FOnAfterExecute := AOnAfterExecute;
    FLogFolder := ALogfolder;
    FAsync := ASync;
    if FAsync then
    begin
      DoExecuteAsync;
    end
    else
    begin
      DoExecute;
    end;
  end
  else
  begin
    raise EScriptRunnerException.Create('Script runner is busy');
  end;

end;

function TScriptRunner.ExecuteScript(AScript: TStrings): boolean;
var
  LScriptFile: TFileName;
begin
  Result := false;
  LScriptFile := GetScriptFileName;
  try
    try
      AScript.SaveToFile(LScriptFile);
      Result := JclSysUtils.Execute(LScriptFile, OnCommandExecuteText,
        true) = 0;
    except
      on E: Exception do
      begin
        Error(E);
      end;
    end;
  finally
    DeleteFile(PChar(LScriptFile));
  end;
end;

function TScriptRunner.GetLogFileName: TFileName;
begin
  Result := '';
  if not TLZString.IsEmptyString(FLogFolder) then
  begin
    Result := IncludeTrailingPathDelimiter(FLogFolder) +
      FormatDateTime('yyyymmddhhnnss', Now) + '.log';
  end;

end;

function TScriptRunner.GetScriptFileName: TFileName;
begin
  Result := TLZFile.GetTempFile('build', '.bat');
end;

function TScriptRunner.GetState: TScriptState;
begin
  Result := FScriptState;
end;

procedure TScriptRunner.InternalOnAfterExecute;
var
  LLogFile: TFileName;
begin
  try
    LLogFile := GetLogFileName;
    if not TLZString.IsEmptyString(LLogFile) then
    begin
      if TLZFile.CheckDirectoryExists(ExtractFilePath(LLogFile), true) then
      begin
        FLog.SaveToFile(LLogFile);
      end
      else
      begin
        Error(Format('Failed to save log "%s"', [LLogFile]));
      end;
    end;
  except
    on E: Exception do
    begin
      Error(E);
    end;
  end;
end;

procedure TScriptRunner.InternalOnBeforeExecute;
begin
  TThread.Queue(nil,
    procedure
    begin
      FScriptState := stBusy;
    end);
end;

procedure TScriptRunner.InternalOnComplete;
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnComplete) then
      begin
        FScriptState := stComplete;
        FOnComplete;
      end;
    end);
end;

procedure TScriptRunner.InternalOnFailed(AMessage: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnFailed) then
      begin
        FScriptState := stFailed;
        FOnFailed(AMessage);
      end;
    end);
end;

procedure TScriptRunner.InternalOnLog(AMessage: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      if Assigned(FOnLog) then
      begin
        FOnLog(AMessage);
      end;
    end);
end;

procedure TScriptRunner.OnCommandExecuteText(const AText: String);
begin
  Debug('OnCommandExecuteText', AText);
  FLog.Add(AText);
  InternalOnLog(AText);
  if FScriptState = stCancelled then
    Abort;
end;

end.
