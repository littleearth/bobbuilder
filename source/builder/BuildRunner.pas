unit BuildRunner;

interface

uses
  Lazy.Types, Lazy.Utils, Classes, SysUtils, Winapi.Windows, Model.Build, Bob.Delphi.Model,
  ProjectBuilder, Bob.Delphi, JclConsole, Bob.Common;

type
  TVTColor = (vtBlack, vtRed, vtGreen, vtYellow, vtBlue, vtMagenta, vtCyan,
    vtWhite, vtBrightBlack, vtBrightRed, vtBrightGreen, vtBrightYellow,
    vtBrightBlue, vtBrightMagenta, vtBrightCyan, vtBrightWhite);

type
  TBuildRunner = class(TLZObject)
  private
    FIsWindowsTerminal: Boolean;
    FFileName: TFileName;
    FCleanup: Boolean;
    FBuildType: TBuildType;
    FDelphiHelper: TDelphiHelper;
    FProjectBuilder: TProjectBuilder;
    FDelphiVersionNumber: Integer;
    FJclScreenBuffer: TJclScreenBuffer;
    FJclConsole: TJclConsole;
    FAniFrame: Integer;
    FBuildInstallGroups: Boolean;
    FProject: string;
    FProjectGroup: string;
    FPlatforms: string;
    FConfigs: string;
    procedure OnBuildProgress(Sender: TObject; const AMessage: string;
      var ACancel: Boolean);
    procedure OnBuildComplete(Sender: TObject; const AComplete: Boolean;
      const AMessage: string);
    procedure OnProcessActive(ASender: TObject; const AProcessName: string;
      var AContinue: Boolean);
    procedure ConsoleWaiting(AText: string = '');
    procedure ConsoleLog(AText: string; AColor: TVTColor = vtWhite;
      AUpdateTitle: Boolean = false);
    function GetAniFrame: string;
    procedure ConsoleError(AText: string);
    function VTColorToJCL(AColor: TVTColor): TJclScreenFontColor;
  protected
    function GetDelphiVersion: TDelphiVersion;
    function GetBuilderFile: TFileName;
    procedure CheckParamters;
    function Run: Integer;
    procedure OnCtrlC(ASender: TObject);
    function IsWindowsTerminal: Boolean;
    function EnableVTProcessing: Boolean;
    procedure WriteConsoleVT(const AText: string; const AColor: TVTColor;
      const AMoveCursor: Boolean);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function Execute: Integer;
  end;

implementation

uses
  VCL.Lazy.Utils.Windows, System.StrUtils;

const
  AniFrame: array [0 .. 3] of char = ('|', '/', '-', '\');

  VT_COLOR_CODES: array [TVTColor] of string = (#27'[30m', // vtBlack
    #27'[31m', // vtRed
    #27'[32m', // vtGreen
    #27'[33m', // vtYellow
    #27'[34m', // vtBlue
    #27'[35m', // vtMagenta
    #27'[36m', // vtCyan
    #27'[37m', // vtWhite
    #27'[90m', // vtBrightBlack (Gray)
    #27'[91m', // vtBrightRed
    #27'[92m', // vtBrightGreen
    #27'[93m', // vtBrightYellow
    #27'[94m', // vtBrightBlue
    #27'[95m', // vtBrightMagenta
    #27'[96m', // vtBrightCyan
    #27'[97m' // vtBrightWhite
    );

  VT_RESET = #27'[0m';

  VT_TO_JCL_COLOR: array [TVTColor] of TJclScreenFontColor = (fclBlack,
    // vtBlack
    fclRed, // vtRed
    fclGreen, // vtGreen
    fclYellow, // vtYellow
    fclBlue, // vtBlue
    fclMagenta, // vtMagenta
    fclCyan, // vtCyan
    fclWhite, // vtWhite
    fclBlack, // vtBrightBlack -> fclBlack
    fclRed, // vtBrightRed -> fclRed
    fclGreen, // vtBrightGreen -> fclGreen
    fclYellow, // vtBrightYellow -> fclYellow
    fclBlue, // vtBrightBlue -> fclBlue
    fclMagenta, // vtBrightMagenta -> fclMagenta
    fclCyan, // vtBrightCyan -> fclCyan
    fclWhite // vtBrightWhite -> fclWhite
    );

function TBuildRunner.GetAniFrame: string;
begin
  REsult := '[ ' + AniFrame[FAniFrame] + ' ]';
  Inc(FAniFrame);
  if FAniFrame >= Length(AniFrame) then
  begin
    FAniFrame := 0;
  end;

end;

procedure TBuildRunner.ConsoleWaiting(AText: string);
var
  LX: Integer;
  LY: Integer;
  LText: string;
  LFrame: string;
begin
  LFrame := GetAniFrame;
  LText := TLZString.StringCleaner(LFrame + ' ' + AText, True, True);

  if FIsWindowsTerminal then
  begin
    // Use VT sequences - stays on same line for animation
    WriteConsoleVT(LText, vtGreen, false);
  end
  else
  begin
    // Use JCL for legacy console
    try
      LX := 0;
      LY := FJclScreenBuffer.Cursor.Position.Y;
      FJclScreenBuffer.Write(LText, LX, LY,
        TJclScreenTextAttribute.Create(fclGreen));
    except
      on E: Exception do
      begin
        FJclScreenBuffer.Clear;
      end;
    end;
  end;
end;

procedure TBuildRunner.ConsoleLog(AText: string; AColor: TVTColor;
  AUpdateTitle: Boolean);
var
  LX: Integer;
  LY: Integer;
begin
  if FIsWindowsTerminal then
  begin
    // Use VT sequences - moves to next line
    WriteConsoleVT(AText, AColor, True);
  end
  else
  begin
    // Use JCL for legacy console
    try
      LX := 0;
      LY := FJclScreenBuffer.Cursor.Position.Y;
      FJclScreenBuffer.Write(AText, LX, LY,
        TJclScreenTextAttribute.Create(VTColorToJCL(AColor)));
      FJclScreenBuffer.Cursor.MoveTo(1, LY + 1);
    except
      on E: Exception do
      begin
        FJclScreenBuffer.Clear;
      end;
    end;
  end;

  if AUpdateTitle then
  begin
    FJclConsole.Title := LeftStr(AText, 255);
  end;

end;

procedure TBuildRunner.ConsoleError(AText: string);
var
  LX: Integer;
  LY: Integer;
begin
  if FIsWindowsTerminal then
  begin
    // Use VT sequences - moves to next line
    WriteConsoleVT(AText, vtRed, True);
  end
  else
  begin
    // Use JCL for legacy console
    try
      LX := 0;
      LY := FJclScreenBuffer.Cursor.Position.Y;
      FJclScreenBuffer.Write(AText, LX, LY,
        TJclScreenTextAttribute.Create(fclRed));
      FJclScreenBuffer.Cursor.MoveTo(1, LY + 1);
    except
      on E: Exception do
      begin
        FJclScreenBuffer.Clear;
      end;
    end;
  end;
end;

function TBuildRunner.GetBuilderFile: TFileName;
var
  LErrorMessage: string;
begin
  Result := TBobCommon.GetBuilderFile(LErrorMessage);
  
  if TLZString.IsEmptyString(Result) then
  begin
    ConsoleError(LErrorMessage);
    Halt(1);
  end
  else
  begin
    ConsoleLog('Using .builder file: ' + ExtractFileName(Result), vtCyan);
  end;
end;

procedure TBuildRunner.CheckParamters;
var
  LParamValue: string;
begin
  FFileName := GetBuilderFile;

  if TLZSystem.GetApplicationParameters('/DELPHIVERSION', LParamValue) then
  begin
    FDelphiVersionNumber := StrToIntDef(LParamValue, -1);
  end;
  if TLZSystem.GetApplicationParameters('/BUILDTYPE', LParamValue) then
  begin
    FBuildType := btStaging;
    if SameText(LParamValue, 'PRODUCTION') or
      SameText(LParamValue, IntTOStr(Integer(btProduction))) then
    begin
      FBuildType := btProduction;
    end;
    if SameText(LParamValue, 'DEVELOPMENT') or
      SameText(LParamValue, IntTOStr(Integer(btDevelopment))) then
    begin
      FBuildType := btDevelopment;
    end;
  end;

  if TLZSystem.GetApplicationParameters('/INSTALLGROUPS', LParamValue) then
  begin
    FBuildInstallGroups := StrToBoolDef(LParamValue, True);
  end;
  // Check /PROJECTGROUP before /PROJECT to avoid partial matching
  if TLZSystem.GetApplicationParameters('/PROJECTGROUP', LParamValue) then
  begin
    FProjectGroup := LParamValue;
  end;
  if TLZSystem.GetApplicationParameters('/PROJECT', LParamValue) then
  begin
    FProject := LParamValue;
    // If /PROJECTGROUP was specified, /PROJECT might partially match and get "GROUP:..."
    // Clear FProject if it looks like a partial match of /PROJECTGROUP
    if Pos('GROUP:', UpperCase(FProject)) > 0 then
    begin
      FProject := '';
    end;
  end;
  if TLZSystem.GetApplicationParameters('/PLATFORMS', LParamValue) then
  begin
    FPlatforms := LParamValue;
  end;
  if TLZSystem.GetApplicationParameters('/CONFIGS', LParamValue) then
  begin
    FConfigs := LParamValue;
  end;
  if TLZSystem.GetApplicationParameters('/CLEANUP', LParamValue) then
  begin
    FCleanup := StrToBoolDef(LParamValue, True);
  end;
end;

procedure TBuildRunner.OnCtrlC(ASender: TObject);
begin
  FProjectBuilder.Cancel;
  while FProjectBuilder.State = stBusy do
  begin
    ConsoleWaiting();
    TLZSystem.Delay(100);
  end;
  ConsoleError('Terminated');
end;

constructor TBuildRunner.Create;
begin
  FProjectBuilder := TProjectBuilder.Create;
  FDelphiHelper := TDelphiHelper.Create;
  FProjectBuilder.OnBuildProgress := OnBuildProgress;
  FProjectBuilder.OnProcessActive := OnProcessActive;
  FProjectBuilder.OnBuildComplete := OnBuildComplete;
  FJclConsole := TJclConsole.Default;
  FJclConsole.OnCtrlC := OnCtrlC;
  FJclConsole.OnCtrlBreak := OnCtrlC;
  FJclConsole.OnShutdown := OnCtrlC;
  FJclConsole.OnClose := OnCtrlC;
  FJclConsole.OnLogOff := OnCtrlC;
  FJclScreenBuffer := FJclConsole.ActiveScreen;
  FAniFrame := 0;
  FIsWindowsTerminal := IsWindowsTerminal;
  FBuildInstallGroups := True; // Default to true
end;

destructor TBuildRunner.Destroy;
begin
  try
    FreeAndNil(FDelphiHelper);
    FreeAndNil(FProjectBuilder);
    FreeAndNil(FJclConsole);
  finally
    inherited;
  end;
end;

function TBuildRunner.Execute: Integer;
begin
  REsult := Run;
end;

function TBuildRunner.GetDelphiVersion: TDelphiVersion;
begin
  REsult := nil;
  if FDelphiHelper.Count > 0 then
  begin
    if FDelphiVersionNumber <> -1 then
    begin
      REsult := FDelphiHelper.Version[FDelphiVersionNumber];
    end
    else
    begin
      REsult := FDelphiHelper.Versions[0];
    end;
  end;
end;

function TBuildRunner.EnableVTProcessing: Boolean;
var
  LStdOut: THandle;
  LMode: DWORD;
begin
  LStdOut := GetStdHandle(STD_OUTPUT_HANDLE);
  if GetConsoleMode(LStdOut, LMode) then
  begin
    // ENABLE_VIRTUAL_TERMINAL_PROCESSING = $0004
    LMode := LMode or $0004;
    REsult := SetConsoleMode(LStdOut, LMode);
  end
  else
  begin
    REsult := false;
  end;
end;

function TBuildRunner.IsWindowsTerminal: Boolean;
var
  LWTSession: string;
  LTermProgram: string;
  LStdOut: THandle;
  LMode: DWORD;
begin
  REsult := false;
  // Try to enable VT processing first
  EnableVTProcessing;

  // Check for Windows Terminal
  LWTSession := GetEnvironmentVariable('WT_SESSION');
  if LWTSession <> '' then
  begin
    REsult := True;
  end;

  if not REsult then
  begin
    // Check for VS Code terminal
    LTermProgram := GetEnvironmentVariable('TERM_PROGRAM');
    if LTermProgram = 'vscode' then
    begin
      REsult := True;
    end;
  end;

  if not REsult then
  begin
    // Check if VT processing is now enabled
    LStdOut := GetStdHandle(STD_OUTPUT_HANDLE);
    if GetConsoleMode(LStdOut, LMode) then
    begin
      REsult := (LMode and $0004) <> 0;
    end;
  end;
  Debug('IsWindowsTerminal', BoolToStr(REsult, True));
end;

function TBuildRunner.VTColorToJCL(AColor: TVTColor): TJclScreenFontColor;
begin
  REsult := VT_TO_JCL_COLOR[AColor];
end;

procedure TBuildRunner.OnBuildComplete(Sender: TObject;
  const AComplete: Boolean; const AMessage: string);

begin

end;

procedure TBuildRunner.OnBuildProgress(Sender: TObject; const AMessage: string;
  var ACancel: Boolean);
var
  LMessage: string;
  LColor: TVTColor;
  LUpdateTitle: Boolean;
begin
  // ConsoleWaiting();
  LMessage := AMessage;
  LColor := vtWhite;
  LUpdateTitle := false;
  if Pos('[STATUS]', AMessage) = 1 then
  begin
    LColor := vtGreen;
    LMessage := StringReplace(LMessage, '[STATUS]', '', []);
    LUpdateTitle := True;
  end;
  LMessage := TLZString.StringCleaner(LMessage, True, True);
  ConsoleLog(LMessage, LColor, LUpdateTitle);
end;

procedure TBuildRunner.OnProcessActive(ASender: TObject;
  const AProcessName: string; var AContinue: Boolean);
var
  LValue: string;
begin
  ConsoleLog(Format('"%s" is running. Press "C" Continue', [AProcessName]));
  Read(LValue);
  AContinue := SameText(LValue, 'C');
end;

function TBuildRunner.Run: Integer;
var
  LProject, LProjectGroup: string;
begin
  FDelphiVersionNumber := -1;
  FFileName := '';
  FCleanup := True;
  FBuildInstallGroups := True;

  ConsoleLog('');
  CheckParamters;
  if FileExists(FFileName) then
  begin
    try
      FProjectBuilder.CleanupEnabled := FCleanup;
      FProjectBuilder.BuildType := FBuildType;
      FProjectBuilder.BuildInstallGroupsEnabled := FBuildInstallGroups;
      FProjectBuilder.FileName := FFileName;
      FProjectBuilder.DelphiVersion := GetDelphiVersion;
      FProjectBuilder.AsyncBuild := false;

      LProject := FProject;
      LProjectGroup := FProjectGroup;
      if not TLZString.IsEmptyString(LProject) then
        LProjectGroup := '';

      if not TLZString.IsEmptyString(FProjectGroup + LProject) then
      begin
        FProjectBuilder.Build(LProjectGroup, '', '', '', LProject, FPlatforms,
          FConfigs);
      end
      else
      begin
        FProjectBuilder.Build;
      end;

      case FProjectBuilder.State of
        stCancelled:
          REsult := 9999;
        stComplete:
          REsult := 0;
        stFailed:
          REsult := 2;
      else
        REsult := -1;
      end;
    except
      on E: Exception do
      begin
        Error(E);
        ConsoleError(E.Message);
        REsult := 3;
      end;
    end;
  end
  else
  begin
    ConsoleError(Format('File "%s" does not exist', [FFileName]));
    REsult := 1;
  end;
  ConsoleLog('');
end;

procedure TBuildRunner.WriteConsoleVT(const AText: string;
  const AColor: TVTColor; const AMoveCursor: Boolean);
var
  LStdOut: THandle;
  LWritten: DWORD;
  LOutput: string;
begin
  LStdOut := GetStdHandle(STD_OUTPUT_HANDLE);

  if AMoveCursor then
  begin
    // Clear line, write colored text, move to next line
    LOutput := #13 + #27'[2K' + VT_COLOR_CODES[AColor] + AText +
      VT_RESET + #13#10;
  end
  else
  begin
    // Clear line, write colored text, stay on same line (for animation)
    LOutput := #13 + #27'[2K' + VT_COLOR_CODES[AColor] + AText + VT_RESET;
  end;

  WriteConsole(LStdOut, PChar(LOutput), Length(LOutput), LWritten, nil);
end;

end.
