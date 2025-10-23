program boblibrarygui;

{$R 'version.res' 'version.rc'}
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
// JCL_DEBUG_EXPERT_GENERATEJDBG ON

// For FastMM4 leak logging to work, add these to Project Options ->
// Delphi Compiler -> Conditional Defines (for Debug configuration):
// FullDebugMode;LogMemoryLeakDetailToFile;LogErrorsToFile;ClearLogFileOnStartup

uses
  FastMM4,
  Bob.StackTrace,
  Vcl.Forms,
  Windows,
  Controls,
  Dialogs,
  SysUtils,
  Printers,
  System.UITypes,
  Bob.Common,
  Lazy.Types,
  Lazy.Log,
  Lazy.Log.FileStream,
  Lazy.Utils.Windows,
  dmDelphiLibraryHelperU
    in 'dmDelphiLibraryHelperU.pas' {dmDelphiLibraryHelper: TDataModule} ,
  frmAddEnvironmentVariableU
    in 'frmAddEnvironmentVariableU.pas' {frmAddEnvironmentVariable} ,
  frmAddLibraryPathU in 'frmAddLibraryPathU.pas' {frmAddLibraryPath} ,
  frmDelphiLibraryHelperU
    in 'frmDelphiLibraryHelperU.pas' {frmDelphiLibraryHelper} ,
  frmFindReplaceU in 'frmFindReplaceU.pas' {frmFindReplace} ,
  frmProgressU in 'frmProgressU.pas' {frmProgress} ,
  frmSearchU in 'frmSearchU.pas' {frmSearch} ,
  LibraryHelperU in 'LibraryHelperU.pas',
  LibraryPathsU in 'LibraryPathsU.pas',
  LibraryEnvOptionsU in 'LibraryEnvOptionsU.pas';

{$R *.res}

procedure InitialiseLogging;
begin
  LazyLog.ApplicationName := 'boblibrarygui';
  LazyLogAddFileSteamHander(TBobCommon.GetLogFileName(LazyLog.ApplicationName));
end;

begin
  InitialiseLogging;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmDelphiLibraryHelper, frmDelphiLibraryHelper);
  Application.CreateForm(TdmDelphiLibraryHelper, dmDelphiLibraryHelper);
  Application.Run;

end.
