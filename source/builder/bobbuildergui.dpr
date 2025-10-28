program bobbuildergui;

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
  Bob.Common,
  System.SysUtils,
  Lazy.Log,
  Lazy.Types,
  Lazy.Log.FileStream,
  Lazy.Utils.Windows,
  VCL.Forms,
  frmBuilderU in 'gui\frmBuilderU.pas' {frmBuilder},
  Bob.BuilderModels in 'Bob.BuilderModels.pas',
  Bob.ProjectBuilder in 'Bob.ProjectBuilder.pas',
  Bob.BuilderSettings in 'Bob.BuilderSettings.pas',
  VCL.Themes,
  VCL.Styles,
  frmVersionInformationU in 'gui\frmVersionInformationU.pas' {frmVersionInformation},
  dmResourcesU in 'gui\dmResourcesU.pas' {dmResources: TDataModule},
  frmBuilderConfigEditorU in 'gui\frmBuilderConfigEditorU.pas' {frmBuilderConfigEditor},
  frmModelEditorU in 'gui\frmModelEditorU.pas' {frmModelEditor},
  Bob.BuilderTreeView in 'Bob.BuilderTreeView.pas';

{$R *.res}

procedure InitialiseLogging;
begin
  LazyLog.ApplicationName := 'bobbuildergui';
  LazyLogAddFileSteamHander(TBobCommon.GetLogFileName(LazyLog.ApplicationName));
  
end;

begin
  InitialiseLogging;
  ReportMemoryLeaksOnShutdown := True;
  Application.Initialize;
  Application.Title := 'BOB Builder';
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmBuilder, frmBuilder);
  Application.CreateForm(TdmResources, dmResources);
  Application.Run;

end.
