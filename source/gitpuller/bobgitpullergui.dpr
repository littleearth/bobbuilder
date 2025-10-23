program bobgitpullergui;

uses
  FastMM4,
  Bob.StackTrace,
  Bob.Common,
  Lazy.Types,
  Lazy.Log,
  Lazy.Log.FileStream,
  Lazy.Utils.Windows,
  Vcl.Forms,
  MainFormU in 'MainFormU.pas' {MainForm};

{$R *.res}

procedure InitialiseLogging;
begin
  LazyLog.ApplicationName := 'bobgitpullergui';
  LazyLogAddFileSteamHander(TBobCommon.GetLogFileName(LazyLog.ApplicationName));
end;

begin
  InitialiseLogging;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;

end.
