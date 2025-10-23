program boblibrary;

{$APPTYPE CONSOLE}
{$R *.res}
{$R 'version.res' 'version.rc'}

uses
  FastMM4,
  System.SysUtils,
  Bob.Common,
  Lazy.Types,
  Lazy.Log,
  Lazy.Log.FileStream,
  Lazy.Log.Console,
  Lazy.Utils.Windows;

procedure InitialiseLogging;
begin
  LazyLog.ApplicationName := 'boblibrary';
  LazyLog.AddHandler('Console', TLZLogConsole);
  LazyLogAddFileSteamHander(TBobCommon.GetLogFileName(LazyLog.ApplicationName));
end;

begin
  try
    InitialiseLogging;
    { TODO -oUser -cConsole Main : Insert code here }
  except
    on E: Exception do
      Error(E.ClassName, E);
  end;

end.
