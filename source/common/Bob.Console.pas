unit Bob.Console;

interface

uses Classes, Windows, SysUtils, Lazy.Utils.Windows, Lazy.Types;

type
  // Standardized exit codes for Bob console applications
  TBobExitCode = (becSuccess = 0, // Operation completed successfully
    becGeneralError = 1, // General/validation error
    becNoParameters = 2, // Required parameters missing
    becNoFiles = 3, // No files found/processed
    becException = 9999 // Unhandled exception
    );

type
  // Base class for Bob console applications with standardized error handling
  TBobConsoleApplication = class(TLZObject)
  private
  protected
    function DoExecute: Integer; virtual; abstract;
    procedure ConsoleExitPrompt;
  public
    constructor Create(const AApplicationName: string); reintroduce;
    function Execute: Integer;
  end;

implementation

uses
  Bob.Common, Lazy.Log, Lazy.Log.Console, Lazy.Log.FileStream;

{ TBobConsoleApplication }

constructor TBobConsoleApplication.Create(const AApplicationName: string);
begin
  inherited Create;
  LazyLog.ApplicationName := AApplicationName;
  LazyLog.AddHandler('Console', TLZLogConsole);
  LazyLogAddFileSteamHander(TBobCommon.GetLogFileName(AApplicationName));
end;

procedure TBobConsoleApplication.ConsoleExitPrompt;
begin
  if TLZSystem.IsDelphiRunning then
  begin
    WriteLn(Format
      ('[Debug Hook Detected] Application has terminated (Exit code: %d), press ENTER to Exit',
      [ExitCode]));
    Readln;
  end;
end;

function TBobConsoleApplication.Execute: Integer;
begin
  try
    Result := DoExecute;
  except
    on E: Exception do
    begin
      LazyLog.Error(E.ClassName, E);
      Result := Integer(becException);
    end;
  end;
  ConsoleExitPrompt;
end;

end.
