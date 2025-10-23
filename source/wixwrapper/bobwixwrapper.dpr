program bobwixwrapper;

{$APPTYPE CONSOLE}
{$R *.res}
{$R 'version.res' 'version.rc'}

uses
  FastMM4,
  Bob.StackTrace,
  System.SysUtils,
  Bob.Common,
  Bob.Console,
  Lazy.Types,
  Bob.WIXWrapper in '..\common\Bob.WIXWrapper.pas';

type
  TWixWrapperApplication = class(TBobConsoleApplication)
  protected
    function DoExecute: Integer; override;
  end;

function TWixWrapperApplication.DoExecute: Integer;
var
  LWixWrapper: Twixwrapper;
begin
  LWixWrapper := Twixwrapper.Create;
  try
    if LWixWrapper.Execute then
      Result := Integer(becSuccess)
    else
      Result := Integer(becGeneralError);
  finally
    LWixWrapper.Free;
  end;
end;

begin
  with TWixWrapperApplication.Create('bobwixwrapper') do
  begin
    try
      ExitCode := Execute;
    finally
      Free;
    end;
  end;
end.
