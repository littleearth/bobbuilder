program bobmd5;

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
  Lazy.Utils,
  Bob.MD5 in '..\common\Bob.MD5.pas';

type
  TMD5Application = class(TBobConsoleApplication)
  protected
    function DoExecute: Integer; override;
  end;

function TMD5Application.DoExecute: Integer;
var
  LMessage: string;
  LResult: Boolean;
  LMD5Console: TMD5Console;
begin
  LMD5Console := TMD5Console.Create;
  try
    LResult := LMD5Console.Execute(LMessage);
    if not TLZString.IsEmptyString(LMessage) then
      Log(LMessage);
    if LResult then
      Result := Integer(becSuccess)
    else
      Result := Integer(becGeneralError);
  finally
    FreeAndNil(LMD5Console);
  end;
end;

begin
  with TMD5Application.Create('bobmd5') do
  begin
    try
      ExitCode := Execute;
    finally
      Free;
    end;
  end;



end.
