program bobcodesign;

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
  Bob.CodeSign in '..\common\Bob.CodeSign.pas',
  Bob.Encryption in '..\common\Bob.Encryption.pas';

type
  TCodeSignApplication = class(TBobConsoleApplication)
  protected
    function DoExecute: Integer; override;
  end;

function TCodeSignApplication.DoExecute: Integer;
var
  LCodeSign: TCodeSignConsole;
  LMessage: string;
begin
  LCodeSign := TCodeSignConsole.Create;
  try
    if LCodeSign.Execute(LMessage) then
      Result := Integer(becSuccess)
    else
    begin
      WriteLn(LMessage);
      Result := Integer(becGeneralError);
    end;
  finally
    LCodeSign.Free;
  end;
end;

begin
  with TCodeSignApplication.Create('bobcodesign') do
  begin
    try
      ExitCode := Execute;
    finally
      Free;
    end;
  end;

end.
