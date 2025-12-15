program bobmdtortf;

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
  Bob.MarkdownToRTF in '..\common\Bob.MarkdownToRTF.pas';

type
  TMarkdownToRTFApplication = class(TBobConsoleApplication)
  protected
    function DoExecute: Integer; override;
  end;

function TMarkdownToRTFApplication.DoExecute: Integer;
var
  LMessage: string;
  LResult: Boolean;
  LMarkdownToRTF: TMarkdownToRTFConsole;
begin
  LMarkdownToRTF := TMarkdownToRTFConsole.Create;
  try
    LResult := LMarkdownToRTF.Execute(LMessage);
    if not TLZString.IsEmptyString(LMessage) then
      Log(LMessage);
    if LResult then
      Result := Integer(becSuccess)
    else
      Result := Integer(becGeneralError);
  finally
    FreeAndNil(LMarkdownToRTF);
  end;
end;

begin
  with TMarkdownToRTFApplication.Create('bobmdtortf') do
  begin
    try
      ExitCode := Execute;
    finally
      Free;
    end;
  end;

end.
