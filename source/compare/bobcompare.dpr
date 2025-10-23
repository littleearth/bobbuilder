program bobcompare;

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
  Bob.Compare in '..\common\Bob.Compare.pas';

type
  TCompareApplication = class(TBobConsoleApplication)
  protected
    function DoExecute: Integer; override;
  end;

function TCompareApplication.DoExecute: Integer;
var
  LCompare: TCompareConsole;
  LMessage: string;
begin
  LCompare := TCompareConsole.Create;
  try
    Result := LCompare.Execute(LMessage);
    Log(LMessage);
  finally
    LCompare.Free;
  end;
end;

begin
  with TCompareApplication.Create('bobcompare') do
  begin
    try
      ExitCode := Execute;
    finally
      Free;
    end;
  end;
end.
