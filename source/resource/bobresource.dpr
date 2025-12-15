program bobresource;

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
  Bob.ResourceGenerator;

type
  TResourceApplication = class(TBobConsoleApplication)
  protected
    function DoExecute: Integer; override;
  end;

function TResourceApplication.DoExecute: Integer;
var
  LResourceBuilder: TResourceBuilder;
  LMessage: string;
begin
  LResourceBuilder := TResourceBuilder.Create;
  try
    if LResourceBuilder.Execute(LMessage) then
      Result := Integer(becSuccess)
    else
    begin
      Log(LMessage);
      Result := Integer(becGeneralError);
    end;
  finally
    LResourceBuilder.Free;
  end;
end;

begin
  with TResourceApplication.Create('bobresource') do
  begin
    try
      ExitCode := Execute;
    finally
      Free;
    end;
  end;

end.
