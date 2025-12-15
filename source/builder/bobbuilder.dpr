program bobbuilder;

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
  Bob.BuilderSettings in 'Bob.BuilderSettings.pas',
  Bob.BuilderModels in 'Bob.BuilderModels.pas',
  Bob.ProjectBuilder in 'Bob.ProjectBuilder.pas',
  Bob.BuilderRunner in 'Bob.BuilderRunner.pas';

type
  TBuilderApplication = class(TBobConsoleApplication)
  protected
    function DoExecute: Integer; override;
  end;

function TBuilderApplication.DoExecute: Integer;
var
  LBuildRunner: TBuildRunner;
begin
  LBuildRunner := TBuildRunner.Create;
  try
    Result := LBuildRunner.Execute;
  finally
    LBuildRunner.Free;
  end;
end;

begin
  with TBuilderApplication.Create('bobbuilder') do
  begin
    try
      ExitCode := Execute;
    finally
      Free;
    end;
  end;

end.
