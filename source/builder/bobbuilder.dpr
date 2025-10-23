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
  BuilderSettings in 'BuilderSettings.pas',
  Model.Build in 'Model.Build.pas',
  ProjectBuilder in 'ProjectBuilder.pas',
  BuildRunner in 'BuildRunner.pas';

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
