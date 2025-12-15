program bobgitpuller;

{$APPTYPE CONSOLE}
{$R *.res}

uses
  FastMM4,
  Bob.StackTrace,
  System.SysUtils,
  System.IOUtils,
  Lazy.Types,
  Bob.Common,
  Bob.Console,
  Bob.Git;

type
  TConsoleGitPuller = class(TBobGitPuller)
  private
    function OnPromptHandler(const ATitle, AQuestion: string): Boolean;
  public
    constructor Create;
  end;

  TGitPullerApplication = class(TBobConsoleApplication)
  protected
    function DoExecute: Integer; override;
  end;

constructor TConsoleGitPuller.Create;
begin
  inherited;
  OnPrompt := OnPromptHandler;
end;

function TConsoleGitPuller.OnPromptHandler(const ATitle,
  AQuestion: string): Boolean;
var
  LInput: string;
begin
  Log(ATitle);
  Log(AQuestion + ' (Y/N): ');
  ReadLn(LInput);
  Result := SameText(LInput, 'Y') or SameText(LInput, 'YES');
end;

function TGitPullerApplication.DoExecute: Integer;
var
  LGitPuller: TConsoleGitPuller;
  LBasePath: string;
begin
  LGitPuller := TConsoleGitPuller.Create;
  try
    if ParamCount > 0 then
      LBasePath := ParamStr(1)
    else
      LBasePath := GetCurrentDir;

    if LGitPuller.PullAllRepositories(LBasePath) then
      Result := Integer(becSuccess)
    else
      Result := Integer(becGeneralError);
  finally
    LGitPuller.Free;
  end;
end;

begin
  with TGitPullerApplication.Create('bobgitpuller') do
  begin
    try
      ExitCode := Execute;
    finally
      Free;
    end;
  end;

end.
