unit Bob.Git;

interface

uses
  Lazy.Types,
  System.Classes, System.SysUtils, System.IOUtils, System.Generics.Collections,
  Winapi.Windows, Winapi.ShellAPI, System.Types;

type
  TGitRepository = record
    WorkTree: string;
    GitDir: string;
  end;

  TGitPullResult = (gprSuccess, gprFailed, gprRequiresReset);

  TOnPromptEvent = function(const ATitle, AQuestion: string): Boolean of object;

  TBobGitPuller = class(TLZObject)
  private
    LRepositories: TList<TGitRepository>;
    LFailedRepositories: TStringList;
    FOnPrompt: TOnPromptEvent;

    function FindGitRepositories(const ABasePath: string)
      : TList<TGitRepository>;
    function ExecuteGitCommand(const AWorkTree, AGitDir,
      ACommand: string): Boolean;
    function PullRepository(const ARepository: TGitRepository): TGitPullResult;
    function ResetRepository(const ARepository: TGitRepository): Boolean;
    function PromptUser(const ATitle, AQuestion: string): Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function PullAllRepositories(const ABasePath: string): Boolean;
    function GetFailedRepositories: TStringList;

    property OnPrompt: TOnPromptEvent read FOnPrompt write FOnPrompt;
  end;

implementation

constructor TBobGitPuller.Create;
begin
  inherited;
  LRepositories := TList<TGitRepository>.Create;
  LFailedRepositories := TStringList.Create;
end;

destructor TBobGitPuller.Destroy;
begin
  LRepositories.Free;
  LFailedRepositories.Free;
  inherited;
end;

function TBobGitPuller.FindGitRepositories(const ABasePath: string)
  : TList<TGitRepository>;
var
  LSearchPattern: string;
  LGitDirs: TStringDynArray;
  LGitDir: string;
  LRepository: TGitRepository;
begin
  Result := TList<TGitRepository>.Create;

  LSearchPattern := TPath.Combine(ABasePath, '.git');
  LGitDirs := TDirectory.GetDirectories(ABasePath, '.git',
    TSearchOption.soAllDirectories);

  for LGitDir in LGitDirs do
  begin
    LRepository.GitDir := LGitDir;
    LRepository.WorkTree := TPath.GetDirectoryName(LGitDir);
    Result.Add(LRepository);
  end;
end;

function TBobGitPuller.ExecuteGitCommand(const AWorkTree, AGitDir,
  ACommand: string): Boolean;
var
  LStartupInfo: TStartupInfo;
  LProcessInfo: TProcessInformation;
  LCommandLine: string;
  LExitCode: DWORD;
begin
  LCommandLine := Format('git --git-dir="%s" --work-tree="%s" %s',
    [AGitDir, AWorkTree, ACommand]);

  ZeroMemory(@LStartupInfo, SizeOf(LStartupInfo));
  LStartupInfo.cb := SizeOf(LStartupInfo);
  LStartupInfo.dwFlags := STARTF_USESHOWWINDOW;
  LStartupInfo.wShowWindow := SW_HIDE;

  Result := CreateProcess(nil, PChar(LCommandLine), nil, nil, False, 0, nil,
    PChar(AWorkTree), LStartupInfo, LProcessInfo);

  if Result then
  begin
    WaitForSingleObject(LProcessInfo.hProcess, INFINITE);
    GetExitCodeProcess(LProcessInfo.hProcess, LExitCode);
    Result := LExitCode = 0;

    CloseHandle(LProcessInfo.hProcess);
    CloseHandle(LProcessInfo.hThread);
  end;
end;

function TBobGitPuller.PullRepository(const ARepository: TGitRepository)
  : TGitPullResult;
begin
  Log(Format('Pulling %s', [ARepository.WorkTree]));

  if ExecuteGitCommand(ARepository.WorkTree, ARepository.GitDir,
    'pull --quiet origin master') then
    Result := gprSuccess
  else
    Result := gprFailed;
end;

function TBobGitPuller.ResetRepository(const ARepository
  : TGitRepository): Boolean;
begin
  Log(Format('Resetting %s', [ARepository.WorkTree]));

  Result := ExecuteGitCommand(ARepository.WorkTree, ARepository.GitDir,
    'reset --hard');
  if Result then
    Result := ExecuteGitCommand(ARepository.WorkTree, ARepository.GitDir,
      'pull --quiet');
end;

function TBobGitPuller.PromptUser(const ATitle, AQuestion: string): Boolean;
begin
  if Assigned(FOnPrompt) then
    Result := FOnPrompt(ATitle, AQuestion)
  else
    Result := False;
end;

function TBobGitPuller.PullAllRepositories(const ABasePath: string): Boolean;
var
  LRepository: TGitRepository;
  LPullResult: TGitPullResult;
  LShouldReset: Boolean;
  LFailedRepository: string;
begin
  LFailedRepositories.Clear;

  Log('Starting Git repository scan and pull operation...');
  Log('Base path: ' + ABasePath);

  LRepositories.Free;
  LRepositories := FindGitRepositories(ABasePath);

  Log(Format('Found %d Git repositories', [LRepositories.Count]));

  for LRepository in LRepositories do
  begin
    LPullResult := PullRepository(LRepository);

    case LPullResult of
      gprSuccess:
        Log(Format('Successfully pulled %s', [LRepository.WorkTree]));

      gprFailed:
        begin
          LShouldReset := PromptUser('Git pull failed',
            Format('Run a local reset on %s?', [LRepository.WorkTree]));

          if LShouldReset then
          begin
            if ResetRepository(LRepository) then
              Log(Format('Successfully reset and pulled %s',
                [LRepository.WorkTree]))
            else
              LFailedRepositories.Add(LRepository.WorkTree);
          end
          else
            LFailedRepositories.Add(LRepository.WorkTree);
        end;
    end;
  end;

  if LFailedRepositories.Count > 0 then
  begin
    Log('Git pull incomplete for the following repositories:');
    Result := False;
    for LFailedRepository in LFailedRepositories do
      Log(Format('  - %s', [LFailedRepository]));
  end
  else
  begin
    Log('Operation complete.');
    Result := True;
  end;
end;

function TBobGitPuller.GetFailedRepositories: TStringList;
begin
  Result := LFailedRepositories;
end;

end.
