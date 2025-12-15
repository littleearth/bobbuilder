unit Bob.Delphi;

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Lazy.Types, Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Bob.Delphi.Model;

type
  TDelphiHelper = class(TLZObject)
  private
    FDelphiVersions: TDelphiVersions;
    function GetCount: integer;
  protected
    function GetDelphiVerionByVersion(AVersion: integer): TDelphiVersion;
    function GetDelphiVersionByIndex(AIndex: integer): TDelphiVersion;
    procedure FindInstallations(AKey: string);
    procedure AddVersion(
      AVersion: integer;
      ARootFolder: string;
      ARegistryKey: string;
      AProductName: string;
      AExecutable: TFileName);
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function IsProcessRunning(AFileName: string): Boolean;
    class function IsDelphiRunning: Boolean;
    procedure AsStrings(AStrings: TStrings);
    function ByProductName(AProductName: string): TDelphiVersion;
    property Count: integer read GetCount;
    property Version[AVersion: integer]: TDelphiVersion
      read GetDelphiVerionByVersion;
    property Versions[AIndex: integer]: TDelphiVersion
      read GetDelphiVersionByIndex;
  end;

implementation

uses
  VCL.Lazy.Utils.Windows, System.Win.Registry, Winapi.TlHelp32;

{ TDelphiHelper }

function TDelphiHelper.IsProcessRunning(AFileName: string): Boolean;
var
  ContinueLoop: BOOL;
  FSnapshotHandle: THandle;
  FProcessEntry32: TProcessEntry32;
begin
  FSnapshotHandle := CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS, 0);
  FProcessEntry32.dwSize := SizeOf(FProcessEntry32);
  ContinueLoop := Process32First(FSnapshotHandle, FProcessEntry32);
  Result := False;
  while integer(ContinueLoop) <> 0 do
  begin
    if ((UpperCase(ExtractFileName(FProcessEntry32.szExeFile))
      = UpperCase(AFileName)) or (UpperCase(FProcessEntry32.szExeFile)
      = UpperCase(AFileName))) then
    begin
      Result := true;
    end;
    ContinueLoop := Process32Next(FSnapshotHandle, FProcessEntry32);
  end;
  CloseHandle(FSnapshotHandle);
end;

class function TDelphiHelper.IsDelphiRunning: Boolean;
var
  LDelphiHelper: TDelphiHelper;
begin
  LDelphiHelper := TDelphiHelper.Create;
  try
    Result := LDelphiHelper.IsProcessRunning('bds.exe');
  finally
    FreeAndNil(LDelphiHelper);
  end;
end;

procedure TDelphiHelper.AddVersion(
  AVersion: integer;
  ARootFolder, ARegistryKey, AProductName: string;
  AExecutable: TFileName);
var
  LDelphiVersion: TDelphiVersion;
begin
  LDelphiVersion := TDelphiVersion.Create;
  FDelphiVersions.Add(LDelphiVersion);
  LDelphiVersion.registryKey := ARegistryKey;
  LDelphiVersion.Version := AVersion;
  LDelphiVersion.rootFolder := ARootFolder;
  LDelphiVersion.productName := AProductName;
  LDelphiVersion.executable := AExecutable;
end;

procedure TDelphiHelper.AsStrings(AStrings: TStrings);
var
  LIdx: integer;
begin
  AStrings.Clear;
  for LIdx := (Pred(Count)) downto 0 do
  begin
    AStrings.Add(Versions[LIdx].productName);
  end;
end;

function TDelphiHelper.ByProductName(AProductName: string): TDelphiVersion;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (LIdx < FDelphiVersions.Count) and (Result = nil) do
  begin
    if SameText(FDelphiVersions.Model[LIdx].productName, AProductName) then
    begin
      Result := FDelphiVersions.Model[LIdx];
    end;
    Inc(LIdx);
  end;
end;

constructor TDelphiHelper.Create;
begin
  inherited;
  FDelphiVersions := TDelphiVersions.Create;
  FindInstallations('SOFTWARE\Embarcadero\BDS');
end;

destructor TDelphiHelper.Destroy;
begin
  try
    FreeAndNil(FDelphiVersions);
  except
  end;
  inherited;
end;

procedure TDelphiHelper.FindInstallations(AKey: string);
var
  LRegistry: TRegistry;
  LKeys: TStringList;
  LKeyIdx: integer;
  LKeyName: string;
  LVersion: integer;
  LRootFolder, LRegistryKey, LProductName: string;
  LExecutable: TFileName;
begin
  LRegistry := TRegistry.Create;
  LKeys := TStringList.Create;
  try
    LRegistry.RootKey := HKEY_LOCAL_MACHINE;
    if LRegistry.OpenKeyReadOnly(AKey) then
    begin
      LRegistry.GetKeyNames(LKeys);
      for LKeyIdx := 0 to Pred(LKeys.Count) do
      begin
        LKeyName := IncludeTrailingBackslash(AKey) + LKeys[LKeyIdx];
        LRegistry.CloseKey;
        if LRegistry.OpenKeyReadOnly(LKeyName) then
        begin
          try
            LVersion := StrToIntDef(LRegistry.ReadString('ProductVersion'), -1);
            LRootFolder := LRegistry.ReadString('RootDir');
            LExecutable := LRegistry.ReadString('App');
            LRegistryKey := LKeyName;
            LRegistry.CloseKey;
            if LRegistry.OpenKeyReadOnly(IncludeTrailingBackslash(LRegistryKey)
              + 'Personalities') then
            begin
              LProductName := LRegistry.ReadString('');
              LRegistry.CloseKey;
            end;
            if FileExists(LExecutable) and (LVersion <> -1) then
            begin
              AddVersion(LVersion, LRootFolder, LRegistryKey, LProductName,
                LExecutable);
            end;
          except
            on E: Exception do
            begin
              Error(E, LKeyName);
            end;
          end;
        end;
        LRegistry.CloseKey;
      end;
    end;
  finally
    FreeAndNil(LRegistry);
    FreeAndNil(LKeys);
  end;
end;

function TDelphiHelper.GetCount: integer;
begin
  Result := FDelphiVersions.Count;
end;

function TDelphiHelper.GetDelphiVerionByVersion(AVersion: integer)
  : TDelphiVersion;
var
  LIdx: integer;
begin
  Result := nil;
  LIdx := 0;
  while (LIdx < FDelphiVersions.Count) and (Result = nil) do
  begin
    if FDelphiVersions.Model[LIdx].Version = AVersion then
    begin
      Result := FDelphiVersions.Model[LIdx];
    end;
    Inc(LIdx);
  end;
end;

function TDelphiHelper.GetDelphiVersionByIndex(AIndex: integer): TDelphiVersion;
begin
  Result := FDelphiVersions.Model[AIndex];
end;

end.
