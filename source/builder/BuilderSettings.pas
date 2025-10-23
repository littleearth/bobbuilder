unit BuilderSettings;

interface

uses
  Classes, SysUtils, 
  Lazy.Types, IniFiles;

type
  TSettings = class(TLZObject)
  private
    FIniFile: TIniFile;
    function GetISSBinary: string;
    function GetISSCompileParams: string;
    function GetISSRunWait: boolean;
    procedure SetISSBinary(const Value: string);
    procedure SetISSCompileParams(const Value: string);
    procedure SetISSRunWait(const Value: boolean);
  protected
    function GetSettingsFolder: string;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    procedure Save;
    property ISSBinary: string read GetISSBinary write SetISSBinary;
    property ISSCompileParams: string read GetISSCompileParams
      write SetISSCompileParams;
    property ISSRunWait: boolean read GetISSRunWait write SetISSRunWait;
  end;

implementation

uses
  VCL.Lazy.Utils.Windows;

{ TSettings }

constructor TSettings.Create;
begin
  inherited Create;
  FIniFile := TIniFile.Create(GetSettingsFolder + 'builder.ini');
  // Set defaults if values don't exist
  if not FIniFile.ValueExists('Settings', 'ISSBinary') then
    FIniFile.WriteString('Settings', 'ISSBinary', '"C:\Program Files (x86)\Inno Setup 6\ISCC.exe"');
  if not FIniFile.ValueExists('Settings', 'ISSCompileParams') then
    FIniFile.WriteString('Settings', 'ISSCompileParams', '/Q %issscriptparams% "%issfilename%"');
  if not FIniFile.ValueExists('Settings', 'ISSRunWait') then
    FIniFile.WriteBool('Settings', 'ISSRunWait', False);
end;

destructor TSettings.Destroy;
begin
  try
    FreeAndNil(FIniFile);
  finally
    inherited;
  end;
end;

function TSettings.GetISSBinary: string;
begin
  Result := FIniFile.ReadString('Settings', 'ISSBinary', '"C:\Program Files (x86)\Inno Setup 6\ISCC.exe"');
end;

function TSettings.GetISSCompileParams: string;
begin
  Result := FIniFile.ReadString('Settings', 'ISSCompileParams', '/Q %issscriptparams% "%issfilename%"');
end;

function TSettings.GetISSRunWait: boolean;
begin
  Result := FIniFile.ReadBool('Settings', 'ISSRunWait', False);
end;

function TSettings.GetSettingsFolder: string;
begin
  Result := IncludeTrailingPathDelimiter(TLZFile.GetUserAppDataFolder);
  Result := IncludeTrailingPathDelimiter(Result + 'Builder');
  TLZFile.CheckDirectoryExists(Result, True);
end;


procedure TSettings.Save;
begin
  FIniFile.UpdateFile;
end;

procedure TSettings.SetISSBinary(const Value: string);
begin
  FIniFile.WriteString('Settings', 'ISSBinary', Value);
end;

procedure TSettings.SetISSCompileParams(const Value: string);
begin
  FIniFile.WriteString('Settings', 'ISSCompileParams', Value);
end;

procedure TSettings.SetISSRunWait(const Value: boolean);
begin
  FIniFile.WriteBool('Settings', 'ISSRunWait', Value);
end;

end.
