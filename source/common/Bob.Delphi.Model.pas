unit Bob.Delphi.Model;

interface

uses
  Classes, SysUtils, System.JSON, Lazy.Model;

type
  TDelphiVersion = class(TLZModel)
  private
    FVersion: double;
    FRootFolder: string;
    FProductName: string;
    FRegistryKey: string;
    Fexecutable: string;
    procedure SetProductName(const Value: string);
    procedure SetRegistryKey(const Value: string);
    procedure SetRootFolder(const Value: string);
    procedure SetVersion(const Value: double);
    procedure Setexecutable(const Value: string);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property registryKey: string read FRegistryKey write SetRegistryKey;
    property version: double read FVersion write SetVersion;
    property rootFolder: string read FRootFolder write SetRootFolder;
    property productName: string read FProductName write SetProductName;
    property executable: string read Fexecutable write Setexecutable;
  end;

  TDelphiVersions = class(TLZModelList<TDelphiVersion>);

implementation

procedure TDelphiVersion.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  if Assigned(AJSONValue) then
  begin
    AJSONValue.TryGetValue<string>('registryKey', FRegistryKey);
    AJSONValue.TryGetValue<double>('version', FVersion);
    AJSONValue.TryGetValue<string>('rootFolder', FRootFolder);
    AJSONValue.TryGetValue<string>('productName', FProductName);
    AJSONValue.TryGetValue<string>('executable', Fexecutable);
  end;
end;

procedure TDelphiVersion.Setexecutable(const Value: string);
begin
  Fexecutable := Value;
end;

procedure TDelphiVersion.SetProductName(const Value: string);
begin
  FProductName := Value;
end;

procedure TDelphiVersion.SetRegistryKey(const Value: string);
begin
  FRegistryKey := Value;
end;

procedure TDelphiVersion.SetRootFolder(const Value: string);
begin
  FRootFolder := Value;
end;

procedure TDelphiVersion.SetVersion(const Value: double);
begin
  FVersion := Value;
end;

function TDelphiVersion.ToJSONValue: TJSONValue;
begin
  // ToJSONValue not implemented - this project only reads configuration files
  Result := nil;
end;

end.
