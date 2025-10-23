unit Bob.ResourceGenerator;

interface

uses Classes, Windows, SysUtils, Lazy.Utils, Lazy.Types, Bob.Common;

type
  TResourceBuilder = class(TLZObject)
  private
    FVariableSuffix: string;
    FVariablePrefix: string;
    procedure SetVariablePrefix(const Value: string);
    procedure SetVariableSuffix(const Value: string);
    function CheckCommandParameter(var AMessage: string; var ASource: string;
      var ADestination: string; var AVariablesSource: string): boolean;
    function GetUsage: string;
  public
    constructor Create; reintroduce;
    function Generate(ASource: TFileName; ADestination: TFileName): boolean;
    function AddVariables(ASource: TFileName): boolean;
    function Execute(var AMessage: string): boolean;
    property VariablePrefix: string read FVariablePrefix
      write SetVariablePrefix;
    property VariableSuffix: string read FVariableSuffix
      write SetVariableSuffix;
  end;

implementation

{ TResourceBuilder }

function TResourceBuilder.AddVariables(ASource: TFileName): boolean;
var
  LFile: TStringList;
  LVariable, LText: string;
  LIdx: integer;
begin
  Result := False;
  if FileExists(ASource) then
  begin
    LFile := TStringList.Create;
    try
      LFile.NameValueSeparator := '=';
      LFile.LoadFromFile(ASource);
      for LIdx := 0 to Pred(LFile.Count) do
      begin
        LVariable := LFile.Names[LIdx];
        LText := LFile.ValueFromIndex[LIdx];
        if not TLZString.IsEmptyString(LVariable) then
        begin
          SetEnvironmentVariable(PChar(LVariable), PChar(LText));
        end;
      end;
    finally
      FreeAndNil(LFile);
    end;
    Result := True;
  end;
end;

procedure TResourceBuilder.SetVariablePrefix(const Value: string);
begin
  FVariablePrefix := Value;
end;

procedure TResourceBuilder.SetVariableSuffix(const Value: string);
begin
  FVariableSuffix := Value;
end;

function TResourceBuilder.GetUsage: string;
begin
  Result := 'Usage instruction:' + #13#10 +
    '/SOURCE:{filename} /DESTINATION:{filename} /VARIABLES:{filename} /VARPREFIX:{text} /VARSUFFIX:{text}'
    + #13#10 + ' - /SOURCE and /DESTINATION are required.' + #13#10 +
    ' - Default prefix and suffix is ":"';
end;

function TResourceBuilder.CheckCommandParameter(var AMessage, ASource,
  ADestination, AVariablesSource: string): boolean;
var
  LValue: string;
begin
  Result := True;
  AMessage := '';
  if Result then
  begin
    if TLZSystem.GetApplicationParameters('/SOURCE', ASource) then
    begin
      Result := FileExists(ASource);
      if not Result then
        AMessage := Format('%s does not exist', [ASource]);
    end
    else
    begin
      AMessage := GetUsage;
      Result := False;
    end;
  end;

  if Result then
  begin
    if TLZSystem.GetApplicationParameters('/VARIABLES', AVariablesSource) then
    begin
      Result := FileExists(AVariablesSource);
      if not Result then
        AMessage := Format('%s does not exist', [AVariablesSource]);
    end;
  end;

  if Result then
  begin
    if not TLZSystem.GetApplicationParameters('/DESTINATION', ADestination) then
    begin
      AMessage := GetUsage;
      Result := False;
    end;
  end;

  if Result then
  begin
    if TLZSystem.GetApplicationParameters('/VARPREFIX', LValue) then
    begin
      FVariablePrefix := LValue;
    end;
  end;

  if Result then
  begin
    if TLZSystem.GetApplicationParameters('/VARSUFFIX', LValue) then
    begin
      FVariableSuffix := LValue;
    end;
  end;

end;

constructor TResourceBuilder.Create;
begin
  inherited;
  FVariableSuffix := ':';
  FVariablePrefix := ':';
end;

function TResourceBuilder.Execute(var AMessage: string): boolean;
var
  LSource, LDestination, LVariablesSource: string;
begin
  Result := CheckCommandParameter(AMessage, LSource, LDestination,
    LVariablesSource);
  if Result then
  begin
    if not TLZString.IsEmptyString(LVariablesSource) then
    begin
      if not AddVariables(LVariablesSource) then
      begin
        Result := False;
        AMessage := Format('Failed to process variable source "%s"',
          [LVariablesSource]);
      end;
    end;
  end;
  if Result then
  begin
    if not Generate(LSource, LDestination) then
    begin
      Result := False;
      AMessage := Format('Failed to process source "%s" and generate "%s"',
        [LSource, LDestination]);
    end;
  end;
end;

function TResourceBuilder.Generate(ASource, ADestination: TFileName): boolean;
var
  LTemplateFile: TStringList;
  LFolder: string;
begin
  Result := False;
  LTemplateFile := TStringList.Create;
  try
    if FileExists(ASource) then
    begin
      LFolder := ExtractFilePath(ADestination);
      if not TLZString.IsEmptyString(LFolder) then
      begin
        TLZFile.CheckDirectoryExists(LFolder, True);
      end;
      if FileExists(ADestination) then
        DeleteFile(ADestination);
      LTemplateFile.LoadFromFile(ASource);
      LTemplateFile.Text := TBobCommon.ProcessEnvironmentVariables
        (LTemplateFile.Text, FVariablePrefix, FVariableSuffix);
      LTemplateFile.SaveToFile(ADestination);
      Result := FileExists(ADestination);
    end
    else
    begin
      Error(Format('File "%s" does not exist', [ASource]));
    end;
  finally
    FreeAndNil(LTemplateFile);
  end;
end;

end.
