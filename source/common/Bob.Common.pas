unit Bob.Common;

interface

uses Classes, Windows, SysUtils, Lazy.Utils.Windows, Lazy.Types;


type
  TBobCommon = class(TLZObject)
  public
    class function ProcessEnvironmentVariables(ASource: string;
      AVariablePrefix: string = '%'; AVariableSuffix: string = '%'): string;
    class procedure GetFileList(ASource: string; AFileList: TStrings;
      ARecurse: Boolean);
    class function GetBuilderFile(out AErrorMessage: string): TFileName;
    class function GetLogFileName(AApplicationName: string): TFileName;
  end;


implementation

uses
  Lazy.Token, WinAPi.KnownFolders;

{ TBobCommon }

class function TBobCommon.ProcessEnvironmentVariables(ASource, AVariablePrefix,
  AVariableSuffix: string): string;
var
  LVariables: TStringList;
  LIdx: integer;
  LVariable, LVariableValue: string;
begin
  LVariables := TStringList.Create;
  try
    Result := ASource;
    TLZSystem.GetEnvironmentVariables(LVariables);
    LVariables.NameValueSeparator := '=';
    for LIdx := 0 to Pred(LVariables.Count) do
    begin
      LVariable := Trim(LVariables.Names[LIdx]);
      LVariableValue := Trim(LVariables.ValueFromIndex[LIdx]);
      if not TLZString.IsEmptyString(LVariable) then
      begin
        Result := StringReplace(Result, AVariablePrefix + LVariable +
          AVariableSuffix, LVariableValue, [rfReplaceAll, rfIgnoreCase]);
      end;
    end;

  finally
    FreeAndNil(LVariables);
  end;
end;

class procedure TBobCommon.GetFileList(ASource: string; AFileList: TStrings;
  ARecurse: Boolean);
var
  LFileMasks: TLZToken;
  LIdx: integer;
  LFileMask, LFolder: string;
begin
  LFileMasks := TLZToken.Create;
  try
    AFileList.Clear;
    LFileMasks.Seperator := ';';
    LFileMasks.Source := ASource;
    for LIdx := 0 to Pred(LFileMasks.Count) do
    begin
      LFileMask := TBobCommon.ProcessEnvironmentVariables
        (LFileMasks.Tokens[LIdx]);
      LFolder := ExtractFilePath(LFileMask);
      LFileMask := StringReplace(LFileMask, LFolder, '',
        [rfReplaceAll, rfIgnoreCase]);
      TLZFile.QuickFileSearch(LFolder, LFileMask, ARecurse, AFileList);
    end;
  finally
    FreeAndNil(LFileMasks);
  end;
end;

class function TBobCommon.GetLogFileName(AApplicationName: string): TFileName;
begin
  Result := TLZFile.GetKnownFolderPath(FOLDERID_LocalAppData);
  Result := IncludeTrailingPathDelimiter(Result + 'bobbuilder');
  if TLZFile.CheckDirectoryExists(Result, true) then
  begin
    Result := Result + AApplicationName + '.log';
  end
  else
  begin
    Result := TLZFile.GetTempFile('bob');
  end;
end;

class function TBobCommon.GetBuilderFile(out AErrorMessage: string): TFileName;
var
  LParamValue: string;
  LParamIndex: integer;
  LFoundFiles: TStringList;
  LCurrentDir: string;
begin
  Result := '';
  AErrorMessage := '';

  // Check each parameter for a .builder file
  if ParamCount > 0 then
  begin
    for LParamIndex := 1 to ParamCount do
    begin
      LParamValue := ParamStr(LParamIndex);
      if FileExists(LParamValue) and SameText(ExtractFileExt(LParamValue),
        '.builder') then
      begin
        Result := LParamValue;
        Exit;
      end;
    end;
  end;

  // If no .builder file found in parameters, search current directory
  LCurrentDir := GetCurrentDir;
  LFoundFiles := TStringList.Create;
  try
    TLZFile.QuickFileSearch(IncludeTrailingPathDelimiter(LCurrentDir),
      '*.builder', False, LFoundFiles);

    if LFoundFiles.Count = 0 then
    begin
      AErrorMessage := 'No .builder file found in current directory';
    end
    else if LFoundFiles.Count > 1 then
    begin
      AErrorMessage := 'Multiple .builder files found in current directory:';
      for LParamIndex := 0 to LFoundFiles.Count - 1 do
      begin
        AErrorMessage := AErrorMessage + #13#10 + '  ' +
          ExtractFileName(LFoundFiles[LParamIndex]);
      end;
      AErrorMessage := AErrorMessage + #13#10 +
        'Please specify which .builder file to use';
    end
    else
    begin
      Result := LFoundFiles[0];
    end;
  finally
    LFoundFiles.Free;
  end;
end;

end.
