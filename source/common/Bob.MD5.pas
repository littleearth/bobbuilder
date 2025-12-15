unit Bob.MD5;

interface

uses Classes, Windows, SysUtils, Lazy.Utils, Lazy.Types, Bob.Common;

type
  THashMode = (hmGenerate, hmCompare);
  THashType = (htMD5, htSHA256, htSHA512);

  TMD5 = class(TLZObject)
  protected
    function GetHashMode(AVersionCompareMode: string): THashMode;
    function GetHashType(AHashTypeString: string): THashType;
    function GetHashFileName(
      AFileName: TFileName;
      AHashType: THashType): TFileName;
    function GetMD5FileName(AFileName: TFileName): TFileName;
    // Deprecated, use GetHashFileName
    function GenerateHash(
      AFileName: TFileName;
      AHashType: THashType): string;
  public
    function LoadMD5(AFileName: TFileName): string;
    function SaveMD5(
      AFileName: TFileName;
      AMD5: string): Boolean;
    function GenerateMD5(AFileName: TFileName): string; overload;
    function GenerateMD5(
      var AMessage: string;
      ASource: string;
      ARecurse: Boolean): Boolean; overload;
    function GenerateMD5(
      var AMessage: string;
      ASource: string;
      ARecurse: Boolean;
      AHashType: THashType): Boolean; overload;
    function CompareMD5(AFileName: TFileName): Boolean; overload;
    function CompareMD5(
      var AMessage: string;
      ASource: string;
      ARecurse: Boolean): Boolean; overload;
    function CompareMD5(
      var AMessage: string;
      ASource: string;
      ARecurse: Boolean;
      AHashType: THashType): Boolean; overload;
  end;

  TMD5Console = class(TMD5)
  protected
    function GetUsage: string;
    function CheckCommandParameter(
      var AMessage: string;
      var AMode: THashMode;
      var ASource: string;
      var ARecurse: Boolean;
      var AHashType: THashType): Boolean;
  public
    function Execute(var AMessage: string): Boolean;
  end;

implementation

uses
  IdHashMessageDigest, idHash, System.Math, System.DateUtils, System.Hash;

function TMD5.GetHashMode(AVersionCompareMode: string): THashMode;
begin
  Result := hmGenerate;
  if SameText(AVersionCompareMode, 'COMPARE') then
    Result := hmCompare;
end;

function TMD5.GetHashType(AHashTypeString: string): THashType;
begin
  Result := htMD5; // Default for backwards compatibility
  if SameText(AHashTypeString, 'SHA256') then
    Result := htSHA256;
  if SameText(AHashTypeString, 'SHA512') then
    Result := htSHA512;
  if SameText(AHashTypeString, 'MD5') then
    Result := htMD5;
end;

function TMD5.GetHashFileName(
  AFileName: TFileName;
  AHashType: THashType): TFileName;
begin
  case AHashType of
    htSHA256:
      Result := ChangeFileExt(AFileName, '.sha256');
    htSHA512:
      Result := ChangeFileExt(AFileName, '.sha512');
  else
    Result := ChangeFileExt(AFileName, '.md5');
  end;
end;

function TMD5.LoadMD5(AFileName: TFileName): string;
var
  LFile: TStringList;
begin
  Result := '';
  if FileExists(AFileName) then
  begin
    LFile := TStringList.Create;
    try
      LFile.LoadFromFile(AFileName);
      Result := Trim(LFile.Text);
    finally
      FreeAndNil(LFile);
    end;
  end;
end;

function TMD5.SaveMD5(
  AFileName: TFileName;
  AMD5: string): Boolean;
var
  LFile: TStringList;
begin
  if FileExists(AFileName) then
    DeleteFile(AFileName);
  if not TLZString.IsEmptyString(AMD5) then
  begin
    LFile := TStringList.Create;
    try
      LFile.Add(AMD5);
      LFile.SaveToFile(AFileName);
    finally
      FreeAndNil(LFile);
    end;
  end;
  Result := FileExists(AFileName);
end;

function TMD5.GenerateHash(
  AFileName: TFileName;
  AHashType: THashType): string;
var
  LIdMD5: TIdHashMessageDigest5;
  LFileStream: TFileStream;
begin
  case AHashType of
    htSHA256:
      begin
        LFileStream := TFileStream.Create(AFileName, fmOpenRead OR
          fmShareDenyWrite);
        try
          Result := THashSHA2.GetHashString(LFileStream, SHA256);
        finally
          LFileStream.Free;
        end;
      end;
    htSHA512:
      begin
        LFileStream := TFileStream.Create(AFileName, fmOpenRead OR
          fmShareDenyWrite);
        try
          Result := THashSHA2.GetHashString(LFileStream, SHA512);
        finally
          LFileStream.Free;
        end;
      end;
  else // htMD5
    begin
      LIdMD5 := TIdHashMessageDigest5.Create;
      LFileStream := TFileStream.Create(AFileName, fmOpenRead OR
        fmShareDenyWrite);
      try
        Result := LIdMD5.HashBytesAsHex(LIdMD5.HashStream(LFileStream));
      finally
        LFileStream.Free;
        LIdMD5.Free;
      end;
    end;
  end;
end;

function TMD5.GetMD5FileName(AFileName: TFileName): TFileName;
begin
  Result := ChangeFileExt(AFileName, '.md5');
end;

function TMD5.GenerateMD5(
  var AMessage: string;
  ASource: string;
  ARecurse: Boolean): Boolean;
begin
  Result := GenerateMD5(AMessage, ASource, ARecurse, htMD5);
end;

function TMD5.GenerateMD5(
  var AMessage: string;
  ASource: string;
  ARecurse: Boolean;
  AHashType: THashType): Boolean;
var
  LFiles: TStringList;
  LFileName, LHashFileName: TFileName;
  LHash: string;
  LSuccess: Boolean;
begin
  Result := True;
  LFiles := TStringList.Create;
  try
    TBobCommon.GetFileList(ASource, LFiles, ARecurse);
    for LFileName in LFiles do
    begin
      LHashFileName := GetHashFileName(LFileName, AHashType);
      LHash := GenerateHash(LFileName, AHashType);
      if not TLZString.IsEmptyString(AMessage) then
        AMessage := AMessage + #13#10;
      AMessage := AMessage + Format('%s = %s', [LFileName, LHash]);
      LSuccess := SaveMD5(LHashFileName, LHash);
      if not LSuccess then
        Result := False;
    end;
  finally
    FreeAndNil(LFiles);
  end;
end;

function TMD5.GenerateMD5(AFileName: TFileName): string;
var
  LIdMD5: TIdHashMessageDigest5;
  LFileStream: TFileStream;
begin
  LIdMD5 := TIdHashMessageDigest5.Create;
  LFileStream := TFileStream.Create(AFileName, fmOpenRead OR fmShareDenyWrite);
  try
    Result := LIdMD5.HashBytesAsHex(LIdMD5.HashStream(LFileStream));
  finally
    LFileStream.Free;
    LIdMD5.Free;
  end;
end;

function TMD5.CompareMD5(AFileName: TFileName): Boolean;
var
  LMD5FileName: TFileName;
  LMD51, LMD52: string;
begin
  Result := False;
  LMD5FileName := GetMD5FileName(AFileName);
  LMD51 := LoadMD5(LMD5FileName);
  if not TLZString.IsEmptyString(LMD51) then
  begin
    LMD52 := GenerateMD5(AFileName);
    Result := SameText(LMD51, LMD52);
  end;
end;

function TMD5.CompareMD5(
  var AMessage: string;
  ASource: string;
  ARecurse: Boolean): Boolean;
begin
  Result := CompareMD5(AMessage, ASource, ARecurse, htMD5);
end;

function TMD5.CompareMD5(
  var AMessage: string;
  ASource: string;
  ARecurse: Boolean;
  AHashType: THashType): Boolean;
var
  LFiles: TStringList;
  LFileName, LHashFileName: TFileName;
  LHash1, LHash2: string;
  LCompare: Boolean;
begin
  Result := True;
  LFiles := TStringList.Create;
  try
    TBobCommon.GetFileList(ASource, LFiles, ARecurse);
    for LFileName in LFiles do
    begin
      LHashFileName := GetHashFileName(LFileName, AHashType);
      LHash1 := LoadMD5(LHashFileName);
      if not TLZString.IsEmptyString(LHash1) then
      begin
        LHash2 := GenerateHash(LFileName, AHashType);
        LCompare := SameText(LHash1, LHash2);
      end
      else
      begin
        LCompare := False;
      end;

      if not LCompare then
      begin
        Result := False;
      end;
      if not TLZString.IsEmptyString(AMessage) then
        AMessage := AMessage + #13#10;
      AMessage := AMessage + Format('%s = %s',
        [LFileName, BoolToStr(LCompare, True)]);
    end;
  finally
    FreeAndNil(LFiles);
  end;
end;

{ TMD5Console }

function TMD5Console.CheckCommandParameter(
  var AMessage: string;
  var AMode: THashMode;
  var ASource: string;
  var ARecurse: Boolean;
  var AHashType: THashType): Boolean;
var
  LValue: string;
begin
  Result := True;
  AMessage := '';
  ARecurse := True;
  AHashType := htMD5; // Default for backwards compatibility

  if TLZSystem.GetApplicationParameters('/RECURSE', LValue) then
  begin
    ARecurse := StrToBoolDef(LValue, ARecurse);
  end;

  if TLZSystem.GetApplicationParameters('/HASHTYPE', LValue) then
  begin
    AHashType := GetHashType(LValue);
  end;

  if Result then
  begin
    if TLZSystem.GetApplicationParameters('/MODE', LValue) then
    begin
      AMode := GetHashMode(LValue);
    end
    else
    begin
      Result := False;
      AMessage := GetUsage;
    end;
  end;
  if Result then
  begin
    if not TLZSystem.GetApplicationParameters('/SOURCE', ASource) then
    begin
      Result := False;
      AMessage := GetUsage;
    end;
  end;

end;

function TMD5Console.Execute(var AMessage: string): Boolean;
var
  LMode: THashMode;
  LSource: string;
  LRecurse: Boolean;
  LHashType: THashType;
begin
  Result := False;
  if CheckCommandParameter(AMessage, LMode, LSource, LRecurse, LHashType) then
  begin
    case LMode of
      hmGenerate:
        begin
          Result := GenerateMD5(AMessage, LSource, LRecurse, LHashType);
        end;
      hmCompare:
        begin
          Result := CompareMD5(AMessage, LSource, LRecurse, LHashType);
        end;
    end;
  end;
end;

function TMD5Console.GetUsage: string;
var
  LUsage: TStringList;
begin
  LUsage := TStringList.Create;
  try
    LUsage.Add('bobmd5 - generate or compare hash checksums for files');
    LUsage.Add('');
    LUsage.Add('NOTE: %ERRORLEVEL% will be set on failures');
    LUsage.Add('');
    LUsage.Add('Usage instruction:');
    LUsage.Add('');
    LUsage.Add
      ('/MODE:{mode} /SOURCE:{filename;filenames} /HASHTYPE:{hashtype} /RECURSE:{true|false}');
    LUsage.Add('');
    LUsage.Add('- /MODE (default GENERATE).');
    LUsage.Add('   Options: GENERATE, COMPARE');
    LUsage.Add('');
    LUsage.Add('- /HASHTYPE (default MD5).');
    LUsage.Add('   Options: MD5, SHA256, SHA512');
    LUsage.Add('   Note: MD5 is default for backwards compatibility');
    LUsage.Add('');
    LUsage.Add('- /SOURCE file or file names (seperator ;)');
    LUsage.Add('- /RECURSE - search source rescursive (default: true)');
    LUsage.Add('');
    LUsage.Add('Hash files created:');
    LUsage.Add('   MD5:    filename.md5');
    LUsage.Add('   SHA256: filename.sha256');
    LUsage.Add('   SHA512: filename.sha512');
    LUsage.Add('');
    LUsage.Add('Examples:');
    LUsage.Add('   /MODE:GENERATE /SOURCE:"C:\Build\*.exe" /RECURSE');
    LUsage.Add('   /MODE:GENERATE /SOURCE:"C:\Build\*.exe" /HASHTYPE:SHA256');
    LUsage.Add('   /MODE:COMPARE /SOURCE:"C:\Build\*.exe" /HASHTYPE:SHA512');
    LUsage.Add('   /MODE:COMPARE /SOURCE:"C:\Build\*.exe" (uses MD5)');
    Result := LUsage.Text;
  finally
    FreeAndNil(LUsage);
  end;
end;

end.
