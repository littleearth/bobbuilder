unit Bob.MD5;

interface

uses Classes, Windows, SysUtils, Lazy.Utils, Lazy.Types, Bob.Common;

type
  TMD5Mode = (md5Generate, md5Compare);

  TMD5 = class(TLZObject)
  protected
    function GetMD5Mode(AVersionCompareMode: string): TMD5Mode;
    function GetMD5FileName(AFileName: TFileName): TFileName;
  public
    function LoadMD5(AFileName: TFileName): string;
    function SaveMD5(AFileName: TFileName; AMD5: string): Boolean;
    function GenerateMD5(AFileName: TFileName): string; overload;
    function GenerateMD5(var AMessage: string; ASource: string;
      ARecurse: Boolean): Boolean; overload;
    function CompareMD5(AFileName: TFileName): Boolean; overload;
    function CompareMD5(var AMessage: string; ASource: string;
      ARecurse: Boolean): Boolean; overload;
  end;

  TMD5Console = class(TMD5)
  protected
    function GetUsage: string;
    function CheckCommandParameter(var AMessage: string; var AMode: TMD5Mode;
      var ASource: string; var ARecurse: Boolean): Boolean;
  public
    function Execute(var AMessage: string): Boolean;
  end;

implementation

uses
  IdHashMessageDigest, idHash, System.Math, System.DateUtils;

function TMD5.GetMD5Mode(AVersionCompareMode: string): TMD5Mode;
begin
  Result := md5Generate;
  if SameText(AVersionCompareMode, 'COMPARE') then
    Result := md5Compare;
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

function TMD5.SaveMD5(AFileName: TFileName; AMD5: string): Boolean;
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

function TMD5.GetMD5FileName(AFileName: TFileName): TFileName;
begin
  Result := ChangeFileExt(AFileName, '.md5');
end;

function TMD5.GenerateMD5(var AMessage: string; ASource: string;
  ARecurse: Boolean): Boolean;
var
  LFiles: TStringList;
  LFileName, LMD5FileName: TFileName;
  LMD5: string;
  LSuccess: Boolean;
begin
  Result := True;
  LFiles := TStringList.Create;
  try
    TBobCommon.GetFileList(ASource, LFiles, ARecurse);
    for LFileName in LFiles do
    begin
      LMD5FileName := GetMD5FileName(LFileName);
      LMD5 := GenerateMD5(LFileName);
      if not TLZString.IsEmptyString(AMessage) then
        AMessage := AMessage + #13#10;
      AMessage := AMessage + Format('%s = %s', [LFileName, LMD5]);
      LSuccess := SaveMD5(LMD5FileName, LMD5);
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

function TMD5.CompareMD5(var AMessage: string; ASource: string;
  ARecurse: Boolean): Boolean;
var
  LFiles: TStringList;
  LFileName: TFileName;
  LCompare: Boolean;
begin
  Result := True;
  LFiles := TStringList.Create;
  try
    TBobCommon.GetFileList(ASource, LFiles, ARecurse);
    for LFileName in LFiles do
    begin
      LCompare := CompareMD5(LFileName);
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

function TMD5Console.CheckCommandParameter(var AMessage: string;
  var AMode: TMD5Mode; var ASource: string; var ARecurse: Boolean): Boolean;
var
  LValue: string;
begin
  Result := True;
  AMessage := '';
  ARecurse := True;
  if TLZSystem.GetApplicationParameters('/RECURSE', LValue) then
  begin
    ARecurse := StrToBoolDef(LValue, ARecurse);
  end;

  if Result then
  begin
    if TLZSystem.GetApplicationParameters('/MODE', LValue) then
    begin
      AMode := GetMD5Mode(LValue);
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
  LMode: TMD5Mode;
  LSource: string;
  LRecurse: Boolean;
begin
  Result := False;
  if CheckCommandParameter(AMessage, LMode, LSource, LRecurse) then
  begin
    case LMode of
      md5Generate:
        begin
          Result := GenerateMD5(AMessage, LSource, LRecurse);
        end;
      md5Compare:
        begin
          Result := CompareMD5(AMessage, LSource, LRecurse);
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
    LUsage.Add('bobmd5 - generate or compare a file list');
    LUsage.Add('');
    LUsage.Add('NOTE: %ERRORLEVEL% will be set on failures');
    LUsage.Add('');
    LUsage.Add('Usage instruction:');
    LUsage.Add('');
    LUsage.Add('/MODE:{mode} /SOURCE:{filename;filenames} /RECURSE:{true|false}');
    LUsage.Add('');
    LUsage.Add('- /MODE (default GENERATE).');
    LUsage.Add('   Options: GENERATE, COMPARE');
    LUsage.Add('');
    LUsage.Add('- /SOURCE file or file names (seperator ;)');
    LUsage.Add('- /RECURSE - search source rescursive (default: true)');
    LUsage.Add('');
    LUsage.Add('Examples:');
    LUsage.Add('   /MODE:GENERATE /SOURCE:"C:\Build\*.exe" /RECURSE');
    LUsage.Add('   /MODE:COMPARE /SOURCE:"C:\Build\*.exe"');
    Result := LUsage.Text;
  finally
    FreeAndNil(LUsage);
  end;
end;

end.
