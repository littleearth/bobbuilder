unit Bob.Compare;

interface

uses Classes, Windows, SysUtils, Lazy.Utils, Lazy.Types;

type
  TCompareMode = (vcString, vcInteger, vcFloat, vcDate, vcTime, vcDateTime,
    vcVersionString, vcVersionFile, vcMD5String, vcMD5File, vcGetMD5,
    vcGetVersionFile, vcGetVersionProduct, vcSHA256String, vcSHA256File,
    vcGetSHA256, vcSHA512String, vcSHA512File, vcGetSHA512);

  TCompare = class(TLZObject)
  private
  protected
    function GetCompareMode(AVersionCompareMode: string): TCompareMode;
  public
    function CompareVersion(
      AVersion1: string;
      AVersion2: string): integer;
    function CompareInteger(AValue1, AValue2: string): integer;
    function CompareFloat(AValue1, AValue2: string): integer;
    function CompareDate(AValue1, AValue2: string): integer;
    function CompareTime(AValue1, AValue2: string): integer;
    function CompareDateTime(AValue1, AValue2: string): integer;
    function GetFileVersion(AFilename: TFileName): string;
    function GetFileProductVersion(AFilename: TFileName): string;
    function GenerateMD5(AFilename: TFileName): string;
    function GenerateSHA256(AFilename: TFileName): string;
    function GenerateSHA512(AFilename: TFileName): string;
  end;

  TCompareConsole = class(TCompare)
  protected
    function GetUsage: string;
    function CheckCommandParameter(
      var AMessage: string;
      var AMode: TCompareMode;
      var AValue1: string;
      var AValue2: string): boolean;
  public
    function Execute(var AMessage: string): integer;
  end;

implementation

uses
  System.Math, System.DateUtils, IdHashMessageDigest, idHash, System.Hash,
  WinApi.FileVersionInformation;

{ TCompare }

function TCompare.GetFileVersion(AFilename: TFileName): string;
var
  LFileVersionInformation: TLZFileVersionInformation;
begin
  LFileVersionInformation := TLZFileVersionInformation.Create;
  try
    LFileVersionInformation.FileName := AFilename;
    Result := LFileVersionInformation.FileVersion;
  finally
    FreeAndNil(LFileVersionInformation);
  end;
end;

function TCompare.GetFileProductVersion(AFilename: TFileName): string;
var
  LFileVersionInformation: TLZFileVersionInformation;
begin
  LFileVersionInformation := TLZFileVersionInformation.Create;
  try
    LFileVersionInformation.FileName := AFilename;
    Result := LFileVersionInformation.ProductVersion;
  finally
    FreeAndNil(LFileVersionInformation);
  end;
end;

function TCompare.GetCompareMode(AVersionCompareMode: string): TCompareMode;
begin
  Result := vcString;
  if SameText(AVersionCompareMode, 'INTEGER') then
    Result := vcInteger;
  if SameText(AVersionCompareMode, 'FLOAT') then
    Result := vcFloat;
  if SameText(AVersionCompareMode, 'DATE') then
    Result := vcDate;
  if SameText(AVersionCompareMode, 'TIME') then
    Result := vcTime;
  if SameText(AVersionCompareMode, 'DATETIME') then
    Result := vcDateTime;
  if SameText(AVersionCompareMode, 'VERSIONFILE') then
    Result := vcVersionFile;
  if SameText(AVersionCompareMode, 'VERSIONSTRING') then
    Result := vcVersionString;
  if SameText(AVersionCompareMode, 'MD5STRING') then
    Result := vcMD5String;
  if SameText(AVersionCompareMode, 'MD5FILE') then
    Result := vcMD5File;
  if SameText(AVersionCompareMode, 'GETMD5') then
    Result := vcGetMD5;
  if SameText(AVersionCompareMode, 'GETVERSIONFILE') then
    Result := vcGetVersionFile;
  if SameText(AVersionCompareMode, 'GETVERSIONPRODUCT') then
    Result := vcGetVersionProduct;
  if SameText(AVersionCompareMode, 'SHA256STRING') then
    Result := vcSHA256String;
  if SameText(AVersionCompareMode, 'SHA256FILE') then
    Result := vcSHA256File;
  if SameText(AVersionCompareMode, 'GETSHA256') then
    Result := vcGetSHA256;
  if SameText(AVersionCompareMode, 'SHA512STRING') then
    Result := vcSHA512String;
  if SameText(AVersionCompareMode, 'SHA512FILE') then
    Result := vcSHA512File;
  if SameText(AVersionCompareMode, 'GETSHA512') then
    Result := vcGetSHA512
end;

function TCompare.CompareDate(AValue1, AValue2: string): integer;
var
  LValue1, LValue2: TDate;
begin
  LValue1 := TLZDateTime.StringToDate(AValue1);
  LValue2 := TLZDateTime.StringToDate(AValue2);
  Result := System.DateUtils.CompareDate(LValue1, LValue2);
end;

function TCompare.CompareDateTime(AValue1, AValue2: string): integer;
var
  LValue1, LValue2: TDateTime;
begin
  LValue1 := TLZDateTime.StringToDateTime(AValue1);
  LValue2 := TLZDateTime.StringToDateTime(AValue2);
  Result := System.DateUtils.CompareDateTime(LValue1, LValue2);
end;

function TCompare.CompareFloat(AValue1, AValue2: string): integer;
var
  LValue1, LValue2: Double;
begin
  LValue1 := StrToFloat(AValue1);
  LValue2 := StrToFloat(AValue2);
  Result := CompareValue(LValue1, LValue2);
end;

function TCompare.CompareInteger(AValue1, AValue2: string): integer;
var
  LValue1, LValue2: integer;
begin
  LValue1 := StrToInt(AValue1);
  LValue2 := StrToInt(AValue2);
  Result := CompareValue(LValue1, LValue2);
end;

function TCompare.CompareTime(AValue1, AValue2: string): integer;
var
  LValue1, LValue2: TTime;
begin
  LValue1 := TLZDateTime.StringToTime(AValue1);
  LValue2 := TLZDateTime.StringToTime(AValue2);
  Result := System.DateUtils.CompareTime(LValue1, LValue2);
end;

function TCompare.CompareVersion(AVersion1, AVersion2: string): integer;
var
  LVersionDetails: TApplicationVersionDetails;
  LVersion1, LVersion2: string;
  LVersion1Current, LVersion2Current: boolean;
begin
  Result := -9999;
  LVersion1 := AVersion1;
  LVersion2 := AVersion2;
  Log('Version1: ' + LVersion1 + ', Version2: ' + LVersion2);
  LVersionDetails := TApplicationVersionDetails.Create(nil);
  try

    LVersionDetails.Version := LVersion1;
    LVersion1 := LVersionDetails.AsString;

    LVersionDetails.Reset;
    LVersionDetails.Version := LVersion2;
    LVersion2 := LVersionDetails.AsString;

    LVersionDetails.Reset;
    LVersionDetails.Version := LVersion2;
    LVersion2Current := LVersionDetails.IsCurrent(LVersion1);

    LVersionDetails.Reset;
    LVersionDetails.Version := LVersion1;
    LVersion1Current := LVersionDetails.IsCurrent(LVersion2);

    Log('Version1: ' + LVersion1 + ', Version2: ' + LVersion2);

    if LVersion1Current and LVersion2Current then
    begin
      Result := 0;
    end
    else
    begin
      if LVersion1Current and (not LVersion2Current) then
      begin
        Result := -1;
      end;
      if LVersion2Current and (not LVersion1Current) then
      begin
        Result := 1;
      end;
    end;
  finally
    FreeAndNil(LVersionDetails);
  end;
end;

function TCompare.GenerateMD5(AFilename: TFileName): string;
var
  LIdMD5: TIdHashMessageDigest5;
  LFileStream: TFileStream;
begin
  LIdMD5 := TIdHashMessageDigest5.Create;
  LFileStream := TFileStream.Create(AFilename, fmOpenRead OR fmShareDenyWrite);
  try
    Result := LIdMD5.HashBytesAsHex(LIdMD5.HashStream(LFileStream));
  finally
    LFileStream.Free;
    LIdMD5.Free;
  end;
end;

function TCompare.GenerateSHA256(AFilename: TFileName): string;
var
  LFileStream: TFileStream;
begin
  LFileStream := TFileStream.Create(AFilename, fmOpenRead OR fmShareDenyWrite);
  try
    Result := THashSHA2.GetHashString(LFileStream, SHA256);
  finally
    LFileStream.Free;
  end;
end;

function TCompare.GenerateSHA512(AFilename: TFileName): string;
var
  LFileStream: TFileStream;
begin
  LFileStream := TFileStream.Create(AFilename, fmOpenRead OR fmShareDenyWrite);
  try
    Result := THashSHA2.GetHashString(LFileStream, SHA512);
  finally
    LFileStream.Free;
  end;
end;

{ TCompareConsole }

function TCompareConsole.CheckCommandParameter(
  var AMessage: string;
  var AMode: TCompareMode;
  var AValue1, AValue2: string): boolean;
var
  LValue: string;
begin
  Result := True;
  AMessage := '';
  if Result then
  begin
    if TLZSystem.GetApplicationParameters('/MODE', LValue) then
    begin
      AMode := GetCompareMode(LValue);
    end
    else
    begin
      Result := False;
      AMessage := GetUsage;
    end;
  end;
  if Result then
  begin
    if not TLZSystem.GetApplicationParameters('/VALUE1', AValue1) then
    begin
      Result := False;
      AMessage := GetUsage;
    end;
  end;
  if Result then
  begin
    if not TLZSystem.GetApplicationParameters('/VALUE2', AValue2) then
    begin
      case AMode of
        vcGetMD5, vcGetVersionProduct, vcGetVersionFile, vcGetSHA256,
          vcGetSHA512:
          begin
            Result := True;
          end;
      else
        begin
          Result := False;
          AMessage := GetUsage;
        end;
      end;
    end;
  end;
end;

function TCompareConsole.Execute(var AMessage: string): integer;
var
  LMode: TCompareMode;
  LValue1, LValue2: string;
begin
  Result := -9999;
  if CheckCommandParameter(AMessage, LMode, LValue1, LValue2) then
  begin
    case LMode of
      vcVersionString:
        begin
          Result := CompareVersion(LValue1, LValue2);
          AMessage := Format('Compare version: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcVersionFile:
        begin
          LValue1 := GetFileVersion(LValue1);
          if TLZFile.IsValidFileName(LValue2) and FileExists(LValue2) then
          begin
            LValue2 := GetFileVersion(LValue2);
          end;
          Result := CompareVersion(LValue1, LValue2);
          AMessage := Format('Compare file: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcMD5File:
        begin
          LValue1 := GenerateMD5(LValue1);
          if TLZFile.IsValidFileName(LValue2) and FileExists(LValue2) then
          begin
            LValue2 := GenerateMD5(LValue2);
          end;
          Result := CompareStr(LValue1, LValue2);
          AMessage := Format('Compare MD5: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcMD5String, vcString:
        begin
          Result := CompareStr(LValue1, LValue2);
          AMessage := Format('Compare string: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcInteger:
        begin
          Result := CompareInteger(LValue1, LValue2);
          AMessage := Format('Compare integer: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcFloat:
        begin
          Result := CompareFloat(LValue1, LValue2);
          AMessage := Format('Compare float: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcDate:
        begin
          Result := CompareDate(LValue1, LValue2);
          AMessage := Format('Compare date: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcTime:
        begin
          Result := CompareTime(LValue1, LValue2);
          AMessage := Format('Compare time: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcDateTime:
        begin
          Result := CompareDateTime(LValue1, LValue2);
          AMessage := Format('Compare date and time: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcGetMD5:
        begin
          AMessage := GenerateMD5(LValue1);
          Result := 0;
        end;
      vcGetVersionFile:
        begin
          AMessage := GetFileVersion(LValue1);
          Result := 0;
        end;
      vcGetVersionProduct:
        begin
          AMessage := GetFileProductVersion(LValue1);
          Result := 0;
        end;
      vcSHA256File:
        begin
          LValue1 := GenerateSHA256(LValue1);
          if TLZFile.IsValidFileName(LValue2) and FileExists(LValue2) then
          begin
            LValue2 := GenerateSHA256(LValue2);
          end;
          Result := CompareStr(LValue1, LValue2);
          AMessage := Format('Compare SHA256: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcSHA256String:
        begin
          Result := CompareStr(LValue1, LValue2);
          AMessage := Format('Compare SHA256 string: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcGetSHA256:
        begin
          AMessage := GenerateSHA256(LValue1);
          Result := 0;
        end;
      vcSHA512File:
        begin
          LValue1 := GenerateSHA512(LValue1);
          if TLZFile.IsValidFileName(LValue2) and FileExists(LValue2) then
          begin
            LValue2 := GenerateSHA512(LValue2);
          end;
          Result := CompareStr(LValue1, LValue2);
          AMessage := Format('Compare SHA512: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcSHA512String:
        begin
          Result := CompareStr(LValue1, LValue2);
          AMessage := Format('Compare SHA512 string: %s <> %s, Result: %d',
            [LValue1, LValue2, Result]);
        end;
      vcGetSHA512:
        begin
          AMessage := GenerateSHA512(LValue1);
          Result := 0;
        end;
    end;
  end;
end;

function TCompareConsole.GetUsage: string;
var
  LUsage: TStringList;
begin
  LUsage := TStringList.Create;
  try
    LUsage.Add('bobcompare will compare 2 values and with the comparison both');
    LUsage.Add('displayed and returned in %ERRORLEVEL%');
    LUsage.Add('');
    LUsage.Add('   0 if Value1 = Value2');
    LUsage.Add('   value less than 0 if Value1 < Value2');
    LUsage.Add('   value greater than 0 if Value1 > Value2');
    LUsage.Add('');
    LUsage.Add('Usage instruction:');
    LUsage.Add('');
    LUsage.Add('/MODE:{mode} /VALUE1:{filename|text} /VALUE2:{filename|text}');
    LUsage.Add('');
    LUsage.Add('- /MODE (default STRING).');
    LUsage.Add('   Options: STRING, INTEGER, FLOAT, DATE, TIME,');
    LUsage.Add('   DATETIME, VERSIONSTRING, VERSIONFILE, VERSIONGET,');
    LUsage.Add('   MD5STRING, MD5FILE, GETMD5, GETVERSIONFILE,');
    LUsage.Add('   GETVERSIONPRODUCT, SHA256STRING, SHA256FILE,');
    LUsage.Add('   GETSHA256, SHA512STRING, SHA512FILE, GETSHA512');
    LUsage.Add('');
    LUsage.Add('- /VALUE1 is required for all options');
    LUsage.Add('');
    LUsage.Add('- /VALUE2 is required for all except');
    LUsage.Add('   GETMD5, GETVERSIONFILE, GETVERSIONPRODUCT,');
    LUsage.Add('   GETSHA256, GETSHA512');
    LUsage.Add('');
    LUsage.Add('Examples:');
    LUsage.Add('   /MODE:VERSIONSTRING /VALUE1:1.1.1.0 /VALUE2:1.1.1');
    LUsage.Add('   /MODE:DATE /VALUE1:2022-01-25 /VALUE2:2022/01/25');
    LUsage.Add('   /MODE:GETMD5 /VALUE1:""C:\Windows\System32\cmd.exe"');
    Result := LUsage.Text;
  finally
    FreeAndNil(LUsage);
  end;
end;

end.
