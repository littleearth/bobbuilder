// JCL_DEBUG_EXPERT_GENERATEJDBG ON
// JCL_DEBUG_EXPERT_INSERTJDBG OFF
// JCL_DEBUG_EXPERT_DELETEMAPFILE OFF
program bobmaptojdbg;

{$APPTYPE CONSOLE}
{$R *.res}
{$R 'version.res' 'version.rc'}

uses
  FastMM4,
  Bob.StackTrace,
  System.SysUtils,
  System.Classes,
  JclDebug,
  Bob.Common,
  Bob.Console,
  Lazy.Types,
  Lazy.Utils.Windows;

type
  TMapToJdbgApplication = class(TBobConsoleApplication)
  private
    function MapFileToJdbgFile(const AMapFileName: TFileName;
      var AJDbgFileName: TFileName): Boolean;
  protected
    function DoExecute: Integer; override;
  end;

function TMapToJdbgApplication.MapFileToJdbgFile(const AMapFileName: TFileName;
  var AJDbgFileName: TFileName): Boolean;
var
  LGenerator: TJclBinDebugGenerator;
begin
  AJDbgFileName := ChangeFileExt(AMapFileName, JclDbgFileExtension);
  LGenerator := TJclBinDebugGenerator.Create(AMapFileName, 0);
  try
    Result := (LGenerator.DataStream.Size > 0) and LGenerator.CalculateCheckSum;
    if Result then
      LGenerator.DataStream.SaveToFile(AJDbgFileName);
  finally
    LGenerator.Free;
  end;
end;

function TMapToJdbgApplication.DoExecute: Integer;
var
  LFileName: string;
  LRecursive: Boolean;
  LCleanup: Boolean;
  LFailOnEmpty: Boolean;
  LFileList: TStringList;
  LJDbgFileName: TFileName;
begin
  Result := Integer(becSuccess);
  
  if ParamCount > 0 then
  begin
    LFileList := TStringList.Create;
    try
      LFileName := ParamStr(1);
      LRecursive := not SameText(ParamStr(2), 'false');
      LCleanup := not SameText(ParamStr(3), 'false');
      LFailOnEmpty := SameText(ParamStr(4), 'true');
      
      Log(Format('MAPtoJDBG, Source "%s", Recursive: %s, Cleanup: %s, Fail on empty: %s',
        [LFileName, BoolToStr(LRecursive, True), BoolToStr(LCleanup, True),
        BoolToStr(LFailOnEmpty, True)]));
        
      if DirectoryExists(LFileName) then
      begin
        Log(Format('Searching "%s", Recursive: %s',
          [LFileName, BoolToStr(LRecursive, True)]));
        TLZFile.QuickFileSearch(LFileName, '*.map', True, LFileList);
      end
      else if FileExists(LFileName) then
      begin
        LFileList.Add(LFileName);
      end;

      if LFileList.Count > 0 then
      begin
        for LFileName in LFileList do
        begin
          Log(Format('Processing "%s"', [LFileName]));
          if MapFileToJdbgFile(LFileName, LJDbgFileName) then
          begin
            Log(Format('Success "%s"', [LJDbgFileName]));
            if LCleanup then
            begin
              Log(Format('Removing "%s"', [LFileName]));
              if not DeleteFile(LFileName) then
              begin
                Log(Format('ERROR: "%s" failed to cleanup.', [LFileName]));
              end;
            end;
          end
          else
          begin
            Log(Format('ERROR: "%s" failed to generate JDBG', [LFileName]));
            Result := Integer(becGeneralError);
          end;
        end;
      end
      else
      begin
        if LFailOnEmpty then
        begin
          Log(Format('ERROR: "%s" does not exists or contains no MAP files',
            [LFileName]));
          Result := Integer(becNoFiles);
        end
        else
        begin
          Log(Format('"%s" does not exists or contains no MAP files',
            [LFileName]));
        end;
      end;
    finally
      LFileList.Free;
    end;
  end
  else
  begin
    Log('ERROR: No parameters specified');
    Result := Integer(becNoParameters);
  end;
end;

begin
  with TMapToJdbgApplication.Create('bobmaptojdbg') do
  begin
    try
      ExitCode := Execute;
    finally
      Free;
    end;
  end;
end.
