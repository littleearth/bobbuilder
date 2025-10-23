unit Bob.WIXWrapper;

interface

uses Classes, Windows, SysUtils, Lazy.Utils.Windows, Lazy.Types, Bob.Common,
  WinApi.FileVersionInformation;

type
  TWIXWrapperParameters = class(TLZObject)
  private
    FInstallUILevel2: string;
    FUninstallUILevel2: string;
    FInstallUILevel5: string;
    FUninstallUILevel5: string;
    FInstallUILevel3_4: string;
    FUninstallUILevel3_4: string;
    FCompanyURL: string;
    FSupportURL: string;
    FProductURL: string;
    FUpgradeGUID: string;
    FKeepWSX: boolean;
    FKeepWIXOBJ: boolean;
    FInstallerVersion: string;
    procedure SetInstallUILevel2(const Value: string);
    procedure SetInstallUILevel3_4(const Value: string);
    procedure SetInstallUILevel5(const Value: string);
    procedure SetUninstallUILevel2(const Value: string);
    procedure SetUninstallUILevel3_4(const Value: string);
    procedure SetUninstallUILevel5(const Value: string);
    procedure SetCompanyURL(const Value: string);
    procedure SetSupportURL(const Value: string);
    procedure SetProductURL(const Value: string);
    procedure SetUpgradeGUID(const Value: string);
    procedure SetKeepWSX(const Value: boolean);
    procedure SetKeepWIXOBJ(const Value: boolean);
    procedure SetInstallerVersion(const Value: string);
  public
    constructor Create; reintroduce;
    property UninstallUILevel2: string read FUninstallUILevel2
      write SetUninstallUILevel2;
    property UninstallUILevel3_4: string read FUninstallUILevel3_4
      write SetUninstallUILevel3_4;
    property UninstallUILevel5: string read FUninstallUILevel5
      write SetUninstallUILevel5;
    property InstallUILevel2: string read FInstallUILevel2
      write SetInstallUILevel2;
    property InstallUILevel3_4: string read FInstallUILevel3_4
      write SetInstallUILevel3_4;
    property InstallUILevel5: string read FInstallUILevel5
      write SetInstallUILevel5;
    property CompanyURL: string read FCompanyURL write SetCompanyURL;
    property SupportURL: string read FSupportURL write SetSupportURL;
    property ProductURL: string read FProductURL write SetProductURL;
    property MSIVersion: string read FInstallerVersion
      write SetInstallerVersion;
    property UpgradeGUID: string read FUpgradeGUID write SetUpgradeGUID;
    property KeepWSX: boolean read FKeepWSX write SetKeepWSX;
    property KeepWIXOBJ: boolean read FKeepWIXOBJ write SetKeepWIXOBJ;
  end;

  TWIXWrapper = class(TLZObject)
  private
    FFileVersionInformation: TLZFileVersionInformation;
    FWIXWrapperParameters: TWIXWrapperParameters;
    function GetTemplateFileName: TFileName;
    function UpdateMSIVersion: boolean;
  protected
    procedure OutputUsage;
    function CheckCommandParameter: boolean;
  public
    constructor Create; reintroduce;
    destructor Destroy; override;
    function GetFileInformation(AFileName: TFileName): boolean;
    function GenerateTemplate(AEXEFileName: TFileName; AWIXTemplate: TFileName;
      AWIXFilename: TFileName): boolean;
    function BuildMSI(AWXSFileName: TFileName; AMSIFileName: TFileName)
      : boolean;
    function Execute(AWIXTemplate: TFileName; AEXEFileName: string;
      AMSIFileName: TFileName; ARecurse: boolean): boolean; overload;
    function Execute: boolean; overload;
  end;

implementation

{ TWIXWrapper }

function TWIXWrapper.BuildMSI(AWXSFileName: TFileName;
  AMSIFileName: TFileName): boolean;
var
  LCommand, LParams: string;
  LWXSObjectFilename: TFileName;
  LMSIFileName: TFileName;
  LCandle, LLight: TFileName;
  LShowCmd: Word;
  LExitCode: Word;
begin
  Result := True;
  LShowCmd := SW_HIDE; // SW_SHOWNORMAL;
  LWXSObjectFilename := ChangeFileExt(AWXSFileName, '.wixobj');
  LMSIFileName := TBobCommon.ProcessEnvironmentVariables(AMSIFileName);
  if FileExists(LMSIFileName) then
    DeleteFile(LMSIFileName);
  Log('MSI Filename: ' + LMSIFileName);
  try
    Log('Working directory: ' + ExtractFilePath(AWXSFileName));
    if not SetCurrentDir(ExtractFilePath(AWXSFileName)) then
    begin
      Error(Format('Failed to set working directory "%s"',
        [ExtractFilePath(AWXSFileName)]));
      Result := False;
    end;

    if Result then
    begin
      if not FileExists(AWXSFileName) then
      begin
        Error(Format('"%s" does not exist', [AWXSFileName]));
        Result := False;
      end;
    end;

    if Result then
    begin
      LCandle := TBobCommon.ProcessEnvironmentVariables('%WIX%bin\candle.exe');
      if not FileExists(LCandle) then
      begin
        Error(Format('Failed to locate candle.exe "%s"', [LCandle]));
        Log('Install WixToolkit (https://wixtoolset.org/)');
        Result := False;
      end;
    end;

    if Result then
    begin
      LLight := TBobCommon.ProcessEnvironmentVariables('%WIX%bin\light.exe');
      if not FileExists(LLight) then
      begin
        Error(Format('Failed to locate light.exe "%s"', [LLight]));
        Log('Install WixToolkit (https://wixtoolset.org/)');
        Result := False;
      end;
    end;

    if Result then
    begin
      if FileExists(LWXSObjectFilename) then
        DeleteFile(LWXSObjectFilename);
      LCommand := LCandle;
      LCommand := TBobCommon.ProcessEnvironmentVariables(LCommand);
      LParams := AnsiQuotedStr(AWXSFileName, '"');
      LParams := TBobCommon.ProcessEnvironmentVariables(LParams);
      Log('Processing: ' + ExtractFileName(AWXSFileName));

      Log(Format('BuildMSI: %s %s', [LCommand, LParams]));

      LExitCode := TLZFile.ExecuteAndWait(LCommand, LParams, LShowCmd);

      Log(Format('BuildMSI: %s %s (Exit code: %d) ', [LCommand, LParams,
        LExitCode]));

      if not FileExists(LWXSObjectFilename) then
      begin
        Error(Format('"%s" does not exist', [LWXSObjectFilename]));
        Result := False;
      end;
    end;

    if Result then
    begin
      if not TLZFile.CheckDirectoryExists(ExtractFilePath(LMSIFileName), True)
      then
      begin
        Error(Format('Folder "%s" does not exist',
          [ExtractFilePath(LMSIFileName)]));
        Result := False;
      end;
    end;

    if Result then
    begin
      LCommand := LLight;
      LCommand := TBobCommon.ProcessEnvironmentVariables(LCommand);
      LParams := '-ext WixUtilExtension.dll ' +
        AnsiQuotedStr(LWXSObjectFilename, '"') + ' -out ' +
        AnsiQuotedStr(LMSIFileName, '"');
      LParams := TBobCommon.ProcessEnvironmentVariables(LParams);

      Log('Building: ' + ExtractFileName(LMSIFileName));
      Log(Format('BuildMSI: %s %s', [LCommand, LParams]));

      LExitCode := TLZFile.ExecuteAndWait(LCommand, LParams, LShowCmd);

      Log(Format('BuildMSI: %s %s (Exit code: %d) ', [LCommand, LParams,
        LExitCode]));

      if FileExists(LMSIFileName) then
      begin
        Log(Format('Success: "%s"', [LMSIFileName]));
      end
      else
      begin
        Error(Format('Failed to generate MSI "%s"', [LMSIFileName]));
        Result := False;
      end;
    end;
  finally
    if not FWIXWrapperParameters.KeepWIXOBJ then
    begin
      if FileExists(LWXSObjectFilename) then
        DeleteFile(LWXSObjectFilename);
    end;
  end;
end;

function TWIXWrapper.CheckCommandParameter: boolean;
var
  LValue: string;
begin
  Result := True;
  if TLZSystem.GetApplicationParameters('/UPGRADEGUID', LValue) then
  begin
    FWIXWrapperParameters.UpgradeGUID := LValue;
  end;
  if TLZSystem.GetApplicationParameters('/COMPANYURL', LValue) then
  begin
    FWIXWrapperParameters.CompanyURL := LValue;
  end;
  if TLZSystem.GetApplicationParameters('/SUPPORTURL', LValue) then
  begin
    FWIXWrapperParameters.SupportURL := LValue;
  end;
  if TLZSystem.GetApplicationParameters('/PRODUCTURL', LValue) then
  begin
    FWIXWrapperParameters.ProductURL := LValue;
  end;
  if TLZSystem.GetApplicationParameters('/INSTALLUILEVEL2', LValue) then
  begin
    FWIXWrapperParameters.InstallUILevel2 := LValue;
  end;
  if TLZSystem.GetApplicationParameters('/INSTALLUILEVEL3_4', LValue) then
  begin
    FWIXWrapperParameters.InstallUILevel3_4 := LValue;
  end;
  if TLZSystem.GetApplicationParameters('/INSTALLUILEVEL5', LValue) then
  begin
    FWIXWrapperParameters.InstallUILevel5 := LValue;
  end;
  if TLZSystem.GetApplicationParameters('/UNINSTALLUILEVEL2', LValue) then
  begin
    FWIXWrapperParameters.UninstallUILevel2 := LValue;
  end;
  if TLZSystem.GetApplicationParameters('/UNINSTALLUILEVEL3_4', LValue) then
  begin
    FWIXWrapperParameters.UninstallUILevel3_4 := LValue;
  end;
  if TLZSystem.GetApplicationParameters('/UNINSTALLUILEVEL5', LValue) then
  begin
    FWIXWrapperParameters.UninstallUILevel5 := LValue;
  end;
  if TLZSystem.GetApplicationParameters('/KEEPWSX', LValue) then
  begin
    FWIXWrapperParameters.KeepWSX := StrToBoolDef(LValue, False);
  end;
  if TLZSystem.GetApplicationParameters('/KEEPWIXOBJ', LValue) then
  begin
    FWIXWrapperParameters.KeepWIXOBJ := StrToBoolDef(LValue, False);
  end;
  if TLZSystem.GetApplicationParameters('/MSIVERSION', LValue) then
  begin
    FWIXWrapperParameters.MSIVersion := TLZString.StringCleaner(LValue,
      True, True);
  end;
end;

constructor TWIXWrapper.Create;
begin
  FFileVersionInformation := TLZFileVersionInformation.Create;
  FFileVersionInformation.ExceptionOnError := True;
  FWIXWrapperParameters := TWIXWrapperParameters.Create;
end;

destructor TWIXWrapper.Destroy;
begin
  FreeAndNil(FFileVersionInformation);
  FreeAndNil(FWIXWrapperParameters);
  inherited;
end;

function TWIXWrapper.Execute: boolean;
var
  LWIXTemplate: TFileName;
  LEXEFileName: TFileName;
  LMSIFileName: TFileName;
  LValue: string;
  LRecurse: boolean;
begin
  LMSIFileName := '';
  LRecurse := True;
  Result := CheckCommandParameter;
  if Result then
  begin
    if TLZSystem.GetApplicationParameters('/EXEFILENAME', LValue) then
    begin
      LEXEFileName := LValue;
    end;
  end;
  if Result then
  begin
    if TLZSystem.GetApplicationParameters('/MSIFILENAME', LValue) then
    begin
      LMSIFileName := LValue;
    end;
  end;
  if Result then
  begin
    LWIXTemplate := GetTemplateFileName;
    if TLZSystem.GetApplicationParameters('/TEMPLATE', LValue) then
    begin
      LWIXTemplate := LValue;
    end;
  end;
  if Result then
  begin
    if TLZSystem.GetApplicationParameters('/RECURSE', LValue) then
    begin
      LRecurse := StrToBoolDef(LValue, LRecurse);
    end;
  end;
  if Result then
  begin
    Result := Execute(LWIXTemplate, LEXEFileName, LMSIFileName, LRecurse);
  end;
  if not Result then
  begin
    OutputUsage;
  end;
end;

function TWIXWrapper.Execute(AWIXTemplate: TFileName; AEXEFileName: string;
  AMSIFileName: TFileName; ARecurse: boolean): boolean;
var
  LFiles: TStringList;
  LEXEFileName, LWXSFileName, LWIXTemplate, LMSIFileName: TFileName;
begin
  Result := False;
  LFiles := TStringList.Create;
  try
    TBobCommon.GetFileList(AEXEFileName, LFiles, ARecurse);
    Log(Format('Processing %d file(s)', [LFiles.Count]));
    for LEXEFileName in LFiles do
    begin
      LWIXTemplate := TBobCommon.ProcessEnvironmentVariables(AWIXTemplate);
      LMSIFileName := TBobCommon.ProcessEnvironmentVariables(AMSIFileName);
      if TLZString.IsEmptyString(LMSIFileName) then
        LMSIFileName := ChangeFileExt(LEXEFileName, '.msi');
      LMSIFileName := StringReplace(LMSIFileName, '%msifilename%',
        ExtractFileName(ChangeFileExt(LEXEFileName, '.msi')),
        [rfReplaceAll, rfIgnoreCase]);
      LMSIFileName := StringReplace(LMSIFileName, '%exefilepath%',
        ExcludeTrailingPathDelimiter(ExtractFilePath(LEXEFileName)),
        [rfReplaceAll, rfIgnoreCase]);
      if GetFileInformation(LEXEFileName) then
      begin
        if UpdateMSIVersion then
        begin
          LWXSFileName := ChangeFileExt(LEXEFileName, '.wsx');
          try
            if GenerateTemplate(LEXEFileName, LWIXTemplate, LWXSFileName) then
            begin
              Result := BuildMSI(LWXSFileName, LMSIFileName);
            end
            else
            begin
              Error('Failed to generate template: ' + LWIXTemplate);
            end;
          finally
            if not FWIXWrapperParameters.KeepWSX then
            begin
              if FileExists(LWXSFileName) then
                DeleteFile(LWXSFileName);
            end;
          end;
        end
        else
        begin
          Error('Failed to set MSI version variable');
        end;
      end
      else
      begin
        Error(Format('Failed to get file "%s" version information',
          [LEXEFileName]));
      end;
    end;
  finally
    FreeAndNil(LFiles);
  end;
end;

function TWIXWrapper.GenerateTemplate(AEXEFileName, AWIXTemplate,
  AWIXFilename: TFileName): boolean;
var
  LTemplate: TStringList;
begin
  Result := FileExists(AWIXTemplate);
  if FileExists(AWIXFilename) then
    DeleteFile(AWIXFilename);
  if Result then
  begin
    Log(Format('Processing template "%s" to "%s"',
      [ExtractFileName(AWIXTemplate), ExtractFileName(AWIXFilename)]));
    LTemplate := TStringList.Create;
    try
      LTemplate.LoadFromFile(AWIXTemplate);

      LTemplate.Text := StringReplace(LTemplate.Text, '%SourceSetupFile%',
        AEXEFileName, [rfReplaceAll, rfIgnoreCase]);

      LTemplate.Text := StringReplace(LTemplate.Text, '%CompanyName%',
        TLZString.StringCleaner(FFileVersionInformation.CompanyName),
        [rfReplaceAll, rfIgnoreCase]);
      LTemplate.Text := StringReplace(LTemplate.Text, '%ProductName%',
        TLZString.StringCleaner(FFileVersionInformation.ProductName),
        [rfReplaceAll, rfIgnoreCase]);
      LTemplate.Text := StringReplace(LTemplate.Text, '%FileName%',
        ExtractFileName(FFileVersionInformation.FileName),
        [rfReplaceAll, rfIgnoreCase]);
      LTemplate.Text := StringReplace(LTemplate.Text, '%FileVersion%',
        TLZString.StringCleaner(FFileVersionInformation.FileVersion),
        [rfReplaceAll, rfIgnoreCase]);
      LTemplate.Text := StringReplace(LTemplate.Text, '%ProductVersion%',
        TLZString.StringCleaner(FFileVersionInformation.ProductVersion),
        [rfReplaceAll, rfIgnoreCase]);
      LTemplate.Text := StringReplace(LTemplate.Text, '%LegalCopyright%',
        TLZString.StringCleaner(FFileVersionInformation.LegalCopyright),
        [rfReplaceAll, rfIgnoreCase]);
      LTemplate.Text := StringReplace(LTemplate.Text, '%LegalTrademarks%',
        TLZString.StringCleaner(FFileVersionInformation.LegalTrademarks),
        [rfReplaceAll, rfIgnoreCase]);
      LTemplate.Text := StringReplace(LTemplate.Text, '%FileDescription%',
        TLZString.StringCleaner(FFileVersionInformation.FileDescription),
        [rfReplaceAll, rfIgnoreCase]);
      LTemplate.Text := StringReplace(LTemplate.Text, '%InternalName%',
        TLZString.StringCleaner(FFileVersionInformation.InternalName),
        [rfReplaceAll, rfIgnoreCase]);
      LTemplate.Text := StringReplace(LTemplate.Text, '%OriginalFileName%',
        TLZString.StringCleaner(FFileVersionInformation.OriginalFileName),
        [rfReplaceAll, rfIgnoreCase]);
      LTemplate.Text := StringReplace(LTemplate.Text, '%Comments%',
        TLZString.StringCleaner(FFileVersionInformation.Comments),
        [rfReplaceAll, rfIgnoreCase]);

      LTemplate.Text := StringReplace(LTemplate.Text, '%UpgradeGUID%',
        FWIXWrapperParameters.UpgradeGUID, [rfReplaceAll, rfIgnoreCase]);

      LTemplate.Text := StringReplace(LTemplate.Text, '%CompanyURL%',
        FWIXWrapperParameters.CompanyURL, [rfReplaceAll, rfIgnoreCase]);

      LTemplate.Text := StringReplace(LTemplate.Text, '%SupportURL%',
        FWIXWrapperParameters.SupportURL, [rfReplaceAll, rfIgnoreCase]);

      LTemplate.Text := StringReplace(LTemplate.Text, '%ProductURL%',
        FWIXWrapperParameters.ProductURL, [rfReplaceAll, rfIgnoreCase]);

      LTemplate.Text := StringReplace(LTemplate.Text, '%MSIVersion%',
        FWIXWrapperParameters.MSIVersion, [rfReplaceAll, rfIgnoreCase]);

      LTemplate.Text := StringReplace(LTemplate.Text,
        '%InstallUILevel2Parameters%', FWIXWrapperParameters.InstallUILevel2,
        [rfReplaceAll, rfIgnoreCase]);
      LTemplate.Text := StringReplace(LTemplate.Text,
        '%InstallUILevel3_4Parameters%',
        FWIXWrapperParameters.InstallUILevel3_4, [rfReplaceAll, rfIgnoreCase]);
      LTemplate.Text := StringReplace(LTemplate.Text,
        '%InstallUILevel5Parameters%', FWIXWrapperParameters.InstallUILevel5,
        [rfReplaceAll, rfIgnoreCase]);

      LTemplate.Text := StringReplace(LTemplate.Text,
        '%UninstallUILevel2Parameters%',
        FWIXWrapperParameters.UninstallUILevel2, [rfReplaceAll, rfIgnoreCase]);
      LTemplate.Text := StringReplace(LTemplate.Text,
        '%UninstallUILevel3_4Parameters%',
        FWIXWrapperParameters.UninstallUILevel3_4,
        [rfReplaceAll, rfIgnoreCase]);
      LTemplate.Text := StringReplace(LTemplate.Text,
        '%UninstallUILevel5Parameters%',
        FWIXWrapperParameters.UninstallUILevel5, [rfReplaceAll, rfIgnoreCase]);
      LTemplate.SaveToFile(AWIXFilename);
      Result := FileExists(AWIXFilename);
    finally
      FreeAndNil(LTemplate);
    end;
  end;
end;

function TWIXWrapper.UpdateMSIVersion: boolean;
var
  LValue: string;
begin
  if TLZString.IsEmptyString(FWIXWrapperParameters.MSIVersion) then
  begin
    LValue := TLZSystem.GetEnvironmentVariableValue('msiversion');
    if TLZString.IsEmptyString(LValue) then
      LValue := FFileVersionInformation.FileVersion;
    if TLZString.IsEmptyString(LValue) then
      LValue := FFileVersionInformation.ProductVersion;
    FWIXWrapperParameters.MSIVersion := LValue;
    Result := not TLZString.IsEmptyString(FWIXWrapperParameters.MSIVersion);
  end
  else
  begin
    Result := True;
  end;
end;

function TWIXWrapper.GetFileInformation(AFileName: TFileName): boolean;
begin
  Result := False;
  if not TLZString.IsEmptyString(AFileName) then
  begin
    if FileExists(AFileName) then
    begin
      Log(Format('Gathering information for "%s"',
        [ExtractFileName(AFileName)]));
      FFileVersionInformation.FileName := AFileName;
      Log('Product name: ' + FFileVersionInformation.ProductName);
      Log('Product version: ' + FFileVersionInformation.ProductVersion);
      Log('File version: ' + FFileVersionInformation.FileVersion);
      Log('Legal copyright: ' + FFileVersionInformation.LegalCopyright);

      if not TLZString.IsEmptyString(FFileVersionInformation.ProductName) then
      begin
        Result := True;
      end
      else
      begin
        Error(Format('"%s" failed to obtain file information', [AFileName]));
      end;
    end
    else
    begin
      Error(Format('"%s" does not exist', [AFileName]));
    end;
  end;
end;

function TWIXWrapper.GetTemplateFileName: TFileName;
begin
  Result := TLZFile.GetApplicationDir + 'wixtemplate.wxs';
end;

procedure TWIXWrapper.OutputUsage;
begin
  Log('bobWIXWrapper - generate msi file from inno setup executable');
  Log('NOTE: %ERRORLEVEL% will be set on failures');
  Log('Usage instruction:');
  Log('/EXEFILENAME:{filename;filename_wildcard} - Required, source ISS binary');
  Log('/TEMPLATE:{filename} - If not specified will use default isstemplate.wxs');
  Log('/COMPANYURL:{url} - Required (unless template has value)');
  Log('/MSIFILENAME:{filename} - If not specified will be same a EXEFILENAME.MSI');
  Log('/UPGRADEGUID:{guid} - If not specified one will be generated');
  Log('/SUPPORTURL:{url} - If not specified will be COMPANYURL');
  Log('/PRODUCTURL:{url} - If not specified will be COMPANYURL');
  Log('/INSTALLUILEVEL2 - Installer parameters (eg /VERYSILENT /SUPPRESSMSGBOXES)');
  Log('/INSTALLUILEVEL3_4 - Installer parameters (eg /VERYSILENT /SUPPRESSMSGBOXES)');
  Log('/INSTALLUILEVEL5 - Installer parameters (eg /VERYSILENT /SUPPRESSMSGBOXES)');
  Log('/UNINSTALLUILEVEL2 - Uninstaller parameters (eg /VERYSILENT /SUPPRESSMSGBOXES)');
  Log('/UNINSTALLUILEVEL3_4 - Uninstaller parameters (eg /VERYSILENT /SUPPRESSMSGBOXES)');
  Log('/UNINSTALLUILEVEL5 - Uninstaller parameters (eg /VERYSILENT /SUPPRESSMSGBOXES)');
  Log('/RECURSE:{true/false} - Recurse file search for EXEFILENAME (default: true)');
  Log('/MSIVERSION:0.0.0.0 - String value for MSI version (default: EXE Product Version)');
  Log('/KEEPWSX - Does not delete the WSX file on tidy up');
  Log('/KEEPWIXOBJ - Does not delete the WIXOBJ file on tidy up');
  Log('Examples:');
  Log('/EXEFILENAME:test.exe /MSIFILENAME:test-setup.msi /COMPANYURL:"https://www.littlearthsolutions.net"');
  Log('/EXEFILENAME:test.exe /MSIFILENAME:test-setup.msi /TEMPLATE:"test.wsx" /MSIVERSION:1.0.1.1');
  if FileExists(GetTemplateFileName) then
  begin
    Log('Default template location: ' + GetTemplateFileName);
  end;

end;

{ TWIXWrapperParameters }

constructor TWIXWrapperParameters.Create;
begin
  FInstallUILevel2 :=
    '/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /RESTARTEXITCODE=0';
  FInstallUILevel3_4 :=
    '/SILENT /SUPPRESSMSGBOXES /NORESTART /RESTARTEXITCODE=0';
  FInstallUILevel5 := '/SILENT /SUPPRESSMSGBOXES /NORESTART /RESTARTEXITCODE=0';

  FUninstallUILevel2 :=
    '/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /RESTARTEXITCODE=0';
  FUninstallUILevel3_4 :=
    '/SILENT /SUPPRESSMSGBOXES /NORESTART /RESTARTEXITCODE=0';
  FUninstallUILevel5 :=
    '/SILENT /SUPPRESSMSGBOXES /NORESTART /RESTARTEXITCODE=0';
  FUpgradeGUID := TGUID.NewGuid.ToString;
end;

procedure TWIXWrapperParameters.SetCompanyURL(const Value: string);
begin
  FCompanyURL := Value;
  if not TLZString.IsEmptyString(FCompanyURL) then
  begin
    if TLZString.IsEmptyString(FSupportURL) then
      FSupportURL := FCompanyURL;
    if TLZString.IsEmptyString(FProductURL) then
      FProductURL := FCompanyURL;
  end;
end;

procedure TWIXWrapperParameters.SetInstallerVersion(const Value: string);
begin
  FInstallerVersion := Trim(Value);
end;

procedure TWIXWrapperParameters.SetInstallUILevel2(const Value: string);
begin
  FInstallUILevel2 := Value;
end;

procedure TWIXWrapperParameters.SetInstallUILevel3_4(const Value: string);
begin
  FInstallUILevel3_4 := Value;
end;

procedure TWIXWrapperParameters.SetInstallUILevel5(const Value: string);
begin
  FInstallUILevel5 := Value;
end;

procedure TWIXWrapperParameters.SetKeepWIXOBJ(const Value: boolean);
begin
  FKeepWIXOBJ := Value;
end;

procedure TWIXWrapperParameters.SetKeepWSX(const Value: boolean);
begin
  FKeepWSX := Value;
end;

procedure TWIXWrapperParameters.SetProductURL(const Value: string);
begin
  FProductURL := Value;
end;

procedure TWIXWrapperParameters.SetSupportURL(const Value: string);
begin
  FSupportURL := Value;
end;

procedure TWIXWrapperParameters.SetUninstallUILevel2(const Value: string);
begin
  FUninstallUILevel2 := Value;
end;

procedure TWIXWrapperParameters.SetUninstallUILevel3_4(const Value: string);
begin
  FUninstallUILevel3_4 := Value;
end;

procedure TWIXWrapperParameters.SetUninstallUILevel5(const Value: string);
begin
  FUninstallUILevel5 := Value;
end;

procedure TWIXWrapperParameters.SetUpgradeGUID(const Value: string);
begin
  FUpgradeGUID := Value;
end;

end.
