unit Bob.CodeSign;

interface

uses Classes, Windows, SysUtils, Lazy.Utils.Windows, Lazy.Types, TlHelp32;

const
  DEFAULT_SIGN_COMMAND_LINE =
    'sign /t :timestampserver: /fd :filedigest: /f ":certificatefilename:" /csp ":csp:" '
    + '/kc "[{{:certificatepassword:}}]=:containername:"';
  DEFAULT_SIGN_TIMESTAMPSERVER = 'http://timestamp.digicert.com';
  DEFAULT_SIGN_FILEDIGEST = 'sha256';
  DEFAULT_SIGN_CSP = 'eToken Base Cryptographic Provider';

type
  ECodeSignException = class(Exception);

  TCodeSignProfile = class(TLZObject)
  private
    FFileName: TFileName;
    FCertificatePassword: string;
    FCertificateFileName: TFileName;
    FPFXPassword: string;
    FCommandLine: string;
    FContainerName: string;
    FTimeStampServer: string;
    FFileDigest: string;
    FCSP: string;
    FIgnoreFail: boolean;
    FSelfSign: boolean;
    procedure SetCertificateFileName(const Value: TFileName);
    procedure SetCertificatePassword(const Value: string);
    procedure SetFileName(const Value: TFileName);
    procedure SetCommandLine(const Value: string);
    procedure SetContainerName(const Value: string);
    procedure SetFileDigest(const Value: string);
    procedure SetTimeStampServer(const Value: string);
    procedure SetCSP(const Value: string);
    procedure SetIgnoreFail(const Value: boolean);
    procedure SetPFXPassword(const Value: string);
    procedure SetSelfSign(const Value: boolean);
  protected
    property FileName: TFileName read FFileName write SetFileName;
  public
    constructor Create; reintroduce;
    procedure Load(AFilename: string; AKey: string);
    procedure Save(AFilename: string; AKey: string);
    function ParseCommandLine(AObfuscate: boolean = false): string;
    property CertificateFileName: TFileName read FCertificateFileName
      write SetCertificateFileName;
    property CertificatePassword: string read FCertificatePassword
      write SetCertificatePassword;
    property CommandLine: string read FCommandLine write SetCommandLine;
    property TimeStampServer: string read FTimeStampServer
      write SetTimeStampServer;
    property FileDigest: string read FFileDigest write SetFileDigest;
    property ContainerName: string read FContainerName write SetContainerName;
    property CSP: string read FCSP write SetCSP;
    property IgnoreFail: boolean read FIgnoreFail write SetIgnoreFail;
    property PFXPassword: string read FPFXPassword write SetPFXPassword;
    property SelfSign: boolean read FSelfSign write SetSelfSign;

  end;

  TCodeSign = class(TLZObject)
  private
    FKey: string;
    FCodeSignFileName: TFileName;
    procedure SetCodeSignFileName(const Value: TFileName);
    procedure SetKey(const Value: string);
    function GetProfileFileName(AProfileName: string): TFileName;
    function IsAdministrator: boolean;
  protected
    function GenerateKey: string;
    function GetSettingsFolder: string;
    function GetSettingsFileName: TFileName;
    function ProcessEnvironmentVariables(ASource: string): string;
    function LoadProfile(AProfileName: string; var ACommandLine: string;
      var ACertificatePassword: string; var AIgnoreFailed: boolean): boolean;
  public
    constructor Create; reintroduce;
    procedure LoadSettings;
    procedure SaveSettings;
    procedure ExecuteProfile(AProfileName: string; AFileMask: string;
      ARecurse: boolean = true);
    procedure AddProfile(AProfileName: string; ACertificateFileName: TFileName;
      ACertificatePassword: string; ACommandLine: string;
      ATimeStampServer: string; AFileDigest: string; AContainerName: string;
      ACSP: string; AIgnoreFail: boolean);
    procedure AddProfilePFX(AProfileName: string; ACertificateFileName: TFileName;
      APFXPassword: string; ACommandLine: string;
      ATimeStampServer: string; AFileDigest: string; AIgnoreFail: boolean; ASelfSign: boolean);
    procedure DeleteProfile(AProfileName: string);
    procedure ListProfiles(var AProfiles: TStringList);
    function GetProfiles: TStringList;
    property Key: string read FKey write SetKey;
    property CodeSignFileName: TFileName read FCodeSignFileName
      write SetCodeSignFileName;
  end;

    TCodeSignConsole = class(TCodeSign)
  private
    function CheckCommandParameter(var AMessage: string; var AMode: string;
      var AFilename: string; var AProfile: string; var APassword: string;
      var ARecurse: boolean): boolean;
    function GetUsage: string;
  protected
  public
    function Execute(var AMessage: string): boolean;
  end;

implementation

uses
  Bob.Encryption, Lazy.CryptINI, System.IniFiles, Winapi.ShellAPI, Lazy.Token, Bob.Common;

const
  BASE_KEY = 'Tc0d3s1gn!';
  PROFILE_EXT = '.bcsp';

function IsUserAnAdmin(): BOOL; external shell32;

{ TCodeSign }

function TCodeSignConsole.GetUsage: string;
var
  LUsage: TStringList;
begin
  LUsage := TStringList.Create;
  try
    LUsage.Add('bobcodesign will use a profile to perform file code');
    LUsage.Add('signing for specific files');
    LUsage.Add('');
    LUsage.Add('Usage instruction:');
    LUsage.Add('');
    LUsage.Add
      ('/MODE:{mode} /PROFILE:{profilename} /FILENAME:{filename / filemask}');
    LUsage.Add('/RECURSE:{true|false} /PASSWORD:{password}');
    LUsage.Add('');
    LUsage.Add('- /MODE (default SIGN).');
    LUsage.Add('   Options: ');
    LUsage.Add('   SETSIGNTOOL - /FILENAME to Set location of signtool.exe');
    LUsage.Add('   SIGN - Sign files specified in /FILENAME');
    LUsage.Add
      ('   ADDPROFILE - Add profile with /FILENAME for certificate file');
    LUsage.Add('                 Use /FILENAME for certificate file (.pfx, .p12, .cer)');
    LUsage.Add('                 Use /CONTAINERNAME for container-based certificates (with .cer)');
    LUsage.Add('                 Use /PFXPASSWORD for PFX-based certificates (.pfx or .p12)');
    LUsage.Add('                 Use /SELFSIGN:true to enable self-signing capability');
    LUsage.Add('   DELPROFILE - Delete profile specified by /PROFILE');
    LUsage.Add('   LISTPROFILES - List all configured profiles');
    LUsage.Add('');
    LUsage.Add('- /FILENAME filename or filemask (eg *.dll) or certificate file (.pfx, .p12, .cer)');
    LUsage.Add('- /PFXPASSWORD password for PFX certificate (only for .pfx or .p12 files)');
    LUsage.Add('- /RECURSE recurse file mask for signing (default :true)');
    LUsage.Add('- /PROFILE Name of profile used in specified MODE');
    LUsage.Add
      ('- /IGNOREFAIL:true If code sign fails ignore it (default: false)');
    LUsage.Add
      ('- /COMMANDLINE override command line for sign default. Variable below can also be included. blank for default.'
      + DEFAULT_SIGN_COMMAND_LINE);
    LUsage.Add
      ('- /TIMESTAMPSERVER specify :timestampserver: in command line. Can be omitted for self-signed certificates. '
      + 'Default: ' + DEFAULT_SIGN_TIMESTAMPSERVER);
    LUsage.Add
      ('- /FILEDIGEST specify :filedigest: in command line, blank for default. '
      + DEFAULT_SIGN_FILEDIGEST);
    LUsage.Add
      ('- /CONTAINERNAME specify :containername: in command line. No default');
    LUsage.Add('- /CSP specify :csp: in command line, blank for default. ' +
      DEFAULT_SIGN_CSP);
    LUsage.Add('');
    LUsage.Add('Examples:');
    LUsage.Add('   /MODE:SETSIGNTOOL /FILENAME:""C:\SignTool\SignTool.exe"');
    LUsage.Add('   /MODE:SIGN /FILENAME:"C:\Build\*.exe /RECURSE:true /PROFILE:DEFAULT');
    LUsage.Add
      ('   /MODE:ADDPROFILE /FILENAME:"C:\BuildServer\CodeSign\certs\Cert.cer" /CONTAINERNAME:Sectigo_123456789 /PASSWORD:secret /PROFILE:DEFAULT');
    LUsage.Add
      ('   /MODE:ADDPROFILE /FILENAME:"C:\Certificates\codesign.pfx" /PFXPASSWORD:pfxpassword /SELFSIGN:true /PROFILE:SELFSIGN');
    LUsage.Add('   /MODE:DELPROFILE /PROFILE:DEFAULT');
    LUsage.Add('   /MODE:LISTPROFILES');
    LUsage.Add('');
    LUsage.Add('Creating a Self-Signed Certificate with PowerShell:');
    LUsage.Add('   Run PowerShell as Administrator and execute:');
    LUsage.Add('   $cert = New-SelfSignedCertificate -Type Custom -Subject "CN=Your Company Code Signing" -KeyUsage DigitalSignature -FriendlyName "MyCodeSignCert" -CertStoreLocation "Cert:\CurrentUser\My" -TextExtension @("2.5.29.37={text}1.3.6.1.5.5.7.3.3")');
    LUsage.Add('   $password = ConvertTo-SecureString -String "YourPassword" -Force -AsPlainText');
    LUsage.Add('   Export-PfxCertificate -cert "Cert:\CurrentUser\My\$($cert.Thumbprint)" -FilePath "C:\MyCert.pfx" -Password $password');
    Result := LUsage.Text;
  finally
    FreeAndNil(LUsage);
  end;
end;

function TCodeSignConsole.CheckCommandParameter(var AMessage: string;
  var AMode: string; var AFilename: string; var AProfile: string;
  var APassword: string; var ARecurse: boolean): boolean;
var
  LValue: string;
  LAdminRequired: boolean;
begin
  Result := false;
  AMessage := '';
  ARecurse := true;
  LAdminRequired := false;

  TLZSystem.GetApplicationParameters('/MODE', AMode);
  TLZSystem.GetApplicationParameters('/PROFILE', AProfile);
  TLZSystem.GetApplicationParameters('/FILENAME', AFilename);
  TLZSystem.GetApplicationParameters('/PASSWORD', APassword);
  if TLZSystem.GetApplicationParameters('/RECURSE', LValue) then
  begin
    ARecurse := StrToBoolDef(LValue, ARecurse);
  end;

  if TLZString.IsEmptyString(AMode) then
    AMode := 'SIGN';

  if SameText(AMode, 'SETSIGNTOOL') then
  begin
    LAdminRequired := true;
    Result := not TLZString.IsEmptyString(AFilename);
  end;

  if SameText(AMode, 'SIGN') then
  begin
    if (not TLZString.IsEmptyString(AProfile)) and
      (not TLZString.IsEmptyString(AFilename)) then
    begin
      Result := true;
    end;
  end;

  if SameText(AMode, 'ADDPROFILE') then
  begin
    if not TLZString.IsEmptyString(AProfile) and
       (not TLZString.IsEmptyString(AFilename)) then
    begin
      LAdminRequired := true;
      // Check if PFX-based (has PFXPASSWORD and .pfx or .p12 extension) or container-based (has PASSWORD and .cer or other extension)
      TLZSystem.GetApplicationParameters('/PFXPASSWORD', LValue);
      if not TLZString.IsEmptyString(LValue) and
         (SameText(ExtractFileExt(AFilename), '.pfx') or 
          SameText(ExtractFileExt(AFilename), '.p12')) then
      begin
        Result := true; // PFX-based profile
      end
      else if not TLZString.IsEmptyString(APassword) then
      begin
        Result := true; // Container-based profile
      end;
    end;
  end;

  if SameText(AMode, 'DELPROFILE') then
  begin
    if (not TLZString.IsEmptyString(AProfile)) then
    begin
      LAdminRequired := true;
      Result := true;
    end;
  end;

  if SameText(AMode, 'LISTPROFILES') then
  begin
    LAdminRequired := false; // No admin required to list profiles
    Result := true;
  end;

  if LAdminRequired and Result then
  begin
    if not IsAdministrator then
    begin
      Result := false;
      AMessage := 'Administrator access required.';
    end;
  end;

  if not Result then
  begin
    if TLZString.IsEmptyString(AMessage) then
      AMessage := GetUsage;
  end;

end;

function TCodeSignConsole.Execute(var AMessage: string): boolean;
var
  LMode, LFilename, LProfile, LPassword: string;
  LCommandLine, LTimeStampServer, LFileDigest, LContainerName, LCSP,
    LIgnoreFailText, LPFXPassword, LSelfSignText: string;
  LRecurse, LIgnoreFail, LSelfSign: boolean;
begin
  Result := CheckCommandParameter(AMessage, LMode, LFilename, LProfile,
    LPassword, LRecurse);
  try
    if Result then
    begin
      if SameText(LMode, 'SETSIGNTOOL') then
      begin
        CodeSignFileName := LFilename;
        SaveSettings;
        Log(Format('Set sign tool filename: %s', [LFilename]));
      end;
    end;

    if Result then
    begin
      if SameText(LMode, 'SIGN') then
      begin
        ExecuteProfile(LProfile, LFilename, LRecurse);
      end;
    end;

    if Result then
    begin
      if SameText(LMode, 'ADDPROFILE') then
      begin
        TLZSystem.GetApplicationParameters('/COMMANDLINE', LCommandLine);
        TLZSystem.GetApplicationParameters('/TIMESTAMPSERVER',
          LTimeStampServer);
        TLZSystem.GetApplicationParameters('/FILEDIGEST', LFileDigest);
        TLZSystem.GetApplicationParameters('/CONTAINERNAME', LContainerName);
        TLZSystem.GetApplicationParameters('/CSP', LCSP);
        TLZSystem.GetApplicationParameters('/PFXPASSWORD', LPFXPassword);
        LIgnoreFail := false;
        LSelfSign := false;
        if TLZSystem.GetApplicationParameters('/IGNOREFAIL', LIgnoreFailText)
        then
        begin
          LIgnoreFail := StrToBoolDef(LIgnoreFailText, LIgnoreFail);
        end;
        if TLZSystem.GetApplicationParameters('/SELFSIGN', LSelfSignText) then
        begin
          LSelfSign := StrToBoolDef(LSelfSignText, LSelfSign);
        end;

        // Check if PFX-based (has PFXPASSWORD and .pfx or .p12 extension) or container-based
        if (not TLZString.IsEmptyString(LPFXPassword)) and
           (SameText(ExtractFileExt(LFilename), '.pfx') or 
            SameText(ExtractFileExt(LFilename), '.p12')) then
        begin
          AddProfilePFX(LProfile, LFilename, LPFXPassword, LCommandLine,
            LTimeStampServer, LFileDigest, LIgnoreFail, LSelfSign);
        end
        else
        begin
          AddProfile(LProfile, LFilename, LPassword, LCommandLine,
            LTimeStampServer, LFileDigest, LContainerName, LCSP, LIgnoreFail);
        end;
      end;
    end;

    if Result then
    begin
      if SameText(LMode, 'DELPROFILE') then
      begin
        DeleteProfile(LProfile);
        Log(Format('Deleted profile: %s', [LProfile]));
      end;
    end;

    if Result then
    begin
      if SameText(LMode, 'LISTPROFILES') then
      begin
        Log(GetProfiles.Text);
      end;
    end;
  except
    on E: Exception do
    begin
      AMessage := E.Message;
      Result := false;
    end;
  end;
end;

{ TCodeSign }

procedure TCodeSign.AddProfile(AProfileName: string;
  ACertificateFileName: TFileName; ACertificatePassword: string;
  ACommandLine: string; ATimeStampServer: string; AFileDigest: string;
  AContainerName: string; ACSP: string; AIgnoreFail: boolean);
var
  LProfile: TCodeSignProfile;
begin
  LProfile := TCodeSignProfile.Create;
  try
    LProfile.CertificateFileName := ACertificateFileName;
    LProfile.CertificatePassword := ACertificatePassword;
    if not TLZString.IsEmptyString(ACommandLine) then
    begin
      LProfile.CommandLine := ACommandLine;
    end;
    if not TLZString.IsEmptyString(ATimeStampServer) then
    begin
      LProfile.TimeStampServer := ATimeStampServer;
    end;
    if not TLZString.IsEmptyString(AFileDigest) then
    begin
      LProfile.FileDigest := AFileDigest;
    end;
    if not TLZString.IsEmptyString(AContainerName) then
    begin
      LProfile.ContainerName := AContainerName;
    end;
    if not TLZString.IsEmptyString(ACSP) then
    begin
      LProfile.CSP := ACSP;
    end;
    LProfile.IgnoreFail := AIgnoreFail;
    LProfile.Save(GetProfileFileName(AProfileName), Key);
    Log(Format('Added profile: %s', [AProfileName]));
  finally
    FreeAndNil(LProfile);
  end;
end;

constructor TCodeSign.Create;
begin
  LoadSettings;
end;

function TCodeSign.GetProfileFileName(AProfileName: string): TFileName;
begin
  Result := GetSettingsFolder + ChangeFileExt(AProfileName, PROFILE_EXT);
end;

procedure TCodeSign.DeleteProfile(AProfileName: string);
var
  LSource, LDestination: TFileName;
begin
  LSource := GetProfileFileName(AProfileName);
  LDestination := ChangeFileExt(LSource, '.del');
  if FileExists(LSource) then
  begin
    if FileExists(LDestination) then
    begin
      Log(Format('Removed profile: %s', [AProfileName]));
      DeleteFile(LDestination);
    end;
    RenameFile(LSource, LDestination);
  end;
end;

procedure TCodeSign.ListProfiles(var AProfiles: TStringList);
var
  LProfileFiles: TStringList;
  LIdx: integer;
  LProfileName: string;
begin
  AProfiles.Clear;
  LProfileFiles := TStringList.Create;
  try
    TLZFile.QuickFileSearch(GetSettingsFolder, '*' + PROFILE_EXT, false, LProfileFiles);
    for LIdx := 0 to Pred(LProfileFiles.Count) do
    begin
      LProfileName := ChangeFileExt(ExtractFileName(LProfileFiles[LIdx]), '');
      AProfiles.Add(LProfileName);
    end;
  finally
    FreeAndNil(LProfileFiles);
  end;
end;

function TCodeSign.GetProfiles: TStringList;
begin
  Result := TStringList.Create;
  ListProfiles(Result);
end;

function TCodeSign.LoadProfile(AProfileName: string; var ACommandLine: string;
  var ACertificatePassword: string; var AIgnoreFailed: boolean): boolean;
var
  LProfile: TCodeSignProfile;
begin
  LProfile := TCodeSignProfile.Create;
  try
    LProfile.Load(GetProfileFileName(AProfileName), Key);
    ACommandLine := LProfile.ParseCommandLine(true);
    ACertificatePassword := LProfile.CertificatePassword;
    AIgnoreFailed := LProfile.IgnoreFail;
    Result := not TLZString.IsEmptyString(ACommandLine);
  finally
    FreeAndNil(LProfile);
  end;
end;

procedure TCodeSign.ExecuteProfile(AProfileName, AFileMask: string;
  ARecurse: boolean);
var
  LFileMask, LFolder, LCommand: string;
  LCertificatePassword: string;
  LIgnoreFail: boolean;
  LFiles: TStringList;
  LFileMasks: TLZToken;
  LIdx: integer;
  LExitCode: integer;
begin
  if LoadProfile(AProfileName, LCommand, LCertificatePassword, LIgnoreFail) then
  begin
    LFiles := TStringList.Create;
    LFileMasks := TLZToken.Create;
    try
      LFileMasks.Seperator := ';';
      LFileMasks.Source := AFileMask;

      for LIdx := 0 to Pred(LFileMasks.Count) do
      begin
        LFileMask := ProcessEnvironmentVariables(LFileMasks.Tokens[LIdx]);
        LFolder := ExtractFilePath(LFileMask);
        LFileMask := StringReplace(LFileMask, LFolder, '',
          [rfReplaceAll, rfIgnoreCase]);
        TLZFile.QuickFileSearch(LFolder, LFileMask, ARecurse, LFiles);
      end;

      if LFiles.Count > 0 then
      begin
        LFiles.Delimiter := ' ';
        Log(Format('Signing with profile %s, Files: %s',
          [AProfileName, LFiles.CommaText]));
        LCommand := LCommand + ' ' + LFiles.DelimitedText;
        Log(Format('Command (password obfuscate): %s %s', [CodeSignFileName,
          LCommand]));

        LExitCode := TLZFile.ExecuteAndWait(CodeSignFileName,
          StringReplace(LCommand, ':certificatepassword:', LCertificatePassword,
          [rfReplaceAll, rfIgnoreCase]), SW_HIDE);
        if LExitCode = 0 then
        begin
          Log(Format('Signing complete, profile %s', [AProfileName]));
        end
        else
        begin
          if not LIgnoreFail then
          begin
            raise ECodeSignException.CreateFmt
              ('Sign process failed with exit code %d. Command (password obfuscated): %s',
              [LExitCode, LCommand]);
          end
          else
          begin
            Log(Format
              ('Ignoring sign process failed with exit code %d. Command (password obfuscated): %s',
              [LExitCode, LCommand]));
          end;
        end;
      end
      else
      begin
        raise ECodeSignException.CreateFmt('No files found %s', [AFileMask]);
      end;
    finally
      FreeAndNil(LFiles);
      FreeAndNil(LFileMasks);
    end;
  end
  else
  begin
    raise ECodeSignException.CreateFmt('Failed to load profile %s',
      [AProfileName]);
  end;
end;

function TCodeSign.GenerateKey: string;
begin
  Result := TLZString.GeneratePassword(20);
end;

function TCodeSign.GetSettingsFileName: TFileName;
begin
  Result := GetSettingsFolder + 'settings.ini';
end;

function TCodeSign.GetSettingsFolder: string;
begin
  Result := TBobCommon.GetPublicSettingsFolder('bobcodesign');
  TLZFile.CheckDirectoryExists(Result, true);
end;

function TCodeSign.IsAdministrator: boolean;
begin
  try
    Result := IsUserAnAdmin;
  except
    Result := false;
  end;
end;

procedure TCodeSign.LoadSettings;
var
  LINIFile: TINIFile;
  LFilename: TFileName;
begin
  LFilename := GetSettingsFileName;
  if FileExists(LFilename) or (IsAdministrator) then
  begin
    LINIFile := TINIFile.Create(GetSettingsFileName);
    try
      FKey := LINIFile.ReadString('Settings', 'Key', '');
      if not TLZString.IsEmptyString(FKey) then
      begin
        FKey := TEncryption.DecryptPassword(FKey, BASE_KEY);
      end;
      FCodeSignFileName := LINIFile.ReadString('Settings', 'CodeSignFileName',
        'signtool.exe');
    finally
      FreeAndNil(LINIFile);
    end;
    if not FileExists(LFilename) then
    begin
      SaveSettings;
    end;
  end
  else
  begin
    raise ECodeSignException.CreateFmt
      ('Settings "%s" file does not exists, administrator access required to create.',
      [LFilename]);
  end;
end;

procedure TCodeSign.SaveSettings;
var
  LINIFile: TINIFile;
begin
  if (IsAdministrator) then
  begin
    LINIFile := TINIFile.Create(GetSettingsFileName);
    try
      if TLZString.IsEmptyString(FKey) then
        FKey := GenerateKey;
      LINIFile.WriteString('Settings', 'Key', TEncryption.EncryptPassword(FKey,
        BASE_KEY));
      LINIFile.WriteString('Settings', 'CodeSignFileName', FCodeSignFileName);
    finally
      FreeAndNil(LINIFile);
    end;
  end
  else
  begin
    raise ECodeSignException.Create
      ('Administrator access required to update settings.');
  end;
end;

procedure TCodeSign.SetCodeSignFileName(const Value: TFileName);
begin
  FCodeSignFileName := Value;
end;

procedure TCodeSign.SetKey(const Value: string);
begin
  FKey := Value;
end;

function TCodeSign.ProcessEnvironmentVariables(ASource: string): string;
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
        Result := StringReplace(Result, '%' + LVariable + '%', LVariableValue,
          [rfReplaceAll, rfIgnoreCase]);
      end;
    end;

  finally
    FreeAndNil(LVariables);
  end;
end;


procedure TCodeSign.AddProfilePFX(AProfileName: string;
  ACertificateFileName: TFileName; APFXPassword: string; ACommandLine: string;
  ATimeStampServer: string; AFileDigest: string; AIgnoreFail: boolean; ASelfSign: boolean);
var
  LProfile: TCodeSignProfile;
  LSignCommand: string;
begin
  LProfile := TCodeSignProfile.Create;
  try
    LProfile.CertificateFileName := ACertificateFileName;
    LProfile.PFXPassword := APFXPassword;
    if TLZString.IsEmptyString(ACommandLine) then
      LSignCommand := 'sign /t :timestampserver: /fd :filedigest: /f ":certificatefilename:" /p :pfxpassword:'
    else
      LSignCommand := ACommandLine;
    LProfile.CommandLine := LSignCommand;
    if not TLZString.IsEmptyString(ATimeStampServer) then
    begin
      LProfile.TimeStampServer := ATimeStampServer;
    end;
    if not TLZString.IsEmptyString(AFileDigest) then
    begin
      LProfile.FileDigest := AFileDigest;
    end;
    LProfile.IgnoreFail := AIgnoreFail;
    LProfile.SelfSign := ASelfSign;
    LProfile.Save(GetProfileFileName(AProfileName), Key);
    Log(Format('Added PFX profile: %s (SelfSign: %s)', [AProfileName, BoolToStr(ASelfSign, true)]));
  finally
    FreeAndNil(LProfile);
  end;
end;

{ TCodeSignProfile }

constructor TCodeSignProfile.Create;
begin
  FCommandLine := DEFAULT_SIGN_COMMAND_LINE;
  FTimeStampServer := ''; // Empty by default - can be set if needed
  FFileDigest := DEFAULT_SIGN_FILEDIGEST;
  FContainerName := '';
  FCSP := DEFAULT_SIGN_CSP;
  FIgnoreFail := false;
  FPFXPassword := '';
  FSelfSign := false;
end;

function TCodeSignProfile.ParseCommandLine(AObfuscate: boolean): string;
begin
  Result := FCommandLine;
  
  // Handle timestamp server - only include /t if timestamp server is provided
  if not TLZString.IsEmptyString(FTimeStampServer) then
  begin
    Result := StringReplace(Result, '/t :timestampserver:', 
      '/t "' + FTimeStampServer + '"', [rfReplaceAll, rfIgnoreCase]);
  end
  else
  begin
    // Remove the /t parameter entirely if no timestamp server
    Result := StringReplace(Result, '/t :timestampserver:', '', 
      [rfReplaceAll, rfIgnoreCase]);
    Result := StringReplace(Result, '  ', ' ', [rfReplaceAll, rfIgnoreCase]); // Clean up double spaces
  end;
  
  Result := StringReplace(Result, ':filedigest:', FFileDigest,
    [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ':certificatefilename:', FCertificateFileName,
    [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ':csp:', FCSP, [rfReplaceAll, rfIgnoreCase]);
  if not AObfuscate then
  begin
    Result := StringReplace(Result, ':certificatepassword:',
      FCertificatePassword, [rfReplaceAll, rfIgnoreCase]);
  end;
  Result := StringReplace(Result, ':containername:', FContainerName,
    [rfReplaceAll, rfIgnoreCase]);
  Result := StringReplace(Result, ':pfxpassword:', FPFXPassword,
    [rfReplaceAll, rfIgnoreCase]);
end;

procedure TCodeSignProfile.Load(AFilename, AKey: string);
var
  LINIFile: TLZCryptINI;
begin
  LINIFile := TLZCryptINI.Create(AFilename, AKey);
  try
    FCertificatePassword := LINIFile.ReadString('Profile',
      'CertificatePassword', '');
    FCertificateFileName := LINIFile.ReadString('Profile',
      'CertificateFileName', '');
    FPFXPassword := LINIFile.ReadString('Profile', 'PFXPassword', '');
    FCommandLine := LINIFile.ReadString('Profile', 'CommandLine', FCommandLine);
    FTimeStampServer := LINIFile.ReadString('Profile', 'TimeStampServer',
      FTimeStampServer);
    FFileDigest := LINIFile.ReadString('Profile', 'FileDigest', FFileDigest);
    FContainerName := LINIFile.ReadString('Profile', 'ContainerName',
      FContainerName);
    FCSP := LINIFile.ReadString('Profile', 'CSP', FCSP);
    FIgnoreFail := LINIFile.ReadBool('Profile', 'IgnoreFail', FIgnoreFail);
    FSelfSign := LINIFile.ReadBool('Profile', 'SelfSign', FSelfSign);
  finally
    FreeAndNil(LINIFile);
  end;
end;

procedure TCodeSignProfile.Save(AFilename, AKey: string);
var
  LINIFile: TLZCryptINI;
begin
  LINIFile := TLZCryptINI.Create(AFilename, AKey);
  try
    LINIFile.WriteString('Profile', 'CertificatePassword',
      FCertificatePassword);
    LINIFile.WriteString('Profile', 'CertificateFileName',
      FCertificateFileName);
    LINIFile.WriteString('Profile', 'PFXPassword', FPFXPassword);
    LINIFile.WriteString('Profile', 'CommandLine', FCommandLine);
    LINIFile.WriteString('Profile', 'TimeStampServer', FTimeStampServer);
    LINIFile.WriteString('Profile', 'FileDigest', FFileDigest);
    LINIFile.WriteString('Profile', 'ContainerName', FContainerName);
    LINIFile.WriteString('Profile', 'CSP', FCSP);
    LINIFile.WriteBool('Profile', 'IgnoreFail', FIgnoreFail);
    LINIFile.WriteBool('Profile', 'SelfSign', FSelfSign);
    LINIFile.UpdateFile;
  finally
    FreeAndNil(LINIFile);
  end;
end;

procedure TCodeSignProfile.SetCertificateFileName(const Value: TFileName);
begin
  FCertificateFileName := Value;
end;

procedure TCodeSignProfile.SetCertificatePassword(const Value: string);
begin
  FCertificatePassword := Value;
end;

procedure TCodeSignProfile.SetCommandLine(const Value: string);
begin
  FCommandLine := Value;
end;

procedure TCodeSignProfile.SetContainerName(const Value: string);
begin
  FContainerName := Value;
end;

procedure TCodeSignProfile.SetCSP(const Value: string);
begin
  FCSP := Value;
end;

procedure TCodeSignProfile.SetFileDigest(const Value: string);
begin
  FFileDigest := Value;
end;

procedure TCodeSignProfile.SetFileName(const Value: TFileName);
begin
  FFileName := Value;
end;

procedure TCodeSignProfile.SetIgnoreFail(const Value: boolean);
begin
  FIgnoreFail := Value;
end;

procedure TCodeSignProfile.SetTimeStampServer(const Value: string);
begin
  FTimeStampServer := Value;
end;

procedure TCodeSignProfile.SetPFXPassword(const Value: string);
begin
  FPFXPassword := Value;
end;

procedure TCodeSignProfile.SetSelfSign(const Value: boolean);
begin
  FSelfSign := Value;
end;

end.
