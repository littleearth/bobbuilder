; ----------------------------------------------------------------------------
; Wingman - Installation Script
; Author: Tristan Marlow
; Purpose: Install application
;
; ----------------------------------------------------------------------------
; Copyright (c) 2020 Little Earth Solutions
; All Rights Reserved
;
; This product is protected by copyright and distributed under
; licenses restricting copying, distribution and decompilation
;
; ----------------------------------------------------------------------------
;
; History: 18/09/2011 - First Release.
;          15/07/2013 - Disable InnoSetup Close/Restart applications
;          28/07/2014 - DNS SVC lookup using LZInstallHelper.dll          
;
;-----------------------------------------------------------------------------
; Application Variables
;-----------------------------------------------------------------------------
#define ConstAppVersion GetFileVersion("..\build\client\bin\win32\release\DelphiCITest.exe") ; define variable
#define ConstAppName "Example"
#define ConstAppID "{{F1A72E63-7BC0-4A8D-88D7-EE7EFE031497}"
#define ConstAppMutex "{{F1A72E63-7BC0-4A8D-88D7-EE7EFE031497}"
#define ConstAppDescription "Example"
#define ConstAppPublisher "Little Earth Solutions"
#define ConstAppCopyright "Copyright (C) 2022 Little Earth Solutions"
#define ConstAppURL "http://www.littleearthsolutions.net/"
#define ConstAppExeName "DelphiCITest.exe"
;-----------------------------------------------------------------------------

#ifdef AppUserMode
  #define ConstAppSuffix "User"
  #define ConstAppUninstallSuffix "System"
  #define ConstPrivilegesRequired="lowest"
  #define ConstAppRecommendation="This version is recommended for user workstations, administration right are not required."
 #else
  #define ConstAppSuffix "System"
  #define ConstAppUninstallSuffix "User"
  #define ConstPrivilegesRequired="admin"
  #define ConstAppRecommendation="This version is recommended for RDS and Citrix servers, administration rights are required."
#endif

[Setup]
AppID={#ConstAppID}-{#ConstAppSuffix}
AppName={#ConstAppName} {#ConstAppSuffix}
AppVersion={#ConstAppVersion}
AppPublisher={#ConstAppPublisher}
AppPublisherURL={#ConstAppURL}
AppSupportURL={#ConstAppURL}
AppUpdatesURL={#ConstAppURL}
AppCopyright={#ConstAppCopyright}
VersionInfoCompany={#ConstAppPublisher}
VersionInfoDescription={#ConstAppName}
VersionInfoCopyright={#ConstAppCopyright}
VersionInfoVersion={#ConstAppVersion}
VersionInfoTextVersion={#ConstAppVersion}
OutputDir=..\build\installer\client\{#ConstAppSuffix}\
OutputBaseFilename=Example-Client-{#ConstAppVersion}-{#ConstAppSuffix}
DefaultDirName={autopf}\{#ConstAppPublisher}\{#ConstAppName}
UninstallDisplayName={#ConstAppName}
DefaultGroupName={#ConstAppPublisher}\{#ConstAppName}
AllowNoIcons=true
MinVersion=0,6.1.7600
InfoBeforeFile=..\build\client\bin\win32\release\Example - Release Notes.rtf
; LicenseFile=..\build\client\bin\win32\release\Example - License.rtf
UninstallDisplayIcon={app}\{#ConstAppExeName}
SolidCompression=True
InternalCompressLevel=ultra
Compression=lzma/ultra
RestartApplications=False
CloseApplications=False
PrivilegesRequired={#ConstPrivilegesRequired}
DisableWelcomePage=False
; "ArchitecturesInstallIn64BitMode=x64" requests that the install be
; done in "64-bit mode" on x64, meaning it should use the native
; 64-bit Program Files directory and the 64-bit view of the registry.
; On all other architectures it will install in "32-bit mode".
ArchitecturesInstallIn64BitMode=x64
; Note: We don't set ProcessorsAllowed because we want this
; installation to run on all architectures (including Itanium,
; since it's capable of running 32-bit code too).
CreateUninstallRegKey=not IsMSI

[Tasks]
Name: "desktopicon"; Description: "Create a &Desktop icon"; GroupDescription: "Additional icons:"

[Files]
; 32 bit
Source: "..\build\client\bin\win32\release\DelphiCITest.exe"; DestDir: "{app}"; Flags: promptifolder replacesameversion; BeforeInstall: TaskKill('{#ConstAppExeName}')
Source: "..\build\client\bin\win32\release\*"; DestDir: "{app}"; Flags: recursesubdirs restartreplace replacesameversion; Excludes: "*.~ra, *.map, *.drc"; Check: not Is64BitInstallMode
; 64 bit
Source: "..\build\client\bin\win64\release\DelphiCITest.exe"; DestDir: "{app}"; Flags: promptifolder replacesameversion; BeforeInstall: TaskKill('{#ConstAppExeName}')
Source: "..\build\client\bin\win64\release\*"; DestDir: "{app}"; Flags: recursesubdirs restartreplace replacesameversion; Excludes: "*.~ra, *.map, *.drc"

[INI]
Filename: {app}\install.ini; Section: "Install"; Key: "PrivilegesRequired"; String: {#ConstPrivilegesRequired}
Filename: {app}\install.ini; Section: "Install"; Key: "Version"; String: {#ConstAppVersion}
Filename: {app}\install.ini; Section: "Install"; Key: "Installed"; String: {code:GetDateTime}

[Icons]
Name: {group}\{#ConstAppName}; Filename: {app}\{#ConstAppExeName}; WorkingDir: {app}; IconFilename: {app}\{#ConstAppName}.ico
Name: {group}\{#ConstAppName} - User Guide; Filename: {app}\{#ConstAppName} - User Guide.pdf
Name: {autodesktop}\{#ConstAppName}; Filename: {app}\{#ConstAppExeName}; Tasks: desktopicon; WorkingDir: {app}; IconFilename: {app}\{#ConstAppName}.ico

[Run]
Filename: "{app}\{#ConstAppExeName}"; WorkingDir: "{app}"; Flags: nowait postinstall runasoriginaluser; Description: "Launch {#ConstAppName}"; Check: IsNotMSI

[Messages]
WelcomeLabel2=This will install [name/ver] on your computer.%n%nNOTE: {#ConstAppRecommendation}%n%nIt is recommended that you close all other applications before continuing.
AdminPrivilegesRequired=You must be logged in as an administrator when installing this program.%n%nTo install without administation rights please obtain the User mode setup.
PowerUserPrivilegesRequired=You must be logged in as an administrator or as a member of the Power Users group when installing this program.%n%nTo install without administation rights please obtain the User mode setup.

[UninstallDelete]
Type: files; Name: "{app}\defaults.ini"
Type: files; Name: "{app}\install.ini"

[InstallDelete]
Type: files; Name: "{app}\*.dll"

[Code]
#include "scripts\closeapplications.iss"
#include "scripts\uninstall.iss"
#include "scripts\msi.iss"

function GetDateTime(Param: String): String;
begin
  result := GetDateTimeString('yyyy-nn-dd hh:nn:ss', '-', ':');
end;

procedure CurStepChanged(CurStep: TSetupStep);
begin
   if (CurStep=ssInstall) then
    begin
      if (IsUpgrade()) then
      begin
        UnInstallOldVersion();
     end;
   end;
 
end;



function NeedRestart(): Boolean;
begin
	{ Do not prompt for restart even if required, ClientUpdate.exe will request a restart as it is in use via Automatic updates }
	Result := False;
end;

