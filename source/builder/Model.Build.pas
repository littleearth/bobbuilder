unit Model.Build;

interface

uses
  Classes, SysUtils, Data.DB, Rtti, System.JSON,
  Lazy.Model, Lazy.Nullable;

type
  TBuildType = (btDevelopment, btStaging, btProduction);

  TVariable = class(TLZModel)
  private
    FvariableName: string;
    FvariableValue: string;
    Fproduction: TLZNullableBoolean;
    Fdevelopment: TLZNullableBoolean;
    Fstaging: TLZNullableBoolean;
    procedure SetvariableName(const Value: string);
    procedure SetvariableValue(const Value: string);
    procedure Setdevelopment(const Value: TLZNullableBoolean);
    procedure Setproduction(const Value: TLZNullableBoolean);
    procedure Setstaging(const Value: TLZNullableBoolean);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property variableName: string read FvariableName write SetvariableName;
    property variableValue: string read FvariableValue write SetvariableValue;
    property development: TLZNullableBoolean read Fdevelopment
      write Setdevelopment;
    property staging: TLZNullableBoolean read Fstaging write Setstaging;
    property production: TLZNullableBoolean read Fproduction
      write Setproduction;
  end;

  TVariables = class(TLZModelList<TVariable>);

  TFile = class(TLZModel)
  private
    FfileName: string;
    FopenWith: string;
    procedure SetfileName(const Value: string);
    procedure SetopenWith(const Value: string);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property filename: string read FfileName write SetfileName;
    property openWith: string read FopenWith write SetopenWith;
  end;

  TFiles = class(TLZModelList<TFile>);

  TFolder = class(TLZModel)
  private
    Ffolder: string;
    FcleanupEnabled: Boolean;
    procedure Setfolder(const Value: string);
    procedure SetcleanupEnabled(const Value: Boolean);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property folder: string read Ffolder write Setfolder;
    property cleanupEnabled: Boolean read FcleanupEnabled
      write SetcleanupEnabled;
  end;

  TFolders = class(TLZModelList<TFolder>);

  TScript = class(TLZModel)
  private
    Fenabled: Boolean;
    FscriptSource: string;
    FscriptName: string;
    Fproduction: TLZNullableBoolean;
    Fstaging: TLZNullableBoolean;
    Fselective: TLZNullableBoolean;
    procedure Setenabled(const Value: Boolean);
    procedure SetscriptSource(const Value: string);
    procedure Setname(const Value: string);
    procedure Setproduction(const Value: TLZNullableBoolean);
    procedure Setstaging(const Value: TLZNullableBoolean);
    procedure Setselective(const Value: TLZNullableBoolean);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property enabled: Boolean read Fenabled write Setenabled;
    property scriptName: string read FscriptName write Setname;
    property scriptSource: string read FscriptSource write SetscriptSource;
    property staging: TLZNullableBoolean read Fstaging write Setstaging;
    property production: TLZNullableBoolean read Fproduction
      write Setproduction;
    property selective: TLZNullableBoolean read Fselective write Setselective;
  end;

  TScripts = class(TLZModelList<TScript>);

  TProject = class(TLZModel)
  private
    Fproject: string;
    Fplatforms: string;
    Fconfigs: string;
    Fenabled: TLZNullableBoolean;
    Fproduction: TLZNullableBoolean;
    Fstaging: TLZNullableBoolean;
    Fproperties: string;
    procedure Setplatforms(const Value: string);
    procedure Setproject(const Value: string);
    procedure Setconfigs(const Value: string);
    procedure Setenabled(const Value: TLZNullableBoolean);
    procedure Setproduction(const Value: TLZNullableBoolean);
    procedure Setstaging(const Value: TLZNullableBoolean);
    procedure Setproperties(const Value: string);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property project: string read Fproject write Setproject;
    property platforms: string read Fplatforms write Setplatforms;
    property configs: string read Fconfigs write Setconfigs;
    property properties: string read Fproperties write Setproperties;
    property enabled: TLZNullableBoolean read Fenabled write Setenabled;
    property staging: TLZNullableBoolean read Fstaging write Setstaging;
    property production: TLZNullableBoolean read Fproduction
      write Setproduction;
  end;

  TProjects = class(TLZModelList<TProject>);

  TTestProject = class(TProject)
  private
    Fparams: string;
    Ffolder: string;
    Fcommand: string;
    FpostBuild: TLZNullableBoolean;
    procedure Setcommand(const Value: string);
    procedure Setfolder(const Value: string);
    procedure Setparams(const Value: string);
    procedure SetpostBuild(const Value: TLZNullableBoolean);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property params: string read Fparams write Setparams;
    property command: string read Fcommand write Setcommand;
    property folder: string read Ffolder write Setfolder;
    property postBuild: TLZNullableBoolean read FpostBuild write SetpostBuild;
  end;

  TTestProjects = class(TLZModelList<TTestProject>);

  TProjectGroup = class(TLZModel)
  private
    FProjects: TProjects;
    Fgroup: string;
    Fenabled: Boolean;
    procedure Setgroup(const Value: string);
    procedure Setenabled(const Value: Boolean);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property enabled: Boolean read Fenabled write Setenabled;
    property group: string read Fgroup write Setgroup;
    property projects: TProjects read FProjects;
  end;

  TProjectGroups = class(TLZModelList<TProjectGroup>);

  TTestProjectGroup = class(TLZModel)
  private
    FTestProjects: TTestProjects;
    Fgroup: string;
    Fenabled: Boolean;
    procedure Setgroup(const Value: string);
    procedure Setenabled(const Value: Boolean);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property enabled: Boolean read Fenabled write Setenabled;
    property group: string read Fgroup write Setgroup;
    property projects: TTestProjects read FTestProjects;
  end;

  TTestProjectGroups = class(TLZModelList<TTestProjectGroup>);

  TInstallScript = class(TLZModel)
  private
    FfileName: string;
    Fenabled: Boolean;
    FscriptType: string;
    Fproduction: TLZNullableBoolean;
    Fstaging: TLZNullableBoolean;
    Fparams: string;
    FscriptName: string;
    procedure Setenabled(const Value: Boolean);
    procedure SetfileName(const Value: string);
    procedure SetscriptType(const Value: string);
    procedure Setproduction(const Value: TLZNullableBoolean);
    procedure Setstaging(const Value: TLZNullableBoolean);
    procedure Setparams(const Value: string);
    procedure SetscriptName(const Value: string);
    function GetfileName: string;
    function GetscriptName: string;
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property enabled: Boolean read Fenabled write Setenabled;
    property scriptName: string read GetscriptName write SetscriptName;
    property scriptType: string read FscriptType write SetscriptType;
    property filename: string read GetfileName write SetfileName;
    property params: string read Fparams write Setparams;
    property staging: TLZNullableBoolean read Fstaging write Setstaging;
    property production: TLZNullableBoolean read Fproduction
      write Setproduction;
  end;

  TInstallScripts = class(TLZModelList<TInstallScript>);

  TInstallScriptGroup = class(TLZModel)
  private
    FInstallScripts: TInstallScripts;
    Fenabled: Boolean;
    Fgroup: string;
    procedure Setenabled(const Value: Boolean);
    procedure Setgroup(const Value: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property enabled: Boolean read Fenabled write Setenabled;
    property group: string read Fgroup write Setgroup;
    property installScripts: TInstallScripts read FInstallScripts;
  end;

  TInstallScriptGroups = class(TLZModelList<TInstallScriptGroup>);

  TProcess = class(TLZModel)
  private
    FprocessName: string;
    FprocessFileName: string;
    procedure SetprocessName(const Value: string);
    procedure SetprocessFileName(const Value: string);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    property processName: string read FprocessName write SetprocessName;
    property processFileName: string read FprocessFileName
      write SetprocessFileName;
  end;

  TVersionInformation = class(TLZModel)
  private
    FfileName: string;
    FfileVersionMinor: string;
    FfileVersionRelease: string;
    FproductVersionMinor: string;
    FfileVersionMajor: string;
    FproductVersionRelease: string;
    FproductVersionMajor: string;
    FfileVersionBuild: string;
    FproductVersionBuild: string;
    procedure SetfileName(const Value: string);
    procedure SetfileVersionBuild(const Value: string);
    procedure SetfileVersionMajor(const Value: string);
    procedure SetfileVersionMinor(const Value: string);
    procedure SetfileVersionRelease(const Value: string);
    procedure SetproductVersionBuild(const Value: string);
    procedure SetproductVersionMajor(const Value: string);
    procedure SetproductVersionMinor(const Value: string);
    procedure SetproductVersionRelease(const Value: string);
  public
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property filename: string read FfileName write SetfileName;
    property fileVersionMajor: string read FfileVersionMajor
      write SetfileVersionMajor;
    property fileVersionMinor: string read FfileVersionMinor
      write SetfileVersionMinor;
    property fileVersionRelease: string read FfileVersionRelease
      write SetfileVersionRelease;
    property fileVersionBuild: string read FfileVersionBuild
      write SetfileVersionBuild;
    property productVersionMajor: string read FproductVersionMajor
      write SetproductVersionMajor;
    property productVersionMinor: string read FproductVersionMinor
      write SetproductVersionMinor;
    property productVersionRelease: string read FproductVersionRelease
      write SetproductVersionRelease;
    property productVersionBuild: string read FproductVersionBuild
      write SetproductVersionBuild;
  end;

  TProjectInformation = class(TLZModel)
  private
    FproductName: string;
    FlegalCopyright: string;
    FcompanyName: string;
    FfileDescription: string;
    FinternalName: string;
    FStagingVersion: TVersionInformation;
    FProductionVersion: TVersionInformation;
    FDevelopmentVersion: TVersionInformation;
    FcompanyURL: string;
    procedure SetcompanyName(const Value: string);
    procedure SetfileDescription(const Value: string);
    procedure SetinternalName(const Value: string);
    procedure SetlegalCopyright(const Value: string);
    procedure SetproductName(const Value: string);
    procedure SetcompanyURL(const Value: string);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property productName: string read FproductName write SetproductName;
    property internalName: string read FinternalName write SetinternalName;
    property companyName: string read FcompanyName write SetcompanyName;
    property fileDescription: string read FfileDescription
      write SetfileDescription;
    property legalCopyright: string read FlegalCopyright
      write SetlegalCopyright;
    property companyURL: string read FcompanyURL write SetcompanyURL;
    property stagingVersion: TVersionInformation read FStagingVersion;
    property productionVersion: TVersionInformation read FProductionVersion;
    property developmentVersion: TVersionInformation read FDevelopmentVersion;
  end;

  TProcesses = class(TLZModelList<TProcess>);

  TBuildProject = class(TLZModel)
  private
    FprojectInformation: TProjectInformation;
    Fvariables: TVariables;
    FTestProjectGroups: TTestProjectGroups;
    FProjectGroups: TProjectGroups;
    FpostCleanupScripts: TScripts;
    FpreBuildScripts: TScripts;
    FpostBuildScripts: TScripts;
    FbuildFolders: TFolders;
    FinstallScriptGroups: TInstallScriptGroups;
    FbuildCompleteScripts: TScripts;
    FcheckActiveProcesses: TProcesses;
    FreviewFiles: TFiles;
    FdefaultProjectGroups: string;
    FlogFolder: string;
    FdefaultInstallScriptGroups: string;
    FdefaultBuildCompleteScripts: string;
    FdefaultTestProjectGroups: string;
    Fgitpull: TLZNullableBoolean;
    procedure SetdefaultProjectGroups(const Value: string);
    procedure SetlogFolder(const Value: string);
    procedure SetdefaultInstallScriptGroups(const Value: string);
    procedure SetbuildCompleteScripts(const Value: TScripts);
    procedure SetdefaultBuildCompleteScripts(const Value: string);
    procedure SetreviewFiles(const Value: TFiles);
    procedure SetdefaultTestProjectGroups(const Value: string);
    procedure Setgitpull(const Value: TLZNullableBoolean);
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure FromJSONValue(AJSONValue: TJSONValue); override;
    function ToJSONValue: TJSONValue; override;
    property projectInformation: TProjectInformation read FprojectInformation;
    property logFolder: string read FlogFolder write SetlogFolder;

    property variables: TVariables read Fvariables;

    property buildFolders: TFolders read FbuildFolders;

    property defaultProjectGroups: string read FdefaultProjectGroups
      write SetdefaultProjectGroups;
    property defaultInstallScriptGroups: string read FdefaultInstallScriptGroups
      write SetdefaultInstallScriptGroups;
    property defaultTestProjectGroups: string read FdefaultTestProjectGroups
      write SetdefaultTestProjectGroups;
    property defaultBuildCompleteScripts: string
      read FdefaultBuildCompleteScripts write SetdefaultBuildCompleteScripts;

    property testProjectGroups: TTestProjectGroups read FTestProjectGroups;
    property projectGroups: TProjectGroups read FProjectGroups;
    property installScriptGroups: TInstallScriptGroups
      read FinstallScriptGroups;

    property postCleanupScripts: TScripts read FpostCleanupScripts;

    property preBuildScripts: TScripts read FpreBuildScripts;
    property postBuildScripts: TScripts read FpostBuildScripts;

    property buildCompleteScripts: TScripts read FbuildCompleteScripts
      write SetbuildCompleteScripts;

    property checkActiveProcesses: TProcesses read FcheckActiveProcesses;
    property reviewFiles: TFiles read FreviewFiles write SetreviewFiles;
    property gitpull: TLZNullableBoolean read Fgitpull write Setgitpull;
  end;

implementation

uses
  Lazy.Utils;

procedure TProject.Setconfigs(const Value: string);
begin
  Fconfigs := Value;
end;

procedure TProject.Setenabled(const Value: TLZNullableBoolean);
begin
  Fenabled := Value;
end;

procedure TProject.Setplatforms(const Value: string);
begin
  Fplatforms := Value;
end;

procedure TProject.Setproduction(const Value: TLZNullableBoolean);
begin
  Fproduction := Value;
end;

procedure TProject.Setproject(const Value: string);
begin
  Fproject := Value;
end;

procedure TProject.Setproperties(const Value: string);
begin
  Fproperties := Value;
end;

procedure TProject.Setstaging(const Value: TLZNullableBoolean);
begin
  Fstaging := Value;
end;

{ TVariable }

procedure TVariable.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  if Assigned(AJSONValue) then
  begin
    AJSONValue.TryGetValue<string>('variableName', FvariableName);
    AJSONValue.TryGetValue<string>('variableValue', FvariableValue);
    Fdevelopment.FromJSON(AJSONValue, 'development');
    Fstaging.FromJSON(AJSONValue, 'staging');
    Fproduction.FromJSON(AJSONValue, 'production');
  end;
end;

{ TFile }

procedure TFile.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  if Assigned(AJSONValue) then
  begin
    AJSONValue.TryGetValue<string>('filename', FfileName);
    AJSONValue.TryGetValue<string>('openWith', FopenWith);
  end;
end;

{ TFolder }

procedure TFolder.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  if Assigned(AJSONValue) then
  begin
    AJSONValue.TryGetValue<string>('folder', Ffolder);
    AJSONValue.TryGetValue<Boolean>('cleanupEnabled', FcleanupEnabled);
  end;
end;

{ TScript }

procedure TScript.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  if Assigned(AJSONValue) then
  begin
    AJSONValue.TryGetValue<Boolean>('enabled', Fenabled);
    AJSONValue.TryGetValue<string>('scriptName', FscriptName);
    AJSONValue.TryGetValue<string>('scriptSource', FscriptSource);
    Fstaging.FromJSON(AJSONValue, 'staging');
    Fproduction.FromJSON(AJSONValue, 'production');
  end;
end;

{ TProject }

procedure TProject.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  if Assigned(AJSONValue) then
  begin
    AJSONValue.TryGetValue<string>('project', Fproject);
    AJSONValue.TryGetValue<string>('platforms', Fplatforms);
    AJSONValue.TryGetValue<string>('configs', Fconfigs);
    AJSONValue.TryGetValue<string>('properties', Fproperties);
    Fstaging.FromJSON(AJSONValue, 'staging');
    Fproduction.FromJSON(AJSONValue, 'production');
  end;
end;

{ TTestProject }

procedure TTestProject.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  if Assigned(AJSONValue) then
  begin
    AJSONValue.TryGetValue<string>('params', Fparams);
    AJSONValue.TryGetValue<string>('command', Fcommand);
    AJSONValue.TryGetValue<string>('folder', Ffolder);
    FpostBuild.FromJSON(AJSONValue, 'postBuild');
  end;
end;

{ TProjectGroup }

constructor TProjectGroup.Create;
begin
  inherited;
  FProjects := TProjects.Create;
end;

destructor TProjectGroup.Destroy;
begin
  FreeAndNil(FProjects);
  inherited;
end;

procedure TProjectGroup.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  if Assigned(AJSONValue) and (AJSONValue is TJSONObject) then
  begin
    AJSONValue.TryGetValue<Boolean>('enabled', Fenabled);
    AJSONValue.TryGetValue<string>('group', Fgroup);

    if Assigned(FProjects) then
      FProjects.FromJSONValue(AJSONValue, False, 'projects');
  end;
end;

procedure TProjectGroup.Setenabled(const Value: Boolean);
begin
  Fenabled := Value;
end;

procedure TProjectGroup.Setgroup(const Value: string);
begin
  Fgroup := Value;
end;

{ TTestProjectGroup }

constructor TTestProjectGroup.Create;
begin
  inherited;
  FTestProjects := TTestProjects.Create;
end;

destructor TTestProjectGroup.Destroy;
begin
  FreeAndNil(FTestProjects);
  inherited;
end;

procedure TTestProjectGroup.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  if Assigned(AJSONValue) and (AJSONValue is TJSONObject) then
  begin
    AJSONValue.TryGetValue<Boolean>('enabled', Fenabled);
    AJSONValue.TryGetValue<string>('group', Fgroup);

    if Assigned(FTestProjects) then
      FTestProjects.FromJSONValue(AJSONValue, False, 'projects');
  end;
end;

{ TInstallScript }

procedure TInstallScript.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  if Assigned(AJSONValue) then
  begin
    AJSONValue.TryGetValue<Boolean>('enabled', Fenabled);
    AJSONValue.TryGetValue<string>('scriptName', FscriptName);
    AJSONValue.TryGetValue<string>('scriptType', FscriptType);
    AJSONValue.TryGetValue<string>('filename', FfileName);
    AJSONValue.TryGetValue<string>('params', Fparams);
    Fstaging.FromJSON(AJSONValue, 'staging');
    Fproduction.FromJSON(AJSONValue, 'production');
  end;
end;

{ TInstallScriptGroup }

constructor TInstallScriptGroup.Create;
begin
  inherited;
  FInstallScripts := TInstallScripts.Create;
end;

destructor TInstallScriptGroup.Destroy;
begin
  FreeAndNil(FInstallScripts);
  inherited;
end;

procedure TInstallScriptGroup.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  if Assigned(AJSONValue) and (AJSONValue is TJSONObject) then
  begin
    AJSONValue.TryGetValue<Boolean>('enabled', Fenabled);
    AJSONValue.TryGetValue<string>('group', Fgroup);

    if Assigned(FInstallScripts) then
      FInstallScripts.FromJSONValue(AJSONValue, False, 'installScripts');
  end;
end;

{ TProcess }

procedure TProcess.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  if Assigned(AJSONValue) then
  begin
    AJSONValue.TryGetValue<string>('processName', FprocessName);
    AJSONValue.TryGetValue<string>('processFileName', FprocessFileName);
  end;
end;

{ TVersionInformation }

procedure TVersionInformation.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  if Assigned(AJSONValue) then
  begin
    AJSONValue.TryGetValue<string>('filename', FfileName);
    AJSONValue.TryGetValue<string>('fileVersionMajor', FfileVersionMajor);
    AJSONValue.TryGetValue<string>('fileVersionMinor', FfileVersionMinor);
    AJSONValue.TryGetValue<string>('fileVersionRelease', FfileVersionRelease);
    AJSONValue.TryGetValue<string>('fileVersionBuild', FfileVersionBuild);
    AJSONValue.TryGetValue<string>('productVersionMajor', FproductVersionMajor);
    AJSONValue.TryGetValue<string>('productVersionMinor', FproductVersionMinor);
    AJSONValue.TryGetValue<string>('productVersionRelease',
      FproductVersionRelease);
    AJSONValue.TryGetValue<string>('productVersionBuild', FproductVersionBuild);
  end;
end;

{ TProjectInformation }

constructor TProjectInformation.Create;
begin
  inherited;
  FStagingVersion := TVersionInformation.Create;
  FProductionVersion := TVersionInformation.Create;
  FDevelopmentVersion := TVersionInformation.Create;
end;

destructor TProjectInformation.Destroy;
begin
  FreeAndNil(FStagingVersion);
  FreeAndNil(FProductionVersion);
  FreeAndNil(FDevelopmentVersion);
  inherited;
end;

procedure TProjectInformation.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  if Assigned(AJSONValue) and (AJSONValue is TJSONObject) then
  begin
    AJSONValue.TryGetValue<string>('productName', FproductName);
    AJSONValue.TryGetValue<string>('internalName', FinternalName);
    AJSONValue.TryGetValue<string>('companyName', FcompanyName);
    AJSONValue.TryGetValue<string>('fileDescription', FfileDescription);
    AJSONValue.TryGetValue<string>('legalCopyright', FlegalCopyright);
    AJSONValue.TryGetValue<string>('companyURL', FcompanyURL);

    if Assigned(FStagingVersion) then
      FStagingVersion.FromJSONValue((AJSONValue as TJSONObject)
        .GetValue('stagingVersion'));
    if Assigned(FProductionVersion) then
      FProductionVersion.FromJSONValue((AJSONValue as TJSONObject)
        .GetValue('productionVersion'));
    if Assigned(FDevelopmentVersion) then
      FDevelopmentVersion.FromJSONValue((AJSONValue as TJSONObject)
        .GetValue('developmentVersion'));
  end;
end;

{ TBuildProject }

constructor TBuildProject.Create;
begin
  inherited;
  FprojectInformation := TProjectInformation.Create;
  Fvariables := TVariables.Create;
  FTestProjectGroups := TTestProjectGroups.Create;
  FProjectGroups := TProjectGroups.Create;
  FpostCleanupScripts := TScripts.Create;
  FpreBuildScripts := TScripts.Create;
  FpostBuildScripts := TScripts.Create;
  FbuildFolders := TFolders.Create;
  FinstallScriptGroups := TInstallScriptGroups.Create;
  FbuildCompleteScripts := TScripts.Create;
  FcheckActiveProcesses := TProcesses.Create;
  FreviewFiles := TFiles.Create;

  FdefaultProjectGroups := '[ALL]';
  FdefaultInstallScriptGroups := '[ALL]';
  FdefaultBuildCompleteScripts := '[ALL]';
  FdefaultTestProjectGroups := '[ALL]';
end;

destructor TBuildProject.Destroy;
begin
  FreeAndNil(FprojectInformation);
  FreeAndNil(Fvariables);
  FreeAndNil(FTestProjectGroups);
  FreeAndNil(FProjectGroups);
  FreeAndNil(FpostCleanupScripts);
  FreeAndNil(FpreBuildScripts);
  FreeAndNil(FpostBuildScripts);
  FreeAndNil(FbuildFolders);
  FreeAndNil(FinstallScriptGroups);
  FreeAndNil(FbuildCompleteScripts);
  FreeAndNil(FcheckActiveProcesses);
  FreeAndNil(FreviewFiles);
  inherited;
end;

procedure TBuildProject.FromJSONValue(AJSONValue: TJSONValue);
begin
  inherited;
  if Assigned(AJSONValue) and (AJSONValue is TJSONObject) then
  begin
    AJSONValue.TryGetValue<string>('logFolder', FlogFolder);
    AJSONValue.TryGetValue<string>('defaultProjectGroups',
      FdefaultProjectGroups);
    AJSONValue.TryGetValue<string>('defaultInstallScriptGroups',
      FdefaultInstallScriptGroups);
    AJSONValue.TryGetValue<string>('defaultTestProjectGroups',
      FdefaultTestProjectGroups);
    AJSONValue.TryGetValue<string>('defaultBuildCompleteScripts',
      FdefaultBuildCompleteScripts);
    Fgitpull.FromJSON(AJSONValue, 'gitpull');

    if Assigned(FprojectInformation) then
      FprojectInformation.FromJSONValue((AJSONValue as TJSONObject)
        .GetValue('projectInformation'));
    if Assigned(Fvariables) then
      Fvariables.FromJSONValue(AJSONValue, False, 'variables');
    if Assigned(FTestProjectGroups) then
      FTestProjectGroups.FromJSONValue(AJSONValue, False, 'testProjectGroups');
    if Assigned(FProjectGroups) then
      FProjectGroups.FromJSONValue(AJSONValue, False, 'projectGroups');
    if Assigned(FpostCleanupScripts) then
      FpostCleanupScripts.FromJSONValue(AJSONValue, False,
        'postCleanupScripts');
    if Assigned(FpreBuildScripts) then
      FpreBuildScripts.FromJSONValue(AJSONValue, False, 'preBuildScripts');
    if Assigned(FpostBuildScripts) then
      FpostBuildScripts.FromJSONValue(AJSONValue, False, 'postBuildScripts');
    if Assigned(FbuildFolders) then
      FbuildFolders.FromJSONValue(AJSONValue, False, 'buildFolders');
    if Assigned(FinstallScriptGroups) then
      FinstallScriptGroups.FromJSONValue(AJSONValue, False,
        'installScriptGroups');
    if Assigned(FbuildCompleteScripts) then
      FbuildCompleteScripts.FromJSONValue(AJSONValue, False,
        'buildCompleteScripts');
    if Assigned(FcheckActiveProcesses) then
      FcheckActiveProcesses.FromJSONValue(AJSONValue, False,
        'checkActiveProcesses');
    if Assigned(FreviewFiles) then
      FreviewFiles.FromJSONValue(AJSONValue, False, 'reviewFiles');
  end;
end;

procedure TBuildProject.SetbuildCompleteScripts(const Value: TScripts);
begin
  FbuildCompleteScripts := Value;
end;

procedure TBuildProject.SetdefaultBuildCompleteScripts(const Value: string);
begin
  FdefaultBuildCompleteScripts := Value;
end;

procedure TBuildProject.SetdefaultInstallScriptGroups(const Value: string);
begin
  FdefaultInstallScriptGroups := Value;
end;

procedure TBuildProject.SetdefaultProjectGroups(const Value: string);
begin
  FdefaultProjectGroups := Value;
end;

procedure TBuildProject.SetdefaultTestProjectGroups(const Value: string);
begin
  FdefaultTestProjectGroups := Value;
end;

procedure TBuildProject.SetlogFolder(const Value: string);
begin
  FlogFolder := Value;
end;

procedure TBuildProject.SetreviewFiles(const Value: TFiles);
begin
  FreviewFiles := Value;
end;

procedure TBuildProject.Setgitpull(const Value: TLZNullableBoolean);
begin
  Fgitpull := Value;
end;

{ TFolder }

procedure TFolder.SetcleanupEnabled(const Value: Boolean);
begin
  FcleanupEnabled := Value;
end;

procedure TFolder.Setfolder(const Value: string);
begin
  Ffolder := Value;
end;

{ TScript }

procedure TScript.Setenabled(const Value: Boolean);
begin
  Fenabled := Value;
end;

procedure TScript.Setname(const Value: string);
begin
  FscriptName := Value;
end;

procedure TScript.Setproduction(const Value: TLZNullableBoolean);
begin
  Fproduction := Value;
end;

procedure TScript.SetscriptSource(const Value: string);
begin
  FscriptSource := Value;
end;

procedure TScript.Setstaging(const Value: TLZNullableBoolean);
begin
  Fstaging := Value;
end;

procedure TScript.Setselective(const Value: TLZNullableBoolean);
begin
  Fselective := Value;
end;

{ TInstallScript }

function TInstallScript.GetfileName: string;
begin
  Result := FfileName;
end;

function TInstallScript.GetscriptName: string;
begin
  Result := FscriptName;
  if TLZString.IsEmptyString(Result) then
    Result := ChangeFileExt(ExtractFileName(FfileName), '');
end;

procedure TInstallScript.Setenabled(const Value: Boolean);
begin
  Fenabled := Value;
end;

procedure TInstallScript.SetfileName(const Value: string);
begin
  FfileName := Value;
end;

procedure TInstallScript.Setparams(const Value: string);
begin
  Fparams := Value;
end;

procedure TInstallScript.Setproduction(const Value: TLZNullableBoolean);
begin
  Fproduction := Value;
end;

procedure TInstallScript.SetscriptName(const Value: string);
begin
  FscriptName := Value;
end;

procedure TInstallScript.SetscriptType(const Value: string);
begin
  FscriptType := Value;
end;

procedure TInstallScript.Setstaging(const Value: TLZNullableBoolean);
begin
  Fstaging := Value;
end;

{ TInstallScriptGroup }

procedure TInstallScriptGroup.Setenabled(const Value: Boolean);
begin
  Fenabled := Value;
end;

procedure TInstallScriptGroup.Setgroup(const Value: string);
begin
  Fgroup := Value;
end;

{ TProcess }

procedure TProcess.SetprocessFileName(const Value: string);
begin
  FprocessFileName := Value;
end;

procedure TProcess.SetprocessName(const Value: string);
begin
  FprocessName := Value;
end;

{ TFile }

procedure TFile.SetfileName(const Value: string);
begin
  FfileName := Value;
end;

procedure TFile.SetopenWith(const Value: string);
begin
  FopenWith := Value;
end;

{ TVersionInformation }

procedure TVersionInformation.SetfileName(const Value: string);
begin
  FfileName := Value;
end;

procedure TVersionInformation.SetfileVersionBuild(const Value: string);
begin
  FfileVersionBuild := Value;
end;

procedure TVersionInformation.SetfileVersionMajor(const Value: string);
begin
  FfileVersionMajor := Value;
end;

procedure TVersionInformation.SetfileVersionMinor(const Value: string);
begin
  FfileVersionMinor := Value;
end;

procedure TVersionInformation.SetfileVersionRelease(const Value: string);
begin
  FfileVersionRelease := Value;
end;

procedure TVersionInformation.SetproductVersionBuild(const Value: string);
begin
  FproductVersionBuild := Value;
end;

procedure TVersionInformation.SetproductVersionMajor(const Value: string);
begin
  FproductVersionMajor := Value;
end;

procedure TVersionInformation.SetproductVersionMinor(const Value: string);
begin
  FproductVersionMinor := Value;
end;

procedure TVersionInformation.SetproductVersionRelease(const Value: string);
begin
  FproductVersionRelease := Value;
end;

{ TProjectInformation }

procedure TProjectInformation.SetcompanyName(const Value: string);
begin
  FcompanyName := Value;
end;

procedure TProjectInformation.SetcompanyURL(const Value: string);
begin
  FcompanyURL := Value;
end;

procedure TProjectInformation.SetfileDescription(const Value: string);
begin
  FfileDescription := Value;
end;

procedure TProjectInformation.SetinternalName(const Value: string);
begin
  FinternalName := Value;
end;

procedure TProjectInformation.SetlegalCopyright(const Value: string);
begin
  FlegalCopyright := Value;
end;

procedure TProjectInformation.SetproductName(const Value: string);
begin
  FproductName := Value;
end;

{ TTestProject }

procedure TTestProject.Setcommand(const Value: string);
begin
  Fcommand := Value;
end;

procedure TTestProject.Setfolder(const Value: string);
begin
  Ffolder := Value;
end;

procedure TTestProject.Setparams(const Value: string);
begin
  Fparams := Value;
end;

procedure TTestProject.SetpostBuild(const Value: TLZNullableBoolean);
begin
  FpostBuild := Value;
end;

{ TTestProjectGroup }

procedure TTestProjectGroup.Setenabled(const Value: Boolean);
begin
  Fenabled := Value;
end;

procedure TTestProjectGroup.Setgroup(const Value: string);
begin
  Fgroup := Value;
end;

{ TVariable }

procedure TVariable.Setdevelopment(const Value: TLZNullableBoolean);
begin
  Fdevelopment := Value;
end;

procedure TVariable.Setproduction(const Value: TLZNullableBoolean);
begin
  Fproduction := Value;
end;

procedure TVariable.Setstaging(const Value: TLZNullableBoolean);
begin
  Fstaging := Value;
end;

procedure TVariable.SetvariableName(const Value: string);
begin
  FvariableName := Value;
end;

procedure TVariable.SetvariableValue(const Value: string);
begin
  FvariableValue := Value;
end;

{ TBuildProject }

function TBuildProject.ToJSONValue: TJSONValue;
begin
  // ToJSONValue not implemented - this project only reads configuration files
  Result := nil;
end;

{ TVersionInformation }

function TVersionInformation.ToJSONValue: TJSONValue;
begin
  // ToJSONValue not implemented - this project only reads configuration files
  Result := nil;
end;

{ TProjectInformation }

function TProjectInformation.ToJSONValue: TJSONValue;
begin
  // ToJSONValue not implemented - this project only reads configuration files
  Result := nil;
end;

end.
