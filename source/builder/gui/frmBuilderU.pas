unit frmBuilderU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, JvExComCtrls,
  JvProgressBar, Vcl.StdCtrls, Vcl.ExtCtrls,
  ProjectBuilder, Bob.Delphi, System.Actions,
  Vcl.ActnList, System.UITypes, Vcl.Mask, JvExMask, JvToolEdit, JvComCtrls,
  JvExControls, Vcl.Imaging.pngimage,
  Vcl.Buttons, Vcl.Menus, System.Notification, Bob.Git;

type
  TfrmBuilder = class(TForm)
    ActionList: TActionList;
    ActionExecute: TAction;
    ActionPreviewScript: TAction;
    ActionDetails: TAction;
    memoLog: TMemo;
    Splitter1: TSplitter;
    Panel1: TPanel;
    pnlSettings: TPanel;
    pnlProjectSelection: TPanel;
    LabelPanel1: TPanel;
    editFileName: TJvFilenameEdit;
    LabelPanel2: TPanel;
    comboDelphiVersions: TComboBox;
    TreeViewBuilder: TJvTreeView;
    Panel3: TGroupBox;
    Panel4: TPanel;
    cbCleanupEnabled: TCheckBox;
    cbCloseOnSuccess: TCheckBox;
    pnlStatus: TPanel;
    lblStatus: TLabel;
    ActionEditItem: TAction;
    ActionOpenFileLocation: TAction;
    PopupMenuTreeView: TPopupMenu;
    Button2: TBitBtn;
    btnExecute: TBitBtn;
    Button1: TBitBtn;
    Edit1: TMenuItem;
    Openfilelocation1: TMenuItem;
    NotificationCenter: TNotificationCenter;
    lblAnimation: TLabel;
    TimerBuilder: TTimer;
    comboBuildType: TComboBox;
    Label1: TLabel;
    Button3: TButton;
    ActionVersionInformation: TAction;
    ActionEditConfig: TAction;
    Button4: TButton;
    ActionGitPull: TAction;
    Button5: TButton;
    Label2: TLabel;
    Label3: TLabel;
    Button6: TButton;
    ActionAbout: TAction;
    Button7: TButton;
    ActionViewLog: TAction;
    procedure ActionDetailsExecute(Sender: TObject);
    procedure ActionEditItemExecute(Sender: TObject);
    procedure ActionEditItemUpdate(Sender: TObject);
    procedure ActionExecuteExecute(Sender: TObject);
    procedure ActionExecuteUpdate(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure ActionPreviewScriptExecute(Sender: TObject);
    procedure ActionPreviewScriptUpdate(Sender: TObject);
    procedure editFileNameChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormDestroy(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure NotificationCenterReceiveLocalNotification(Sender: TObject;
      ANotification: TNotification);
    procedure TimerBuilderTimer(Sender: TObject);
    procedure TreeViewBuilderDblClick(Sender: TObject);
    procedure TreeViewBuilderNodeCheckedChange(Sender: TObject;
      Node: TJvTreeNode);
    procedure ActionVersionInformationExecute(Sender: TObject);
    procedure ActionVersionInformationUpdate(Sender: TObject);
    procedure ActionEditConfigExecute(Sender: TObject);
    procedure ActionEditConfigUpdate(Sender: TObject);
    procedure ActionGitPullExecute(Sender: TObject);
    procedure ActionGitPullUpdate(Sender: TObject);
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionViewLogExecute(Sender: TObject);
    procedure Label3DblClick(Sender: TObject);
  private
    FAppActive: Boolean;
    FAutoClose: Boolean;
    FProjectBuilder: TProjectBuilder;
    FDelphiHelper: TDelphiHelper;
    FGitPuller: TBobGitPuller;
    FTreeNodeProjectGroups: TTreeNode;
    FTreeNodeTestProjectGroups: TTreeNode;
    FTreeNodeInstallScripGroups: TTreeNode;
    FTreeNodeBuildCompleteScripts: TTreeNode;
    FTreeNodePreBuildScripts: TTreeNode;
    FTreeNodePostBuildScripts: TTreeNode;
    FTreeNodeReviewFiles: TTreeNode;
    FTreeNodeVariables: TTreeNode;
    FTreeNodeGitPull: TTreeNode;
    FAniFrame: integer;
    FLog: TStringList;
    function GetAniFrame: string;
    procedure LoadProject;
    procedure GetProjectSettings;
    procedure SetProjectSettings;
    function IsProjectLoaded: Boolean;
    function GetBusy: Boolean;
    procedure ScrollToLastLine(Memo: TMemo);
    procedure CancelBuild;
    procedure ExecuteBuild;
    property IsBusy: Boolean read GetBusy;
    procedure OnBuildProgress(ASender: TObject; const AMessage: string;
      var ACancel: Boolean);
    procedure OnBuildComplete(Sender: TObject; const AComplete: Boolean;
      const AMessage: string);
    procedure OnProcessActive(ASender: TObject; const AProcessName: string;
      var AContinue: Boolean);
    procedure CheckParamters;
    procedure ShowDetails;
    procedure HideDetails;
    function IsDetailsVisible: Boolean;
    procedure AddNotification(AText: string; AName: string);
    procedure UpdateLog;
    function OnGitPrompt(const ATitle, AQuestion: string): Boolean;
  public
    { Public declarations }
  end;

var
  frmBuilder: TfrmBuilder;

implementation

{$R *.dfm}

uses
  Vcl.Lazy.Utils.Windows, Lazy.Log, Lazy.Types, Model.Build,
  Lazy.Model, frmVersionInformationU, dmResourcesU, frmBuilderConfigEditorU,
  Bob.Common, Bob.AboutForm, Lazy.Dialogs, Lazy.ExceptionDialog;

const
  AniFrame: array [0 .. 3] of char = ('|', '/', '-', '\');

procedure TfrmBuilder.ActionAboutExecute(Sender: TObject);
begin
  TBobAboutForm.ShowAboutDialog;
end;

procedure TfrmBuilder.ActionDetailsExecute(Sender: TObject);
begin
  if IsDetailsVisible then
  begin
    HideDetails;
  end
  else
  begin
    ShowDetails;
  end;
end;

procedure TfrmBuilder.ActionEditConfigExecute(Sender: TObject);
begin
  with TfrmBuilderConfigEditor.Create(Self) do
  begin
    try
      if Execute(editFileName.FileName) then
      begin
        LoadProject;
      end;
    finally
      Free;
    end;
  end;
end;

procedure TfrmBuilder.ActionEditConfigUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := FileExists(editFileName.FileName);
end;

procedure TfrmBuilder.ActionEditItemExecute(Sender: TObject);
var
  LTreeNode: TTreeNode;
  LFileName: TFileName;
  LOpenWith: string;
  LOpenFolder: Boolean;
begin
  LFileName := '';
  LOpenWith := '';
  LOpenFolder := Sender = ActionOpenFileLocation;
  LTreeNode := TreeViewBuilder.Selected;
  if Assigned(LTreeNode) and (LTreeNode.Data <> nil) then
  begin
    if TLZModel(LTreeNode.Data) is Tfile then
    begin
      LOpenWith := Tfile(LTreeNode.Data).openWith;
      LFileName := FProjectBuilder.ExpandFileName(Tfile(LTreeNode.Data)
        .FileName);
    end;
    if TLZModel(LTreeNode.Data) is TInstallScript then
    begin
      LFileName := FProjectBuilder.ExpandFileName(TInstallScript(LTreeNode.Data)
        .FileName);
    end;
    if TLZModel(LTreeNode.Data) is TProject then
    begin
      LFileName := FProjectBuilder.ExpandFileName
        (TProject(LTreeNode.Data).project);
      LOpenFolder := True;
    end;
  end;
  if FileExists(LFileName) then
  begin
    if LOpenFolder then
    begin
      LazyLog.Log(Self, format('Opening folder containing file "%s"',
        [LFileName]));
      TLZFile.OpenFolderAndSelectFile(LFileName);
    end
    else
    begin
      if TLZString.IsEmptyString(LOpenWith) then
      begin
        LazyLog.Log(Self,
          format('Opening default associated application for file "%s"',
          [LFileName]));
        TLZFile.OpenDefaultAssociatedApplication(LFileName);
      end
      else
      begin
        LazyLog.Log(Self, format('Opening "%s" application for file "%s"',
          [LOpenWith, LFileName]));
        TLZFile.ExecuteFile('open', LOpenWith, AnsiQuotedStr(LFileName, '"'),
          ExtractFilePath(LFileName), SW_SHOWNORMAL);
      end;
    end;
  end
  else
  begin
    if not TLZString.IsEmptyString(LFileName) then
    begin
      MessageDlg(format('"%s" does not exist.', [LFileName]), mtError,
        [mbOk], 0);
    end
    else
    begin
      LazyLog.Log(Self, format('No filename available', [LFileName]));
    end;
  end;
end;

procedure TfrmBuilder.ActionEditItemUpdate(Sender: TObject);
begin
  if (Sender is TAction) then
  begin
    (Sender as TAction).Enabled := Assigned(TreeViewBuilder.Selected) and
      (TreeViewBuilder.Selected.Data <> nil) and (not IsBusy) and
      (IsProjectLoaded);
    (Sender as TAction).Visible := (Sender as TAction).Enabled;
  end;
end;

procedure TfrmBuilder.ActionExecuteExecute(Sender: TObject);
begin
  if not IsBusy then
  begin
    ExecuteBuild;
  end
  else
  begin
    CancelBuild;
  end;
end;

procedure TfrmBuilder.ExecuteBuild;
begin
  if IsProjectLoaded then
  begin
    SetProjectSettings;
    FProjectBuilder.Build;
  end
  else
  begin
    MessageDlg('Project has not loaded.', mtError, [mbOk], 0);
  end;
end;

procedure TfrmBuilder.CancelBuild;
begin
  if MessageDlg('Cancel current build?.', mtConfirmation, [mbYes, mbNo], 0) = mrYes
  then
  begin
    FProjectBuilder.Cancel;
  end;
end;

procedure TfrmBuilder.ActionExecuteUpdate(Sender: TObject);
var
  LEnabled: Boolean;
  LCaption: string;
begin
  if IsBusy then
  begin
    LCaption := 'Cancel';
    LEnabled := True;
  end
  else
  begin
    LCaption := 'Execute';
    LEnabled := (IsProjectLoaded);
  end;
  (Sender as TAction).Caption := LCaption;
  (Sender as TAction).Enabled := LEnabled;
end;

procedure TfrmBuilder.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  pnlProjectSelection.Enabled := (not IsBusy);
end;

procedure TfrmBuilder.ActionPreviewScriptExecute(Sender: TObject);
var
  LScript: TStringList;
begin
  if not IsBusy then
  begin
    LScript := TStringList.Create;
    try
      if IsProjectLoaded then
      begin
        SetProjectSettings;
        if FProjectBuilder.GenerateScript(LScript) then
        begin
          TLZDialogs.MemoDialogShow(LScript.Text, 'Script preview');
        end;
      end
      else
      begin
        MessageDlg('Project has not loaded.', mtError, [mbOk], 0);
      end;
    finally
      FreeAndNil(LScript);
    end;
  end;
end;

procedure TfrmBuilder.ActionPreviewScriptUpdate(Sender: TObject);
var
  LEnabled: Boolean;
begin
  LEnabled := (not IsBusy) and (IsProjectLoaded);
  (Sender as TAction).Enabled := LEnabled;
end;

procedure TfrmBuilder.ActionVersionInformationExecute(Sender: TObject);
begin
  with TfrmVersionInformation.Create(Self) do
  begin
    if Execute(FProjectBuilder) then
    begin
      FProjectBuilder.Save;
    end;
  end;
end;

procedure TfrmBuilder.ActionVersionInformationUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := IsProjectLoaded;
end;

procedure TfrmBuilder.ActionViewLogExecute(Sender: TObject);
begin
  TLZDialogs.MemoDialogShow(LazyLogCache, 'Log');
end;

procedure TfrmBuilder.AddNotification(AText, AName: string);
var
  LNotification: TNotification;
begin
  LNotification := NotificationCenter.CreateNotification;
  try
    LNotification.Name := AName;
    LNotification.Title := Application.Title;
    LNotification.AlertBody := AText;
    LNotification.HasAction := True;
    NotificationCenter.PresentNotification(LNotification);
  finally
    LNotification.Free;
  end;
end;

procedure TfrmBuilder.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FGitPuller);
  FreeAndNil(FDelphiHelper);
  FreeAndNil(FProjectBuilder);
  FreeAndNil(FLog);
end;

procedure TfrmBuilder.FormCreate(Sender: TObject);
begin
  FProjectBuilder := TProjectBuilder.Create;
  FDelphiHelper := TDelphiHelper.Create;
  FLog := TStringList.Create;
  FGitPuller := TBobGitPuller.Create;

  FProjectBuilder.OnProcessActive := OnProcessActive;
  FProjectBuilder.OnBuildProgress := OnBuildProgress;
  FProjectBuilder.OnBuildComplete := OnBuildComplete;

  FGitPuller.OnPrompt := OnGitPrompt;

  TreeViewBuilder.Items.Clear;

  FTreeNodeGitPull := TreeViewBuilder.Items.Add(nil, 'Git Pull Before Build');
  FTreeNodeGitPull.ImageIndex := 3;
  FTreeNodeGitPull.SelectedIndex := 3;

  FTreeNodeReviewFiles := TreeViewBuilder.Items.Add(nil, 'Review Files');
  FTreeNodeReviewFiles.ImageIndex := 0;
  FTreeNodeReviewFiles.SelectedIndex := 0;

  FTreeNodeProjectGroups := TreeViewBuilder.Items.Add(nil, 'Project Groups');
  FTreeNodeProjectGroups.ImageIndex := 1;
  FTreeNodeProjectGroups.SelectedIndex := 1;
  FTreeNodeProjectGroups.DeleteChildren;

  FTreeNodeTestProjectGroups := TreeViewBuilder.Items.Add(nil,
    'Test Project Groups');
  FTreeNodeTestProjectGroups.ImageIndex := 1;
  FTreeNodeTestProjectGroups.SelectedIndex := 1;
  FTreeNodeTestProjectGroups.DeleteChildren;

  FTreeNodeInstallScripGroups := TreeViewBuilder.Items.Add(nil,
    'Install Script Groups');
  FTreeNodeInstallScripGroups.ImageIndex := 2;
  FTreeNodeInstallScripGroups.SelectedIndex := 2;
  FTreeNodeInstallScripGroups.DeleteChildren;

  FTreeNodeBuildCompleteScripts := TreeViewBuilder.Items.Add(nil,
    'Build Complete Scripts');
  FTreeNodeBuildCompleteScripts.ImageIndex := 3;
  FTreeNodeBuildCompleteScripts.SelectedIndex := 3;
  FTreeNodeBuildCompleteScripts.DeleteChildren;

  FTreeNodePreBuildScripts := TreeViewBuilder.Items.Add(nil,
    'Pre-Build Scripts');
  FTreeNodePreBuildScripts.ImageIndex := 3;
  FTreeNodePreBuildScripts.SelectedIndex := 3;
  FTreeNodePreBuildScripts.DeleteChildren;

  FTreeNodePostBuildScripts := TreeViewBuilder.Items.Add(nil,
    'Post-Build Scripts');
  FTreeNodePostBuildScripts.ImageIndex := 3;
  FTreeNodePostBuildScripts.SelectedIndex := 3;
  FTreeNodePostBuildScripts.DeleteChildren;

  FTreeNodeVariables := TreeViewBuilder.Items.Add(nil, 'Variables');
  FTreeNodeVariables.ImageIndex := 3;
  FTreeNodeVariables.SelectedIndex := 3;
  FTreeNodeVariables.DeleteChildren;

  TreeViewBuilder.Checked[FTreeNodeTestProjectGroups] := True;
  TreeViewBuilder.Checked[FTreeNodeProjectGroups] := True;
  TreeViewBuilder.Checked[FTreeNodeInstallScripGroups] := True;
  TreeViewBuilder.Checked[FTreeNodeBuildCompleteScripts] := True;
  TreeViewBuilder.Checked[FTreeNodePreBuildScripts] := True;
  TreeViewBuilder.Checked[FTreeNodePostBuildScripts] := True;
  TreeViewBuilder.Checked[FTreeNodeGitPull] := False;

  FDelphiHelper.AsStrings(comboDelphiVersions.Items);
  if comboDelphiVersions.Items.Count > 0 then
    comboDelphiVersions.ItemIndex := 0;
  HideDetails;
  FAppActive := False;
  FAutoClose := False;
  FAniFrame := 0;
end;

procedure TfrmBuilder.ScrollToLastLine(Memo: TMemo);
begin
  SendMessage(Memo.Handle, EM_LINESCROLL, 0, Memo.Lines.Count);
end;

procedure TfrmBuilder.OnBuildComplete(Sender: TObject; const AComplete: Boolean;
  const AMessage: string);
begin
  if AComplete then
  begin
    AddNotification(ExtractFileName(FProjectBuilder.FileName) +
      ' build complete.', 'Complete');
    lblStatus.Caption := 'Build Complete';
    lblAnimation.Caption := '';
    if cbCloseOnSuccess.Checked then
    begin
      FAutoClose := True;
    end;
  end
  else
  begin
    AddNotification(ExtractFileName(FProjectBuilder.FileName) +
      ' build failed.', 'Failed');
    lblStatus.Caption := 'Build Failed';
    lblAnimation.Caption := '';
  end;
  Self.Caption := 'Builder';
end;

procedure TfrmBuilder.OnBuildProgress(ASender: TObject; const AMessage: string;
  var ACancel: Boolean);
var
  LMessage: string;
begin
  FLog.Add(AMessage);
  if Pos('[STATUS]', AMessage) = 1 then
  begin
    LMessage := AMessage;
    LMessage := StringReplace(LMessage, '[STATUS]', '', [rfReplaceAll]);
    LMessage := TLZString.StringCleaner(LMessage, True, True);
    lblStatus.Caption := LMessage;
    Self.Caption := 'Builder - ' + LMessage;
  end;
  UpdateLog;
  Application.ProcessMessages;
end;

procedure TfrmBuilder.OnProcessActive(ASender: TObject;
  const AProcessName: string; var AContinue: Boolean);
begin
  AContinue := MessageDlg(format('"%s" is running. Continue?', [AProcessName]),
    mtWarning, [mbYes, mbNo], 0) = mrYes;
end;

procedure TfrmBuilder.CheckParamters;
var
  LParamValue: string;
  LFileName: TFileName;
  LErrorMessage: string;
begin
  LFileName := TBobCommon.GetBuilderFile(LErrorMessage);
  if not TLZString.IsEmptyString(LFileName) then
  begin
    editFileName.FileName := LFileName;
  end
  else if not TLZString.IsEmptyString(LErrorMessage) then
  begin
    // Show error message but don't halt - let user select file manually
    AddNotification(LErrorMessage, 'Warning');
  end;

  if TLZSystem.GetApplicationParameters('/CLEANUP', LParamValue) then
  begin
    cbCleanupEnabled.Checked := StrToBoolDef(LParamValue, True);
  end;
  if TLZSystem.GetApplicationParameters('/BUILD-PROJECTS', LParamValue) then
  begin
    TreeViewBuilder.Checked[FTreeNodeProjectGroups] :=
      StrToBoolDef(LParamValue, True);
  end;
  if TLZSystem.GetApplicationParameters('/BUILD-INSTALLATION', LParamValue) then
  begin
    TreeViewBuilder.Checked[FTreeNodeInstallScripGroups] :=
      StrToBoolDef(LParamValue, True);
  end;
  if TLZSystem.GetApplicationParameters('/DETAILED', LParamValue) then
  begin
    ShowDetails;
  end;
  if TLZSystem.GetApplicationParameters('/CLOSE', LParamValue) then
  begin
    cbCloseOnSuccess.Checked := StrToBoolDef(LParamValue, True);
  end;
  if TLZSystem.GetApplicationParameters('/EXECUTE', LParamValue) then
  begin
    ActionExecute.Execute;
  end;

end;

procedure TfrmBuilder.editFileNameChange(Sender: TObject);
begin
  LoadProject;
end;

procedure TfrmBuilder.FormActivate(Sender: TObject);
begin
  if not FAppActive then
  begin
    FAppActive := True;
    CheckParamters;
    TimerBuilder.Enabled := True;
  end;
end;

procedure TfrmBuilder.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  CanClose := not IsBusy;
  if not CanClose then
  begin
    CancelBuild;
  end;
end;

procedure TfrmBuilder.SetProjectSettings;
var
  LProjectGroup: TProjectGroup;
  LProject: TProject;
  LInstallGroup: TInstallScriptGroup;
  LInstallScript: TInstallScript;
  LScript: TScript;
  LProjectGroupNode, LProjectNode: TTreeNode;
  LProjectGroupIdx, LProjectIdx: integer;
begin
  if IsProjectLoaded then
  begin
    FProjectBuilder.BuildType := TBuildType(comboBuildType.ItemIndex);
    FProjectBuilder.CleanupEnabled := cbCleanupEnabled.Checked;
    FProjectBuilder.BuildProjectGroupsEnabled := TreeViewBuilder.Checked
      [FTreeNodeProjectGroups];
    FProjectBuilder.BuildInstallGroupsEnabled := TreeViewBuilder.Checked
      [FTreeNodeInstallScripGroups];
    FProjectBuilder.project.gitpull := TreeViewBuilder.Checked
      [FTreeNodeGitPull];

    // project group
    for LProjectGroupIdx := 0 to Pred(FTreeNodeProjectGroups.Count) do
    begin
      LProjectGroupNode := FTreeNodeProjectGroups.Item[LProjectGroupIdx];
      LProjectGroup := TProjectGroup(LProjectGroupNode.Data);
      LProjectGroup.Enabled := TreeViewBuilder.Checked[LProjectGroupNode];
      for LProjectIdx := 0 to Pred(LProjectGroupNode.Count) do
      begin
        LProjectNode := LProjectGroupNode.Item[LProjectIdx];
        LProject := TProject(LProjectNode.Data);
        LProject.Enabled := TreeViewBuilder.Checked[LProjectNode] and
          LProjectGroup.Enabled;
      end;
    end;

    // install
    for LProjectGroupIdx := 0 to Pred(FTreeNodeInstallScripGroups.Count) do
    begin
      LProjectGroupNode := FTreeNodeInstallScripGroups.Item[LProjectGroupIdx];
      LInstallGroup := TInstallScriptGroup(LProjectGroupNode.Data);
      LInstallGroup.Enabled := TreeViewBuilder.Checked[LProjectGroupNode];
      for LProjectIdx := 0 to Pred(LProjectGroupNode.Count) do
      begin
        LProjectNode := LProjectGroupNode.Item[LProjectIdx];
        LInstallScript := TInstallScript(LProjectNode.Data);
        LInstallScript.Enabled := TreeViewBuilder.Checked[LProjectNode] and
          LInstallGroup.Enabled;
      end;
    end;

    // completion scripts
    for LProjectGroupIdx := 0 to Pred(FTreeNodeBuildCompleteScripts.Count) do
    begin
      LProjectGroupNode := FTreeNodeBuildCompleteScripts.Item[LProjectGroupIdx];
      LScript := TScript(LProjectGroupNode.Data);
      LScript.Enabled := TreeViewBuilder.Checked[LProjectGroupNode] and
        TreeViewBuilder.Checked[FTreeNodeBuildCompleteScripts];
    end;
  end;
end;

procedure TfrmBuilder.ShowDetails;
begin
  memoLog.Visible := True;
  Self.Constraints.MinHeight := 600;
  UpdateLog;
end;

function TfrmBuilder.GetAniFrame: string;
begin
  REsult := '[ ' + AniFrame[FAniFrame] + ' ]';
  Inc(FAniFrame);
  if FAniFrame >= Length(AniFrame) then
  begin
    FAniFrame := 0;
  end;

end;

function TfrmBuilder.GetBusy: Boolean;
begin
  REsult := Assigned(FProjectBuilder) and (FProjectBuilder.State = stBusy);
end;

procedure TfrmBuilder.GetProjectSettings;
var
  LProjectGroup: TProjectGroup;
  LProject: TProject;
  LTestProjectGroup: TTestProjectGroup;
  LTestProject: TTestProject;
  LInstallGroup: TInstallScriptGroup;
  LInstallScript: TInstallScript;
  LScript: TScript;
  LVariable: TVariable;
  LFile: Tfile;
  LProjectGroupNode, LProjectNode: TTreeNode;
begin
  TreeViewBuilder.Items.BeginUpdate;
  try
    // Clear all tree nodes before repopulating
    FTreeNodeProjectGroups.DeleteChildren;
    FTreeNodeTestProjectGroups.DeleteChildren;
    FTreeNodeInstallScripGroups.DeleteChildren;
    FTreeNodeBuildCompleteScripts.DeleteChildren;
    FTreeNodePreBuildScripts.DeleteChildren;
    FTreeNodePostBuildScripts.DeleteChildren;
    FTreeNodeReviewFiles.DeleteChildren;
    FTreeNodeVariables.DeleteChildren;

    if IsProjectLoaded then
    begin
      TreeViewBuilder.Checked[FTreeNodeGitPull] :=
        FProjectBuilder.project.gitpull.GetValueOrDefault(False);

      for LFile in FProjectBuilder.project.reviewFiles do
      begin
        LProjectGroupNode := TreeViewBuilder.Items.AddChild
          (FTreeNodeReviewFiles, ExtractFileName(LFile.FileName));
        LProjectGroupNode.Data := LFile;
        LProjectGroupNode.ImageIndex := FTreeNodeReviewFiles.ImageIndex;
        LProjectGroupNode.SelectedIndex := LProjectGroupNode.ImageIndex;
      end;

      for LVariable in FProjectBuilder.project.variables do
      begin
        LProjectGroupNode := TreeViewBuilder.Items.AddChild(FTreeNodeVariables,
          LVariable.variableName);
        LProjectGroupNode.Data := LVariable;
        LProjectGroupNode.ImageIndex := FTreeNodeVariables.ImageIndex;
        LProjectGroupNode.SelectedIndex := FTreeNodeVariables.SelectedIndex;
      end;

      for LProjectGroup in FProjectBuilder.project.projectGroups do
      begin
        LProjectGroupNode := TreeViewBuilder.Items.AddChild
          (FTreeNodeProjectGroups, LProjectGroup.group);
        LProjectGroupNode.Data := LProjectGroup;
        LProjectGroupNode.ImageIndex := FTreeNodeProjectGroups.ImageIndex;
        LProjectGroupNode.SelectedIndex := LProjectGroupNode.ImageIndex;
        TreeViewBuilder.Checked[LProjectGroupNode] := LProjectGroup.Enabled;
        for LProject in LProjectGroup.projects do
        begin
          LProjectNode := TreeViewBuilder.Items.AddChild(LProjectGroupNode,
            ExtractFileName(LProject.project));
          LProjectNode.Data := LProject;
          LProjectNode.ImageIndex := FTreeNodeProjectGroups.ImageIndex;
          LProjectNode.SelectedIndex := LProjectGroupNode.ImageIndex;
          TreeViewBuilder.Checked[LProjectNode] :=
            LProject.Enabled.GetValueOrDefault(True);
        end;
      end;

      for LTestProjectGroup in FProjectBuilder.project.testProjectGroups do
      begin
        LProjectGroupNode := TreeViewBuilder.Items.AddChild
          (FTreeNodeTestProjectGroups, LTestProjectGroup.group);
        LProjectGroupNode.Data := LTestProjectGroup;
        LProjectGroupNode.ImageIndex := FTreeNodeTestProjectGroups.ImageIndex;
        LProjectGroupNode.SelectedIndex := LProjectGroupNode.ImageIndex;
        TreeViewBuilder.Checked[LProjectGroupNode] := LTestProjectGroup.Enabled;
        for LTestProject in LTestProjectGroup.projects do
        begin
          LProjectNode := TreeViewBuilder.Items.AddChild(LProjectGroupNode,
            ExtractFileName(LTestProject.project));
          LProjectNode.Data := LTestProject;
          LProjectNode.ImageIndex := FTreeNodeTestProjectGroups.ImageIndex;
          LProjectNode.SelectedIndex := LProjectGroupNode.ImageIndex;
          TreeViewBuilder.Checked[LProjectNode] :=
            LTestProject.Enabled.GetValueOrDefault(True);
        end;
      end;

      for LInstallGroup in FProjectBuilder.project.installScriptGroups do
      begin
        LProjectGroupNode := TreeViewBuilder.Items.AddChild
          (FTreeNodeInstallScripGroups, LInstallGroup.group);
        TreeViewBuilder.Checked[LProjectGroupNode] := LInstallGroup.Enabled;
        LProjectGroupNode.Data := LInstallGroup;
        LProjectGroupNode.ImageIndex := FTreeNodeInstallScripGroups.ImageIndex;
        LProjectGroupNode.SelectedIndex := LProjectGroupNode.ImageIndex;
        for LInstallScript in LInstallGroup.installScripts do
        begin
          LProjectNode := TreeViewBuilder.Items.AddChild(LProjectGroupNode,
            LInstallScript.scriptName);
          LProjectNode.Data := LInstallScript;
          LProjectNode.ImageIndex := FTreeNodeInstallScripGroups.ImageIndex;
          LProjectNode.SelectedIndex := LProjectGroupNode.ImageIndex;
          TreeViewBuilder.Checked[LProjectNode] := LInstallScript.Enabled;
        end;
      end;

      for LScript in FProjectBuilder.project.buildCompleteScripts do
      begin
        LProjectGroupNode := TreeViewBuilder.Items.AddChild
          (FTreeNodeBuildCompleteScripts, LScript.scriptName);
        LProjectGroupNode.Data := LScript;
        LProjectGroupNode.ImageIndex :=
          FTreeNodeBuildCompleteScripts.ImageIndex;
        LProjectGroupNode.SelectedIndex := LProjectGroupNode.ImageIndex;
        TreeViewBuilder.Checked[LProjectGroupNode] := LScript.Enabled;
      end;

      for LScript in FProjectBuilder.project.preBuildScripts do
      begin
        LProjectGroupNode := TreeViewBuilder.Items.AddChild
          (FTreeNodePreBuildScripts, LScript.scriptName);
        LProjectGroupNode.Data := LScript;
        LProjectGroupNode.ImageIndex := FTreeNodePreBuildScripts.ImageIndex;
        LProjectGroupNode.SelectedIndex := LProjectGroupNode.ImageIndex;
        TreeViewBuilder.Checked[LProjectGroupNode] := LScript.Enabled;
      end;

      for LScript in FProjectBuilder.project.postBuildScripts do
      begin
        LProjectGroupNode := TreeViewBuilder.Items.AddChild
          (FTreeNodePostBuildScripts, LScript.scriptName);
        LProjectGroupNode.Data := LScript;
        LProjectGroupNode.ImageIndex := FTreeNodePostBuildScripts.ImageIndex;
        LProjectGroupNode.SelectedIndex := LProjectGroupNode.ImageIndex;
        TreeViewBuilder.Checked[LProjectGroupNode] := LScript.Enabled;
      end;
    end;

  finally
    TreeViewBuilder.FullExpand;
    if TreeViewBuilder.Items.Count > 0 then
      TreeViewBuilder.Items[0].MakeVisible;
    TreeViewBuilder.Items.EndUpdate;

  end;
end;

procedure TfrmBuilder.HideDetails;
begin
  Self.Constraints.MinHeight := 400;
  if (WindowState <> wsMaximized) then
  begin
    Self.Height := Self.Height - memoLog.Height;
  end;
  memoLog.Visible := False;
end;

function TfrmBuilder.IsDetailsVisible: Boolean;
begin
  REsult := memoLog.Visible;
end;

function TfrmBuilder.IsProjectLoaded: Boolean;
begin
  REsult := Assigned(FProjectBuilder) and (FProjectBuilder.Loaded);
end;

procedure TfrmBuilder.Label3DblClick(Sender: TObject);
begin
  TLZExceptionDialog.ForceException;
end;

procedure TfrmBuilder.LoadProject;
begin
  if FileExists(editFileName.FileName) and (comboDelphiVersions.ItemIndex <> -1)
  then
  begin
    FProjectBuilder.FileName := editFileName.FileName;
    FProjectBuilder.DelphiVersion := FDelphiHelper.ByProductName
      (comboDelphiVersions.Items[comboDelphiVersions.ItemIndex]);
    if not IsProjectLoaded then
    begin
      TLZDialogs.ErrorMessage('Failed to load project');
    end;
  end;
  GetProjectSettings;
end;

procedure TfrmBuilder.NotificationCenterReceiveLocalNotification
  (Sender: TObject; ANotification: TNotification);
begin
  Application.BringToFront;
end;

procedure TfrmBuilder.TimerBuilderTimer(Sender: TObject);
begin
  if FAutoClose then
  begin
    Self.Close;
  end;
  if IsBusy then
  begin
    lblAnimation.Caption := GetAniFrame;
  end;
end;

procedure TfrmBuilder.TreeViewBuilderDblClick(Sender: TObject);
begin
  ActionEditItem.Execute;
end;

procedure TfrmBuilder.TreeViewBuilderNodeCheckedChange(Sender: TObject;
  Node: TJvTreeNode);
var
  LModel: TLZModel;
begin
  if not Node.Checked then
  begin
    LModel := TLZModel(Node.Data);
    if LModel is TProjectGroup then
    begin
      cbCleanupEnabled.Checked := False;
    end;
  end;
end;

procedure TfrmBuilder.UpdateLog;
begin
  if IsDetailsVisible then
  begin
    memoLog.Lines.BeginUpdate;
    try
      memoLog.Lines.Assign(FLog);
    finally
      memoLog.Lines.EndUpdate;
      ScrollToLastLine(memoLog);
    end;
  end;
end;

procedure TfrmBuilder.ActionGitPullExecute(Sender: TObject);
var
  LBasePath: string;
begin
  // Get the base path from the current builder file's directory
  if FileExists(editFileName.FileName) then
  begin
    LBasePath := ExtractFilePath(editFileName.FileName);
  end
  else
  begin
    LBasePath := GetCurrentDir;
  end;

  if TLZDialogs.ConfirmationMessage
    (format('Pull all Git repositories under:'#13#10'%s'#13#10#13#10'Continue?',
    [LBasePath]), [mbYes, mbNo]) = mrYes then
  begin
    UpdateLog;
    ShowDetails;
    Application.ProcessMessages;
    FGitPuller.PullAllRepositories(LBasePath);
    UpdateLog;
    AddNotification('Git pull operation completed', 'Git Pull');
  end;
end;

procedure TfrmBuilder.ActionGitPullUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := not IsBusy;
end;

function TfrmBuilder.OnGitPrompt(const ATitle, AQuestion: string): Boolean;
begin
  REsult := TLZDialogs.ConfirmationMessage(AQuestion, [mbYes, mbNo]) = mrYes;
end;

end.
