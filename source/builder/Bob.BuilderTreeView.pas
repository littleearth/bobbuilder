unit Bob.BuilderTreeView;

interface

uses
  System.Classes, VCl.Controls, VCl.ComCtrls, JvComCtrls, JvExComCtrls,
  JvExControls, Bob.BuilderModels, Bob.ProjectBuilder,
  System.Generics.Collections,
  Winapi.CommCtrl, Lazy.Model;

type
  TBuilderTreeView = class(TJvTreeView)
  private
    FProjectBuilder: TProjectBuilder;
    FUpdatingCheckbox: Boolean;
    FTreeNodeGitPull: TTreeNode;
    FTreeNodeReviewFiles: TTreeNode;
    FTreeNodeProjectGroups: TTreeNode;
    FTreeNodeTestProjectGroups: TTreeNode;
    FTreeNodeInstallScripGroups: TTreeNode;
    FTreeNodeBuildCompleteScripts: TTreeNode;
    FTreeNodePreBuildScripts: TTreeNode;
    FTreeNodePostBuildScripts: TTreeNode;
    FTreeNodeCodeFormatScripts: TTreeNode;
    FTreeNodeVariables: TTreeNode;
    procedure SetProjectBuilder(const Value: TProjectBuilder);
    function GetProjectFolder: string;
  protected
    procedure AddProjectToGroup(
      AGroup: TProjectGroup;
      ANode: TTreeNode);
    procedure AddTestProjectToGroup(
      AGroup: TTestProjectGroup;
      ANode: TTreeNode);
    procedure AddInstallScriptToGroup(
      AGroup: TInstallScriptGroup;
      ANode: TTreeNode);
    procedure CheckNotReadOnly;
    procedure SetParent(AParent: TWinControl); override;
    procedure Reset;
    function IsProjectLoaded: Boolean;
    procedure Edit(const Item: TTVItem); override;
    function CanEdit(Node: TTreeNode): Boolean; override;
    procedure DblClick; override;
    procedure TreeNodeCheckedChange(Sender: TObject); override;
    procedure UpdateModelFromNode(ANode: TTreeNode);
    procedure UpdateCheckboxFromModel(
      ANode: TTreeNode;
      AModel: TLZModel);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadProject;
    procedure SaveProject;
    // Add operations
    procedure AddProjectGroup;
    procedure AddProjectToCurrentGroup;
    procedure AddTestProjectGroup;
    procedure AddTestProjectToCurrentGroup;
    procedure AddInstallScriptGroup;
    procedure AddInstallScriptToCurrentGroup;
    procedure AddBuildCompleteScript;
    procedure AddPreBuildScript;
    procedure AddPostBuildScript;
    procedure AddCodeFormatScript;
    procedure AddReviewFile;
    procedure AddVariable;
    procedure Add;
    // Smart Add that determines which Add method to call based on selection
    // Edit operations
    function EditSelectedNode: Boolean;
    // Delete operations
    procedure DeleteSelectedNode;
    property ProjectBuilder: TProjectBuilder read FProjectBuilder
      write SetProjectBuilder;
    property ProjectFolder: string read GetProjectFolder;
    property TreeNodeGitPull: TTreeNode read FTreeNodeGitPull;
    property TreeNodeReviewFiles: TTreeNode read FTreeNodeReviewFiles;
    property TreeNodeProjectGroups: TTreeNode read FTreeNodeProjectGroups;
    property TreeNodeTestProjectGroups: TTreeNode
      read FTreeNodeTestProjectGroups;
    property TreeNodeInstallScripGroups: TTreeNode
      read FTreeNodeInstallScripGroups;
    property TreeNodeBuildCompleteScripts: TTreeNode
      read FTreeNodeBuildCompleteScripts;
    property TreeNodePreBuildScripts: TTreeNode read FTreeNodePreBuildScripts;
    property TreeNodePostBuildScripts: TTreeNode read FTreeNodePostBuildScripts;
    property TreeNodeCodeFormatScripts: TTreeNode
      read FTreeNodeCodeFormatScripts;
    property TreeNodeVariables: TTreeNode read FTreeNodeVariables;
  end;

implementation

uses
  System.SysUtils, VCl.Dialogs, frmModelEditorU;

{ TBuilderTreeView }

constructor TBuilderTreeView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ReadOnly := True;
  FUpdatingCheckbox := False;
  // Wire up event handlers
end;

destructor TBuilderTreeView.Destroy;
begin
  // Nodes are owned by the tree view and will be freed automatically
  inherited Destroy;
end;

procedure TBuilderTreeView.Reset;
begin
  Checkboxes := True;
  Items.Clear;

  FTreeNodeGitPull := Items.Add(nil, 'Git Pull Before Build');
  FTreeNodeGitPull.ImageIndex := 3;
  FTreeNodeGitPull.SelectedIndex := 3;

  FTreeNodeReviewFiles := Items.Add(nil, 'Review Files');
  FTreeNodeReviewFiles.ImageIndex := 0;
  FTreeNodeReviewFiles.SelectedIndex := 0;

  FTreeNodeProjectGroups := Items.Add(nil, 'Project Groups');
  FTreeNodeProjectGroups.ImageIndex := 1;
  FTreeNodeProjectGroups.SelectedIndex := 1;
  FTreeNodeProjectGroups.DeleteChildren;

  FTreeNodeTestProjectGroups := Items.Add(nil, 'Test Project Groups');
  FTreeNodeTestProjectGroups.ImageIndex := 1;
  FTreeNodeTestProjectGroups.SelectedIndex := 1;
  FTreeNodeTestProjectGroups.DeleteChildren;

  FTreeNodeInstallScripGroups := Items.Add(nil, 'Install Script Groups');
  FTreeNodeInstallScripGroups.ImageIndex := 2;
  FTreeNodeInstallScripGroups.SelectedIndex := 2;
  FTreeNodeInstallScripGroups.DeleteChildren;

  FTreeNodeBuildCompleteScripts := Items.Add(nil, 'Build Complete Scripts');
  FTreeNodeBuildCompleteScripts.ImageIndex := 3;
  FTreeNodeBuildCompleteScripts.SelectedIndex := 3;
  FTreeNodeBuildCompleteScripts.DeleteChildren;

  FTreeNodePreBuildScripts := Items.Add(nil, 'Pre-Build Scripts');
  FTreeNodePreBuildScripts.ImageIndex := 3;
  FTreeNodePreBuildScripts.SelectedIndex := 3;
  FTreeNodePreBuildScripts.DeleteChildren;

  FTreeNodePostBuildScripts := Items.Add(nil, 'Post-Build Scripts');
  FTreeNodePostBuildScripts.ImageIndex := 3;
  FTreeNodePostBuildScripts.SelectedIndex := 3;
  FTreeNodePostBuildScripts.DeleteChildren;

  FTreeNodeCodeFormatScripts := Items.Add(nil, 'Code Format Scripts');
  FTreeNodeCodeFormatScripts.ImageIndex := 3;
  FTreeNodeCodeFormatScripts.SelectedIndex := 3;
  FTreeNodeCodeFormatScripts.DeleteChildren;

  FTreeNodeVariables := Items.Add(nil, 'Variables');
  FTreeNodeVariables.ImageIndex := 3;
  FTreeNodeVariables.SelectedIndex := 3;
  FTreeNodeVariables.DeleteChildren;

  Checked[FTreeNodeTestProjectGroups] := True;
  Checked[FTreeNodeProjectGroups] := True;
  Checked[FTreeNodeInstallScripGroups] := True;
  Checked[FTreeNodeBuildCompleteScripts] := True;
  Checked[FTreeNodePreBuildScripts] := True;
  Checked[FTreeNodePostBuildScripts] := True;
  Checked[FTreeNodeCodeFormatScripts] := True;
  Checked[FTreeNodeGitPull] := False;
end;

function TBuilderTreeView.IsProjectLoaded: Boolean;
begin
  Result := Assigned(FProjectBuilder) and FProjectBuilder.Loaded;
end;

procedure TBuilderTreeView.LoadProject;
var
  LProjectGroup: TProjectGroup;
  LProject: TProject;
  LTestProjectGroup: TTestProjectGroup;
  LTestProject: TTestProject;
  LInstallGroup: TInstallScriptGroup;
  LInstallScript: TInstallScript;
  LScript: TScript;
  LVariable: TVariable;
  LFile: TFile;
  LProjectGroupNode, LProjectNode: TTreeNode;
begin
  if not Assigned(Parent) then
    Exit;
  Reset;
  if not Assigned(FProjectBuilder) then
    Exit;
  Items.BeginUpdate;
  try
    // Clear all tree nodes before repopulating
    FTreeNodeProjectGroups.DeleteChildren;
    FTreeNodeTestProjectGroups.DeleteChildren;
    FTreeNodeInstallScripGroups.DeleteChildren;
    FTreeNodeBuildCompleteScripts.DeleteChildren;
    FTreeNodePreBuildScripts.DeleteChildren;
    FTreeNodePostBuildScripts.DeleteChildren;
    FTreeNodeCodeFormatScripts.DeleteChildren;
    FTreeNodeReviewFiles.DeleteChildren;
    FTreeNodeVariables.DeleteChildren;

    if IsProjectLoaded and Assigned(FProjectBuilder.Project) then
    begin
      Checked[FTreeNodeGitPull] :=
        FProjectBuilder.Project.gitpull.GetValueOrDefault(False);

      for LFile in FProjectBuilder.Project.reviewFiles do
      begin
        LProjectGroupNode := Items.AddChild(FTreeNodeReviewFiles,
          ExtractFileName(LFile.filename));
        LProjectGroupNode.Data := LFile;
        LProjectGroupNode.ImageIndex := FTreeNodeReviewFiles.ImageIndex;
        LProjectGroupNode.SelectedIndex := LProjectGroupNode.ImageIndex;
      end;

      for LVariable in FProjectBuilder.Project.variables do
      begin
        LProjectGroupNode := Items.AddChild(FTreeNodeVariables,
          LVariable.variableName);
        LProjectGroupNode.Data := LVariable;
        LProjectGroupNode.ImageIndex := FTreeNodeVariables.ImageIndex;
        LProjectGroupNode.SelectedIndex := FTreeNodeVariables.SelectedIndex;
      end;

      for LProjectGroup in FProjectBuilder.Project.projectGroups do
      begin
        LProjectGroupNode := Items.AddChild(FTreeNodeProjectGroups,
          LProjectGroup.group);
        LProjectGroupNode.Data := LProjectGroup;
        LProjectGroupNode.ImageIndex := FTreeNodeProjectGroups.ImageIndex;
        LProjectGroupNode.SelectedIndex := LProjectGroupNode.ImageIndex;
        Checked[LProjectGroupNode] := LProjectGroup.Enabled;
        for LProject in LProjectGroup.projects do
        begin
          LProjectNode := Items.AddChild(LProjectGroupNode,
            ExtractFileName(LProject.Project));
          LProjectNode.Data := LProject;
          LProjectNode.ImageIndex := FTreeNodeProjectGroups.ImageIndex;
          LProjectNode.SelectedIndex := LProjectGroupNode.ImageIndex;
          Checked[LProjectNode] := LProject.Enabled.GetValueOrDefault(True);
        end;
      end;

      for LTestProjectGroup in FProjectBuilder.Project.testProjectGroups do
      begin
        LProjectGroupNode := Items.AddChild(FTreeNodeTestProjectGroups,
          LTestProjectGroup.group);
        LProjectGroupNode.Data := LTestProjectGroup;
        LProjectGroupNode.ImageIndex := FTreeNodeTestProjectGroups.ImageIndex;
        LProjectGroupNode.SelectedIndex := LProjectGroupNode.ImageIndex;
        Checked[LProjectGroupNode] := LTestProjectGroup.Enabled;
        for LTestProject in LTestProjectGroup.projects do
        begin
          LProjectNode := Items.AddChild(LProjectGroupNode,
            ExtractFileName(LTestProject.Project));
          LProjectNode.Data := LTestProject;
          LProjectNode.ImageIndex := FTreeNodeTestProjectGroups.ImageIndex;
          LProjectNode.SelectedIndex := LProjectGroupNode.ImageIndex;
          Checked[LProjectNode] := LTestProject.Enabled.GetValueOrDefault(True);
        end;
      end;

      for LInstallGroup in FProjectBuilder.Project.installScriptGroups do
      begin
        LProjectGroupNode := Items.AddChild(FTreeNodeInstallScripGroups,
          LInstallGroup.group);
        Checked[LProjectGroupNode] := LInstallGroup.Enabled;
        LProjectGroupNode.Data := LInstallGroup;
        LProjectGroupNode.ImageIndex := FTreeNodeInstallScripGroups.ImageIndex;
        LProjectGroupNode.SelectedIndex := LProjectGroupNode.ImageIndex;
        for LInstallScript in LInstallGroup.installScripts do
        begin
          LProjectNode := Items.AddChild(LProjectGroupNode,
            LInstallScript.scriptName);
          LProjectNode.Data := LInstallScript;
          LProjectNode.ImageIndex := FTreeNodeInstallScripGroups.ImageIndex;
          LProjectNode.SelectedIndex := LProjectGroupNode.ImageIndex;
          Checked[LProjectNode] := LInstallScript.Enabled;
        end;
      end;

      for LScript in FProjectBuilder.Project.buildCompleteScripts do
      begin
        LProjectGroupNode := Items.AddChild(FTreeNodeBuildCompleteScripts,
          LScript.scriptName);
        LProjectGroupNode.Data := LScript;
        LProjectGroupNode.ImageIndex :=
          FTreeNodeBuildCompleteScripts.ImageIndex;
        LProjectGroupNode.SelectedIndex := LProjectGroupNode.ImageIndex;
        Checked[LProjectGroupNode] := LScript.Enabled;
      end;

      for LScript in FProjectBuilder.Project.preBuildScripts do
      begin
        LProjectGroupNode := Items.AddChild(FTreeNodePreBuildScripts,
          LScript.scriptName);
        LProjectGroupNode.Data := LScript;
        LProjectGroupNode.ImageIndex := FTreeNodePreBuildScripts.ImageIndex;
        LProjectGroupNode.SelectedIndex := LProjectGroupNode.ImageIndex;
        Checked[LProjectGroupNode] := LScript.Enabled;
      end;

      for LScript in FProjectBuilder.Project.postBuildScripts do
      begin
        LProjectGroupNode := Items.AddChild(FTreeNodePostBuildScripts,
          LScript.scriptName);
        LProjectGroupNode.Data := LScript;
        LProjectGroupNode.ImageIndex := FTreeNodePostBuildScripts.ImageIndex;
        LProjectGroupNode.SelectedIndex := LProjectGroupNode.ImageIndex;
        Checked[LProjectGroupNode] := LScript.Enabled;
      end;

      for LScript in FProjectBuilder.Project.codeFormatScripts do
      begin
        LProjectGroupNode := Items.AddChild(FTreeNodeCodeFormatScripts,
          LScript.scriptName);
        LProjectGroupNode.Data := LScript;
        LProjectGroupNode.ImageIndex := FTreeNodeCodeFormatScripts.ImageIndex;
        LProjectGroupNode.SelectedIndex := LProjectGroupNode.ImageIndex;
        Checked[LProjectGroupNode] := LScript.Enabled;
      end;
    end;

  finally
    FullExpand;
    if Items.Count > 0 then
      Items[0].MakeVisible;
    Items.EndUpdate;
  end;
end;

procedure TBuilderTreeView.SaveProject;
begin
  FProjectBuilder.BuildProjectGroupsEnabled := Checked[FTreeNodeProjectGroups];
  FProjectBuilder.BuildInstallGroupsEnabled :=
    Checked[FTreeNodeInstallScripGroups];
  FProjectBuilder.Project.gitpull := Checked[FTreeNodeGitPull];

end;

procedure TBuilderTreeView.SetParent(AParent: TWinControl);
begin
  inherited;
  LoadProject;
end;

procedure TBuilderTreeView.SetProjectBuilder(const Value: TProjectBuilder);
begin
  if Value <> FProjectBuilder then
  begin
    FProjectBuilder := Value;
    LoadProject;
  end;
end;

procedure TBuilderTreeView.TreeNodeCheckedChange(Sender: TObject);
var
  LNode: TTreeNode;
  LIsChecked: Boolean;
  LChild: TTreeNode;
  LChildIndex: Integer;
begin
  if FUpdatingCheckbox then
    Exit;

  LNode := Selected;
  // Get the currently selected node that triggered the change
  if LNode = nil then
  begin
    inherited;
    Exit;
  end;

  try
    FUpdatingCheckbox := True;
    LIsChecked := Checked[LNode];

    // Update the model for the parent node
    if LNode.Data <> nil then
      UpdateModelFromNode(LNode);

    // Recursively update all child nodes
    LChildIndex := 0;
    while LChildIndex < LNode.Count do
    begin
      LChild := LNode.Item[LChildIndex];
      Checked[LChild] := LIsChecked;
      // Also update the child's model since we're changing its checkbox state
      if LChild.Data <> nil then
        UpdateModelFromNode(LChild);
      Inc(LChildIndex);
    end;
  finally
    FUpdatingCheckbox := False;
  end;

  inherited;
end;

function TBuilderTreeView.CanEdit(Node: TTreeNode): Boolean;
begin
  Result := False;
end;

procedure TBuilderTreeView.CheckNotReadOnly;
begin
  if ReadOnly then
    raise Exception.Create('TreeView is ReadOnly. Cannot modify items.');
end;

// Add operations
procedure TBuilderTreeView.AddProjectGroup;
var
  LGroup: TProjectGroup;
  LNode: TTreeNode;
  LEditor: TfrmModelEditor;
begin
  if not IsProjectLoaded then
    Exit;
  CheckNotReadOnly;
  LGroup := TProjectGroup.Create;
  try
    LEditor := TfrmModelEditor.Create(nil);
    try
      if LEditor.Execute(LGroup, ProjectFolder) then
      begin
        FProjectBuilder.Project.projectGroups.Add(LGroup);
        LNode := Items.AddChild(FTreeNodeProjectGroups, LGroup.group);
        LNode.Data := LGroup;
        LNode.ImageIndex := FTreeNodeProjectGroups.ImageIndex;
        LNode.SelectedIndex := LNode.ImageIndex;
        Checked[LNode] := True;
      end
      else
      begin
        LGroup.Free;
      end;
    finally
      LEditor.Free;
    end;
  except
    LGroup.Free;
    raise;
  end;
end;

procedure TBuilderTreeView.AddProjectToGroup(
  AGroup: TProjectGroup;
  ANode: TTreeNode);
var
  LProject: TProject;
  LNode: TTreeNode;
  LEditor: TfrmModelEditor;
begin
  LProject := TProject.Create;
  try
    LEditor := TfrmModelEditor.Create(nil);
    try
      if LEditor.Execute(LProject, ProjectFolder) then
      begin
        AGroup.projects.Add(LProject);
        LNode := Items.AddChild(ANode, ExtractFileName(LProject.Project));
        LNode.Data := LProject;
        LNode.ImageIndex := FTreeNodeProjectGroups.ImageIndex;
        LNode.SelectedIndex := ANode.ImageIndex;
        Checked[LNode] := True;
      end
      else
      begin
        LProject.Free;
      end;
    finally
      LEditor.Free;
    end;
  except
    LProject.Free;
    raise;
  end;
end;

procedure TBuilderTreeView.AddProjectToCurrentGroup;
var
  LSelected: TTreeNode;
begin
  LSelected := Selected;
  if (LSelected <> nil) and (LSelected.Data <> nil) then
  begin
    if TLZModel(LSelected.Data) is TProjectGroup then
      AddProjectToGroup(TProjectGroup(LSelected.Data), LSelected);
  end;
end;

procedure TBuilderTreeView.AddTestProjectGroup;
var
  LGroup: TTestProjectGroup;
  LNode: TTreeNode;
  LEditor: TfrmModelEditor;
begin
  if not IsProjectLoaded then
    Exit;
  CheckNotReadOnly;
  LGroup := TTestProjectGroup.Create;
  try
    LEditor := TfrmModelEditor.Create(nil);
    try
      if LEditor.Execute(LGroup, ProjectFolder) then
      begin
        FProjectBuilder.Project.testProjectGroups.Add(LGroup);
        LNode := Items.AddChild(FTreeNodeTestProjectGroups, LGroup.group);
        LNode.Data := LGroup;
        LNode.ImageIndex := FTreeNodeTestProjectGroups.ImageIndex;
        LNode.SelectedIndex := LNode.ImageIndex;
        Checked[LNode] := True;
      end
      else
      begin
        LGroup.Free;
      end;
    finally
      LEditor.Free;
    end;
  except
    LGroup.Free;
    raise;
  end;
end;

procedure TBuilderTreeView.AddTestProjectToGroup(
  AGroup: TTestProjectGroup;
  ANode: TTreeNode);
var
  LProject: TTestProject;
  LNode: TTreeNode;
  LEditor: TfrmModelEditor;
begin
  LProject := TTestProject.Create;
  try
    LEditor := TfrmModelEditor.Create(nil);
    try
      if LEditor.Execute(LProject, ProjectFolder) then
      begin
        AGroup.projects.Add(LProject);
        LNode := Items.AddChild(ANode, ExtractFileName(LProject.Project));
        LNode.Data := LProject;
        LNode.ImageIndex := FTreeNodeTestProjectGroups.ImageIndex;
        LNode.SelectedIndex := ANode.ImageIndex;
        Checked[LNode] := True;
      end
      else
      begin
        LProject.Free;
      end;
    finally
      LEditor.Free;
    end;
  except
    LProject.Free;
    raise;
  end;
end;

procedure TBuilderTreeView.AddTestProjectToCurrentGroup;
var
  LSelected: TTreeNode;
begin
  LSelected := Selected;
  if (LSelected <> nil) and (LSelected.Data <> nil) then
  begin
    if TLZModel(LSelected.Data) is TTestProjectGroup then
      AddTestProjectToGroup(TTestProjectGroup(LSelected.Data), LSelected);
  end;
end;

procedure TBuilderTreeView.AddInstallScriptGroup;
var
  LGroup: TInstallScriptGroup;
  LNode: TTreeNode;
  LEditor: TfrmModelEditor;
begin
  if not IsProjectLoaded then
    Exit;
  CheckNotReadOnly;
  LGroup := TInstallScriptGroup.Create;
  try
    LEditor := TfrmModelEditor.Create(nil);
    try
      if LEditor.Execute(LGroup, ProjectFolder) then
      begin
        FProjectBuilder.Project.installScriptGroups.Add(LGroup);
        LNode := Items.AddChild(FTreeNodeInstallScripGroups, LGroup.group);
        LNode.Data := LGroup;
        LNode.ImageIndex := FTreeNodeInstallScripGroups.ImageIndex;
        LNode.SelectedIndex := LNode.ImageIndex;
        Checked[LNode] := True;
      end
      else
      begin
        LGroup.Free;
      end;
    finally
      LEditor.Free;
    end;
  except
    LGroup.Free;
    raise;
  end;
end;

procedure TBuilderTreeView.AddInstallScriptToGroup(
  AGroup: TInstallScriptGroup;
  ANode: TTreeNode);
var
  LScript: TInstallScript;
  LNode: TTreeNode;
  LEditor: TfrmModelEditor;
begin
  LScript := TInstallScript.Create;
  try
    LEditor := TfrmModelEditor.Create(nil);
    try
      if LEditor.Execute(LScript, ProjectFolder) then
      begin
        AGroup.installScripts.Add(LScript);
        LNode := Items.AddChild(ANode, LScript.scriptName);
        LNode.Data := LScript;
        LNode.ImageIndex := FTreeNodeInstallScripGroups.ImageIndex;
        LNode.SelectedIndex := ANode.ImageIndex;
        Checked[LNode] := True;
      end
      else
      begin
        LScript.Free;
      end;
    finally
      LEditor.Free;
    end;
  except
    LScript.Free;
    raise;
  end;
end;

procedure TBuilderTreeView.AddInstallScriptToCurrentGroup;
var
  LSelected: TTreeNode;
begin
  LSelected := Selected;
  if (LSelected <> nil) and (LSelected.Data <> nil) then
  begin
    if TLZModel(LSelected.Data) is TInstallScriptGroup then
      AddInstallScriptToGroup(TInstallScriptGroup(LSelected.Data), LSelected);
  end;
end;

procedure TBuilderTreeView.AddBuildCompleteScript;
var
  LScript: TSelectiveScript;
  LNode: TTreeNode;
  LEditor: TfrmModelEditor;
begin
  if not IsProjectLoaded then
    Exit;
  CheckNotReadOnly;
  LScript := TSelectiveScript.Create;
  try
    LEditor := TfrmModelEditor.Create(nil);
    try
      if LEditor.Execute(LScript, ProjectFolder) then
      begin
        FProjectBuilder.Project.buildCompleteScripts.Add(LScript);
        LNode := Items.AddChild(FTreeNodeBuildCompleteScripts,
          LScript.scriptName);
        LNode.Data := LScript;
        LNode.ImageIndex := FTreeNodeBuildCompleteScripts.ImageIndex;
        LNode.SelectedIndex := LNode.ImageIndex;
        Checked[LNode] := True;
      end
      else
      begin
        LScript.Free;
      end;
    finally
      LEditor.Free;
    end;
  except
    LScript.Free;
    raise;
  end;
end;

procedure TBuilderTreeView.AddPreBuildScript;
var
  LScript: TSelectiveScript;
  LNode: TTreeNode;
  LEditor: TfrmModelEditor;
begin
  if not IsProjectLoaded then
    Exit;
  CheckNotReadOnly;
  LScript := TSelectiveScript.Create;
  try
    LEditor := TfrmModelEditor.Create(nil);
    try
      if LEditor.Execute(LScript, ProjectFolder) then
      begin
        FProjectBuilder.Project.preBuildScripts.Add(LScript);
        LNode := Items.AddChild(FTreeNodePreBuildScripts, LScript.scriptName);
        LNode.Data := LScript;
        LNode.ImageIndex := FTreeNodePreBuildScripts.ImageIndex;
        LNode.SelectedIndex := LNode.ImageIndex;
        Checked[LNode] := True;
      end
      else
      begin
        LScript.Free;
      end;
    finally
      LEditor.Free;
    end;
  except
    LScript.Free;
    raise;
  end;
end;

procedure TBuilderTreeView.AddPostBuildScript;
var
  LScript: TSelectiveScript;
  LNode: TTreeNode;
  LEditor: TfrmModelEditor;
begin
  if not IsProjectLoaded then
    Exit;
  CheckNotReadOnly;
  LScript := TSelectiveScript.Create;
  try
    LEditor := TfrmModelEditor.Create(nil);
    try
      if LEditor.Execute(LScript, ProjectFolder) then
      begin
        FProjectBuilder.Project.postBuildScripts.Add(LScript);
        LNode := Items.AddChild(FTreeNodePostBuildScripts, LScript.scriptName);
        LNode.Data := LScript;
        LNode.ImageIndex := FTreeNodePostBuildScripts.ImageIndex;
        LNode.SelectedIndex := LNode.ImageIndex;
        Checked[LNode] := True;
      end
      else
      begin
        LScript.Free;
      end;
    finally
      LEditor.Free;
    end;
  except
    LScript.Free;
    raise;
  end;
end;

procedure TBuilderTreeView.AddCodeFormatScript;
var
  LScript: TScript;
  LNode: TTreeNode;
  LEditor: TfrmModelEditor;
begin
  if not IsProjectLoaded then
    Exit;
  CheckNotReadOnly;
  LScript := TScript.Create;
  try
    LEditor := TfrmModelEditor.Create(nil);
    try
      if LEditor.Execute(LScript, ProjectFolder) then
      begin
        FProjectBuilder.Project.codeFormatScripts.Add(LScript);
        LNode := Items.AddChild(FTreeNodeCodeFormatScripts, LScript.scriptName);
        LNode.Data := LScript;
        LNode.ImageIndex := FTreeNodeCodeFormatScripts.ImageIndex;
        LNode.SelectedIndex := LNode.ImageIndex;
        Checked[LNode] := True;
      end
      else
      begin
        LScript.Free;
      end;
    finally
      LEditor.Free;
    end;
  except
    LScript.Free;
    raise;
  end;
end;

procedure TBuilderTreeView.AddReviewFile;
var
  LFile: TFile;
  LNode: TTreeNode;
  LEditor: TfrmModelEditor;
begin
  if not IsProjectLoaded then
    Exit;
  CheckNotReadOnly;
  LFile := TFile.Create;
  try
    LEditor := TfrmModelEditor.Create(nil);
    try
      if LEditor.Execute(LFile, ProjectFolder) then
      begin
        FProjectBuilder.Project.reviewFiles.Add(LFile);
        LNode := Items.AddChild(FTreeNodeReviewFiles,
          ExtractFileName(LFile.filename));
        LNode.Data := LFile;
        LNode.ImageIndex := FTreeNodeReviewFiles.ImageIndex;
        LNode.SelectedIndex := LNode.ImageIndex;
      end
      else
      begin
        LFile.Free;
      end;
    finally
      LEditor.Free;
    end;
  except
    LFile.Free;
    raise;
  end;
end;

procedure TBuilderTreeView.AddVariable;
var
  LVariable: TVariable;
  LNode: TTreeNode;
  LEditor: TfrmModelEditor;
begin
  if not IsProjectLoaded then
    Exit;
  CheckNotReadOnly;
  LVariable := TVariable.Create;
  try
    LEditor := TfrmModelEditor.Create(nil);
    try
      if LEditor.Execute(LVariable, ProjectFolder) then
      begin
        FProjectBuilder.Project.variables.Add(LVariable);
        LNode := Items.AddChild(FTreeNodeVariables, LVariable.variableName);
        LNode.Data := LVariable;
        LNode.ImageIndex := FTreeNodeVariables.ImageIndex;
        LNode.SelectedIndex := LNode.ImageIndex;
      end
      else
      begin
        LVariable.Free;
      end;
    finally
      LEditor.Free;
    end;
  except
    LVariable.Free;
    raise;
  end;
end;

procedure TBuilderTreeView.Add;
var
  LSelected: TTreeNode;
  LModel: TLZModel;
begin
  LSelected := Selected;

  // If no selection, cannot add
  if LSelected = nil then
    Exit;

  // If we have a data object, check its type to determine what to add
  if LSelected.Data <> nil then
  begin
    LModel := TLZModel(LSelected.Data);

    // Determine what type of item to add based on parent
    if LModel is TProjectGroup then
      AddProjectToCurrentGroup
    else if LModel is TTestProjectGroup then
      AddTestProjectToCurrentGroup
    else if LModel is TInstallScriptGroup then
      AddInstallScriptToCurrentGroup
  end
  else
  begin
    // For root level nodes, we need to check the tree structure
    if LSelected = FTreeNodeProjectGroups then
      AddProjectGroup
    else if LSelected = FTreeNodeTestProjectGroups then
      AddTestProjectGroup
    else if LSelected = FTreeNodeInstallScripGroups then
      AddInstallScriptGroup
    else if LSelected = FTreeNodeBuildCompleteScripts then
      AddBuildCompleteScript
    else if LSelected = FTreeNodePreBuildScripts then
      AddPreBuildScript
    else if LSelected = FTreeNodePostBuildScripts then
      AddPostBuildScript
    else if LSelected = FTreeNodeCodeFormatScripts then
      AddCodeFormatScript
    else if LSelected = FTreeNodeReviewFiles then
      AddReviewFile
    else if LSelected = FTreeNodeVariables then
      AddVariable;
  end;
end;

procedure TBuilderTreeView.Edit(const Item: TTVItem);
begin
  // Disable inline editing of node text
  // Text can only be changed through the Edit dialog
  Abort;
end;

// Edit operations
function TBuilderTreeView.EditSelectedNode: Boolean;
var
  LSelected: TTreeNode;
  LModel: TLZModel;
  LEditor: TfrmModelEditor;
  LDisplayName: string;
begin
  Result := False;
  CheckNotReadOnly;
  LSelected := Selected;
  if (LSelected = nil) or (LSelected.Data = nil) then
    Exit;

  LModel := TLZModel(LSelected.Data);
  LEditor := TfrmModelEditor.Create(nil);
  try
    if LEditor.Execute(LModel, ProjectFolder) then
    begin
      // Update the display name based on model type
      if LModel is TProjectGroup then
        LDisplayName := TProjectGroup(LModel).group
      else if LModel is TTestProjectGroup then
        LDisplayName := TTestProjectGroup(LModel).group
      else if LModel is TInstallScriptGroup then
        LDisplayName := TInstallScriptGroup(LModel).group
      else if LModel is TScript then
        LDisplayName := TScript(LModel).scriptName
      else if LModel is TVariable then
        LDisplayName := TVariable(LModel).variableName
      else if LModel is TFile then
        LDisplayName := ExtractFileName(TFile(LModel).filename)
      else if LModel is TProject then
        LDisplayName := ExtractFileName(TProject(LModel).Project)
      else if LModel is TTestProject then
        LDisplayName := ExtractFileName(TTestProject(LModel).Project)
      else if LModel is TInstallScript then
        LDisplayName := TInstallScript(LModel).scriptName
      else
        LDisplayName := '';

      if LDisplayName <> '' then
      begin
        LSelected.Text := LDisplayName;
        Result := True;
      end;

      // Update the checkbox to reflect the current model state
      UpdateCheckboxFromModel(LSelected, LModel);
    end;
  finally
    LEditor.Free;
  end;
end;

function TBuilderTreeView.GetProjectFolder: string;
begin
  Result := '';
  if Assigned(FProjectBuilder) then
    Result := FProjectBuilder.ProjectFolder;
end;

procedure TBuilderTreeView.DblClick;
begin
  if ReadOnly then
  begin
    inherited;
  end
  else
  begin
    EditSelectedNode;
  end;
end;

// Delete operations
procedure TBuilderTreeView.DeleteSelectedNode;
var
  LSelected: TTreeNode;
  LModel: TLZModel;
  LParent: TTreeNode;
  LParentData: TObject;
  LProjectGroup: TProjectGroup;
  LTestProjectGroup: TTestProjectGroup;
  LInstallScriptGroup: TInstallScriptGroup;
begin
  CheckNotReadOnly;
  LSelected := Selected;
  if (LSelected = nil) or (LSelected.Data = nil) then
    Exit;

  LModel := TLZModel(LSelected.Data);
  LParent := LSelected.Parent;
  LParentData := nil;
  try
    if (LParent <> nil) and (LParent.Data <> nil) then
      LParentData := TObject(LParent.Data);
  except
    // Ignore invalid pointer
  end;

  // Remove from collection
  if LModel is TProjectGroup then
    FProjectBuilder.Project.projectGroups.Remove(TProjectGroup(LModel))
  else if LModel is TTestProjectGroup then
    FProjectBuilder.Project.testProjectGroups.Remove(TTestProjectGroup(LModel))
  else if LModel is TInstallScriptGroup then
    FProjectBuilder.Project.installScriptGroups.Remove
      (TInstallScriptGroup(LModel))
  else if LModel is TProject then
  begin
    if Assigned(LParentData) and (LParentData is TProjectGroup) then
    begin
      LProjectGroup := TProjectGroup(LParentData);
      LProjectGroup.projects.Remove(TProject(LModel));
    end;
  end
  else if LModel is TTestProject then
  begin
    if Assigned(LParentData) and (LParentData is TTestProjectGroup) then
    begin
      LTestProjectGroup := TTestProjectGroup(LParentData);
      LTestProjectGroup.projects.Remove(TTestProject(LModel));
    end;
  end
  else if LModel is TInstallScript then
  begin
    if Assigned(LParentData) and (LParentData is TInstallScriptGroup) then
    begin
      LInstallScriptGroup := TInstallScriptGroup(LParentData);
      LInstallScriptGroup.installScripts.Remove(TInstallScript(LModel));
    end;
  end
  else if LModel is TScript then
  begin
    if LParent = FTreeNodeBuildCompleteScripts then
      FProjectBuilder.Project.buildCompleteScripts.Remove(TSelectiveScript(LModel))
    else if LParent = FTreeNodePreBuildScripts then
      FProjectBuilder.Project.preBuildScripts.Remove(TSelectiveScript(LModel))
    else if LParent = FTreeNodePostBuildScripts then
      FProjectBuilder.Project.postBuildScripts.Remove(TSelectiveScript(LModel))
    else if LParent = FTreeNodeCodeFormatScripts then
      FProjectBuilder.Project.codeFormatScripts.Remove(TScript(LModel));
  end
  else if LModel is TVariable then
    FProjectBuilder.Project.variables.Remove(TVariable(LModel))
  else if LModel is TFile then
    FProjectBuilder.Project.reviewFiles.Remove(TFile(LModel));

  // Remove from tree
  LSelected.Delete;
end;

procedure TBuilderTreeView.UpdateModelFromNode(ANode: TTreeNode);
var
  LModel: TLZModel;
begin
  if (ANode = nil) or (ANode.Data = nil) then
    Exit;

  LModel := TLZModel(ANode.Data);
  if not Assigned(LModel) then
    Exit;

  // Update enabled state based on checkbox state
  if LModel is TProjectGroup then
    TProjectGroup(LModel).Enabled := Checked[ANode]
  else if LModel is TTestProjectGroup then
    TTestProjectGroup(LModel).Enabled := Checked[ANode]
  else if LModel is TInstallScriptGroup then
    TInstallScriptGroup(LModel).Enabled := Checked[ANode]
  else if LModel is TScript then
    TScript(LModel).Enabled := Checked[ANode]
  else if LModel is TInstallScript then
    TInstallScript(LModel).Enabled := Checked[ANode]
  else if LModel is TProject then
  begin
    if Checked[ANode] then
      TProject(LModel).Enabled := True
    else
      TProject(LModel).Enabled := False;
  end
  else if LModel is TTestProject then
  begin
    if Checked[ANode] then
      TTestProject(LModel).Enabled := True
    else
      TTestProject(LModel).Enabled := False;
  end
  else if ANode = FTreeNodeGitPull then
    FProjectBuilder.Project.gitpull := Checked[ANode]
  else if ANode = FTreeNodeProjectGroups then
    FProjectBuilder.BuildProjectGroupsEnabled := Checked[ANode]
  else if ANode = FTreeNodeInstallScripGroups then
    FProjectBuilder.BuildInstallGroupsEnabled := Checked[ANode];
end;

procedure TBuilderTreeView.UpdateCheckboxFromModel(
  ANode: TTreeNode;
  AModel: TLZModel);
begin
  if (ANode = nil) or (AModel = nil) or FUpdatingCheckbox then
    Exit;

  try
    FUpdatingCheckbox := True;

    // Update checkbox based on model enabled state
    if AModel is TProjectGroup then
      Checked[ANode] := TProjectGroup(AModel).Enabled
    else if AModel is TTestProjectGroup then
      Checked[ANode] := TTestProjectGroup(AModel).Enabled
    else if AModel is TInstallScriptGroup then
      Checked[ANode] := TInstallScriptGroup(AModel).Enabled
    else if AModel is TScript then
      Checked[ANode] := TScript(AModel).Enabled
    else if AModel is TInstallScript then
      Checked[ANode] := TInstallScript(AModel).Enabled
    else if AModel is TProject then
      Checked[ANode] := TProject(AModel).Enabled.GetValueOrDefault(True)
    else if AModel is TTestProject then
      Checked[ANode] := TTestProject(AModel).Enabled.GetValueOrDefault(True)
    else if ANode = FTreeNodeGitPull then
      Checked[ANode] := FProjectBuilder.Project.gitpull.GetValueOrDefault(False)
    else if ANode = FTreeNodeProjectGroups then
      Checked[ANode] := FProjectBuilder.BuildProjectGroupsEnabled
    else if ANode = FTreeNodeInstallScripGroups then
      Checked[ANode] := FProjectBuilder.BuildInstallGroupsEnabled;
  finally
    FUpdatingCheckbox := False;
  end;
end;

end.
