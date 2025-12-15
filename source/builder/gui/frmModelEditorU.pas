unit frmModelEditorU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls,
  Vcl.ExtCtrls, Vcl.Buttons,
  Lazy.Model, Lazy.Nullable, Bob.BuilderModels, System.Generics.Collections,
  System.Rtti;

type
  TfrmModelEditor = class(TForm)
    PanelBottom: TPanel;
    BtnOK: TButton;
    BtnCancel: TButton;
    ScrollBoxMain: TScrollBox;
    lblModelClass: TLabel;
  private
    FModel: TLZModel;
    FControls: TDictionary<string, TControl>;
    FProjectFolder: string;
    procedure ClearControls;
    procedure CreateControlsForModel;
    procedure LoadModelData;
    procedure SaveModelData;
    procedure CreateNullableCheckBox(
      AParent: TWinControl;
      ALeft, ATop: Integer;
      ACaption: string;
      const AKey: string;
      var LCurrentTop: Integer);
    procedure LoadNullableCheckBox(
      const AKey: string;
      AValue: TLZNullableBoolean);
    // Model-specific control creation procedures
    procedure CreateProjectGroupControls(
      AGroupBox: TGroupBox;
      var LTop: Integer);
    procedure CreateTestProjectGroupControls(
      AGroupBox: TGroupBox;
      var LTop: Integer);
    procedure CreateInstallScriptGroupControls(
      AGroupBox: TGroupBox;
      var LTop: Integer);
    procedure CreateScriptControls(
      AGroupBox: TGroupBox;
      var LTop: Integer);
    procedure CreateSelectiveScriptControls(
      AGroupBox: TGroupBox;
      var LTop: Integer);
    procedure CreateVariableControls(
      AGroupBox: TGroupBox;
      var LTop: Integer);
    procedure CreateFileControls(
      AGroupBox: TGroupBox;
      var LTop: Integer);
    procedure CreateProjectControls(
      AGroupBox: TGroupBox;
      var LTop: Integer);
    procedure CreateTestProjectControls(
      AGroupBox: TGroupBox;
      var LTop: Integer);
    procedure CreateInstallScriptControls(
      AGroupBox: TGroupBox;
      var LTop: Integer);
    procedure BrowseButtonClick(ASender: TObject);
    function BrowseForFile(
      const AEditKey: string;
      const AFilter: string): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Execute(
      AModel: TLZModel;
      AProjectFolder: string): Boolean;
  end;

implementation

{$R *.dfm}
{ TfrmModelEditor }

constructor TfrmModelEditor.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FControls := TDictionary<string, TControl>.Create;
end;

destructor TfrmModelEditor.Destroy;
begin
  FControls.Free;
  inherited Destroy;
end;

function TfrmModelEditor.Execute(
  AModel: TLZModel;
  AProjectFolder: string): Boolean;
begin
  FModel := AModel;
  FProjectFolder := AProjectFolder;
  ClearControls;
  CreateControlsForModel;
  LoadModelData;
  Result := ShowModal = mrOk;
  if Result then
    SaveModelData;
end;

procedure TfrmModelEditor.ClearControls;
var
  LControl: TControl;
begin
  lblModelClass.Caption := '';
  for LControl in FControls.Values do
    LControl.Free;
  FControls.Clear;
end;

procedure TfrmModelEditor.CreateControlsForModel;
var
  LTop: Integer;
  LGroupBox: TGroupBox;
begin
  LGroupBox := TGroupBox.Create(Self);
  LGroupBox.Parent := ScrollBoxMain;
  LGroupBox.Caption := 'Properties';
  LGroupBox.Align := alTop;

  LTop := 16;

  lblModelClass.Caption := FModel.ClassName;

  if FModel is TProjectGroup then
    CreateProjectGroupControls(LGroupBox, LTop)
  else if FModel is TTestProjectGroup then
    CreateTestProjectGroupControls(LGroupBox, LTop)
  else if FModel is TInstallScriptGroup then
    CreateInstallScriptGroupControls(LGroupBox, LTop)
  else if FModel is TSelectiveScript then
    CreateSelectiveScriptControls(LGroupBox, LTop)
  else if FModel is TScript then
    CreateScriptControls(LGroupBox, LTop)
  else if FModel is TVariable then
    CreateVariableControls(LGroupBox, LTop)
  else if FModel is TFile then
    CreateFileControls(LGroupBox, LTop)
  else if FModel is TProject then
    CreateProjectControls(LGroupBox, LTop)
  else if FModel is TTestProject then
    CreateTestProjectControls(LGroupBox, LTop)
  else if FModel is TInstallScript then
    CreateInstallScriptControls(LGroupBox, LTop);

  LGroupBox.Height := LTop + 20;
end;

procedure TfrmModelEditor.LoadModelData;
var
  LControl: TControl;
begin
  if FModel is TProjectGroup then
  begin
    if FControls.TryGetValue('enabled', LControl) then
      TCheckBox(LControl).Checked := TProjectGroup(FModel).enabled;
    if FControls.TryGetValue('group', LControl) then
      TEdit(LControl).Text := TProjectGroup(FModel).group;
  end
  else if FModel is TTestProjectGroup then
  begin
    if FControls.TryGetValue('enabled', LControl) then
      TCheckBox(LControl).Checked := TTestProjectGroup(FModel).enabled;
    if FControls.TryGetValue('group', LControl) then
      TEdit(LControl).Text := TTestProjectGroup(FModel).group;
  end
  else if FModel is TInstallScriptGroup then
  begin
    if FControls.TryGetValue('enabled', LControl) then
      TCheckBox(LControl).Checked := TInstallScriptGroup(FModel).enabled;
    if FControls.TryGetValue('group', LControl) then
      TEdit(LControl).Text := TInstallScriptGroup(FModel).group;
  end
  else if FModel is TSelectiveScript then
  begin
    if FControls.TryGetValue('enabled', LControl) then
      TCheckBox(LControl).Checked := TSelectiveScript(FModel).enabled;
    if FControls.TryGetValue('scriptName', LControl) then
      TEdit(LControl).Text := TSelectiveScript(FModel).scriptName;
    if FControls.TryGetValue('scriptSource', LControl) then
      TEdit(LControl).Text := TSelectiveScript(FModel).scriptSource;
    LoadNullableCheckBox('staging', TSelectiveScript(FModel).staging);
    LoadNullableCheckBox('production', TSelectiveScript(FModel).production);
    LoadNullableCheckBox('selective', TSelectiveScript(FModel).selective);
  end
  else if FModel is TScript then
  begin
    if FControls.TryGetValue('enabled', LControl) then
      TCheckBox(LControl).Checked := TScript(FModel).enabled;
    if FControls.TryGetValue('scriptName', LControl) then
      TEdit(LControl).Text := TScript(FModel).scriptName;
    if FControls.TryGetValue('scriptSource', LControl) then
      TEdit(LControl).Text := TScript(FModel).scriptSource;
  end
  else if FModel is TVariable then
  begin
    if FControls.TryGetValue('variableName', LControl) then
      TEdit(LControl).Text := TVariable(FModel).variableName;
    if FControls.TryGetValue('variableValue', LControl) then
      TEdit(LControl).Text := TVariable(FModel).variableValue;
  end
  else if FModel is TFile then
  begin
    if FControls.TryGetValue('filename', LControl) then
      TEdit(LControl).Text := TFile(FModel).filename;
    if FControls.TryGetValue('openWith', LControl) then
      TEdit(LControl).Text := TFile(FModel).openWith;
  end
  else if FModel is TProject then
  begin
    LoadNullableCheckBox('enabled', TProject(FModel).enabled);
    if FControls.TryGetValue('project', LControl) then
      TEdit(LControl).Text := TProject(FModel).Project;
    if FControls.TryGetValue('platforms', LControl) then
      TEdit(LControl).Text := TProject(FModel).platforms;
    if FControls.TryGetValue('configs', LControl) then
      TEdit(LControl).Text := TProject(FModel).configs;
    if FControls.TryGetValue('properties', LControl) then
      TEdit(LControl).Text := TProject(FModel).properties;
    LoadNullableCheckBox('staging', TProject(FModel).staging);
    LoadNullableCheckBox('production', TProject(FModel).production);
  end
  else if FModel is TTestProject then
  begin
    LoadNullableCheckBox('enabled', TTestProject(FModel).enabled);
    if FControls.TryGetValue('project', LControl) then
      TEdit(LControl).Text := TTestProject(FModel).Project;
    if FControls.TryGetValue('platforms', LControl) then
      TEdit(LControl).Text := TTestProject(FModel).platforms;
    if FControls.TryGetValue('configs', LControl) then
      TEdit(LControl).Text := TTestProject(FModel).configs;
    if FControls.TryGetValue('properties', LControl) then
      TEdit(LControl).Text := TTestProject(FModel).properties;
    LoadNullableCheckBox('staging', TTestProject(FModel).staging);
    LoadNullableCheckBox('production', TTestProject(FModel).production);
    if FControls.TryGetValue('params', LControl) then
      TEdit(LControl).Text := TTestProject(FModel).params;
    if FControls.TryGetValue('command', LControl) then
      TEdit(LControl).Text := TTestProject(FModel).command;
    if FControls.TryGetValue('folder', LControl) then
      TEdit(LControl).Text := TTestProject(FModel).folder;
    LoadNullableCheckBox('postBuild', TTestProject(FModel).postBuild);
  end
  else if FModel is TInstallScript then
  begin
    if FControls.TryGetValue('enabled', LControl) then
      TCheckBox(LControl).Checked := TInstallScript(FModel).enabled;
    if FControls.TryGetValue('scriptName', LControl) then
      TEdit(LControl).Text := TInstallScript(FModel).scriptName;
    if FControls.TryGetValue('scriptType', LControl) then
      TEdit(LControl).Text := TInstallScript(FModel).scriptType;
    if FControls.TryGetValue('filename', LControl) then
      TEdit(LControl).Text := TInstallScript(FModel).filename;
    if FControls.TryGetValue('params', LControl) then
      TEdit(LControl).Text := TInstallScript(FModel).params;
    LoadNullableCheckBox('staging', TInstallScript(FModel).staging);
    LoadNullableCheckBox('production', TInstallScript(FModel).production);
  end;
end;

procedure TfrmModelEditor.SaveModelData;
var
  LControl: TControl;
begin
  if FModel is TProjectGroup then
  begin
    if FControls.TryGetValue('enabled', LControl) then
      TProjectGroup(FModel).enabled := TCheckBox(LControl).Checked;
    if FControls.TryGetValue('group', LControl) then
      TProjectGroup(FModel).group := TEdit(LControl).Text;
  end
  else if FModel is TTestProjectGroup then
  begin
    if FControls.TryGetValue('enabled', LControl) then
      TTestProjectGroup(FModel).enabled := TCheckBox(LControl).Checked;
    if FControls.TryGetValue('group', LControl) then
      TTestProjectGroup(FModel).group := TEdit(LControl).Text;
  end
  else if FModel is TInstallScriptGroup then
  begin
    if FControls.TryGetValue('enabled', LControl) then
      TInstallScriptGroup(FModel).enabled := TCheckBox(LControl).Checked;
    if FControls.TryGetValue('group', LControl) then
      TInstallScriptGroup(FModel).group := TEdit(LControl).Text;
  end
  else if FModel is TSelectiveScript then
  begin
    if FControls.TryGetValue('enabled', LControl) then
      TSelectiveScript(FModel).enabled := TCheckBox(LControl).Checked;
    if FControls.TryGetValue('scriptName', LControl) then
      TSelectiveScript(FModel).scriptName := TEdit(LControl).Text;
    if FControls.TryGetValue('scriptSource', LControl) then
      TSelectiveScript(FModel).scriptSource := TEdit(LControl).Text;
    // Handle nullable booleans
    if FControls.TryGetValue('staging', LControl) then
    begin
      case TCheckBox(LControl).State of
        cbGrayed:
          TSelectiveScript(FModel).staging.Clear;
        cbChecked:
          TSelectiveScript(FModel).staging := True;
        cbUnchecked:
          TSelectiveScript(FModel).staging := False;
      end;
    end;
    if FControls.TryGetValue('production', LControl) then
    begin
      case TCheckBox(LControl).State of
        cbGrayed:
          TSelectiveScript(FModel).production.Clear;
        cbChecked:
          TSelectiveScript(FModel).production := True;
        cbUnchecked:
          TSelectiveScript(FModel).production := False;
      end;
    end;
    if FControls.TryGetValue('selective', LControl) then
    begin
      case TCheckBox(LControl).State of
        cbGrayed:
          TSelectiveScript(FModel).selective.Clear;
        cbChecked:
          TSelectiveScript(FModel).selective := True;
        cbUnchecked:
          TSelectiveScript(FModel).selective := False;
      end;
    end;
  end
  else if FModel is TScript then
  begin
    if FControls.TryGetValue('enabled', LControl) then
      TScript(FModel).enabled := TCheckBox(LControl).Checked;
    if FControls.TryGetValue('scriptName', LControl) then
      TScript(FModel).scriptName := TEdit(LControl).Text;
    if FControls.TryGetValue('scriptSource', LControl) then
      TScript(FModel).scriptSource := TEdit(LControl).Text;
  end
  else if FModel is TVariable then
  begin
    if FControls.TryGetValue('variableName', LControl) then
      TVariable(FModel).variableName := TEdit(LControl).Text;
    if FControls.TryGetValue('variableValue', LControl) then
      TVariable(FModel).variableValue := TEdit(LControl).Text;
  end
  else if FModel is TFile then
  begin
    if FControls.TryGetValue('filename', LControl) then
      TFile(FModel).filename := TEdit(LControl).Text;
    if FControls.TryGetValue('openWith', LControl) then
      TFile(FModel).openWith := TEdit(LControl).Text;
  end
  else if FModel is TProject then
  begin
    // Handle nullable boolean enabled
    if FControls.TryGetValue('enabled', LControl) then
    begin
      case TCheckBox(LControl).State of
        cbGrayed:
          TProject(FModel).enabled.Clear;
        cbChecked:
          TProject(FModel).enabled := True;
        cbUnchecked:
          TProject(FModel).enabled := False;
      end;
    end;
    if FControls.TryGetValue('project', LControl) then
      TProject(FModel).Project := TEdit(LControl).Text;
    if FControls.TryGetValue('platforms', LControl) then
      TProject(FModel).platforms := TEdit(LControl).Text;
    if FControls.TryGetValue('configs', LControl) then
      TProject(FModel).configs := TEdit(LControl).Text;
    if FControls.TryGetValue('properties', LControl) then
      TProject(FModel).properties := TEdit(LControl).Text;
    // Handle nullable boolean staging
    if FControls.TryGetValue('staging', LControl) then
    begin
      case TCheckBox(LControl).State of
        cbGrayed:
          TProject(FModel).staging.Clear;
        cbChecked:
          TProject(FModel).staging := True;
        cbUnchecked:
          TProject(FModel).staging := False;
      end;
    end;
    // Handle nullable boolean production
    if FControls.TryGetValue('production', LControl) then
    begin
      case TCheckBox(LControl).State of
        cbGrayed:
          TProject(FModel).production.Clear;
        cbChecked:
          TProject(FModel).production := True;
        cbUnchecked:
          TProject(FModel).production := False;
      end;
    end;
  end
  else if FModel is TTestProject then
  begin
    // Handle nullable boolean enabled
    if FControls.TryGetValue('enabled', LControl) then
    begin
      case TCheckBox(LControl).State of
        cbGrayed:
          TTestProject(FModel).enabled.Clear;
        cbChecked:
          TTestProject(FModel).enabled := True;
        cbUnchecked:
          TTestProject(FModel).enabled := False;
      end;
    end;
    if FControls.TryGetValue('project', LControl) then
      TTestProject(FModel).Project := TEdit(LControl).Text;
    if FControls.TryGetValue('platforms', LControl) then
      TTestProject(FModel).platforms := TEdit(LControl).Text;
    if FControls.TryGetValue('configs', LControl) then
      TTestProject(FModel).configs := TEdit(LControl).Text;
    if FControls.TryGetValue('properties', LControl) then
      TTestProject(FModel).properties := TEdit(LControl).Text;
    // Handle nullable boolean staging
    if FControls.TryGetValue('staging', LControl) then
    begin
      case TCheckBox(LControl).State of
        cbGrayed:
          TTestProject(FModel).staging.Clear;
        cbChecked:
          TTestProject(FModel).staging := True;
        cbUnchecked:
          TTestProject(FModel).staging := False;
      end;
    end;
    // Handle nullable boolean production
    if FControls.TryGetValue('production', LControl) then
    begin
      case TCheckBox(LControl).State of
        cbGrayed:
          TTestProject(FModel).production.Clear;
        cbChecked:
          TTestProject(FModel).production := True;
        cbUnchecked:
          TTestProject(FModel).production := False;
      end;
    end;
    // TTestProject specific properties
    if FControls.TryGetValue('params', LControl) then
      TTestProject(FModel).params := TEdit(LControl).Text;
    if FControls.TryGetValue('command', LControl) then
      TTestProject(FModel).command := TEdit(LControl).Text;
    if FControls.TryGetValue('folder', LControl) then
      TTestProject(FModel).folder := TEdit(LControl).Text;
    // Handle nullable boolean postBuild
    if FControls.TryGetValue('postBuild', LControl) then
    begin
      case TCheckBox(LControl).State of
        cbGrayed:
          TTestProject(FModel).postBuild.Clear;
        cbChecked:
          TTestProject(FModel).postBuild := True;
        cbUnchecked:
          TTestProject(FModel).postBuild := False;
      end;
    end;
  end
  else if FModel is TInstallScript then
  begin
    if FControls.TryGetValue('enabled', LControl) then
      TInstallScript(FModel).enabled := TCheckBox(LControl).Checked;
    if FControls.TryGetValue('scriptName', LControl) then
      TInstallScript(FModel).scriptName := TEdit(LControl).Text;
    if FControls.TryGetValue('scriptType', LControl) then
      TInstallScript(FModel).scriptType := TEdit(LControl).Text;
    if FControls.TryGetValue('filename', LControl) then
      TInstallScript(FModel).filename := TEdit(LControl).Text;
    if FControls.TryGetValue('params', LControl) then
      TInstallScript(FModel).params := TEdit(LControl).Text;
    // Handle nullable booleans
    if FControls.TryGetValue('staging', LControl) then
    begin
      case TCheckBox(LControl).State of
        cbGrayed:
          TInstallScript(FModel).staging.Clear;
        cbChecked:
          TInstallScript(FModel).staging := True;
        cbUnchecked:
          TInstallScript(FModel).staging := False;
      end;
    end;
    if FControls.TryGetValue('production', LControl) then
    begin
      case TCheckBox(LControl).State of
        cbGrayed:
          TInstallScript(FModel).production.Clear;
        cbChecked:
          TInstallScript(FModel).production := True;
        cbUnchecked:
          TInstallScript(FModel).production := False;
      end;
    end;
  end;
end;

procedure TfrmModelEditor.CreateNullableCheckBox(
  AParent: TWinControl;
  ALeft, ATop: Integer;
  ACaption: string;
  const AKey: string;
  var LCurrentTop: Integer);
var
  LCheckBox: TCheckBox;
begin
  LCheckBox := TCheckBox.Create(Self);
  LCheckBox.Parent := AParent;
  LCheckBox.Left := ALeft;
  LCheckBox.Top := ATop;
  LCheckBox.Caption := ACaption;
  LCheckBox.Width := AParent.ClientWidth - ALeft - 16;
  LCheckBox.AllowGrayed := True;
  LCheckBox.State := cbGrayed; // Default to null
  LCheckBox.Anchors := [akLeft, akTop, akRight];
  FControls.Add(AKey, LCheckBox);
  Inc(LCurrentTop, 25); // Account for checkbox height
end;

procedure TfrmModelEditor.LoadNullableCheckBox(
  const AKey: string;
  AValue: TLZNullableBoolean);
var
  LControl: TControl;
begin
  if FControls.TryGetValue(AKey, LControl) then
  begin
    if not AValue.HasValue then
      TCheckBox(LControl).State := cbGrayed
    else if AValue.Value then
      TCheckBox(LControl).State := cbChecked
    else
      TCheckBox(LControl).State := cbUnchecked;
  end;
end;

procedure TfrmModelEditor.CreateProjectGroupControls(
  AGroupBox: TGroupBox;
  var LTop: Integer);
var
  LLabel: TLabel;
  LEdit: TEdit;
  LCheckBox: TCheckBox;
begin
  // enabled
  LCheckBox := TCheckBox.Create(Self);
  LCheckBox.Parent := AGroupBox;
  LCheckBox.Left := 8;
  LCheckBox.Top := LTop;
  LCheckBox.Caption := 'Enabled';
  LCheckBox.Width := AGroupBox.Width - 16;
  FControls.Add('enabled', LCheckBox);
  Inc(LTop, 25);

  // group
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Group:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 88;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('group', LEdit);
  Inc(LTop, 25);
end;

procedure TfrmModelEditor.CreateTestProjectGroupControls(
  AGroupBox: TGroupBox;
  var LTop: Integer);
var
  LLabel: TLabel;
  LEdit: TEdit;
  LCheckBox: TCheckBox;
begin
  // enabled
  LCheckBox := TCheckBox.Create(Self);
  LCheckBox.Parent := AGroupBox;
  LCheckBox.Left := 8;
  LCheckBox.Top := LTop;
  LCheckBox.Caption := 'Enabled';
  LCheckBox.Width := AGroupBox.Width - 16;
  FControls.Add('enabled', LCheckBox);
  Inc(LTop, 25);

  // group
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Group:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 88;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('group', LEdit);
  Inc(LTop, 25);
end;

procedure TfrmModelEditor.CreateInstallScriptGroupControls(
  AGroupBox: TGroupBox;
  var LTop: Integer);
var
  LLabel: TLabel;
  LEdit: TEdit;
  LCheckBox: TCheckBox;
begin
  // enabled
  LCheckBox := TCheckBox.Create(Self);
  LCheckBox.Parent := AGroupBox;
  LCheckBox.Left := 8;
  LCheckBox.Top := LTop;
  LCheckBox.Caption := 'Enabled';
  LCheckBox.Width := AGroupBox.Width - 16;
  FControls.Add('enabled', LCheckBox);
  Inc(LTop, 25);

  // group
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Group:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 88;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('group', LEdit);
  Inc(LTop, 25);
end;

procedure TfrmModelEditor.CreateScriptControls(
  AGroupBox: TGroupBox;
  var LTop: Integer);
var
  LLabel: TLabel;
  LEdit: TEdit;
  LCheckBox: TCheckBox;
begin
  // enabled
  LCheckBox := TCheckBox.Create(Self);
  LCheckBox.Parent := AGroupBox;
  LCheckBox.Left := 8;
  LCheckBox.Top := LTop;
  LCheckBox.Caption := 'Enabled';
  LCheckBox.Width := AGroupBox.Width - 16;
  FControls.Add('enabled', LCheckBox);
  Inc(LTop, 25);

  // scriptName
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Script Name:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 88;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('scriptName', LEdit);
  Inc(LTop, 25);

  // scriptSource
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Script Source:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 88;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('scriptSource', LEdit);
  Inc(LTop, 25);

end;

procedure TfrmModelEditor.CreateSelectiveScriptControls(
  AGroupBox: TGroupBox;
  var LTop: Integer);
var
  LLabel: TLabel;
  LEdit: TEdit;
  LCheckBox: TCheckBox;
begin
  // enabled
  LCheckBox := TCheckBox.Create(Self);
  LCheckBox.Parent := AGroupBox;
  LCheckBox.Left := 8;
  LCheckBox.Top := LTop;
  LCheckBox.Caption := 'Enabled';
  LCheckBox.Width := AGroupBox.Width - 16;
  FControls.Add('enabled', LCheckBox);
  Inc(LTop, 25);

  // scriptName
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Script Name:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 88;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('scriptName', LEdit);
  Inc(LTop, 25);

  // scriptSource
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Script Source:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 88;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('scriptSource', LEdit);
  Inc(LTop, 25);

  // staging
  CreateNullableCheckBox(AGroupBox, 8, LTop, 'Staging', 'staging', LTop);

  // production
  CreateNullableCheckBox(AGroupBox, 8, LTop, 'Production', 'production', LTop);

  // selective
  CreateNullableCheckBox(AGroupBox, 8, LTop, 'Selective', 'selective', LTop);
end;

procedure TfrmModelEditor.CreateVariableControls(
  AGroupBox: TGroupBox;
  var LTop: Integer);
var
  LLabel: TLabel;
  LEdit: TEdit;
begin
  // variableName
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Variable Name:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 100;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 108;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('variableName', LEdit);
  Inc(LTop, 30);

  // variableValue
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Value:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 100;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 108;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('variableValue', LEdit);
  Inc(LTop, 25);
end;

procedure TfrmModelEditor.CreateFileControls(
  AGroupBox: TGroupBox;
  var LTop: Integer);
var
  LLabel: TLabel;
  LEdit: TEdit;
begin
  // filename
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Filename:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 128;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('filename', LEdit);
  // Browse button
  with TSpeedButton.Create(Self) do
  begin
    Parent := AGroupBox;
    Left := AGroupBox.Width - 46;
    Top := LTop - 2;
    Width := 25;
    Height := 23;
    Caption := '...';
    Hint := 'filename';
    OnClick := BrowseButtonClick;
    Anchors := [akTop, akRight];
  end;
  Inc(LTop, 25);

  // openWith
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Open With:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 88;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('openWith', LEdit);
  Inc(LTop, 25);
end;

procedure TfrmModelEditor.CreateProjectControls(
  AGroupBox: TGroupBox;
  var LTop: Integer);
var
  LLabel: TLabel;
  LEdit: TEdit;
begin
  // enabled (nullable boolean - needs 3-state checkbox)
  CreateNullableCheckBox(AGroupBox, 8, LTop, 'Enabled', 'enabled', LTop);

  // Project
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Project:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 128;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('project', LEdit);
  // Browse button
  with TSpeedButton.Create(Self) do
  begin
    Parent := AGroupBox;
    Left := AGroupBox.Width - 46;
    Top := LTop - 2;
    Width := 25;
    Height := 23;
    Caption := '...';
    Hint := 'project';
    OnClick := BrowseButtonClick;
    Anchors := [akTop, akRight];
  end;
  Inc(LTop, 25);

  // platforms
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Platforms:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 88;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('platforms', LEdit);
  Inc(LTop, 25);

  // configs
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Configs:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 88;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('configs', LEdit);
  Inc(LTop, 25);

  // properties
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Properties:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 88;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('properties', LEdit);
  Inc(LTop, 25);

  // staging (nullable boolean)
  CreateNullableCheckBox(AGroupBox, 8, LTop, 'Staging', 'staging', LTop);

  // production (nullable boolean)
  CreateNullableCheckBox(AGroupBox, 8, LTop, 'Production', 'production', LTop);
end;

procedure TfrmModelEditor.CreateTestProjectControls(
  AGroupBox: TGroupBox;
  var LTop: Integer);
var
  LLabel: TLabel;
  LEdit: TEdit;
begin
  // enabled (nullable boolean - needs 3-state checkbox)
  CreateNullableCheckBox(AGroupBox, 8, LTop, 'Enabled', 'enabled', LTop);

  // Project
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Project:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 128;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('project', LEdit);
  // Browse button
  with TSpeedButton.Create(Self) do
  begin
    Parent := AGroupBox;
    Left := AGroupBox.Width - 46;
    Top := LTop - 2;
    Width := 25;
    Height := 23;
    Caption := '...';
    Hint := 'project';
    OnClick := BrowseButtonClick;
    Anchors := [akTop, akRight];
  end;
  Inc(LTop, 25);

  // platforms
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Platforms:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 88;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('platforms', LEdit);
  Inc(LTop, 25);

  // configs
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Configs:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 88;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('configs', LEdit);
  Inc(LTop, 25);

  // properties
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Properties:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 88;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('properties', LEdit);
  Inc(LTop, 25);

  // staging (nullable boolean)
  CreateNullableCheckBox(AGroupBox, 8, LTop, 'Staging', 'staging', LTop);

  // production (nullable boolean)
  CreateNullableCheckBox(AGroupBox, 8, LTop, 'Production', 'production', LTop);

  // params (TTestProject specific)
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Params:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 88;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('params', LEdit);
  Inc(LTop, 25);

  // command
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Command:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 88;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('command', LEdit);
  Inc(LTop, 25);

  // folder
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Folder:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 88;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('folder', LEdit);
  Inc(LTop, 25);

  // postBuild (nullable boolean)
  CreateNullableCheckBox(AGroupBox, 8, LTop, 'Post Build', 'postBuild', LTop);
end;

procedure TfrmModelEditor.CreateInstallScriptControls(
  AGroupBox: TGroupBox;
  var LTop: Integer);
var
  LLabel: TLabel;
  LEdit: TEdit;
  LCheckBox: TCheckBox;
begin
  // enabled
  LCheckBox := TCheckBox.Create(Self);
  LCheckBox.Parent := AGroupBox;
  LCheckBox.Left := 8;
  LCheckBox.Top := LTop;
  LCheckBox.Caption := 'Enabled';
  LCheckBox.Width := AGroupBox.Width - 16;
  FControls.Add('enabled', LCheckBox);
  Inc(LTop, 25);

  // scriptName
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Script Name:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 88;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('scriptName', LEdit);
  Inc(LTop, 25);

  // scriptType
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Script Type:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 88;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('scriptType', LEdit);
  Inc(LTop, 25);

  // filename
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Filename:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 128;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('filename', LEdit);
  // Browse button
  with TSpeedButton.Create(Self) do
  begin
    Parent := AGroupBox;
    Left := AGroupBox.Width - 46;
    Top := LTop - 2;
    Width := 25;
    Height := 23;
    Caption := '...';
    Hint := 'filename';
    OnClick := BrowseButtonClick;
    Anchors := [akTop, akRight];
  end;
  Inc(LTop, 25);

  // params
  LLabel := TLabel.Create(Self);
  LLabel.Parent := AGroupBox;
  LLabel.Left := 8;
  LLabel.Top := LTop;
  LLabel.Caption := 'Params:';
  LEdit := TEdit.Create(Self);
  LEdit.Parent := AGroupBox;
  LEdit.Left := 80;
  LEdit.Top := LTop - 2;
  LEdit.Width := AGroupBox.Width - 88;
  LEdit.Anchors := [akLeft, akTop, akRight];
  FControls.Add('params', LEdit);
  Inc(LTop, 25);

  // staging
  CreateNullableCheckBox(AGroupBox, 8, LTop, 'Staging', 'staging', LTop);

  // production
  CreateNullableCheckBox(AGroupBox, 8, LTop, 'Production', 'production', LTop);
end;

procedure TfrmModelEditor.BrowseButtonClick(ASender: TObject);
var
  LSender: TSpeedButton;
  LEditKey: string;
  LFilter: string;
begin
  if not(ASender is TSpeedButton) then
    Exit;
  LSender := TSpeedButton(ASender);
  LEditKey := LSender.Hint; // Store the edit key in the hint

  // Determine filter based on the hint
  if LEditKey = 'project' then
    LFilter := 'Delphi Projects (*.dproj)|*.dproj|All Files (*.*)|*.*'
  else
    LFilter := 'All Files (*.*)|*.*';

  if BrowseForFile(LEditKey, LFilter) then
  begin
    // Path has been updated, nothing else to do
  end;
end;

function TfrmModelEditor.BrowseForFile(
  const AEditKey: string;
  const AFilter: string): Boolean;
var
  LOpenDialog: TOpenDialog;
  LControl: TControl;
  LEdit: TEdit;
  LAbsolutePath: string;
  LRelativePath: string;
  LCurrentDir: string;
begin
  Result := False;

  if not FControls.TryGetValue(AEditKey, LControl) then
    Exit;

  if not(LControl is TEdit) then
    Exit;

  LEdit := TEdit(LControl);

  LOpenDialog := TOpenDialog.Create(Self);
  try
    LOpenDialog.Filter := AFilter;

    // Use project folder as the base directory
    if FProjectFolder <> '' then
      LCurrentDir := IncludeTrailingPathDelimiter(FProjectFolder)
    else
      LCurrentDir := IncludeTrailingPathDelimiter(GetCurrentDir);

    // If there's a current value, use it to set the initial directory
    if LEdit.Text <> '' then
    begin
      // Expand relative paths to absolute paths
      if ExtractFileDrive(LEdit.Text) = '' then
        LAbsolutePath := ExpandFileName(LCurrentDir + LEdit.Text)
      else
        LAbsolutePath := LEdit.Text;

      // Check if the path exists
      if DirectoryExists(ExtractFilePath(LAbsolutePath)) then
        LOpenDialog.InitialDir := ExtractFilePath(LAbsolutePath);

      // Set the filename if it's just a filename or file path
      if FileExists(LAbsolutePath) then
        LOpenDialog.filename := ExtractFileName(LAbsolutePath)
      else if ExtractFileName(LAbsolutePath) <> '' then
        LOpenDialog.filename := ExtractFileName(LAbsolutePath);
    end
    else
    begin
      // No current value - start in the project folder or current working directory
      LOpenDialog.InitialDir := LCurrentDir;
    end;

    if LOpenDialog.Execute then
    begin
      LAbsolutePath := LOpenDialog.filename;

      // Check if the selected file is within the project folder
      if SameText(Copy(LAbsolutePath, 1, Length(LCurrentDir)), LCurrentDir) then
      begin
        // Strip the project folder from the path
        LRelativePath := Copy(LAbsolutePath, Length(LCurrentDir) + 1, MaxInt);
      end
      else
      begin
        // File is outside project folder - keep absolute path
        LRelativePath := LAbsolutePath;
      end;

      LEdit.Text := LRelativePath;
      Result := True;
    end;
  finally
    LOpenDialog.Free;
  end;
end;

end.
