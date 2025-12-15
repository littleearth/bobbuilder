unit frmVersionInformationU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, System.UITypes,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls,
  System.Actions, Vcl.ActnList, Bob.ProjectBuilder, Vcl.Buttons;

type
  TOnFindControl = reference to function(AControl: TControl): boolean;

  TfrmVersionInformation = class(TForm)
    Panel1: TPanel;
    Panel17: TPanel;
    pnlVersionNumberDetails: TGridPanel;
    GridPanel5: TGridPanel;
    Panel11: TPanel;
    Label1: TLabel;
    editProductName: TEdit;
    Panel12: TPanel;
    Label10: TLabel;
    editFileDescription: TEdit;
    Panel13: TPanel;
    Label11: TLabel;
    editInternalName: TEdit;
    Panel14: TPanel;
    Label12: TLabel;
    editCompanyName: TEdit;
    Panel15: TPanel;
    Label13: TLabel;
    editLegalCopyright: TEdit;
    Panel2: TPanel;
    Label14: TLabel;
    GridPanel2: TGridPanel;
    Panel16: TPanel;
    editProductionMajorVersion: TEdit;
    cbMajorVersionAdjustAll: TCheckBox;
    Panel3: TPanel;
    Label2: TLabel;
    Panel4: TPanel;
    Label3: TLabel;
    GridPanel3: TGridPanel;
    Panel5: TPanel;
    Label4: TLabel;
    editStagingMajorVersion: TEdit;
    Panel6: TPanel;
    Label5: TLabel;
    editStagingMinorVersion: TEdit;
    Panel7: TPanel;
    Label6: TLabel;
    editStagingReleaseVersion: TEdit;
    Panel8: TPanel;
    Label7: TLabel;
    editStagingBuildVersion: TEdit;
    GridPanel4: TGridPanel;
    Panel9: TPanel;
    Label8: TLabel;
    editDevelopmentMajorVersion: TEdit;
    Panel10: TPanel;
    Label9: TLabel;
    editDevelopmentMinorVersion: TEdit;
    Panel18: TPanel;
    Label15: TLabel;
    editDevelopmentReleaseVersion: TEdit;
    Panel19: TPanel;
    Label16: TLabel;
    editDevelopmentBuildVersion: TEdit;
    Panel20: TPanel;
    editProductionMinorVersion: TEdit;
    cbMinorVersionAdjustAll: TCheckBox;
    Panel21: TPanel;
    editProductionReleaseVersion: TEdit;
    cbReleaseVersionAdjustAll: TCheckBox;
    Panel22: TPanel;
    editProductionBuildVersion: TEdit;
    cbBuildVersionAdjustAll: TCheckBox;
    ActionList: TActionList;
    ActionSave: TAction;
    ActionCancel: TAction;
    listboxVariables: TListBox;
    Panel23: TPanel;
    editProductionFilename: TEdit;
    cbFilenameVersionAdjustAll: TCheckBox;
    Panel24: TPanel;
    Label17: TLabel;
    editStagingFilename: TEdit;
    Panel25: TPanel;
    Label18: TLabel;
    editDevelopmentFilename: TEdit;
    Panel26: TPanel;
    editProductionProductMajorVersion: TEdit;
    CheckBox1: TCheckBox;
    Panel27: TPanel;
    editProductionProductMinorVersion: TEdit;
    CheckBox2: TCheckBox;
    Panel28: TPanel;
    editProductionProductReleaseVersion: TEdit;
    CheckBox3: TCheckBox;
    Panel29: TPanel;
    editProductionProductBuildVersion: TEdit;
    CheckBox4: TCheckBox;
    Panel30: TPanel;
    Label19: TLabel;
    editDevelopmentProductMajorVersion: TEdit;
    Panel31: TPanel;
    Label20: TLabel;
    editDevelopmentProductMinorVersion: TEdit;
    Panel32: TPanel;
    Label21: TLabel;
    editDevelopmentProductReleaseVersion: TEdit;
    Panel33: TPanel;
    Label22: TLabel;
    editDevelopmentProductBuildVersion: TEdit;
    Panel34: TPanel;
    Panel35: TPanel;
    Label24: TLabel;
    editStagingProductMajorVersion: TEdit;
    Panel36: TPanel;
    Label25: TLabel;
    editStagingProductMinorVersion: TEdit;
    Panel37: TPanel;
    Label26: TLabel;
    editStagingProductReleaseVersion: TEdit;
    Panel38: TPanel;
    Label27: TLabel;
    editStagingProductBuildVersion: TEdit;
    Panel39: TPanel;
    Button1: TBitBtn;
    Button2: TBitBtn;
    Panel40: TPanel;
    Label23: TLabel;
    editCompanyURL: TEdit;
    procedure editProductionMajorVersionChange(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionCancelExecute(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure listboxVariablesMouseDown(
      Sender: TObject;
      Button: TMouseButton;
      Shift: TShiftState;
      X, Y: Integer);
    procedure editProductNameDragOver(
      Sender, Source: TObject;
      X, Y: Integer;
      State: TDragState;
      var Accept: boolean);
    procedure editProductNameDragDrop(
      Sender, Source: TObject;
      X, Y: Integer);
  private
    FProjectBuilder: TProjectBuilder;
    function FindCheckBox(
      AParent: TWinControl;
      ATag: Integer): TCheckBox;
    function FindControl(
      AParent: TWinControl;
      AOnCheckControl: TOnFindControl;
      ARecursive: boolean = true): TControl;
    procedure SetFormValues;
    procedure GetFormValues;
    function ValidateForm(var AMessage: String): boolean;
    procedure UpdateVariables;
  public
    function Execute(AProjectBuilder: TProjectBuilder): boolean;
  end;

implementation

{$R *.dfm}

uses System.Math, dmResourcesU;

function TfrmVersionInformation.FindControl(
  AParent: TWinControl;
  AOnCheckControl: TOnFindControl;
  ARecursive: boolean): TControl;
var
  LIdx: Integer;
  LWinControl: TWinControl;
begin
  Result := nil;
  LIdx := 0;
  while (LIdx < AParent.ControlCount) and (not Assigned(Result)) do
  begin
    if Assigned(AOnCheckControl) then
    begin
      if AOnCheckControl(AParent.Controls[LIdx]) then
      begin
        Result := AParent.Controls[LIdx];
      end;
    end;

    if (not Assigned(Result)) then
    begin
      if (AParent.Controls[LIdx] is TWinControl) then
      begin
        LWinControl := (AParent.Controls[LIdx] as TWinControl);
        if (LWinControl.ControlCount > 0) then
        begin
          Result := FindControl(LWinControl, AOnCheckControl, ARecursive);
        end;
      end;
    end;
    Inc(LIdx);
  end;

end;

procedure TfrmVersionInformation.FormShow(Sender: TObject);
begin
  GetFormValues;
  UpdateVariables;
end;

procedure TfrmVersionInformation.GetFormValues;
begin
  editProductName.Text := FProjectBuilder.Project.projectInformation.
    productName;
  editInternalName.Text := FProjectBuilder.Project.projectInformation.
    internalName;
  editCompanyName.Text := FProjectBuilder.Project.projectInformation.
    companyName;
  editFileDescription.Text := FProjectBuilder.Project.projectInformation.
    fileDescription;
  editLegalCopyright.Text := FProjectBuilder.Project.projectInformation.
    legalCopyright;
  editCompanyURL.Text := FProjectBuilder.Project.projectInformation.companyURL;

  editProductionMajorVersion.Text := FProjectBuilder.Project.projectInformation.
    productionVersion.fileVersionMajor;
  editProductionMinorVersion.Text := FProjectBuilder.Project.projectInformation.
    productionVersion.fileVersionMinor;
  editProductionReleaseVersion.Text :=
    FProjectBuilder.Project.projectInformation.productionVersion.
    fileVersionRelease;
  editProductionBuildVersion.Text := FProjectBuilder.Project.projectInformation.
    productionVersion.fileVersionBuild;
  editProductionFilename.Text := FProjectBuilder.Project.projectInformation.
    productionVersion.filename;

  editStagingMajorVersion.Text := FProjectBuilder.Project.projectInformation.
    StagingVersion.fileVersionMajor;
  editStagingMinorVersion.Text := FProjectBuilder.Project.projectInformation.
    StagingVersion.fileVersionMinor;
  editStagingReleaseVersion.Text := FProjectBuilder.Project.projectInformation.
    StagingVersion.fileVersionRelease;
  editStagingBuildVersion.Text := FProjectBuilder.Project.projectInformation.
    StagingVersion.fileVersionBuild;
  editStagingFilename.Text := FProjectBuilder.Project.projectInformation.
    StagingVersion.filename;

  editDevelopmentMajorVersion.Text :=
    FProjectBuilder.Project.projectInformation.DevelopmentVersion.
    fileVersionMajor;
  editDevelopmentMinorVersion.Text :=
    FProjectBuilder.Project.projectInformation.DevelopmentVersion.
    fileVersionMinor;
  editDevelopmentReleaseVersion.Text :=
    FProjectBuilder.Project.projectInformation.DevelopmentVersion.
    fileVersionRelease;
  editDevelopmentBuildVersion.Text :=
    FProjectBuilder.Project.projectInformation.DevelopmentVersion.
    fileVersionBuild;
  editDevelopmentFilename.Text := FProjectBuilder.Project.projectInformation.
    DevelopmentVersion.filename;

  editProductionProductMajorVersion.Text :=
    FProjectBuilder.Project.projectInformation.productionVersion.
    productVersionMajor;
  editProductionProductMinorVersion.Text :=
    FProjectBuilder.Project.projectInformation.productionVersion.
    productVersionMinor;
  editProductionProductReleaseVersion.Text :=
    FProjectBuilder.Project.projectInformation.productionVersion.
    productVersionRelease;
  editProductionProductBuildVersion.Text :=
    FProjectBuilder.Project.projectInformation.productionVersion.
    productVersionBuild;

  editStagingProductMajorVersion.Text :=
    FProjectBuilder.Project.projectInformation.StagingVersion.
    productVersionMajor;
  editStagingProductMinorVersion.Text :=
    FProjectBuilder.Project.projectInformation.StagingVersion.
    productVersionMinor;
  editStagingProductReleaseVersion.Text :=
    FProjectBuilder.Project.projectInformation.StagingVersion.
    productVersionRelease;
  editStagingProductBuildVersion.Text :=
    FProjectBuilder.Project.projectInformation.StagingVersion.
    productVersionBuild;

  editDevelopmentProductMajorVersion.Text :=
    FProjectBuilder.Project.projectInformation.DevelopmentVersion.
    productVersionMajor;
  editDevelopmentProductMinorVersion.Text :=
    FProjectBuilder.Project.projectInformation.DevelopmentVersion.
    productVersionMinor;
  editDevelopmentProductReleaseVersion.Text :=
    FProjectBuilder.Project.projectInformation.DevelopmentVersion.
    productVersionRelease;
  editDevelopmentProductBuildVersion.Text :=
    FProjectBuilder.Project.projectInformation.DevelopmentVersion.
    productVersionBuild;

end;

procedure TfrmVersionInformation.listboxVariablesMouseDown(
  Sender: TObject;
  Button: TMouseButton;
  Shift: TShiftState;
  X, Y: Integer);
begin
  if Button = TMouseButton.mbLeft then
  begin
    if listboxVariables.ItemIndex <> -1 then
    begin
      listboxVariables.BeginDrag(false);
    end;
  end;
end;

procedure TfrmVersionInformation.SetFormValues;
begin
  FProjectBuilder.Project.projectInformation.productName :=
    editProductName.Text;
  FProjectBuilder.Project.projectInformation.internalName :=
    editInternalName.Text;
  FProjectBuilder.Project.projectInformation.companyName :=
    editCompanyName.Text;
  FProjectBuilder.Project.projectInformation.fileDescription :=
    editFileDescription.Text;
  FProjectBuilder.Project.projectInformation.legalCopyright :=
    editLegalCopyright.Text;
  FProjectBuilder.Project.projectInformation.companyURL := editCompanyURL.Text;

  FProjectBuilder.Project.projectInformation.productionVersion.fileVersionMajor
    := editProductionMajorVersion.Text;
  FProjectBuilder.Project.projectInformation.productionVersion.fileVersionMinor
    := editProductionMinorVersion.Text;
  FProjectBuilder.Project.projectInformation.productionVersion.
    fileVersionRelease := editProductionReleaseVersion.Text;
  FProjectBuilder.Project.projectInformation.productionVersion.fileVersionBuild
    := editProductionBuildVersion.Text;
  FProjectBuilder.Project.projectInformation.productionVersion.filename :=
    editProductionFilename.Text;

  FProjectBuilder.Project.projectInformation.StagingVersion.fileVersionMajor :=
    editStagingMajorVersion.Text;
  FProjectBuilder.Project.projectInformation.StagingVersion.fileVersionMinor :=
    editStagingMinorVersion.Text;
  FProjectBuilder.Project.projectInformation.StagingVersion.fileVersionRelease
    := editStagingReleaseVersion.Text;
  FProjectBuilder.Project.projectInformation.StagingVersion.fileVersionBuild :=
    editStagingBuildVersion.Text;
  FProjectBuilder.Project.projectInformation.StagingVersion.filename :=
    editStagingFilename.Text;

  FProjectBuilder.Project.projectInformation.DevelopmentVersion.fileVersionMajor
    := editDevelopmentMajorVersion.Text;
  FProjectBuilder.Project.projectInformation.DevelopmentVersion.fileVersionMinor
    := editDevelopmentMinorVersion.Text;
  FProjectBuilder.Project.projectInformation.DevelopmentVersion.
    fileVersionRelease := editDevelopmentReleaseVersion.Text;
  FProjectBuilder.Project.projectInformation.DevelopmentVersion.fileVersionBuild
    := editDevelopmentBuildVersion.Text;
  FProjectBuilder.Project.projectInformation.DevelopmentVersion.filename :=
    editDevelopmentFilename.Text;

  FProjectBuilder.Project.projectInformation.productionVersion.
    productVersionMajor := editProductionProductMajorVersion.Text;
  FProjectBuilder.Project.projectInformation.productionVersion.
    productVersionMinor := editProductionProductMinorVersion.Text;
  FProjectBuilder.Project.projectInformation.productionVersion.
    productVersionRelease := editProductionProductReleaseVersion.Text;
  FProjectBuilder.Project.projectInformation.productionVersion.
    productVersionBuild := editProductionProductBuildVersion.Text;

  FProjectBuilder.Project.projectInformation.StagingVersion.productVersionMajor
    := editStagingProductMajorVersion.Text;
  FProjectBuilder.Project.projectInformation.StagingVersion.productVersionMinor
    := editStagingProductMinorVersion.Text;
  FProjectBuilder.Project.projectInformation.StagingVersion.
    productVersionRelease := editStagingProductReleaseVersion.Text;
  FProjectBuilder.Project.projectInformation.StagingVersion.productVersionBuild
    := editStagingProductBuildVersion.Text;

  FProjectBuilder.Project.projectInformation.DevelopmentVersion.
    productVersionMajor := editDevelopmentProductMajorVersion.Text;
  FProjectBuilder.Project.projectInformation.DevelopmentVersion.
    productVersionMinor := editDevelopmentProductMinorVersion.Text;
  FProjectBuilder.Project.projectInformation.DevelopmentVersion.
    productVersionRelease := editDevelopmentProductReleaseVersion.Text;
  FProjectBuilder.Project.projectInformation.DevelopmentVersion.
    productVersionBuild := editDevelopmentProductBuildVersion.Text;

end;

procedure TfrmVersionInformation.UpdateVariables;
begin
  FProjectBuilder.GetVariables(listboxVariables.Items);
end;

function TfrmVersionInformation.ValidateForm(var AMessage: String): boolean;
begin
  Result := true;
  AMessage := '';
end;

function TfrmVersionInformation.Execute(AProjectBuilder
  : TProjectBuilder): boolean;
begin
  Result := false;
  if Assigned(AProjectBuilder) then
  begin
    FProjectBuilder := AProjectBuilder;
    Result := Self.ShowModal = mrOk;
  end;
end;

function TfrmVersionInformation.FindCheckBox(
  AParent: TWinControl;
  ATag: Integer): TCheckBox;
begin
  Result := FindControl(AParent,
    function(AControl: TControl): boolean
    begin
      Result := false;
      if (AControl is TCheckBox) then
      begin
        Result := SameValue(AControl.Tag, ATag);
      end;
    end) as TCheckBox;
end;

procedure TfrmVersionInformation.ActionCancelExecute(Sender: TObject);
begin
  Self.ModalResult := mrCancel;
end;

procedure TfrmVersionInformation.ActionSaveExecute(Sender: TObject);
var
  LMessage: string;
begin
  if ValidateForm(LMessage) then
  begin
    SetFormValues;
    Self.ModalResult := mrOk;
  end
  else
  begin
    MessageDlg(LMessage, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end;

end;

procedure TfrmVersionInformation.editProductionMajorVersionChange
  (Sender: TObject);
var
  LTag: Integer;
  LCheckBox: TCheckBox;
begin
  if (Sender is TEdit) then
  begin
    LTag := (Sender as TEdit).Tag;
  end;
  LCheckBox := FindCheckBox(pnlVersionNumberDetails, LTag);
  if Assigned(LCheckBox) then
  begin
    if LCheckBox.Checked then
    begin
      FindControl(pnlVersionNumberDetails,
        function(AControl: TControl): boolean
        begin
          // return false we want to loop through all control
          Result := false;
          if (AControl is TEdit) then
          begin
            if SameValue(AControl.Tag, LTag) then
            begin
              if (AControl <> Sender) and (AControl is TEdit) then
              begin
                (AControl as TEdit).Text := (Sender as TEdit).Text;
              end;
            end;
          end;
        end);
    end;

  end;
end;

procedure TfrmVersionInformation.editProductNameDragDrop(
  Sender, Source: TObject;
  X, Y: Integer);
begin
  if (Source is TListBox) and (Sender is TEdit) then
  begin
    (Sender as TEdit).Text := (Source as TListBox)
      .Items[(Source as TListBox).ItemIndex];
  end;
end;

procedure TfrmVersionInformation.editProductNameDragOver(
  Sender, Source: TObject;
  X, Y: Integer;
  State: TDragState;
  var Accept: boolean);
begin
  if Source is TListBox then
  begin
    Accept := true;
  end;
end;

end.
