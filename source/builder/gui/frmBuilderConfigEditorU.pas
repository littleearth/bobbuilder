unit frmBuilderConfigEditorU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, System.JSON, System.Generics.Collections, System.UITypes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls,
  SynEditHighlighter, SynHighlighterJSON, SynEdit, System.Actions,
  Vcl.ActnList, Vcl.StdCtrls, Vcl.Buttons, Vcl.ComCtrls, Vcl.Grids, Vcl.Menus,
  Bob.BuilderModels, Bob.ProjectBuilder, Bob.BuilderTreeView, Lazy.Model;

type
  TfrmBuilderConfigEditor = class(TForm)
    SynEditConfig: TSynEdit;
    SynJSONSyn: TSynJSONSyn;
    Panel1: TPanel;
    ActionList: TActionList;
    ActionSave: TAction;
    ActionCancel: TAction;
    ActionReload: TAction;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    lblJSONStatus: TLabel;
    PageControl: TPageControl;
    tabRawJSON: TTabSheet;
    tabVisualEditor: TTabSheet;
    pnlTreeViewEditor: TPanel;
    pnlTreeViewTools: TPanel;
    BitBtn4: TBitBtn;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    ActionAddItem: TAction;
    ActionEditItem: TAction;
    ActionDeleteItem: TAction;
    PopupMenuEditor: TPopupMenu;
    Add1: TMenuItem;
    Edit1: TMenuItem;
    Delete1: TMenuItem;
    N1: TMenuItem;
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionCancelExecute(Sender: TObject);
    procedure ActionReloadExecute(Sender: TObject);
    procedure SynEditConfigChange(Sender: TObject);
    procedure PageControlChange(Sender: TObject);
    procedure ActionEditItemExecute(Sender: TObject);
    procedure ActionDeleteItemExecute(Sender: TObject);
    procedure ActionAddItemExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FTempFileName: TFileName;
    FProjectBuilder: TProjectBuilder;
    FBuilderTreeView: TBuilderTreeView;
    procedure Load;
    procedure Save;
    function Validate(var AMessage: string): Boolean;
    procedure SyncJSONToVisual;
    procedure SyncVisualToJSON;
  public
    function Execute(AFileName: TFileName): Boolean;
  end;

implementation

uses
  dmResourcesU, Lazy.Utils.Windows, Lazy.Dialogs;

{$R *.dfm}
{ TfrmBuilderConfigEditor }

procedure TfrmBuilderConfigEditor.ActionCancelExecute(Sender: TObject);
begin
  Self.ModalResult := mrCancel;
end;

procedure TfrmBuilderConfigEditor.ActionAddItemExecute(Sender: TObject);
begin
  if FBuilderTreeView.Selected = nil then
  begin
    MessageDlg
      ('Please select an item to add a child to, or select a root node.',
      TMsgDlgType.mtInformation, [TMsgDlgBtn.mbOK], 0);
    Exit;
  end;

  // Call the smart Add method that determines which Add method to call
  FBuilderTreeView.Add;
end;

procedure TfrmBuilderConfigEditor.ActionDeleteItemExecute(Sender: TObject);
begin
  FBuilderTreeView.DeleteSelectedNode;
  SyncVisualToJSON;
end;

procedure TfrmBuilderConfigEditor.ActionEditItemExecute(Sender: TObject);
begin
  FBuilderTreeView.EditSelectedNode;
  SyncVisualToJSON;
end;

procedure TfrmBuilderConfigEditor.ActionReloadExecute(Sender: TObject);
begin
  Load;
end;

procedure TfrmBuilderConfigEditor.ActionSaveExecute(Sender: TObject);
var
  LMessage: string;
begin
  if PageControl.ActivePage = tabVisualEditor then
  begin
    SyncVisualToJSON;
  end;

  if Validate(LMessage) then
  begin
    Save;
    Self.ModalResult := mrOk;
  end
  else
  begin
    MessageDlg(LMessage, TMsgDlgType.mtError, [TMsgDlgBtn.mbOK], 0);
  end;
end;

function TfrmBuilderConfigEditor.Execute(AFileName: TFileName): Boolean;
begin
  Result := False;
  FTempFileName := ChangeFileExt(AFileName,'.builder.tmp');
  if TLZFile.Copy(AFileName, FTempFileName) then
  begin
    Load;
    if Self.ShowModal = mrOk then
    begin
      if TLZFile.Move(FTempFileName, AFileName) then
      begin
        Result := true;
      end
      else
      begin
        TLZDialogs.ErrorMessage(Format('Failed to save configuration %s -> %s.',
          [FTempFileName, AFileName]));
      end;
    end;
  end
  else
  begin
    TLZDialogs.ErrorMessage
      (Format('Failed to create temporary configuration file.',
      [FTempFileName]));
  end;
end;

procedure TfrmBuilderConfigEditor.FormCreate(Sender: TObject);
begin
  FProjectBuilder := TProjectBuilder.Create;
  FBuilderTreeView := TBuilderTreeView.Create(Self);
  FBuilderTreeView.Parent := pnlTreeViewEditor;
  FBuilderTreeView.Align := alClient;
  FBuilderTreeView.ReadOnly := False;
  FBuilderTreeView.ProjectBuilder := FProjectBuilder;
  FBuilderTreeView.PopupMenu := PopupMenuEditor;
  // Enable inline editing
  FBuilderTreeView.ReadOnly := False;
  PageControl.ActivePageIndex := 0;
end;

procedure TfrmBuilderConfigEditor.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FProjectBuilder);
end;

procedure TfrmBuilderConfigEditor.Load;
begin
  SynEditConfig.Lines.LoadFromFile(FTempFileName);
  SyncJSONToVisual;
end;

procedure TfrmBuilderConfigEditor.Save;
begin
  SynEditConfig.Lines.SaveToFile(FTempFileName);
end;

procedure TfrmBuilderConfigEditor.SynEditConfigChange(Sender: TObject);
var
  LMessage: string;
begin
  if Validate(LMessage) then
  begin
    lblJSONStatus.Caption := 'JSON is valid';
  end
  else
  begin
    lblJSONStatus.Caption := Format('Error: %s', [LMessage]);
  end;

end;

function TfrmBuilderConfigEditor.Validate(var AMessage: string): Boolean;
var
  LModel: TBuildProject;
  LJSON: string;
begin
  Result := true;
  LModel := TBuildProject.Create;
  try
    try
      LModel.FromJSON(SynEditConfig.Lines.Text);
      LJSON := LModel.ToJSON;
      if TLZString.IsEmptyString(LJSON) then
      begin
        AMessage := 'No JSON found.';
        Result := False;
      end;
    except
      on E: Exception do
      begin
        AMessage := E.Message;
        Result := False;
      end;
    end;
  finally
    FreeAndNil(LModel);
  end;
end;

procedure TfrmBuilderConfigEditor.PageControlChange(Sender: TObject);
begin
  if PageControl.ActivePage = tabVisualEditor then
  begin
    SyncJSONToVisual;
  end;
  if PageControl.ActivePage = tabRawJSON then
  begin
    SyncVisualToJSON;
  end;
end;

procedure TfrmBuilderConfigEditor.SyncJSONToVisual;
var
  LMessage: string;
begin
  if Validate(LMessage) then
  begin
    try
      FProjectBuilder.FileName := '';
      SynEditConfig.Lines.SaveToFile(FTempFileName);
      FProjectBuilder.FileName := FTempFileName;
      FBuilderTreeView.LoadProject;
    except
      on E: Exception do
      begin
        // Show error but don't prevent tab switch
        lblJSONStatus.Caption := Format('Error: %s', [E.Message]);
      end;
    end;
  end;
end;

procedure TfrmBuilderConfigEditor.SyncVisualToJSON;
begin
  if Assigned(FProjectBuilder) then
  begin
    try
      FBuilderTreeView.SaveProject;
      FProjectBuilder.SaveAs(FTempFileName);
      SynEditConfig.Lines.LoadFromFile(FTempFileName);
    except
      on E: Exception do
      begin
        lblJSONStatus.Caption := Format('Error: %s', [E.Message]);
      end;
    end;
  end;
end;

end.
