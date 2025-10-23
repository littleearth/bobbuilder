unit frmBuilderConfigEditorU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ExtCtrls, SynEditHighlighter,
  SynHighlighterJSON, SynEdit, System.Actions, Vcl.ActnList, Vcl.StdCtrls,
  Vcl.Buttons;

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
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionCancelExecute(Sender: TObject);
    procedure ActionReloadExecute(Sender: TObject);
    procedure SynEditConfigChange(Sender: TObject);
  private
    FFileName: TFileName;
    procedure Load;
    procedure Save;
    function Validate(var AMessage: string): Boolean;
  public
    function Execute(AFileName: TFileName): Boolean;
  end;

implementation

uses
  dmResourcesU, Model.Build, Lazy.Dialogs;

{$R *.dfm}
{ TfrmBuilderConfigEditor }

procedure TfrmBuilderConfigEditor.ActionCancelExecute(Sender: TObject);
begin
  Self.ModalResult := mrCancel;
end;

procedure TfrmBuilderConfigEditor.ActionReloadExecute(Sender: TObject);
begin
  Load;
end;

procedure TfrmBuilderConfigEditor.ActionSaveExecute(Sender: TObject);
var
  LMessage: string;
begin
  if Validate(LMessage) then
  begin
    Save;
    Self.ModalResult := mrOk;
  end
  else
  begin
    TLZDialogs.ErrorMessage(LMessage);
  end;

end;

function TfrmBuilderConfigEditor.Execute(AFileName: TFileName): Boolean;
begin
  FFileName := AFileName;
  Load;
  Result := Self.ShowModal = mrOk;
end;

procedure TfrmBuilderConfigEditor.Load;
begin
  SynEditConfig.Lines.LoadFromFile(FFileName);
end;

procedure TfrmBuilderConfigEditor.Save;
begin
  SynEditConfig.Lines.SaveToFile(FFileName);
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
begin
  Result := True;
  LModel := TBuildProject.Create;
  try
    try
      LModel.FromJSON(SynEditConfig.Lines.Text);
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

end.
