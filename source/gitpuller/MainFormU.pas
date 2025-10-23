unit MainFormU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.FileCtrl, System.IOUtils, System.UITypes,
  Bob.Git;

type
  TMainForm = class(TForm)
    pnlTop: TPanel;
    lblPath: TLabel;
    edtPath: TEdit;
    btnBrowse: TButton;
    btnPull: TButton;
    pnlMain: TPanel;
    memoLog: TMemo;
    pnlBottom: TPanel;
    btnExit: TButton;
    TimerLog: TTimer;
    procedure btnBrowseClick(Sender: TObject);
    procedure btnPullClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TimerLogTimer(Sender: TObject);
  private
    LGitPuller: TBobGitPuller;
    function OnPromptHandler(const ATitle, AQuestion: string): Boolean;
  public
  end;

var
  MainForm: TMainForm;

implementation

{$R *.dfm}

uses
  Lazy.Log, Lazy.Dialogs;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  LGitPuller := TBobGitPuller.Create;
  LGitPuller.OnPrompt := OnPromptHandler;

  edtPath.Text := GetCurrentDir;
  memoLog.Clear;

  Caption := 'Git Repository Puller';
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  LGitPuller.Free;
end;

procedure TMainForm.btnBrowseClick(Sender: TObject);
var
  LDirectory: string;
begin
  LDirectory := edtPath.Text;
  if SelectDirectory('Select base directory to scan for Git repositories', '',
    LDirectory) then
    edtPath.Text := LDirectory;
end;

procedure TMainForm.btnPullClick(Sender: TObject);
begin
  if not TDirectory.Exists(edtPath.Text) then
  begin
    TLZDialogs.ErrorMessage('Selected directory does not exist!');
    Exit;
  end;
  memoLog.Clear;
  btnPull.Enabled := False;
  btnBrowse.Enabled := False;
  try
    LGitPuller.PullAllRepositories(edtPath.Text);
  finally
    btnPull.Enabled := True;
    btnBrowse.Enabled := True;
  end;
end;

procedure TMainForm.btnExitClick(Sender: TObject);
begin
  Close;
end;

function TMainForm.OnPromptHandler(const ATitle, AQuestion: string): Boolean;
begin
  Result := TLZDialogs.ConfirmationMessage(AQuestion, [mbYes, mbNo]) = mrYes;
end;

procedure TMainForm.TimerLogTimer(Sender: TObject);
begin
  memoLog.Lines.Text := LazyLogCache;
end;

end.
