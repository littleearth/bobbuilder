unit frmCITestU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, PersonU, Vcl.ComCtrls, Vcl.StdCtrls,
  Vcl.ExtCtrls;

type
  TfrmCITest = class(TForm)
    GridPanel1: TGridPanel;
    Panel1: TPanel;
    Label1: TLabel;
    editFirstName: TEdit;
    Panel2: TPanel;
    Label2: TLabel;
    editLastName: TEdit;
    Panel3: TPanel;
    Label3: TLabel;
    Panel4: TPanel;
    Label4: TLabel;
    editAge: TEdit;
    dateOfBirth: TDateTimePicker;
    lblAgeInformation: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure dateOfBirthChange(Sender: TObject);
  private
    FPerson: TPerson;
  public
    { Public declarations }
  end;

var
  frmCITest: TfrmCITest;

implementation

{$R *.dfm}

procedure TfrmCITest.dateOfBirthChange(Sender: TObject);
begin
  FPerson.FirstName := editFirstName.Text;
  FPerson.LastName := editLastName.Text;
  FPerson.dateOfBirth := dateOfBirth.Date;
  editAge.Text := IntToStr(FPerson.Age);
  lblAgeInformation.Caption := Format('%s %s is %d year(s) old',
    [FPerson.FirstName, FPerson.LastName, FPerson.Age]);
end;

procedure TfrmCITest.FormCreate(Sender: TObject);
begin
  FPerson := TPerson.Create;
  editFirstName.Text := 'Bob';
  editLastName.Text := 'Jones';
  dateOfBirth.Date := EncodeDate(1980, 04, 01);
  dateOfBirthChange(Sender);
end;

procedure TfrmCITest.FormDestroy(Sender: TObject);
begin
  FPerson.Free;
end;

end.
