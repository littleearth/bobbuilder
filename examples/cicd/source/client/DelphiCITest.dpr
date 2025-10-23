program DelphiCITest;

{$R 'version.res' 'version.rc'}

uses
  Vcl.Forms,
  frmCITestU in 'frmCITestU.pas' {frmCITest},
  PersonU in '..\common\PersonU.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfrmCITest, frmCITest);
  Application.Run;
end.
