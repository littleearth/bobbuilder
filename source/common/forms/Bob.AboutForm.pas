unit Bob.AboutForm;

interface

uses
  WinApi.Windows, System.SysUtils, System.Classes, Vcl.Graphics,
  Vcl.Forms, Vcl.Controls, Vcl.StdCtrls, Vcl.ExtCtrls,
  Vcl.Imaging.pngimage, Lazy.Types;

type
  TBobAboutForm = class(TLZObject)
  private
    FForm: TForm;
    FPanel1: TPanel;
    FPanel2: TPanel;
    FPanel3: TPanel;
    FPanel4: TPanel;
    FPanel5: TPanel;
    FPanel6: TPanel;
    FProgramIcon: TImage;
    FProductName: TLabel;
    FVersion: TLabel;
    FCompanyName: TLabel;
    FBevel1: TBevel;
    FComments: TLabel;
    FMemoCredits: TMemo;
    FOKButton: TButton;
    procedure BuildForm;
    procedure UpdateProductInformation;
    procedure FormCreateHandler(ASender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    function ShowModal: Integer;
    class procedure ShowAboutDialog;
  end;

implementation

uses
  WinApi.FileVersionInformation;

{ TBobAboutForm }

constructor TBobAboutForm.Create;
begin
  inherited Create;
  FForm := TForm.Create(nil);
  BuildForm;
  FormCreateHandler(nil);
end;

destructor TBobAboutForm.Destroy;
begin
  FreeAndNil(FForm);
  inherited;
end;

function TBobAboutForm.ShowModal: Integer;
begin
  Result := FForm.ShowModal;
end;

procedure TBobAboutForm.BuildForm;
begin
  // Form properties
  FForm.BorderIcons := [biSystemMenu, biMaximize];
  FForm.Caption := 'About';
  FForm.ClientHeight := 344;
  FForm.ClientWidth := 378;
  FForm.Color := clBtnFace;
  FForm.ParentFont := True;
  FForm.Position := poScreenCenter;

  // Main content panel
  FPanel1 := TPanel.Create(FForm);
  FPanel1.Parent := FForm;
  FPanel1.AlignWithMargins := True;
  FPanel1.Left := 3;
  FPanel1.Top := 3;
  FPanel1.Width := 372;
  FPanel1.Height := 291;
  FPanel1.Align := alClient;
  FPanel1.BevelInner := bvRaised;
  FPanel1.BevelOuter := bvLowered;
  FPanel1.ParentColor := True;
  FPanel1.TabOrder := 0;

  // Top panel containing icon and product info
  FPanel3 := TPanel.Create(FForm);
  FPanel3.Parent := FPanel1;
  FPanel3.Left := 2;
  FPanel3.Top := 2;
  FPanel3.Width := 368;
  FPanel3.Height := 143;
  FPanel3.Align := alTop;
  FPanel3.BevelOuter := bvNone;
  FPanel3.TabOrder := 0;

  // Product name panel (top section with icon)
  FPanel6 := TPanel.Create(FForm);
  FPanel6.Parent := FPanel3;
  FPanel6.AlignWithMargins := True;
  FPanel6.Left := 3;
  FPanel6.Top := 3;
  FPanel6.Width := 362;
  FPanel6.Height := 51;
  FPanel6.Align := alTop;
  FPanel6.BevelOuter := bvNone;
  FPanel6.TabOrder := 0;

  // Icon panel (inside Panel6)
  FPanel4 := TPanel.Create(FForm);
  FPanel4.Parent := FPanel6;
  FPanel4.AlignWithMargins := True;
  FPanel4.Left := 3;
  FPanel4.Top := 3;
  FPanel4.Width := 45;
  FPanel4.Height := 45;
  FPanel4.Align := alLeft;
  FPanel4.BevelOuter := bvNone;
  FPanel4.TabOrder := 0;

  // Program icon
  FProgramIcon := TImage.Create(FForm);
  FProgramIcon.Parent := FPanel4;
  FProgramIcon.AlignWithMargins := True;
  FProgramIcon.Left := 3;
  FProgramIcon.Top := 3;
  FProgramIcon.Width := 39;
  FProgramIcon.Height := 39;
  FProgramIcon.Align := alClient;
  FProgramIcon.Stretch := True;

  // Product name label (takes remaining space in Panel6)
  FProductName := TLabel.Create(FForm);
  FProductName.Parent := FPanel6;
  FProductName.Left := 51;
  FProductName.Top := 0;
  FProductName.Width := 311;
  FProductName.Height := 51;
  FProductName.Align := alClient;
  FProductName.Caption := 'Product Name';
  FProductName.WordWrap := True;

  // Info panel (contains Company, Version, Comments)
  FPanel5 := TPanel.Create(FForm);
  FPanel5.Parent := FPanel3;
  FPanel5.AlignWithMargins := True;
  FPanel5.Left := 3;
  FPanel5.Top := 60;
  FPanel5.Width := 362;
  FPanel5.Height := 80;
  FPanel5.Align := alClient;
  FPanel5.BevelOuter := bvNone;
  FPanel5.TabOrder := 1;

  // Company name label
  FCompanyName := TLabel.Create(FForm);
  FCompanyName.Parent := FPanel5;
  FCompanyName.AlignWithMargins := True;
  FCompanyName.Left := 3;
  FCompanyName.Top := 3;
  FCompanyName.Width := 356;
  FCompanyName.Height := 15;
  FCompanyName.Align := alTop;
  FCompanyName.Caption := 'Company Name';

  // Version label
  FVersion := TLabel.Create(FForm);
  FVersion.Parent := FPanel5;
  FVersion.AlignWithMargins := True;
  FVersion.Left := 3;
  FVersion.Top := 24;
  FVersion.Width := 356;
  FVersion.Height := 15;
  FVersion.Align := alTop;
  FVersion.Caption := '0.0.0.0';

  // Comments label (takes remaining space in Panel5)
  FComments := TLabel.Create(FForm);
  FComments.Parent := FPanel5;
  FComments.AlignWithMargins := True;
  FComments.Left := 3;
  FComments.Top := 45;
  FComments.Width := 356;
  FComments.Height := 32;
  FComments.Align := alClient;
  FComments.Caption := 'Comments';
  FComments.WordWrap := True;

  // Bevel separator
  FBevel1 := TBevel.Create(FForm);
  FBevel1.Parent := FPanel1;
  FBevel1.AlignWithMargins := True;
  FBevel1.Left := 5;
  FBevel1.Top := 148;
  FBevel1.Width := 362;
  FBevel1.Height := 9;
  FBevel1.Align := alTop;
  FBevel1.Shape := bsTopLine;

  // Credits memo
  FMemoCredits := TMemo.Create(FForm);
  FMemoCredits.Parent := FPanel1;
  FMemoCredits.AlignWithMargins := True;
  FMemoCredits.Left := 5;
  FMemoCredits.Top := 163;
  FMemoCredits.Width := 362;
  FMemoCredits.Height := 123;
  FMemoCredits.Align := alClient;
  FMemoCredits.Alignment := taCenter;
  FMemoCredits.Lines.Clear;
  FMemoCredits.Lines.Add('Development');
  FMemoCredits.Lines.Add('Tristan Marlow');
  FMemoCredits.Lines.Add('');
  FMemoCredits.Lines.Add('Icons from Icons8');
  FMemoCredits.ReadOnly := True;
  FMemoCredits.TabOrder := 1;

  // Bottom button panel
  FPanel2 := TPanel.Create(FForm);
  FPanel2.Parent := FForm;
  FPanel2.AlignWithMargins := True;
  FPanel2.Left := 3;
  FPanel2.Top := 300;
  FPanel2.Width := 372;
  FPanel2.Height := 41;
  FPanel2.Align := alBottom;
  FPanel2.BevelInner := bvRaised;
  FPanel2.BevelOuter := bvLowered;
  FPanel2.TabOrder := 1;

  // OK button
  FOKButton := TButton.Create(FForm);
  FOKButton.Parent := FPanel2;
  FOKButton.AlignWithMargins := True;
  FOKButton.Left := 102;
  FOKButton.Top := 5;
  FOKButton.Width := 174;
  FOKButton.Height := 31;
  FOKButton.Margins.Left := 100;
  FOKButton.Margins.Right := 100;
  FOKButton.Align := alClient;
  FOKButton.Caption := 'OK';
  FOKButton.Default := True;
  FOKButton.ModalResult := mrOk;
  FOKButton.TabOrder := 0;
end;

procedure TBobAboutForm.FormCreateHandler(ASender: TObject);
begin
  FProductName.Font.Size := FProductName.Font.Size * 2;
  FCompanyName.Font.Size := FCompanyName.Font.Size + 2;
  UpdateProductInformation;
end;

procedure TBobAboutForm.UpdateProductInformation;
var
  LFileVersionInformation: TLZFileVersionInformation;
begin
  LFileVersionInformation := TLZFileVersionInformation.Create;
  try
    try
      LFileVersionInformation.FileName := ParamStr(0);
      FProductName.Caption := LFileVersionInformation.ProductName;
      FVersion.Caption := LFileVersionInformation.FileVersion;
      FCompanyName.Caption := LFileVersionInformation.CompanyName;
      FComments.Caption := LFileVersionInformation.Comments;
      if LFileVersionInformation.Icon <> nil then
        FProgramIcon.Picture.Assign(LFileVersionInformation.Icon);
    except
      // Silently handle exceptions if version information is not available
    end;
  finally
    FreeAndNil(LFileVersionInformation);
  end;
end;

class procedure TBobAboutForm.ShowAboutDialog;
var
  LAboutForm: TBobAboutForm;
begin
  LAboutForm := TBobAboutForm.Create;
  try
    LAboutForm.ShowModal;
  finally
    FreeAndNil(LAboutForm);
  end;
end;

end.
