unit frmDelphiLibraryHelperU;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, System.UITypes, Vcl.Buttons, Vcl.ExtCtrls, LibraryHelperU,
  LibraryPathsU, Vcl.ComCtrls, System.Actions, Vcl.ActnList, System.ImageList,
  Vcl.ImgList, Vcl.Menus, System.Types;

type
  TfrmDelphiLibraryHelper = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    GroupBox3: TGroupBox;
    Panel1: TPanel;
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    BitBtn3: TBitBtn;
    BitBtn4: TBitBtn;
    BitBtn7: TBitBtn;
    StatusBar: TStatusBar;
    ActionList: TActionList;
    ActionAddEnvironmentVariables: TAction;
    ActionDeleteEnvironmentVariable: TAction;
    ActionAddLibraryPath: TAction;
    ActionDeleteLibraryPath: TAction;
    ActionDeleteAllLibraryPaths: TAction;
    ActionSave: TAction;
    ActionLoad: TAction;
    ListViewLibrary: TListView;
    ActionApplyTemplate: TAction;
    BitBtn8: TBitBtn;
    MainMenu: TMainMenu;
    File1: TMenuItem;
    Load1: TMenuItem;
    Save1: TMenuItem;
    N1: TMenuItem;
    emplates1: TMenuItem;
    ApplyTemplate2: TMenuItem;
    Help1: TMenuItem;
    ActionExit: TAction;
    ActionAbout: TAction;
    Exit1: TMenuItem;
    About1: TMenuItem;
    ActionFindReplace: TAction;
    BitBtn9: TBitBtn;
    PopupMenuLibrary: TPopupMenu;
    Add1: TMenuItem;
    Delete1: TMenuItem;
    N2: TMenuItem;
    DeleteAll1: TMenuItem;
    N3: TMenuItem;
    Replace1: TMenuItem;
    N4: TMenuItem;
    ActionCopyLibraryPath: TAction;
    ActionOpenFolder: TAction;
    Openfolder1: TMenuItem;
    ActionCopyLibraryValue: TAction;
    Copy1: TMenuItem;
    Copypath1: TMenuItem;
    Copyvalue1: TMenuItem;
    Panel6: TPanel;
    comboDelphiInstallations: TComboBox;
    ActionSearch: TAction;
    BitBtn10: TBitBtn;
    Search1: TMenuItem;
    ActionSystemProperties: TAction;
    ActionExport: TAction;
    ActionImport: TAction;
    GroupBox4: TGroupBox;
    BitBtn17: TBitBtn;
    BitBtn18: TBitBtn;
    BitBtn12: TBitBtn;
    BitBtn13: TBitBtn;
    BitBtn14: TBitBtn;
    BitBtn15: TBitBtn;
    Export1: TMenuItem;
    Import1: TMenuItem;
    ImportExport1: TMenuItem;
    SystemProperties1: TMenuItem;
    Panel8: TPanel;
    comboLibraries: TComboBox;
    comboPathType: TComboBox;
    TimerMain: TTimer;
    btnTools: TBitBtn;
    PopupMenuTools: TPopupMenu;
    ActionCopySearchToBrowse: TAction;
    ActionCopyBrowseToSearch: TAction;
    Copybrowsepathstosearchpaths1: TMenuItem;
    Copysearchpathstobrowsepaths1: TMenuItem;
    N5: TMenuItem;
    Copysearchpathstobrowsepaths2: TMenuItem;
    Copybrowsepathstosearchpaths2: TMenuItem;
    ActionCleanUp: TAction;
    N6: TMenuItem;
    Cleanup1: TMenuItem;
    N7: TMenuItem;
    Cleanup2: TMenuItem;
    ools1: TMenuItem;
    Copybrowsepathstosearchpaths3: TMenuItem;
    Copysearchpathstobrowsepaths3: TMenuItem;
    Cleanup3: TMenuItem;
    Export2: TMenuItem;
    Import2: TMenuItem;
    N8: TMenuItem;
    N9: TMenuItem;
    ActionViewLog: TAction;
    Viewlog1: TMenuItem;
    ActionRemoveBrowseFromSearch: TAction;
    Removebrowsepathsfromsearchpaths1: TMenuItem;
    Removebrowsepathsfromsearchpaths2: TMenuItem;
    ActionDeduplicate: TAction;
    Deduplicate1: TMenuItem;
    Deduplicate2: TMenuItem;
    Deduplicate3: TMenuItem;
    Panel9: TPanel;
    cbDeduplicateOnSave: TCheckBox;
    Viewlog2: TMenuItem;
    N10: TMenuItem;
    GridPanel1: TGridPanel;
    Panel2: TPanel;
    Label1: TLabel;
    ListViewEnvironmentVariables: TListView;
    Panel4: TPanel;
    BitBtn5: TBitBtn;
    BitBtn6: TBitBtn;
    Panel3: TPanel;
    Label2: TLabel;
    ListViewSystemEnvironmentVariables: TListView;
    Panel5: TPanel;
    BitBtn11: TBitBtn;
    Panel7: TPanel;
    GridPanel2: TGridPanel;
    lblRootPath: TLabel;
    lblEnvOptionsPath: TLabel;
    cbUpdateEnvOptions: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure comboDelphiInstallationsChange(Sender: TObject);
    procedure ActionLoadExecute(Sender: TObject);
    procedure ActionSaveUpdate(Sender: TObject);
    procedure ActionSaveExecute(Sender: TObject);
    procedure ActionLoadUpdate(Sender: TObject);
    procedure comboLibrariesChange(Sender: TObject);
    procedure ActionApplyTemplateUpdate(Sender: TObject);
    procedure ActionApplyTemplateExecute(Sender: TObject);
    procedure ActionDeleteLibraryPathExecute(Sender: TObject);
    procedure ActionDeleteAllLibraryPathsExecute(Sender: TObject);
    procedure ActionDeleteEnvironmentVariableExecute(Sender: TObject);
    procedure ActionAddLibraryPathExecute(Sender: TObject);
    procedure ActionListUpdate(Action: TBasicAction; var Handled: Boolean);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure ActionAddEnvironmentVariablesExecute(Sender: TObject);
    procedure ListViewLibraryDblClick(Sender: TObject);
    procedure ActionExitExecute(Sender: TObject);
    procedure ActionAboutExecute(Sender: TObject);
    procedure ActionCleanUpExecute(Sender: TObject);
    procedure ActionCopyBrowseToSearchExecute(Sender: TObject);
    procedure ActionFindReplaceExecute(Sender: TObject);
    procedure ListViewLibraryCustomDrawItem(Sender: TCustomListView;
      Item: TListItem; State: TCustomDrawState; var DefaultDraw: Boolean);
    procedure ActionCopyLibraryPathUpdate(Sender: TObject);
    procedure ActionOpenFolderExecute(Sender: TObject);
    procedure ActionCopyLibraryPathExecute(Sender: TObject);
    procedure ListViewEnvironmentVariablesCustomDrawItem
      (Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
      var DefaultDraw: Boolean);
    procedure ActionCopyLibraryValueExecute(Sender: TObject);
    procedure ActionCopySearchToBrowseExecute(Sender: TObject);
    procedure ActionDeduplicateExecute(Sender: TObject);
    procedure lblRootPathClick(Sender: TObject);
    procedure ActionSearchExecute(Sender: TObject);
    procedure ActionSystemPropertiesExecute(Sender: TObject);
    procedure ActionExportExecute(Sender: TObject);
    procedure ActionImportExecute(Sender: TObject);
    procedure ActionRemoveBrowseFromSearchExecute(Sender: TObject);
    procedure ActionViewLogExecute(Sender: TObject);
    procedure btnToolsClick(Sender: TObject);
    procedure TimerMainTimer(Sender: TObject);
    procedure cbUpdateEnvOptionsClick(Sender: TObject);
  private
    FApplicationActive: Boolean;
    FModified: Boolean;
    FLibraryHelper: TLibraryHelper;
    FDelphiIsRunning: Boolean;
    FDelphiInstallation: TDelphiInstallation;
    FActiveDelphiLibrary: TDelphiLibrary;
    FActiveLibraryPathType: TLibraryPathType;
    procedure LoadDelphiInstallation;
    procedure LoadEnvironmentVariables;
    procedure SaveEnvironmentVariables;
    procedure LoadLibrary;
    procedure SaveLibrary;
    procedure LoadSystemEnvironmentVariables;
    function ValidatePath(APath: string): Boolean;
    procedure ProcessParameters;
    procedure Cleanup;
    procedure ApplyTemplate(AFileName: TFileName;
      AApplyToAllInstallations: Boolean;
      ADeduplicate, AUpdateEnvOptions: Boolean);
    function GetApplicationParameters(AParameter: string;
      var AValue: string): Boolean;

  public
    { Public declarations }
  end;

var
  frmDelphiLibraryHelper: TfrmDelphiLibraryHelper;

implementation

{$R *.dfm}

uses
  dmDelphiLibraryHelperU, frmAddLibraryPathU, frmAddEnvironmentVariableU,
  frmFindReplaceU, frmSearchU, frmProgressU, Lazy.Log, Lazy.Dialogs,
  Bob.AboutForm;

procedure TfrmDelphiLibraryHelper.ActionAboutExecute(Sender: TObject);
begin
  TBobAboutForm.ShowAboutDialog;
end;

procedure TfrmDelphiLibraryHelper.ActionAddEnvironmentVariablesExecute
  (Sender: TObject);
var
  LfrmAddEnvironmentVariable: TfrmAddEnvironmentVariable;
begin
  LfrmAddEnvironmentVariable := TfrmAddEnvironmentVariable.Create(Self);
  try
    if LfrmAddEnvironmentVariable.Add(FDelphiInstallation) then
    begin
      FModified := True;
      LoadEnvironmentVariables;
    end;
  finally
    FreeAndNil(LfrmAddEnvironmentVariable);
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionAddLibraryPathExecute(Sender: TObject);
var
  LfrmAddLibraryPath: TfrmAddLibraryPath;
begin
  LfrmAddLibraryPath := TfrmAddLibraryPath.Create(nil);
  try
    if LfrmAddLibraryPath.Add(FActiveDelphiLibrary, FDelphiInstallation) then
    begin
      FModified := True;
      LoadLibrary;
    end;
  finally
    FreeAndNil(LfrmAddLibraryPath);
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionApplyTemplateExecute(Sender: TObject);
var
  LOpenDialog: TOpenDialog;
  LApplyToAllInstallations: Boolean;
begin
  LOpenDialog := TOpenDialog.Create(Self);
  try
    LOpenDialog.DefaultExt := '.dlht';
    LOpenDialog.Filter :=
      'Delphi Library Helper Template (*.dlht)|*.dlht|All Files (*.*)|*' + '.*';
    LOpenDialog.Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
    LOpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
    if LOpenDialog.Execute then
    begin
      LApplyToAllInstallations := False;
      if FLibraryHelper.InstalledCount > 1 then
      begin
        MessageDlg
          (Format('Select from the following options on how to apply template "%s".'
          + #13#10 + #13#10 + '[Yes to All] - Apply to all "%d" installations' +
          #13#10 + '[Yes] - Apply to "%s" selected installation' + #13#10 +
          '[No] - Do not apply template to any installation',
          [ExtractFileName(LOpenDialog.FileName), FLibraryHelper.InstalledCount,
          FDelphiInstallation.ProductName]), mtConfirmation,
          [mbYesToAll, mbYes, mbNo], 0);
      end;
      ApplyTemplate(LOpenDialog.FileName, LApplyToAllInstallations,
        cbDeduplicateOnSave.Checked, cbUpdateEnvOptions.Checked);
    end;
  finally
    FreeAndNil(LOpenDialog);
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionApplyTemplateUpdate(Sender: TObject);
begin
  ActionApplyTemplate.Enabled := Assigned(FDelphiInstallation);
end;

procedure TfrmDelphiLibraryHelper.ActionCleanUpExecute(Sender: TObject);
begin
  if (MessageDlg('Remove all invalid paths?', mtConfirmation, [mbYes, mbNo], 0)
    = mrYes) then
  begin
    ShowProgress('Performing Cleanup...');
    try
      Cleanup;
      LoadLibrary;
      FModified := True;
    finally
      HideProgress;
    end;
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionCopyBrowseToSearchExecute
  (Sender: TObject);
begin
  if (MessageDlg('Copy browse paths to search paths', mtConfirmation,
    [mbYes, mbNo], 0) = mrYes) then
  begin
    ShowProgress('Copy browse to search...');
    try
      FDelphiInstallation.CopyBrowseToSearch;
      LoadLibrary;
      FModified := True;
    finally
      HideProgress;
    end;
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionCopyLibraryPathExecute(Sender: TObject);
begin
  if (Assigned(FDelphiInstallation)) and (Assigned(ListViewLibrary.Selected))
  then
  begin
    FDelphiInstallation.CopyToClipBoard(ListViewLibrary.Selected.Caption,
      FActiveDelphiLibrary);
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionCopyLibraryPathUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := Assigned(FDelphiInstallation) and
    Assigned(ListViewLibrary.Selected);
end;

procedure TfrmDelphiLibraryHelper.ActionCopyLibraryValueExecute
  (Sender: TObject);
begin
  if (Assigned(FDelphiInstallation)) and (Assigned(ListViewLibrary.Selected))
  then
  begin
    FDelphiInstallation.CopyToClipBoard(ListViewLibrary.Selected.Caption,
      FActiveDelphiLibrary, False);
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionCopySearchToBrowseExecute
  (Sender: TObject);
begin
  if (MessageDlg('Copy search paths to browse paths', mtConfirmation,
    [mbYes, mbNo], 0) = mrYes) then
  begin
    ShowProgress('Copy search to browse...');
    try
      FDelphiInstallation.CopySearchToBrowse;
      LoadLibrary;
      FModified := True;
    finally
      HideProgress;
    end;
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionDeduplicateExecute(Sender: TObject);
var
  LExecute: Boolean;
  LCount: integer;
begin
  case MessageDlg
    ('All paths will be expanded and duplicates removed, continue?',
    mtConfirmation, [mbYes, mbNo], 0) of
    mrYes:
      LExecute := True;
  else
    LExecute := False;
  end;

  if LExecute then
  begin
    ShowProgress('Deduplicating paths...');
    try
      LCount := FDelphiInstallation.Deduplicate;
      HideProgress;
      MessageDlg(Format('%d path(s) have been removed.', [LCount]),
        mtInformation, [mbOK], 0);
      if LCount > 0 then
      begin
        LoadLibrary;
        FModified := True;
      end;
    finally
      HideProgress;
    end;
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionDeleteAllLibraryPathsExecute
  (Sender: TObject);
begin
  case MessageDlg
    (Format('Delete all from active library (Yes) or from across all libraries (All)?',
    []), mtConfirmation, [mbAll, mbYes, mbCancel], 0) of
    mrAll:
      begin
        FDelphiInstallation.DeleteAll(FActiveLibraryPathType);
        LoadLibrary;
      end;
    mrYes:
      begin
        ListViewLibrary.Clear;
        SaveLibrary;
      end;
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionDeleteEnvironmentVariableExecute
  (Sender: TObject);
begin
  if (MessageDlg('Delete Selected?', mtConfirmation, [mbYes, mbNo], 0) = mrYes)
  then
  begin
    ListViewEnvironmentVariables.DeleteSelected;
    SaveEnvironmentVariables;
    FModified := True;
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionDeleteLibraryPathExecute
  (Sender: TObject);
begin
  if (MessageDlg('Delete Selected?', mtConfirmation, [mbYes, mbNo], 0) = mrYes)
  then
  begin
    ListViewLibrary.DeleteSelected;
    SaveLibrary;
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionExitExecute(Sender: TObject);
begin
  Self.Close;
end;

procedure TfrmDelphiLibraryHelper.ActionExportExecute(Sender: TObject);
var
  LSaveDialog: TSaveDialog;
begin
  LSaveDialog := TSaveDialog.Create(Self);
  try
    LSaveDialog.DefaultExt := '.dlht';
    LSaveDialog.Filter :=
      'Delphi Library Helper Template (*.dlht)|*.dlht|All Files (*.*)|*' + '.*';
    LSaveDialog.Options := [ofHideReadOnly, ofEnableSizing];
    LSaveDialog.InitialDir := ExtractFilePath(ParamStr(0));
    if LSaveDialog.Execute then
    begin
      FDelphiInstallation.ExportLibrary(LSaveDialog.FileName);
      FDelphiInstallation.OpenFolder(ExtractFilePath(LSaveDialog.FileName),
        FActiveDelphiLibrary);
    end;
  finally
    FreeAndNil(LSaveDialog);
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionFindReplaceExecute(Sender: TObject);
var
  LFindReplace: TfrmFindReplace;
begin
  LFindReplace := TfrmFindReplace.Create(Self);
  try
    if LFindReplace.Execute(FDelphiInstallation) then
    begin
      FModified := True;
      LoadLibrary;
    end;
  finally
    FreeAndNil(LFindReplace);
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionImportExecute(Sender: TObject);
var
  LOpenDialog: TOpenDialog;
begin
  LOpenDialog := TOpenDialog.Create(Self);
  try
    LOpenDialog.DefaultExt := '.dlht';
    LOpenDialog.Filter :=
      'Delphi Library Helper Template (*.dlht)|*.dlht|All Files (*.*)|*' + '.*';
    LOpenDialog.Options := [ofHideReadOnly, ofFileMustExist, ofEnableSizing];
    LOpenDialog.InitialDir := ExtractFilePath(ParamStr(0));
    if LOpenDialog.Execute then
    begin
      FDelphiInstallation.ImportLibrary(LOpenDialog.FileName);
      comboLibrariesChange(nil);
      FModified := True;
    end;
  finally
    FreeAndNil(LOpenDialog);
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionListUpdate(Action: TBasicAction;
  var Handled: Boolean);
begin
  if FModified then
  begin
    StatusBar.Panels[0].Text := 'Modified';
  end
  else
  begin
    StatusBar.Panels[0].Text := '';
  end;
  if FDelphiIsRunning then
  begin
    StatusBar.Panels[1].Text := 'Delphi running.';
  end
  else
  begin
    StatusBar.Panels[1].Text := 'Delphi is not running.';
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionLoadExecute(Sender: TObject);
begin
  if comboDelphiInstallations.ItemIndex <> -1 then
  begin
    ShowProgress('Loading...');
    try
      FDelphiInstallation := FLibraryHelper.Installation
        [comboDelphiInstallations.Text];
      FDelphiInstallation.Load;
      lblRootPath.Caption := FDelphiInstallation.RootPath;
      lblEnvOptionsPath.Caption := FDelphiInstallation.EnvOptionsFileName;
      LoadSystemEnvironmentVariables;
      LoadEnvironmentVariables;
      comboLibrariesChange(nil);
      FModified := False;
    finally
      HideProgress;
    end;
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionLoadUpdate(Sender: TObject);
begin
  ActionLoad.Enabled := comboDelphiInstallations.ItemIndex <> -1;
end;

procedure TfrmDelphiLibraryHelper.ActionOpenFolderExecute(Sender: TObject);
begin
  if (Assigned(FDelphiInstallation)) and (Assigned(ListViewLibrary.Selected))
  then
  begin
    FDelphiInstallation.OpenFolder(ListViewLibrary.Selected.Caption,
      FActiveDelphiLibrary);
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionRemoveBrowseFromSearchExecute
  (Sender: TObject);
var
  LExecute, LSmartEnabled: Boolean;
  LCount: integer;
begin
  LExecute := True;
  LSmartEnabled := True;
  case MessageDlg
    ('Would you like to check folders for important files before removing from search folders? (This may take longer.)',
    mtConfirmation, [mbYes, mbNo, mbCancel], 0) of
    mrYes:
      LSmartEnabled := True;
    mrNo:
      LSmartEnabled := False;
  else
    LExecute := False;
  end;
  if LExecute then
  begin
    ShowProgress('Removing browse paths from search paths...');
    try
      LCount := FDelphiInstallation.RemoveBrowseFromSearch(LSmartEnabled);
      HideProgress;
      MessageDlg(Format('%d path(s) have been removed.', [LCount]),
        mtInformation, [mbOK], 0);
      if LCount > 0 then
      begin
        LoadLibrary;
        FModified := True;
      end;
    finally
      HideProgress;
    end;
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionSaveExecute(Sender: TObject);
var
  LAllow: Boolean;
begin
  LAllow := True;
  if FLibraryHelper.IsDelphiRunning then
  begin
    LAllow := False;
    if (MessageDlg('Delphi is still running, continue with save?', mtWarning,
      [mbYes, mbNo], 0) = mrYes) then
    begin
      LAllow := True;
    end;
  end;

  if LAllow then
  begin
    if Assigned(FDelphiInstallation) then
    begin
      ShowProgress('Saving...');
      try
        FDelphiInstallation.Save(cbDeduplicateOnSave.Checked,
          cbUpdateEnvOptions.Checked);
        LoadLibrary;
        FModified := False;
      finally
        HideProgress;
      end;
    end;
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionSaveUpdate(Sender: TObject);
begin
  ActionSave.Enabled := (Assigned(FDelphiInstallation)) and (FModified);
end;

procedure TfrmDelphiLibraryHelper.ActionSearchExecute(Sender: TObject);
var
  LfrmSearch: TfrmSearch;
begin
  LfrmSearch := TfrmSearch.Create(Self);
  try
    LfrmSearch.Execute(FDelphiInstallation);
  finally
    FreeAndNil(LfrmSearch);
  end;
end;

procedure TfrmDelphiLibraryHelper.ActionSystemPropertiesExecute
  (Sender: TObject);
begin
  FDelphiInstallation.ExecuteFile('open', 'SystemPropertiesAdvanced', '', '',
    SW_SHOWNORMAL);
end;

procedure TfrmDelphiLibraryHelper.ActionViewLogExecute(Sender: TObject);
begin
  TLZDialogs.MemoDialogShow(LazyLogCache);
end;

procedure TfrmDelphiLibraryHelper.ApplyTemplate(AFileName: TFileName;
  AApplyToAllInstallations: Boolean; ADeduplicate, AUpdateEnvOptions: Boolean);
var
  LDelphiInstallation: TDelphiInstallation;
  LIdx, LTotal, LApplied: integer;
begin
  LApplied := 0;
  ShowProgress('Applying Template...');
  try
    if AApplyToAllInstallations then
    begin
      LTotal := FLibraryHelper.Count;
      for LIdx := 0 to Pred(LTotal) do
      begin
        LDelphiInstallation := FLibraryHelper.Installations[LIdx];
        if LDelphiInstallation.Installed then
        begin
          UpdateProgress(LIdx + 1, LTotal + 1, 'Applying template to ' +
            LDelphiInstallation.ProductName);
          LApplied := LApplied + LDelphiInstallation.Apply(AFileName);
          LDelphiInstallation.Save(ADeduplicate, AUpdateEnvOptions);
        end;
      end;
    end
    else
    begin
      LApplied := FDelphiInstallation.Apply(AFileName);
    end;
    UpdateProgress(100, Format('%d paths applied', [LApplied]));
    comboLibrariesChange(nil);
    FModified := not AApplyToAllInstallations;
  finally
    HideProgress;
  end;
end;

procedure TfrmDelphiLibraryHelper.btnToolsClick(Sender: TObject);
begin
  with btnTools.ClientToScreen(point(0, 0)) do
    PopupMenuTools.Popup(X, Y);
end;

procedure TfrmDelphiLibraryHelper.cbUpdateEnvOptionsClick(Sender: TObject);
begin
  FModified := True;
end;

procedure TfrmDelphiLibraryHelper.Cleanup;
begin
  FDelphiInstallation.Cleanup;
end;

procedure TfrmDelphiLibraryHelper.comboDelphiInstallationsChange
  (Sender: TObject);
begin
  ActionLoad.Execute;
end;

procedure TfrmDelphiLibraryHelper.comboLibrariesChange(Sender: TObject);
begin
  if (comboLibraries.ItemIndex <> -1) and (comboPathType.ItemIndex <> -1) then
  begin
    FActiveLibraryPathType := TLibraryPathType(comboPathType.ItemIndex + 2);
    FActiveDelphiLibrary := TDelphiLibrary(comboLibraries.ItemIndex);
    LoadLibrary;
  end;
end;

procedure TfrmDelphiLibraryHelper.FormActivate(Sender: TObject);
begin
  if not FApplicationActive then
  begin
    FApplicationActive := True;
    LoadDelphiInstallation;
    ProcessParameters;
    TimerMainTimer(Sender);
  end;
end;

procedure TfrmDelphiLibraryHelper.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  CanClose := not FModified;
  if not CanClose then
  begin
    if (MessageDlg('You have unsaved changes, are you sure you want to exit?',
      mtConfirmation, [mbYes, mbNo], 0) = mrYes) then
    begin
      CanClose := True;
    end;
  end;

end;

procedure TfrmDelphiLibraryHelper.FormCreate(Sender: TObject);
begin
  Self.Width := Screen.Width - (Screen.Width div 4);
  Self.Height := Screen.Height - (Screen.Height div 4);
  FApplicationActive := False;
  FLibraryHelper := TLibraryHelper.Create;
  comboLibraries.ItemIndex := 0;
  lblRootPath.Font.Style := [fsUnderline];
  lblRootPath.Font.Size := Self.Font.Size - 1;
  lblRootPath.Font.Color := clHighlight;
  lblEnvOptionsPath.Font.Style := [fsUnderline];
  lblEnvOptionsPath.Font.Size := Self.Font.Size - 1;
  lblEnvOptionsPath.Font.Color := clHighlight;
end;

procedure TfrmDelphiLibraryHelper.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FLibraryHelper);
end;

procedure TfrmDelphiLibraryHelper.lblRootPathClick(Sender: TObject);
begin
  FDelphiInstallation.OpenFolder((Sender as TLabel).Caption, dlWin32);
end;

procedure TfrmDelphiLibraryHelper.ListViewEnvironmentVariablesCustomDrawItem
  (Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if Odd(Item.Index) then
  begin
    Sender.Canvas.Font.Color := clBlack;
    Sender.Canvas.Brush.Color := clLtGray;
  end
  else
  begin
    Sender.Canvas.Font.Color := clBlack;
    Sender.Canvas.Brush.Color := clWhite;
  end;
end;

procedure TfrmDelphiLibraryHelper.ListViewLibraryCustomDrawItem
  (Sender: TCustomListView; Item: TListItem; State: TCustomDrawState;
  var DefaultDraw: Boolean);
begin
  if Odd(Item.Index) then
  begin
    Sender.Canvas.Font.Color := clBlack;
    Sender.Canvas.Brush.Color := clLtGray;
  end
  else
  begin
    Sender.Canvas.Font.Color := clBlack;
    Sender.Canvas.Brush.Color := clWhite;
  end;
  if not ValidatePath(Item.Caption) then
  begin
    Sender.Canvas.Font.Color := clWhite;
    Sender.Canvas.Brush.Color := clMaroon;
  end;
end;

procedure TfrmDelphiLibraryHelper.ListViewLibraryDblClick(Sender: TObject);
begin
  ActionOpenFolder.Execute;
end;

procedure TfrmDelphiLibraryHelper.LoadDelphiInstallation;
var
  LIdx: integer;
begin
  comboDelphiInstallations.ItemIndex := -1;
  comboDelphiInstallations.Items.Clear;
  try
    FLibraryHelper.Load;
    for LIdx := 0 to Pred(FLibraryHelper.Count) do
    begin
      if FLibraryHelper.Installations[LIdx].Installed then
      begin
        comboDelphiInstallations.Items.Add(FLibraryHelper.Installations[LIdx]
          .ProductName);
      end;
    end;
    FLibraryHelper.GetLibraryNames(comboLibraries.Items);
  finally
    if comboPathType.Items.Count > 0 then
    begin
      comboPathType.ItemIndex := 0;
    end;
    if comboLibraries.Items.Count > 0 then
    begin
      comboLibraries.ItemIndex := 0;
    end;
    if comboDelphiInstallations.Items.Count > 0 then
    begin
      comboDelphiInstallations.ItemIndex := 0;
      comboDelphiInstallationsChange(nil);
    end;
  end;
end;

procedure TfrmDelphiLibraryHelper.LoadEnvironmentVariables;
var
  LIdx: integer;
begin
  ListViewEnvironmentVariables.Clear;
  if Assigned(FDelphiInstallation) then
  begin
    ListViewEnvironmentVariables.Items.BeginUpdate;
    try
      for LIdx := 0 to Pred(FDelphiInstallation.EnvironmentVariables.Count) do
      begin
        with ListViewEnvironmentVariables.Items.Add do
        begin
          Caption := FDelphiInstallation.EnvironmentVariables.Variable
            [LIdx].Name;
          SubItems.Add(FDelphiInstallation.EnvironmentVariables.Variable
            [LIdx].Value);
        end;
      end;
    finally
      ListViewEnvironmentVariables.Items.EndUpdate;
    end;
  end;
end;

procedure TfrmDelphiLibraryHelper.LoadSystemEnvironmentVariables;
var
  LIdx: integer;
begin
  ListViewSystemEnvironmentVariables.Clear;
  if Assigned(FDelphiInstallation) then
  begin
    ListViewSystemEnvironmentVariables.Items.BeginUpdate;
    try
      for LIdx :=
        0 to Pred(FDelphiInstallation.SystemEnvironmentVariables.Count) do
      begin
        with ListViewSystemEnvironmentVariables.Items.Add do
        begin
          Caption := FDelphiInstallation.SystemEnvironmentVariables.Variable
            [LIdx].Name;
          SubItems.Add(FDelphiInstallation.SystemEnvironmentVariables.Variable
            [LIdx].Value);
        end;
      end;
    finally
      ListViewSystemEnvironmentVariables.Items.EndUpdate;
    end;
  end;
end;

function TfrmDelphiLibraryHelper.GetApplicationParameters(AParameter: string;
  var AValue: string): Boolean;
var
  LParamIdx: integer;
begin
  Result := False;
  LParamIdx := 1;
  While (LParamIdx <= ParamCount) and (not Result) do
  begin
    try
      if Pos(AParameter, ParamStr(LParamIdx)) = 1 then
      begin
        AValue := ParamStr(LParamIdx);
        AValue := StringReplace(AValue, AParameter + ':', '',
          [rfReplaceAll, rfIgnoreCase]);
        AValue := StringReplace(AValue, AParameter, '',
          [rfReplaceAll, rfIgnoreCase]);
        AValue := AnsiDequotedStr(AValue, '"');
        Result := True;
      end;
    finally
      Inc(LParamIdx);
    end;
  end;
end;

procedure TfrmDelphiLibraryHelper.ProcessParameters;
var
  LParam: string;
  LDeduplicate, LUpdateEnvOptions: Boolean;
begin
  LDeduplicate := False;
  LUpdateEnvOptions := False;
  if GetApplicationParameters('/CLEANUP', LParam) then
  begin
    Cleanup;
  end;
  if GetApplicationParameters('/DEDUPLICATE', LParam) then
  begin
    LDeduplicate := True;
  end;
  if GetApplicationParameters('/UPDATEENVOPTIONS', LParam) then
  begin
    LUpdateEnvOptions := StrToBoolDef(LParam, True);
  end;
  if GetApplicationParameters('/TEMPLATE', LParam) then
  begin
    if FileExists(LParam) then
    begin
      ApplyTemplate(LParam, True, LDeduplicate, LUpdateEnvOptions);
      if GetApplicationParameters('/CLOSE', LParam) then
      begin
        ShowProgress('Closing...');
        Application.ProcessMessages;
        Sleep(2000);
        Application.ProcessMessages;
        HideProgress;
        Application.Terminate;
      end;
    end;
  end;

end;

procedure TfrmDelphiLibraryHelper.SaveEnvironmentVariables;
var
  LIdx: integer;
  LName, LValue: string;
begin
  if Assigned(FDelphiInstallation) then
  begin
    FDelphiInstallation.EnvironmentVariables.Clear;
    for LIdx := 0 to Pred(ListViewEnvironmentVariables.Items.Count) do
    begin
      LName := ListViewEnvironmentVariables.Items[LIdx].Caption;
      LValue := ListViewEnvironmentVariables.Items[LIdx].SubItems[0];
      FDelphiInstallation.EnvironmentVariables.Add(LName, LValue);
    end;
  end;
end;

procedure TfrmDelphiLibraryHelper.SaveLibrary;
var
  LLibrary: TStringList;
  LIdx: integer;
begin
  LLibrary := TStringList.Create;
  try
    FModified := True;
    for LIdx := 0 to Pred(ListViewLibrary.Items.Count) do
    begin
      LLibrary.Add(ListViewLibrary.Items[LIdx].Caption);
    end;
    case FActiveDelphiLibrary of
      dlAndroid32:
        FDelphiInstallation.LibraryAndroid32 := LLibrary.Text;
      dlAndroid64:
        FDelphiInstallation.LibraryAndroid64 := LLibrary.Text;
      dlIOS32:
        FDelphiInstallation.LibraryIOS32 := LLibrary.Text;
      dlIOS64:
        FDelphiInstallation.LibraryIOS64 := LLibrary.Text;
      dlIOSimulator:
        FDelphiInstallation.LibraryIOSSimulator := LLibrary.Text;
      dlOSX32:
        FDelphiInstallation.LibraryOSX32 := LLibrary.Text;
      dlOSX64:
        FDelphiInstallation.LibraryOSX64 := LLibrary.Text;
      dlOSXARM64:
        FDelphiInstallation.LibraryOSXARM64 := LLibrary.Text;
      dlWin32:
        FDelphiInstallation.LibraryWin32 := LLibrary.Text;
      dlWin64:
        FDelphiInstallation.LibraryWin64 := LLibrary.Text;
      dlLinux64:
        FDelphiInstallation.LibraryLinux64 := LLibrary.Text;
    end;
    LoadLibrary;
  finally
    FreeAndNil(LLibrary);
  end;
end;

procedure TfrmDelphiLibraryHelper.LoadLibrary;
var
  LLibrary: TStringList;
  LIdx: integer;
  LLibraryEntry, LLibraryPath: string;
begin
  ListViewLibrary.Clear;
  if Assigned(FDelphiInstallation) then
  begin
    LLibrary := TStringList.Create;
    ListViewLibrary.Items.BeginUpdate;
    try
      FDelphiInstallation.LibraryPathType := FActiveLibraryPathType;
      case FActiveDelphiLibrary of
        dlAndroid32:
          LLibrary.Text := FDelphiInstallation.LibraryAndroid32;
        dlAndroid64:
          LLibrary.Text := FDelphiInstallation.LibraryAndroid64;
        dlIOS32:
          LLibrary.Text := FDelphiInstallation.LibraryIOS32;
        dlIOS64:
          LLibrary.Text := FDelphiInstallation.LibraryIOS64;
        dlIOSimulator:
          LLibrary.Text := FDelphiInstallation.LibraryIOSSimulator;
        dlOSX32:
          LLibrary.Text := FDelphiInstallation.LibraryOSX32;
        dlOSX64:
          LLibrary.Text := FDelphiInstallation.LibraryOSX64;
        dlOSXARM64:
          LLibrary.Text := FDelphiInstallation.LibraryOSXARM64;
        dlWin32:
          LLibrary.Text := FDelphiInstallation.LibraryWin32;
        dlWin64:
          LLibrary.Text := FDelphiInstallation.LibraryWin64;
        dlLinux64:
          LLibrary.Text := FDelphiInstallation.LibraryLinux64;
      end;
      for LIdx := 0 to Pred(LLibrary.Count) do
      begin
        LLibraryEntry := LLibrary[LIdx];
        LLibraryPath := FDelphiInstallation.ExpandLibraryPath(LLibraryEntry,
          FActiveDelphiLibrary);

        if not DirectoryExists(LLibraryPath) then
        begin
          LLibraryPath := '*' + LLibraryPath;
        end;

        with ListViewLibrary.Items.Add do
        begin
          Caption := LLibraryEntry;

          SubItems.Add(LLibraryPath);
        end;
      end;
    finally
      FreeAndNil(LLibrary);
      ListViewLibrary.Items.EndUpdate;
    end;
  end;
end;

procedure TfrmDelphiLibraryHelper.TimerMainTimer(Sender: TObject);
begin
  FDelphiIsRunning := Assigned(FLibraryHelper) and
    (FLibraryHelper.IsDelphiRunning);
end;

function TfrmDelphiLibraryHelper.ValidatePath(APath: string): Boolean;
var
  LPath: string;
begin
  Result := False;
  LPath := APath;
  if Trim(LPath) <> '' then
  begin
    LPath := FDelphiInstallation.ExpandLibraryPath(LPath, FActiveDelphiLibrary);
    Result := DirectoryExists(LPath);
  end;
end;

end.
