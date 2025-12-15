object frmModelEditor: TfrmModelEditor
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Edit Model'
  ClientHeight = 328
  ClientWidth = 450
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  TextHeight = 15
  object ScrollBoxMain: TScrollBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 444
    Height = 275
    Align = alClient
    BorderStyle = bsNone
    TabOrder = 0
  end
  object PanelBottom: TPanel
    Left = 0
    Top = 281
    Width = 450
    Height = 47
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    object lblModelClass: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 282
      Height = 41
      Align = alClient
      Layout = tlCenter
      ExplicitWidth = 3
      ExplicitHeight = 15
    end
    object BtnOK: TButton
      AlignWithMargins = True
      Left = 291
      Top = 3
      Width = 75
      Height = 41
      Align = alRight
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
    object BtnCancel: TButton
      AlignWithMargins = True
      Left = 372
      Top = 3
      Width = 75
      Height = 41
      Align = alRight
      Cancel = True
      Caption = 'Cancel'
      ModalResult = 2
      TabOrder = 1
    end
  end
end
