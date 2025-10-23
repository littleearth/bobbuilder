object frmBuilderConfigEditor: TfrmBuilderConfigEditor
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Config editor'
  ClientHeight = 527
  ClientWidth = 772
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  TextHeight = 15
  object SynEditConfig: TSynEdit
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 766
    Height = 474
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    TabOrder = 0
    CodeFolding.GutterShapeSize = 11
    CodeFolding.CollapsedLineColor = clGrayText
    CodeFolding.FolderBarLinesColor = clGrayText
    CodeFolding.IndentGuidesColor = clGray
    CodeFolding.IndentGuides = True
    CodeFolding.ShowCollapsedLine = False
    CodeFolding.ShowHintMark = True
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Courier New'
    Gutter.Font.Style = []
    Gutter.ShowLineNumbers = True
    Gutter.ShowModification = True
    Highlighter = SynJSONSyn
    Lines.Strings = (
      'SynEditConfig')
    OnChange = SynEditConfigChange
    FontSmoothing = fsmNone
  end
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 483
    Width = 766
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 1
    object lblJSONStatus: TLabel
      AlignWithMargins = True
      Left = 84
      Top = 3
      Width = 67
      Height = 15
      Align = alClient
      Alignment = taCenter
      Caption = 'JSON is valid'
      Layout = tlCenter
    end
    object BitBtn1: TBitBtn
      AlignWithMargins = True
      Left = 694
      Top = 3
      Width = 75
      Height = 35
      Action = ActionCancel
      Align = alRight
      Caption = 'Cancel'
      TabOrder = 2
      ExplicitLeft = 688
    end
    object BitBtn2: TBitBtn
      AlignWithMargins = True
      Left = 613
      Top = 3
      Width = 75
      Height = 35
      Action = ActionSave
      Align = alRight
      Caption = 'Save'
      TabOrder = 1
      ExplicitLeft = 607
    end
    object BitBtn3: TBitBtn
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 75
      Height = 35
      Action = ActionReload
      Align = alLeft
      Caption = 'Reload'
      TabOrder = 0
    end
  end
  object SynJSONSyn: TSynJSONSyn
    Options.AutoDetectEnabled = False
    Options.AutoDetectLineLimit = 0
    Options.Visible = False
    Left = 72
    Top = 304
  end
  object ActionList: TActionList
    Images = dmResources.ImageListCommon
    Left = 176
    Top = 268
    object ActionSave: TAction
      Caption = 'Save'
      ImageIndex = 9
      OnExecute = ActionSaveExecute
    end
    object ActionCancel: TAction
      Caption = 'Cancel'
      ImageIndex = 10
      OnExecute = ActionCancelExecute
    end
    object ActionReload: TAction
      Caption = 'Reload'
      ImageIndex = 2
      OnExecute = ActionReloadExecute
    end
  end
end
