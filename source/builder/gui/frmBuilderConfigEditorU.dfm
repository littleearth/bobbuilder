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
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object PageControl: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 766
    Height = 474
    ActivePage = tabVisualEditor
    Align = alClient
    TabOrder = 0
    OnChange = PageControlChange
    object tabVisualEditor: TTabSheet
      Caption = 'Visual Editor'
      ImageIndex = 1
      object pnlTreeViewEditor: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 561
        Height = 438
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
      end
      object pnlTreeViewTools: TPanel
        AlignWithMargins = True
        Left = 570
        Top = 3
        Width = 185
        Height = 438
        Align = alRight
        BevelOuter = bvNone
        TabOrder = 1
        object BitBtn4: TBitBtn
          AlignWithMargins = True
          Left = 3
          Top = 65
          Width = 179
          Height = 25
          Action = ActionDeleteItem
          Align = alTop
          Caption = 'Delete'
          TabOrder = 2
        end
        object BitBtn5: TBitBtn
          AlignWithMargins = True
          Left = 3
          Top = 34
          Width = 179
          Height = 25
          Action = ActionEditItem
          Align = alTop
          Caption = 'Edit'
          TabOrder = 1
        end
        object BitBtn6: TBitBtn
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 179
          Height = 25
          Action = ActionAddItem
          Align = alTop
          Caption = 'Add'
          TabOrder = 0
        end
      end
    end
    object tabRawJSON: TTabSheet
      Caption = 'Raw JSON'
      object SynEditConfig: TSynEdit
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 752
        Height = 438
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
    end
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
      Width = 517
      Height = 35
      Align = alClient
      Alignment = taCenter
      Caption = 'JSON is valid'
      Layout = tlCenter
      ExplicitWidth = 67
      ExplicitHeight = 15
    end
    object BitBtn1: TBitBtn
      AlignWithMargins = True
      Left = 688
      Top = 3
      Width = 75
      Height = 35
      Action = ActionCancel
      Align = alRight
      Caption = 'Cancel'
      TabOrder = 2
    end
    object BitBtn2: TBitBtn
      AlignWithMargins = True
      Left = 607
      Top = 3
      Width = 75
      Height = 35
      Action = ActionSave
      Align = alRight
      Caption = 'Save'
      TabOrder = 1
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
    object ActionAddItem: TAction
      Caption = 'Add'
      OnExecute = ActionAddItemExecute
    end
    object ActionEditItem: TAction
      Caption = 'Edit'
      OnExecute = ActionEditItemExecute
    end
    object ActionDeleteItem: TAction
      Caption = 'Delete'
      OnExecute = ActionDeleteItemExecute
    end
  end
  object PopupMenuEditor: TPopupMenu
    Left = 380
    Top = 268
    object Add1: TMenuItem
      Action = ActionAddItem
    end
    object Edit1: TMenuItem
      Action = ActionEditItem
    end
    object N1: TMenuItem
      Caption = '-'
    end
    object Delete1: TMenuItem
      Action = ActionDeleteItem
    end
  end
end
