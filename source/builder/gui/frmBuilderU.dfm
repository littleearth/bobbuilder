object frmBuilder: TfrmBuilder
  Left = 0
  Top = 0
  Caption = 'BOB Builder'
  ClientHeight = 729
  ClientWidth = 1008
  Color = clBtnFace
  Constraints.MinHeight = 600
  Constraints.MinWidth = 400
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object Splitter1: TSplitter
    Left = 0
    Top = 473
    Width = 1008
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    ResizeStyle = rsUpdate
    ExplicitTop = 0
    ExplicitWidth = 302
  end
  object memoLog: TMemo
    Left = 0
    Top = 476
    Width = 1008
    Height = 253
    Align = alBottom
    Color = clBlack
    Font.Charset = ANSI_CHARSET
    Font.Color = clWhite
    Font.Height = -11
    Font.Name = 'Consolas'
    Font.Style = []
    ParentFont = False
    PopupMenu = PopupMenuTreeView
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 0
    Width = 1008
    Height = 473
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel1'
    Constraints.MinHeight = 288
    ShowCaption = False
    TabOrder = 0
    object pnlSettings: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 1002
      Height = 422
      Align = alClient
      BevelOuter = bvNone
      ShowCaption = False
      TabOrder = 0
      object pnlProjectSelection: TPanel
        Left = 0
        Top = 0
        Width = 1002
        Height = 57
        Align = alTop
        BevelOuter = bvNone
        Caption = 'pnlProjectSelection'
        ShowCaption = False
        TabOrder = 0
        object LabelPanel1: TPanel
          Left = 0
          Top = 0
          Width = 801
          Height = 57
          Align = alClient
          BevelOuter = bvNone
          Caption = 'Config'
          ShowCaption = False
          TabOrder = 0
          object Label2: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 31
            Height = 13
            Align = alTop
            Caption = 'Config'
          end
          object editFileName: TJvFilenameEdit
            AlignWithMargins = True
            Left = 3
            Top = 22
            Width = 795
            Height = 21
            Align = alTop
            AddQuotes = False
            DefaultExt = '.builder'
            Filter = 'Builder files (*.builder)|*.builder|All files (*.*)|*.*'
            FilterIndex = 0
            DialogOptions = [ofHideReadOnly, ofFileMustExist]
            DialogTitle = 'Configuration file'
            TabOrder = 0
            Text = ''
            OnChange = editFileNameChange
            OnDblClick = editFileNameChange
          end
        end
        object LabelPanel2: TPanel
          Left = 801
          Top = 0
          Width = 201
          Height = 57
          Align = alRight
          BevelOuter = bvNone
          Caption = 'Delphi'
          ShowCaption = False
          TabOrder = 1
          object lblDelphiVersion: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 29
            Height = 13
            Align = alTop
            Caption = 'Delphi'
            OnDblClick = lblDelphiVersionDblClick
          end
          object comboDelphiVersions: TComboBox
            AlignWithMargins = True
            Left = 3
            Top = 22
            Width = 195
            Height = 21
            Align = alTop
            Style = csDropDownList
            TabOrder = 0
          end
        end
      end
      object Panel3: TGroupBox
        AlignWithMargins = True
        Left = 816
        Top = 60
        Width = 183
        Height = 359
        Align = alRight
        Caption = 'Options'
        Color = clBtnFace
        ParentColor = False
        TabOrder = 1
        object Panel4: TPanel
          Left = 2
          Top = 15
          Width = 179
          Height = 342
          Align = alClient
          BevelOuter = bvNone
          Caption = 'Panel4'
          ShowCaption = False
          TabOrder = 0
          object Label1: TLabel
            AlignWithMargins = True
            Left = 3
            Top = 96
            Width = 173
            Height = 13
            Align = alTop
            Caption = 'Build type'
            ExplicitWidth = 47
          end
          object cbCleanupEnabled: TCheckBox
            AlignWithMargins = True
            Left = 3
            Top = 142
            Width = 173
            Height = 20
            Align = alTop
            Caption = 'Cleanup'
            Checked = True
            State = cbChecked
            TabOrder = 4
          end
          object cbCloseOnSuccess: TCheckBox
            AlignWithMargins = True
            Left = 3
            Top = 168
            Width = 173
            Height = 20
            Align = alTop
            Caption = 'Close on successful build'
            TabOrder = 5
          end
          object comboBuildType: TComboBox
            AlignWithMargins = True
            Left = 3
            Top = 115
            Width = 173
            Height = 21
            Align = alTop
            Style = csDropDownList
            ItemIndex = 0
            TabOrder = 3
            Text = 'Development'
            Items.Strings = (
              'Development'
              'Staging'
              'Production')
          end
          object Button3: TButton
            AlignWithMargins = True
            Left = 3
            Top = 34
            Width = 173
            Height = 25
            Action = ActionVersionInformation
            Align = alTop
            TabOrder = 1
          end
          object Button4: TButton
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 173
            Height = 25
            Action = ActionEditConfig
            Align = alTop
            TabOrder = 0
          end
          object Button5: TButton
            AlignWithMargins = True
            Left = 3
            Top = 65
            Width = 173
            Height = 25
            Action = ActionGitPull
            Align = alTop
            TabOrder = 2
          end
          object Button6: TButton
            AlignWithMargins = True
            Left = 3
            Top = 314
            Width = 173
            Height = 25
            Action = ActionAbout
            Align = alBottom
            TabOrder = 7
          end
          object Button7: TButton
            AlignWithMargins = True
            Left = 3
            Top = 283
            Width = 173
            Height = 25
            Action = ActionViewLog
            Align = alBottom
            TabOrder = 6
          end
        end
      end
    end
    object pnlStatus: TPanel
      AlignWithMargins = True
      Left = 3
      Top = 431
      Width = 1002
      Height = 39
      Align = alBottom
      BevelOuter = bvNone
      ShowCaption = False
      TabOrder = 1
      object lblStatus: TLabel
        AlignWithMargins = True
        Left = 12
        Top = 3
        Width = 3
        Height = 13
        Align = alClient
        Layout = tlCenter
      end
      object lblAnimation: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 3
        Height = 13
        Align = alLeft
        Layout = tlCenter
      end
      object Button2: TBitBtn
        AlignWithMargins = True
        Left = 889
        Top = 3
        Width = 110
        Height = 33
        Action = ActionDetails
        Align = alRight
        Caption = 'Details...'
        ImageIndex = 8
        Glyph.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF0F0F0FFAFAFAFFFAEAEAEFFEEEE
          EEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFBABABAFF111111FF030303FF030303FF0E0E
          0EFFB3B3B3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFB5B5B5FF070707FF5D5D5DFFF2F2F2FFF3F3F3FF6464
          64FF050505FFAEAEAEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFB2B2B2FF060606FF646464FFFDFDFDFFFFFFFFFFFFFFFFFFFEFE
          FEFF6C6C6CFF040404FFABABABFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFAFAFAFFF050505FF676767FFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFEFEFEFF6F6F6FFF030303FFA8A8A8FFFFFFFFFFFFFFFFFFFFFFFFFFACAC
          ACFF040404FF6B6B6BFFFEFEFEFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFF737373FF030303FFA5A5A5FFFFFFFFFFA9A9A9FF0303
          03FF6F6F6FFFFEFEFEFFFFFFFFFFFFFFFFFFFEFEFEFFBEBEBEFFBCBCBCFFFDFD
          FDFFFFFFFFFFFFFFFFFFFFFFFFFF777777FF020202FFA1A1A1FF181818FF7373
          73FFFFFFFFFFFFFFFFFFFFFFFFFFFBFBFBFF585858FF000000FF000000FF5151
          51FFF9F9F9FFFFFFFFFFFFFFFFFFFFFFFFFF7B7B7BFF121212FFF6F6F6FFFFFF
          FFFFFFFFFFFFFFFFFFFFFAFAFAFF545454FF0B0B0BFFBFBFBFFFC5C5C5FF0E0E
          0EFF4D4D4DFFF8F8F8FFFFFFFFFFFFFFFFFFFFFFFFFFF8F8F8FFFFFFFFFFFFFF
          FFFFFFFFFFFFFAFAFAFF515151FF0C0C0CFFC3C3C3FFFFFFFFFFFFFFFFFFC9C9
          C9FF0F0F0FFF4A4A4AFFF7F7F7FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFF9F9F9FF4E4E4EFF0E0E0EFFC6C6C6FFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFCBCBCBFF101010FF474747FFF7F7F7FFFFFFFFFFFFFFFFFFFFFFFFFFF8F8
          F8FF4A4A4AFF0F0F0FFFC8C8C8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFCECECEFF121212FF444444FFF5F5F5FFFFFFFFFFF7F7F7FF4747
          47FF101010FFCBCBCBFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFD0D0D0FF141414FF414141FFF4F4F4FF484848FF1212
          12FFCDCDCDFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFD2D2D2FF151515FF404040FF777777FFD0D0
          D0FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFD5D5D5FF757575FF}
        ParentDoubleBuffered = True
        TabOrder = 2
      end
      object btnExecute: TBitBtn
        AlignWithMargins = True
        Left = 773
        Top = 3
        Width = 110
        Height = 33
        Action = ActionExecute
        Align = alRight
        Caption = 'Execute'
        ParentDoubleBuffered = True
        TabOrder = 1
      end
      object Button1: TBitBtn
        AlignWithMargins = True
        Left = 657
        Top = 3
        Width = 110
        Height = 33
        Action = ActionPreviewScript
        Align = alRight
        Caption = 'Preview Script'
        ImageIndex = 7
        Glyph.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FFFF
          FFFFC8C8C8FF373737FF020202FF000000FF000000FF000000FF000000FF0000
          00FF000000FF020202FF353535FFC5C5C5FFFFFFFFFFFF00FF00FF00FF00DCDC
          DCFF0B0B0BFF444444FFA5A5A5FFACACACFFACACACFFACACACFFACACACFFACAC
          ACFFACACACFFA6A6A6FF474747FF0A0A0AFFD9D9D9FFFF00FF00FF00FF007474
          74FF222222FFFAFAFAFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFBFBFBFF262626FF707070FFFF00FF00FF00FF005757
          57FF515151FFFFFFFFFFDDDDDDFF555555FF525252FF525252FF545454FFDADA
          DAFFFFFFFFFFFFFFFFFFFFFFFFFF555555FF525252FFFF00FF00FF00FF005656
          56FF525252FFFFFFFFFFDFDFDFFF5A5A5AFF575757FF575757FF595959FFDCDC
          DCFFFFFFFFFFFFFFFFFFFFFFFFFF565656FF525252FFFF00FF00FF00FF005656
          56FF525252FFFFFFFFFFFFFFFFFFFEFEFEFFFDFDFDFFFDFDFDFFFDFDFDFFFDFD
          FDFFFDFDFDFFFFFFFFFFFFFFFFFF565656FF525252FFFF00FF00FF00FF005656
          56FF525252FFFFFFFFFFC4C4C4FF010101FF000000FF000000FF000000FF0000
          00FF010101FFC0C0C0FFFFFFFFFF565656FF525252FFFF00FF00FF00FF005656
          56FF525252FFFFFFFFFFF7F7F7FFAFAFAFFFACACACFFACACACFFACACACFFACAC
          ACFFAFAFAFFFF6F6F6FFFFFFFFFF565656FF525252FFFF00FF00FF00FF005656
          56FF525252FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF565656FF525252FFFF00FF00FF00FF005656
          56FF525252FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE3E3E3FF6969
          69FF525252FF525252FF525252FF1A1A1AFF5B5B5BFFFF00FF00FF00FF005656
          56FF525252FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF363636FF2222
          22FF575757FF575757FF444444FF000000FF949494FFFF00FF00FF00FF005656
          56FF525252FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF020202FFA5A5
          A5FFFFFFFFFFF5F5F5FF404040FF1C1C1CFFF2F2F2FFFF00FF00FF00FF005757
          57FF515151FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF010101FFA8A8
          A8FFF5F5F5FF424242FF131313FFD1D1D1FFFFFFFFFFFF00FF00FF00FF007676
          76FF202020FFF9F9F9FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF010101FF9C9C
          9CFF424242FF131313FFD0D0D0FFFFFFFFFFFFFFFFFFFF00FF00FF00FF00DEDE
          DEFF0D0D0DFF404040FFA0A0A0FFA7A7A7FFA7A7A7FFA6A6A6FF000000FF0707
          07FF141414FFD0D0D0FFFFFFFFFFFFFFFFFFFFFFFFFFFF00FF00FF00FF00FFFF
          FFFFCCCCCCFF3C3C3CFF050505FF010101FF010101FF010101FF161616FF6262
          62FFE3E3E3FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF00FF00}
        ParentDoubleBuffered = True
        TabOrder = 0
      end
    end
  end
  object ActionList: TActionList
    Images = dmResources.ImageListCommon
    OnUpdate = ActionListUpdate
    Left = 64
    Top = 332
    object ActionExecute: TAction
      Caption = 'Execute'
      ImageIndex = 6
      ShortCut = 120
      OnExecute = ActionExecuteExecute
      OnUpdate = ActionExecuteUpdate
    end
    object ActionPreviewScript: TAction
      Caption = 'Preview Script'
      ImageIndex = 7
      ShortCut = 24696
      OnExecute = ActionPreviewScriptExecute
      OnUpdate = ActionPreviewScriptUpdate
    end
    object ActionDetails: TAction
      Caption = 'Details...'
      ImageIndex = 8
      ShortCut = 114
      OnExecute = ActionDetailsExecute
    end
    object ActionEditItem: TAction
      Caption = 'Edit...'
      ImageIndex = 4
      OnExecute = ActionEditItemExecute
      OnUpdate = ActionEditItemUpdate
    end
    object ActionOpenFileLocation: TAction
      Caption = 'Open file location'
      ImageIndex = 5
      OnExecute = ActionEditItemExecute
      OnUpdate = ActionEditItemUpdate
    end
    object ActionVersionInformation: TAction
      Caption = 'Version information'
      ImageIndex = 4
      OnExecute = ActionVersionInformationExecute
      OnUpdate = ActionVersionInformationUpdate
    end
    object ActionEditConfig: TAction
      Caption = 'Edit config'
      ImageIndex = 4
      OnExecute = ActionEditConfigExecute
      OnUpdate = ActionEditConfigUpdate
    end
    object ActionGitPull: TAction
      Caption = 'Git Pull'
      ImageIndex = 5
      OnExecute = ActionGitPullExecute
      OnUpdate = ActionGitPullUpdate
    end
    object ActionAbout: TAction
      Caption = 'About'
      OnExecute = ActionAboutExecute
    end
    object ActionViewLog: TAction
      Caption = 'View log'
      OnExecute = ActionViewLogExecute
    end
  end
  object PopupMenuTreeView: TPopupMenu
    Images = dmResources.ImageListCommon
    Left = 248
    Top = 372
    object Edit1: TMenuItem
      Action = ActionEditItem
      Default = True
    end
    object Openfilelocation1: TMenuItem
      Action = ActionOpenFileLocation
    end
  end
  object NotificationCenter: TNotificationCenter
    OnReceiveLocalNotification = NotificationCenterReceiveLocalNotification
    Left = 91
    Top = 431
  end
  object TimerBuilder: TTimer
    Enabled = False
    OnTimer = TimerBuilderTimer
    Left = 208
    Top = 436
  end
end
