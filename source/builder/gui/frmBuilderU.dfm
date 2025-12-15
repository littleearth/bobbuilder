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
            Width = 795
            Height = 13
            Align = alTop
            Caption = 'Config'
            ExplicitWidth = 31
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
            Width = 195
            Height = 13
            Align = alTop
            Caption = 'Delphi'
            OnDblClick = lblDelphiVersionDblClick
            ExplicitWidth = 29
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
          object Button3: TBitBtn
            AlignWithMargins = True
            Left = 3
            Top = 34
            Width = 173
            Height = 25
            Action = ActionVersionInformation
            Align = alTop
            Caption = 'Version information'
            DoubleBufferedMode = dbmDefault
            ParentDoubleBuffered = True
            TabOrder = 1
          end
          object Button4: TBitBtn
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 173
            Height = 25
            Action = ActionEditConfig
            Align = alTop
            Caption = 'Edit config'
            DoubleBufferedMode = dbmDefault
            ParentDoubleBuffered = True
            TabOrder = 0
          end
          object Button5: TBitBtn
            AlignWithMargins = True
            Left = 3
            Top = 65
            Width = 173
            Height = 25
            Action = ActionGitPull
            Align = alTop
            Caption = 'Git Pull'
            DoubleBufferedMode = dbmDefault
            ParentDoubleBuffered = True
            TabOrder = 2
          end
          object Button6: TBitBtn
            AlignWithMargins = True
            Left = 3
            Top = 314
            Width = 173
            Height = 25
            Action = ActionAbout
            Align = alBottom
            Caption = 'About'
            DoubleBufferedMode = dbmDefault
            ParentDoubleBuffered = True
            TabOrder = 7
          end
          object Button7: TBitBtn
            AlignWithMargins = True
            Left = 3
            Top = 283
            Width = 173
            Height = 25
            Action = ActionViewLog
            Align = alBottom
            Caption = 'View log'
            DoubleBufferedMode = dbmDefault
            ParentDoubleBuffered = True
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
        Width = 523
        Height = 33
        Align = alClient
        Layout = tlCenter
        ExplicitWidth = 3
        ExplicitHeight = 13
      end
      object lblAnimation: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 3
        Height = 33
        Align = alLeft
        Layout = tlCenter
        ExplicitHeight = 13
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
        Glyph.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00B2B2B2FFB2B2B2FFFF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00151515FF000000FF000000FF1515
          15FFB2B2B2FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00000000FF5E5E5EFFFF00FF00FF00FF006565
          65FF000000FFB2B2B2FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00B2B2B2FF000000FF656565FFFF00FF00FF00FF00FF00FF00FF00
          FF00656565FF000000FFB2B2B2FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00B2B2B2FF000000FF656565FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00767676FF000000FFA4A0A0FFFF00FF00FF00FF00FF00FF00B2B2
          B2FF000000FF656565FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00767676FF000000FFA4A0A0FFFF00FF00FF00FF000000
          00FF767676FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00767676FF000000FFFF00FF00FF00FF007676
          76FFFF00FF00FF00FF00FF00FF00FF00FF00545454FF000000FF000000FF5454
          54FFFF00FF00FF00FF00FF00FF00FF00FF00767676FFFF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00545454FF000000FFFF00FF00FF00FF001515
          15FF4C4C4CFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00545454FF151515FFFF00FF00FF00FF00FF00FF00FF00
          FF00151515FF4C4C4CFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF004C4C4CFF151515FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00151515FF414141FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF004C4C4CFF151515FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00151515FF414141FFFF00FF00FF00FF00FF00FF004141
          41FF151515FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00151515FF414141FFFF00FF00FF00FF001515
          15FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00151515FFFF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        ParentDoubleBuffered = True
        TabOrder = 3
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
        Glyph.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00151515FFFF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00A4A0
          A0FFA4A0A0FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FFFF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00767676FF0000
          00FF000000FF000000FF969696FFFF00FF00FF00FF00B2B2B2FF000000FFFF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00868686FF767676FFFF00FF00656565FF000000FF414141FFFF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00151515FF151515FFA4A0A0FFFF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00282828FF414141FFFF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00212121FF767676FFFF00FF00151515FFB2B2B2FFFF00
          FF00FF00FF00656565FF545454FF545454FFFF00FF00FF00FF00FF00FF002121
          21FFA4A0A0FFFF00FF00808080FF151515FFFF00FF00414141FF414141FFFF00
          FF001B1B1BFF282828FF545454FF545454FFFF00FF00FF00FF00FF00FF005E5E
          5EFF212121FFFF00FF00FF00FF00000000FFA4A0A0FFB2B2B2FF000000FF5454
          54FF151515FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00000000FF656565FFA4A0A0FF282828FF282828FFA4A0A0FF000000FF0000
          00FF969696FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00969696FF151515FF000000FF000000FF000000FF000000FF000000FF5E5E
          5EFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00000000FF0000
          00FF656565FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00808080FF000000FF0000
          00FF323232FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00282828FF1515
          15FFB2B2B2FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        ParentDoubleBuffered = True
        TabOrder = 2
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
        Glyph.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00383838FF000000FF000000FF000000FF000000FF000000FF0000
          00FF000000FF000000FF323232FFFF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00000000FF414141FFA4A0A0FFB2B2B2FFB2B2B2FFB2B2B2FFB2B2B2FFB2B2
          B2FFB2B2B2FFA4A0A0FF414141FF000000FFFF00FF00FF00FF00FF00FF007676
          76FF212121FFF8F8F8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFF8F8F8FF282828FF767676FFFF00FF00FF00FF005454
          54FF545454FFFFFFFFFFDDDDDDFF545454FF545454FF545454FF545454FFD7D7
          D7FFFFFFFFFFFFFFFFFFFFFFFFFF545454FF545454FFFF00FF00FF00FF005454
          54FF545454FFFFFFFFFFDDDDDDFF545454FF545454FF545454FF545454FFDDDD
          DDFFFFFFFFFFFFFFFFFFFFFFFFFF545454FF545454FFFF00FF00FF00FF005454
          54FF545454FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF545454FF545454FFFF00FF00FF00FF005454
          54FF545454FFFFFFFFFFC0C0C0FF000000FF000000FF000000FF000000FF0000
          00FF000000FFC0C0C0FFFFFFFFFF545454FF545454FFFF00FF00FF00FF005454
          54FF545454FFFFFFFFFFF8F8F8FFB2B2B2FFB2B2B2FFB2B2B2FFB2B2B2FFB2B2
          B2FFB2B2B2FFF8F8F8FFFFFFFFFF545454FF545454FFFF00FF00FF00FF005454
          54FF545454FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFF545454FF545454FFFF00FF00FF00FF005454
          54FF545454FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFE3E3E3FF6565
          65FF545454FF545454FF545454FF1B1B1BFF5E5E5EFFFF00FF00FF00FF005454
          54FF545454FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF383838FF2121
          21FF545454FF545454FF414141FF000000FF969696FFFF00FF00FF00FF005454
          54FF545454FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFA4A0
          A0FFFFFFFFFFF8F8F8FF414141FF1B1B1BFFFF00FF00FF00FF00FF00FF005454
          54FF545454FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FFA4A0
          A0FFF8F8F8FF414141FF151515FFFF00FF00FF00FF00FF00FF00FF00FF007676
          76FF212121FFF8F8F8FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF000000FF9999
          99FF414141FF151515FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00151515FF414141FFA4A0A0FFA4A0A0FFA4A0A0FFA4A0A0FF000000FF0000
          00FF151515FFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00383838FF000000FF000000FF000000FF000000FF151515FF5E5E
          5EFFFF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        ParentDoubleBuffered = True
        TabOrder = 1
      end
      object Button8: TBitBtn
        AlignWithMargins = True
        Left = 541
        Top = 3
        Width = 110
        Height = 33
        Action = ActionCodeFormat
        Align = alRight
        Caption = 'Code format'
        DoubleBufferedMode = dbmDefault
        ParentDoubleBuffered = True
        TabOrder = 0
        ExplicitLeft = 516
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
      OnExecute = ActionVersionInformationExecute
      OnUpdate = ActionVersionInformationUpdate
    end
    object ActionEditConfig: TAction
      Caption = 'Edit config'
      OnExecute = ActionEditConfigExecute
      OnUpdate = ActionEditConfigUpdate
    end
    object ActionGitPull: TAction
      Caption = 'Git Pull'
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
    object ActionCodeFormat: TAction
      Caption = 'Code format'
      ImageIndex = 11
      OnExecute = ActionCodeFormatExecute
      OnUpdate = ActionCodeFormatUpdate
    end
  end
  object PopupMenuTreeView: TPopupMenu
    Images = dmResources.ImageListCommon
    Left = 208
    Top = 364
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
