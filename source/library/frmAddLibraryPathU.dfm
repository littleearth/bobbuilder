object frmAddLibraryPath: TfrmAddLibraryPath
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Add Library Path'
  ClientHeight = 261
  ClientWidth = 639
  Color = clBtnFace
  ParentFont = True
  Position = poOwnerFormCenter
  TextHeight = 15
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 633
    Height = 208
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 639
    ExplicitHeight = 182
    object Label1: TLabel
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 63
      Height = 15
      Align = alTop
      Caption = 'Library Path'
    end
    object GroupBoxDestination: TGroupBox
      AlignWithMargins = True
      Left = 3
      Top = 53
      Width = 633
      Height = 169
      Align = alClient
      Caption = 'Destination'
      TabOrder = 1
      ExplicitHeight = 126
      object GridPanel1: TGridPanel
        AlignWithMargins = True
        Left = 5
        Top = 20
        Width = 623
        Height = 144
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = cbAndroid32
            Row = 0
          end
          item
            Column = 1
            Control = cbIOS32
            Row = 0
          end
          item
            Column = 0
            Control = cbIOS64
            Row = 1
          end
          item
            Column = 1
            Control = cbIOSSimulator
            Row = 1
          end
          item
            Column = 0
            Control = cbOSX
            Row = 2
          end
          item
            Column = 1
            Control = cbWin32
            Row = 2
          end
          item
            Column = 0
            Control = cbWin64
            Row = 3
          end
          item
            Column = 1
            Control = cbLinux64
            Row = 3
          end>
        ExpandStyle = emFixedSize
        PopupMenu = PopupMenuDestinations
        RowCollection = <
          item
            Value = 25.000000000000000000
          end
          item
            Value = 25.000000000000000000
          end
          item
            Value = 25.000000000000000000
          end
          item
            Value = 25.000000000000000000
          end>
        TabOrder = 0
        ExplicitHeight = 101
        object cbAndroid32: TCheckBox
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 306
          Height = 30
          Align = alClient
          Caption = 'Android32'
          PopupMenu = PopupMenuDestinations
          TabOrder = 0
          ExplicitHeight = 19
        end
        object cbIOS32: TCheckBox
          AlignWithMargins = True
          Left = 315
          Top = 3
          Width = 305
          Height = 30
          Align = alClient
          Caption = 'IOS32'
          PopupMenu = PopupMenuDestinations
          TabOrder = 1
          ExplicitHeight = 19
        end
        object cbIOS64: TCheckBox
          AlignWithMargins = True
          Left = 3
          Top = 39
          Width = 306
          Height = 30
          Align = alClient
          Caption = 'IOS64'
          PopupMenu = PopupMenuDestinations
          TabOrder = 2
          ExplicitTop = 28
          ExplicitHeight = 19
        end
        object cbIOSSimulator: TCheckBox
          AlignWithMargins = True
          Left = 315
          Top = 39
          Width = 305
          Height = 30
          Align = alClient
          Caption = 'IOSSimulator'
          PopupMenu = PopupMenuDestinations
          TabOrder = 3
          ExplicitTop = 28
          ExplicitHeight = 19
        end
        object cbOSX: TCheckBox
          AlignWithMargins = True
          Left = 3
          Top = 75
          Width = 306
          Height = 30
          Align = alClient
          Caption = 'OSX'
          PopupMenu = PopupMenuDestinations
          TabOrder = 4
          ExplicitTop = 53
          ExplicitHeight = 19
        end
        object cbWin32: TCheckBox
          AlignWithMargins = True
          Left = 315
          Top = 75
          Width = 305
          Height = 30
          Align = alClient
          Caption = 'Win32'
          PopupMenu = PopupMenuDestinations
          TabOrder = 5
          ExplicitTop = 53
          ExplicitHeight = 19
        end
        object cbWin64: TCheckBox
          AlignWithMargins = True
          Left = 3
          Top = 111
          Width = 306
          Height = 30
          Align = alClient
          Caption = 'Win64'
          PopupMenu = PopupMenuDestinations
          TabOrder = 6
          ExplicitTop = 78
          ExplicitHeight = 19
        end
        object cbLinux64: TCheckBox
          AlignWithMargins = True
          Left = 315
          Top = 111
          Width = 305
          Height = 30
          Align = alClient
          Caption = 'Linux64'
          PopupMenu = PopupMenuDestinations
          TabOrder = 7
          ExplicitTop = 78
          ExplicitHeight = 19
        end
      end
    end
    object Panel3: TPanel
      Left = 0
      Top = 21
      Width = 639
      Height = 29
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      object editPath: TComboBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 552
        Height = 23
        Align = alClient
        Sorted = True
        TabOrder = 0
        OnDropDown = editPathDropDown
      end
      object BitBtn3: TBitBtn
        AlignWithMargins = True
        Left = 555
        Top = 3
        Width = 75
        Height = 23
        Action = ActionSelectFolder
        Align = alRight
        ImageIndex = 9
        Glyph.Data = {
          36040000424D3604000000000000360000002800000010000000100000000100
          2000000000000004000000000000000000000000000000000000FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00049D
          FFFB00A0FFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CA
          FFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFFFF00FF00FF00FF00FF00FF0000A0
          FFFF00A0FFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CA
          FFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF41B2FFE1FF00FF00FF00FF0000A0
          FFFF00A0FFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CA
          FFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFFFF00FF00FF00FF0000A0
          FFFF00A0FFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CA
          FFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFFFF00FF00FF00FF0000A0
          FFFF00A0FFFF20C3FFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CA
          FFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFFFF00FF00FF00FF0000A0
          FFFF00A0FFFF00A0FFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CA
          FFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFFFF00FF00FF00FF0000A0
          FFFF00A0FFFF00A0FFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CA
          FFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFF27CAFFFFFF00FF00FF00FF0000A0
          FFFF00A0FFFF00A0FFFF00A0FFFF00A0FFFF00A0FFFF00A0FFFF00A0FFFF00A0
          FFFF00A0FFFF00A0FFFF00A0FFFF00A0FFFFFF00FF00FF00FF00FF00FF0000A0
          FFFF00A0FFFF00A0FFFF00A0FFFF00A0FFFF00A0FFFF00A0FFFF00A0FFFF00A0
          FFFF00A0FFFF00A0FFFF00A0FFFF00A0FFFFFF00FF00FF00FF00FF00FF0000A0
          FFFF00A0FFFF00A0FFFF00A0FFFF00A0FFFF00A0FFFFFF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00
          FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00FF00}
        TabOrder = 1
        ExplicitLeft = 561
      end
    end
  end
  object Panel2: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 217
    Width = 633
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitTop = 191
    ExplicitWidth = 639
    object BitBtn1: TBitBtn
      AlignWithMargins = True
      Left = 430
      Top = 3
      Width = 100
      Height = 35
      Action = ActionOk
      Align = alRight
      Caption = 'Ok'
      TabOrder = 0
    end
    object BitBtn2: TBitBtn
      AlignWithMargins = True
      Left = 536
      Top = 3
      Width = 100
      Height = 35
      Action = ActionCancel
      Align = alRight
      Caption = 'Cancel'
      TabOrder = 1
    end
  end
  object PopupMenuDestinations: TPopupMenu
    Images = dmDelphiLibraryHelper.ImageListCommon
    Left = 80
    Top = 191
    object CheckAll1: TMenuItem
      Caption = 'Check All'
      OnClick = CheckAll1Click
    end
    object UncheckAll1: TMenuItem
      Caption = 'Uncheck All'
      OnClick = UncheckAll1Click
    end
  end
  object ActionList: TActionList
    Images = dmDelphiLibraryHelper.ImageListCommon
    Left = 16
    Top = 191
    object ActionOk: TAction
      Caption = 'Ok'
      ImageIndex = 21
      OnExecute = ActionOkExecute
      OnUpdate = ActionOkUpdate
    end
    object ActionCancel: TAction
      Caption = 'Cancel'
      ImageIndex = 3
      OnExecute = ActionCancelExecute
    end
    object ActionSelectFolder: TAction
      ImageIndex = 9
      OnExecute = ActionSelectFolderExecute
    end
  end
end
