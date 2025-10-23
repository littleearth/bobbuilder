object frmDelphiLibraryHelper: TfrmDelphiLibraryHelper
  Left = 0
  Top = 0
  Caption = 'Delphi library helper'
  ClientHeight = 660
  ClientWidth = 766
  Color = clBtnFace
  ParentFont = True
  Menu = MainMenu
  Position = poScreenCenter
  OnActivate = FormActivate
  OnCloseQuery = FormCloseQuery
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 15
  object GroupBox1: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 760
    Height = 90
    Align = alTop
    Caption = 'Delphi Installation'
    TabOrder = 0
    ExplicitWidth = 766
    object BitBtn4: TBitBtn
      AlignWithMargins = True
      Left = 611
      Top = 20
      Width = 150
      Height = 65
      Action = ActionApplyTemplate
      Align = alRight
      Caption = 'Apply Template'
      TabOrder = 5
      ExplicitLeft = 623
      ExplicitHeight = 45
    end
    object BitBtn7: TBitBtn
      AlignWithMargins = True
      Left = 449
      Top = 20
      Width = 75
      Height = 65
      Action = ActionSave
      Align = alRight
      Caption = 'Save'
      TabOrder = 3
      ExplicitLeft = 461
      ExplicitHeight = 45
    end
    object BitBtn8: TBitBtn
      AlignWithMargins = True
      Left = 530
      Top = 20
      Width = 75
      Height = 65
      Action = ActionLoad
      Align = alRight
      Caption = 'Load'
      TabOrder = 4
      ExplicitLeft = 542
      ExplicitHeight = 45
    end
    object Panel6: TPanel
      Left = 2
      Top = 17
      Width = 281
      Height = 71
      Align = alLeft
      BevelOuter = bvNone
      TabOrder = 0
      object comboDelphiInstallations: TComboBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 275
        Height = 23
        Align = alTop
        Style = csDropDownList
        TabOrder = 0
        OnChange = comboDelphiInstallationsChange
      end
      object GridPanel2: TGridPanel
        Left = 0
        Top = 29
        Width = 281
        Height = 42
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            Value = 100.000000000000000000
          end>
        ControlCollection = <
          item
            Column = 0
            Control = lblRootPath
            Row = 0
          end
          item
            Column = 0
            Control = lblEnvOptionsPath
            Row = 1
          end>
        RowCollection = <
          item
            Value = 50.000000000000000000
          end
          item
            Value = 50.000000000000000000
          end>
        TabOrder = 1
        ExplicitLeft = 72
        ExplicitTop = 32
        ExplicitWidth = 185
        ExplicitHeight = 41
        object lblRootPath: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 3
          Height = 15
          Cursor = crHandPoint
          Align = alClient
          Alignment = taCenter
          Layout = tlCenter
          WordWrap = True
          OnClick = lblRootPathClick
        end
        object lblEnvOptionsPath: TLabel
          AlignWithMargins = True
          Left = 275
          Top = 24
          Width = 3
          Height = 15
          Cursor = crHandPoint
          Align = alClient
          Alignment = taRightJustify
          Layout = tlCenter
          WordWrap = True
          OnClick = lblRootPathClick
        end
      end
    end
    object Panel9: TPanel
      Left = 305
      Top = 17
      Width = 135
      Height = 71
      Align = alRight
      BevelOuter = bvNone
      TabOrder = 2
      ExplicitLeft = 323
      ExplicitHeight = 51
      object cbDeduplicateOnSave: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 26
        Width = 129
        Height = 17
        Align = alTop
        Caption = 'Deduplicate on save'
        Checked = True
        State = cbChecked
        TabOrder = 1
        ExplicitTop = 6
      end
      object cbUpdateEnvOptions: TCheckBox
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 129
        Height = 17
        Align = alTop
        Caption = 'Update EnvOptions'
        TabOrder = 0
        OnClick = cbUpdateEnvOptionsClick
        ExplicitLeft = 6
        ExplicitTop = 7
      end
    end
    object Panel7: TPanel
      Left = 283
      Top = 17
      Width = 22
      Height = 71
      Align = alClient
      BevelOuter = bvNone
      TabOrder = 1
      ExplicitWidth = 40
      ExplicitHeight = 51
    end
  end
  object GroupBox2: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 99
    Width = 760
    Height = 240
    Align = alTop
    Caption = 'Environment Variables'
    TabOrder = 1
    ExplicitTop = 79
    ExplicitWidth = 778
    object GridPanel1: TGridPanel
      Left = 2
      Top = 17
      Width = 774
      Height = 221
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
          Control = Panel2
          Row = 0
        end
        item
          Column = 1
          Control = Panel3
          Row = 0
        end>
      RowCollection = <
        item
          Value = 100.000000000000000000
        end>
      TabOrder = 0
      object Panel2: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 381
        Height = 215
        Align = alClient
        BevelOuter = bvNone
        Caption = 'Panel2'
        TabOrder = 0
        object Label1: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 23
          Height = 15
          Align = alTop
          Caption = 'User'
        end
        object ListViewEnvironmentVariables: TListView
          Left = 0
          Top = 62
          Width = 381
          Height = 153
          Align = alClient
          Columns = <
            item
              Caption = 'Name'
              Width = 200
            end
            item
              AutoSize = True
              Caption = 'Value'
            end>
          GridLines = True
          MultiSelect = True
          ReadOnly = True
          RowSelect = True
          TabOrder = 1
          ViewStyle = vsReport
          OnCustomDrawItem = ListViewEnvironmentVariablesCustomDrawItem
        end
        object Panel4: TPanel
          Left = 0
          Top = 21
          Width = 381
          Height = 41
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object BitBtn5: TBitBtn
            AlignWithMargins = True
            Left = 89
            Top = 3
            Width = 80
            Height = 35
            Action = ActionDeleteEnvironmentVariable
            Align = alLeft
            Caption = 'Delete'
            TabOrder = 1
          end
          object BitBtn6: TBitBtn
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 80
            Height = 35
            Action = ActionAddEnvironmentVariables
            Align = alLeft
            Caption = 'Add'
            TabOrder = 0
          end
        end
      end
      object Panel3: TPanel
        AlignWithMargins = True
        Left = 390
        Top = 3
        Width = 381
        Height = 215
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object Label2: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 38
          Height = 15
          Align = alTop
          Caption = 'System'
        end
        object ListViewSystemEnvironmentVariables: TListView
          Left = 0
          Top = 62
          Width = 381
          Height = 153
          Align = alClient
          Columns = <
            item
              Caption = 'Name'
              Width = 200
            end
            item
              AutoSize = True
              Caption = 'Value'
            end>
          GridLines = True
          MultiSelect = True
          ReadOnly = True
          RowSelect = True
          TabOrder = 1
          ViewStyle = vsReport
          OnCustomDrawItem = ListViewEnvironmentVariablesCustomDrawItem
        end
        object Panel5: TPanel
          Left = 0
          Top = 21
          Width = 381
          Height = 41
          Align = alTop
          BevelOuter = bvNone
          TabOrder = 0
          object BitBtn11: TBitBtn
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 146
            Height = 35
            Action = ActionSystemProperties
            Align = alLeft
            Caption = 'System Properties'
            TabOrder = 0
          end
        end
      end
    end
  end
  object GroupBox3: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 345
    Width = 760
    Height = 236
    Align = alClient
    Caption = 'Library'
    TabOrder = 2
    ExplicitTop = 325
    ExplicitWidth = 778
    ExplicitHeight = 337
    object Panel1: TPanel
      Left = 2
      Top = 17
      Width = 756
      Height = 56
      Align = alTop
      BevelOuter = bvNone
      TabOrder = 0
      ExplicitWidth = 774
      object BitBtn1: TBitBtn
        AlignWithMargins = True
        Left = 658
        Top = 3
        Width = 100
        Height = 50
        Action = ActionSearch
        Align = alLeft
        Caption = 'Search'
        TabOrder = 5
      end
      object BitBtn2: TBitBtn
        AlignWithMargins = True
        Left = 365
        Top = 3
        Width = 75
        Height = 50
        Action = ActionDeleteLibraryPath
        Align = alLeft
        Caption = 'Delete'
        TabOrder = 2
      end
      object BitBtn3: TBitBtn
        AlignWithMargins = True
        Left = 284
        Top = 3
        Width = 75
        Height = 50
        Action = ActionAddLibraryPath
        Align = alLeft
        Caption = 'Add'
        TabOrder = 1
      end
      object BitBtn9: TBitBtn
        AlignWithMargins = True
        Left = 446
        Top = 3
        Width = 100
        Height = 50
        Action = ActionDeleteAllLibraryPaths
        Align = alLeft
        Caption = 'Delete All'
        TabOrder = 3
      end
      object BitBtn10: TBitBtn
        AlignWithMargins = True
        Left = 552
        Top = 3
        Width = 100
        Height = 50
        Action = ActionFindReplace
        Align = alLeft
        Caption = 'Replace'
        TabOrder = 4
      end
      object Panel8: TPanel
        Left = 0
        Top = 0
        Width = 281
        Height = 56
        Align = alLeft
        BevelOuter = bvNone
        TabOrder = 0
        object comboLibraries: TComboBox
          AlignWithMargins = True
          Left = 3
          Top = 32
          Width = 275
          Height = 23
          Align = alTop
          Style = csDropDownList
          TabOrder = 1
          OnChange = comboLibrariesChange
        end
        object comboPathType: TComboBox
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 275
          Height = 23
          Align = alTop
          Style = csDropDownList
          TabOrder = 0
          OnChange = comboLibrariesChange
          Items.Strings = (
            'Search'
            'Browse'
            'Debug DCU')
        end
      end
    end
    object StatusBar: TStatusBar
      Left = 2
      Top = 215
      Width = 756
      Height = 19
      Panels = <
        item
          Width = 150
        end
        item
          Width = 50
        end>
      ExplicitTop = 316
      ExplicitWidth = 774
    end
    object ListViewLibrary: TListView
      Left = 2
      Top = 73
      Width = 756
      Height = 142
      Align = alClient
      Columns = <
        item
          AutoSize = True
          Caption = 'Entry'
        end
        item
          AutoSize = True
          Caption = 'Path'
        end>
      GridLines = True
      MultiSelect = True
      ReadOnly = True
      RowSelect = True
      PopupMenu = PopupMenuLibrary
      TabOrder = 1
      ViewStyle = vsReport
      OnCustomDrawItem = ListViewLibraryCustomDrawItem
      OnDblClick = ListViewLibraryDblClick
      ExplicitWidth = 774
      ExplicitHeight = 243
    end
  end
  object GroupBox4: TGroupBox
    AlignWithMargins = True
    Left = 3
    Top = 587
    Width = 760
    Height = 70
    Align = alBottom
    Caption = 'Tools'
    TabOrder = 3
    ExplicitTop = 668
    ExplicitWidth = 778
    object BitBtn17: TBitBtn
      AlignWithMargins = True
      Left = 693
      Top = 20
      Width = 80
      Height = 45
      Action = ActionAbout
      Align = alRight
      Caption = 'About'
      TabOrder = 6
    end
    object BitBtn18: TBitBtn
      AlignWithMargins = True
      Left = 5
      Top = 20
      Width = 80
      Height = 45
      Action = ActionExport
      Align = alLeft
      Caption = 'Export'
      TabOrder = 0
    end
    object BitBtn12: TBitBtn
      AlignWithMargins = True
      Left = 91
      Top = 20
      Width = 80
      Height = 45
      Action = ActionImport
      Align = alLeft
      Caption = 'Import'
      TabOrder = 1
    end
    object BitBtn13: TBitBtn
      AlignWithMargins = True
      Left = 177
      Top = 20
      Width = 140
      Height = 45
      Action = ActionOpenFolder
      Align = alLeft
      Caption = 'Open folder'
      TabOrder = 2
    end
    object BitBtn14: TBitBtn
      AlignWithMargins = True
      Left = 323
      Top = 20
      Width = 140
      Height = 45
      Action = ActionCopyLibraryPath
      Align = alLeft
      Caption = 'Copy path'
      TabOrder = 3
    end
    object BitBtn15: TBitBtn
      AlignWithMargins = True
      Left = 469
      Top = 20
      Width = 140
      Height = 45
      Action = ActionCopyLibraryValue
      Align = alLeft
      Caption = 'Copy value'
      TabOrder = 4
    end
    object btnTools: TBitBtn
      AlignWithMargins = True
      Left = 615
      Top = 20
      Width = 66
      Height = 45
      Align = alLeft
      Caption = 'Tools'
      TabOrder = 5
      OnClick = btnToolsClick
    end
  end
  object ActionList: TActionList
    Images = dmDelphiLibraryHelper.ImageListCommon
    OnUpdate = ActionListUpdate
    Left = 616
    Top = 665
    object ActionAddEnvironmentVariables: TAction
      Caption = 'Add'
      ImageIndex = 10
      OnExecute = ActionAddEnvironmentVariablesExecute
    end
    object ActionDeleteEnvironmentVariable: TAction
      Caption = 'Delete'
      ImageIndex = 8
      OnExecute = ActionDeleteEnvironmentVariableExecute
    end
    object ActionAddLibraryPath: TAction
      Caption = 'Add'
      ImageIndex = 10
      OnExecute = ActionAddLibraryPathExecute
    end
    object ActionDeleteLibraryPath: TAction
      Caption = 'Delete'
      ImageIndex = 8
      OnExecute = ActionDeleteLibraryPathExecute
    end
    object ActionDeleteAllLibraryPaths: TAction
      Caption = 'Delete All'
      ImageIndex = 4
      OnExecute = ActionDeleteAllLibraryPathsExecute
    end
    object ActionSave: TAction
      Caption = 'Save'
      ImageIndex = 11
      ShortCut = 16467
      OnExecute = ActionSaveExecute
      OnUpdate = ActionSaveUpdate
    end
    object ActionLoad: TAction
      Caption = 'Load'
      ImageIndex = 12
      OnExecute = ActionLoadExecute
      OnUpdate = ActionLoadUpdate
    end
    object ActionApplyTemplate: TAction
      Caption = 'Apply Template'
      ImageIndex = 5
      ShortCut = 16506
      OnExecute = ActionApplyTemplateExecute
      OnUpdate = ActionApplyTemplateUpdate
    end
    object ActionExit: TAction
      Caption = 'Exit'
      ImageIndex = 6
      OnExecute = ActionExitExecute
    end
    object ActionAbout: TAction
      Caption = 'About'
      ImageIndex = 0
      OnExecute = ActionAboutExecute
    end
    object ActionFindReplace: TAction
      Caption = 'Replace'
      ImageIndex = 13
      OnExecute = ActionFindReplaceExecute
    end
    object ActionCopyLibraryPath: TAction
      Caption = 'Copy path'
      ImageIndex = 14
      OnExecute = ActionCopyLibraryPathExecute
      OnUpdate = ActionCopyLibraryPathUpdate
    end
    object ActionOpenFolder: TAction
      Caption = 'Open folder'
      ImageIndex = 9
      OnExecute = ActionOpenFolderExecute
      OnUpdate = ActionCopyLibraryPathUpdate
    end
    object ActionCopyLibraryValue: TAction
      Caption = 'Copy value'
      ImageIndex = 14
      OnExecute = ActionCopyLibraryValueExecute
      OnUpdate = ActionCopyLibraryPathUpdate
    end
    object ActionSearch: TAction
      Caption = 'Search'
      ImageIndex = 15
      OnExecute = ActionSearchExecute
    end
    object ActionSystemProperties: TAction
      Caption = 'System Properties'
      ImageIndex = 16
      OnExecute = ActionSystemPropertiesExecute
    end
    object ActionExport: TAction
      Caption = 'Export'
      ImageIndex = 17
      OnExecute = ActionExportExecute
    end
    object ActionImport: TAction
      Caption = 'Import'
      ImageIndex = 18
      OnExecute = ActionImportExecute
    end
    object ActionCopySearchToBrowse: TAction
      Caption = 'Copy search paths to browse paths'
      ImageIndex = 20
      OnExecute = ActionCopySearchToBrowseExecute
    end
    object ActionCopyBrowseToSearch: TAction
      Caption = 'Copy browse paths to search paths'
      ImageIndex = 20
      OnExecute = ActionCopyBrowseToSearchExecute
    end
    object ActionCleanUp: TAction
      Caption = 'Cleanup'
      ImageIndex = 19
      OnExecute = ActionCleanUpExecute
    end
    object ActionViewLog: TAction
      Caption = 'View log'
      ImageIndex = 22
      OnExecute = ActionViewLogExecute
    end
    object ActionRemoveBrowseFromSearch: TAction
      Caption = 'Remove browse paths from search paths'
      ImageIndex = 19
      OnExecute = ActionRemoveBrowseFromSearchExecute
    end
    object ActionDeduplicate: TAction
      Caption = 'Deduplicate'
      ImageIndex = 23
      OnExecute = ActionDeduplicateExecute
    end
  end
  object MainMenu: TMainMenu
    Images = dmDelphiLibraryHelper.ImageListCommon
    Left = 644
    Top = 665
    object File1: TMenuItem
      Caption = 'File'
      object Load1: TMenuItem
        Action = ActionLoad
      end
      object Save1: TMenuItem
        Action = ActionSave
      end
      object ImportExport1: TMenuItem
        Caption = 'Tools'
        object Import1: TMenuItem
          Action = ActionImport
        end
        object Export1: TMenuItem
          Action = ActionExport
        end
        object SystemProperties1: TMenuItem
          Action = ActionSystemProperties
        end
        object N5: TMenuItem
          Caption = '-'
        end
        object Copysearchpathstobrowsepaths2: TMenuItem
          Action = ActionCopySearchToBrowse
        end
        object Copybrowsepathstosearchpaths2: TMenuItem
          Action = ActionCopyBrowseToSearch
        end
        object N7: TMenuItem
          Caption = '-'
        end
        object Deduplicate1: TMenuItem
          Action = ActionDeduplicate
        end
        object Cleanup2: TMenuItem
          Action = ActionCleanUp
        end
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object Exit1: TMenuItem
        Action = ActionExit
      end
    end
    object emplates1: TMenuItem
      Caption = 'Templates'
      object ApplyTemplate2: TMenuItem
        Action = ActionApplyTemplate
      end
    end
    object Help1: TMenuItem
      Caption = 'Help'
      object Viewlog1: TMenuItem
        Action = ActionViewLog
      end
      object About1: TMenuItem
        Action = ActionAbout
      end
    end
  end
  object PopupMenuLibrary: TPopupMenu
    Images = dmDelphiLibraryHelper.ImageListCommon
    Left = 708
    Top = 665
    object Openfolder1: TMenuItem
      Action = ActionOpenFolder
      Default = True
    end
    object Copy1: TMenuItem
      Caption = 'Copy'
      ImageIndex = 14
      object Copypath1: TMenuItem
        Action = ActionCopyLibraryPath
      end
      object Copyvalue1: TMenuItem
        Action = ActionCopyLibraryValue
      end
    end
    object N4: TMenuItem
      Caption = '-'
    end
    object ools1: TMenuItem
      Caption = 'Tools'
      object Export2: TMenuItem
        Action = ActionExport
      end
      object Import2: TMenuItem
        Action = ActionImport
      end
      object N9: TMenuItem
        Caption = '-'
      end
      object Copybrowsepathstosearchpaths3: TMenuItem
        Action = ActionCopyBrowseToSearch
      end
      object Copysearchpathstobrowsepaths3: TMenuItem
        Action = ActionCopySearchToBrowse
      end
      object Removebrowsepathsfromsearchpaths2: TMenuItem
        Action = ActionRemoveBrowseFromSearch
      end
      object N8: TMenuItem
        Caption = '-'
      end
      object Deduplicate3: TMenuItem
        Action = ActionDeduplicate
      end
      object Cleanup3: TMenuItem
        Action = ActionCleanUp
      end
    end
    object Replace1: TMenuItem
      Action = ActionFindReplace
    end
    object Search1: TMenuItem
      Action = ActionSearch
    end
    object N3: TMenuItem
      Caption = '-'
    end
    object Add1: TMenuItem
      Action = ActionAddLibraryPath
    end
    object Delete1: TMenuItem
      Action = ActionDeleteLibraryPath
    end
    object N2: TMenuItem
      Caption = '-'
    end
    object DeleteAll1: TMenuItem
      Action = ActionDeleteAllLibraryPaths
    end
  end
  object TimerMain: TTimer
    Interval = 30000
    OnTimer = TimerMainTimer
    Left = 740
    Top = 665
  end
  object PopupMenuTools: TPopupMenu
    Images = dmDelphiLibraryHelper.ImageListCommon
    Left = 676
    Top = 665
    object Viewlog2: TMenuItem
      Action = ActionViewLog
    end
    object N10: TMenuItem
      Caption = '-'
    end
    object Copybrowsepathstosearchpaths1: TMenuItem
      Action = ActionCopyBrowseToSearch
    end
    object Copysearchpathstobrowsepaths1: TMenuItem
      Action = ActionCopySearchToBrowse
    end
    object Removebrowsepathsfromsearchpaths1: TMenuItem
      Action = ActionRemoveBrowseFromSearch
    end
    object N6: TMenuItem
      Caption = '-'
    end
    object Deduplicate2: TMenuItem
      Action = ActionDeduplicate
    end
    object Cleanup1: TMenuItem
      Action = ActionCleanUp
    end
  end
end
