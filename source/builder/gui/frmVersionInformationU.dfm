object frmVersionInformation: TfrmVersionInformation
  Left = 0
  Top = 0
  BorderIcons = [biSystemMenu, biMaximize]
  Caption = 'Version information'
  ClientHeight = 444
  ClientWidth = 766
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poOwnerFormCenter
  OnShow = FormShow
  TextHeight = 15
  object Panel1: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 406
    Width = 760
    Height = 35
    Align = alBottom
    BevelOuter = bvNone
    Caption = 'Panel1'
    ShowCaption = False
    TabOrder = 2
    ExplicitTop = 423
    ExplicitWidth = 766
    object Button1: TBitBtn
      AlignWithMargins = True
      Left = 688
      Top = 3
      Width = 75
      Height = 29
      Action = ActionCancel
      Align = alRight
      Caption = 'Cancel'
      ParentDoubleBuffered = True
      TabOrder = 1
    end
    object Button2: TBitBtn
      AlignWithMargins = True
      Left = 607
      Top = 3
      Width = 75
      Height = 29
      Action = ActionSave
      Align = alRight
      Caption = 'Save'
      ParentDoubleBuffered = True
      TabOrder = 0
    end
  end
  object Panel17: TPanel
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 567
    Height = 397
    Align = alClient
    BevelOuter = bvNone
    Caption = 'Panel17'
    ShowCaption = False
    TabOrder = 0
    ExplicitWidth = 573
    ExplicitHeight = 414
    object pnlVersionNumberDetails: TGridPanel
      AlignWithMargins = True
      Left = 3
      Top = 97
      Width = 567
      Height = 314
      Align = alClient
      BevelOuter = bvNone
      Caption = 'pnlVersionNumberDetails'
      ColumnCollection = <
        item
          Value = 100.000000000000000000
        end>
      ControlCollection = <
        item
          Column = 0
          Control = Panel2
          Row = 0
        end
        item
          Column = 0
          Control = Panel3
          Row = 1
        end
        item
          Column = 0
          Control = Panel4
          Row = 2
        end>
      RowCollection = <
        item
          Value = 33.333333333333340000
        end
        item
          Value = 33.333333333333340000
        end
        item
          Value = 33.333333333333310000
        end>
      ShowCaption = False
      TabOrder = 1
      object Panel2: TPanel
        Left = 0
        Top = 0
        Width = 567
        Height = 105
        Align = alClient
        BevelOuter = bvNone
        Caption = 'Panel2'
        ShowCaption = False
        TabOrder = 0
        object Label14: TLabel
          Left = 0
          Top = 0
          Width = 182
          Height = 15
          Align = alTop
          Caption = 'Production (File / Product version)'
        end
        object GridPanel2: TGridPanel
          Left = 0
          Top = 15
          Width = 567
          Height = 90
          Align = alClient
          BevelOuter = bvNone
          Caption = 'GridPanel2'
          ColumnCollection = <
            item
              Value = 20.000000000000000000
            end
            item
              Value = 20.000000000000000000
            end
            item
              Value = 20.000000000000000000
            end
            item
              Value = 20.000000000000000000
            end
            item
              Value = 20.000000000000000000
            end>
          ControlCollection = <
            item
              Column = 0
              Control = Panel16
              Row = 0
            end
            item
              Column = 1
              Control = Panel20
              Row = 0
            end
            item
              Column = 2
              Control = Panel21
              Row = 0
            end
            item
              Column = 3
              Control = Panel22
              Row = 0
            end
            item
              Column = 4
              Control = Panel23
              Row = 0
            end
            item
              Column = 0
              Control = Panel26
              Row = 1
            end
            item
              Column = 1
              Control = Panel27
              Row = 1
            end
            item
              Column = 2
              Control = Panel28
              Row = 1
            end
            item
              Column = 3
              Control = Panel29
              Row = 1
            end>
          RowCollection = <
            item
              Value = 50.000000000000000000
            end
            item
              Value = 50.000000000000000000
            end>
          ShowCaption = False
          TabOrder = 0
          object Panel16: TPanel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 107
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object editProductionMajorVersion: TEdit
              Tag = 1
              Left = 0
              Top = 17
              Width = 107
              Height = 23
              Align = alTop
              TabOrder = 1
              OnChange = editProductionMajorVersionChange
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
            object cbMajorVersionAdjustAll: TCheckBox
              Tag = 1
              Left = 0
              Top = 0
              Width = 107
              Height = 17
              Align = alTop
              Caption = 'File major'
              Checked = True
              State = cbChecked
              TabOrder = 0
            end
          end
          object Panel20: TPanel
            AlignWithMargins = True
            Left = 116
            Top = 3
            Width = 108
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            object editProductionMinorVersion: TEdit
              Tag = 2
              Left = 0
              Top = 17
              Width = 108
              Height = 23
              Align = alTop
              TabOrder = 1
              OnChange = editProductionMajorVersionChange
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
            object cbMinorVersionAdjustAll: TCheckBox
              Tag = 2
              Left = 0
              Top = 0
              Width = 108
              Height = 17
              Align = alTop
              Caption = 'File minor'
              Checked = True
              State = cbChecked
              TabOrder = 0
            end
          end
          object Panel21: TPanel
            AlignWithMargins = True
            Left = 230
            Top = 3
            Width = 107
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 2
            object editProductionReleaseVersion: TEdit
              Tag = 3
              Left = 0
              Top = 17
              Width = 107
              Height = 23
              Align = alTop
              TabOrder = 1
              OnChange = editProductionMajorVersionChange
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
            object cbReleaseVersionAdjustAll: TCheckBox
              Tag = 3
              Left = 0
              Top = 0
              Width = 107
              Height = 17
              Align = alTop
              Caption = 'File release'
              Checked = True
              State = cbChecked
              TabOrder = 0
            end
          end
          object Panel22: TPanel
            AlignWithMargins = True
            Left = 343
            Top = 3
            Width = 108
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 3
            object editProductionBuildVersion: TEdit
              Tag = 4
              Left = 0
              Top = 17
              Width = 108
              Height = 23
              Align = alTop
              TabOrder = 1
              OnChange = editProductionMajorVersionChange
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
            object cbBuildVersionAdjustAll: TCheckBox
              Tag = 4
              Left = 0
              Top = 0
              Width = 108
              Height = 17
              Align = alTop
              Caption = 'File build'
              Checked = True
              State = cbChecked
              TabOrder = 0
            end
          end
          object Panel23: TPanel
            AlignWithMargins = True
            Left = 457
            Top = 3
            Width = 107
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 4
            object editProductionFilename: TEdit
              Tag = 5
              Left = 0
              Top = 17
              Width = 107
              Height = 23
              Align = alTop
              TabOrder = 1
              OnChange = editProductionMajorVersionChange
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
            object cbFilenameVersionAdjustAll: TCheckBox
              Tag = 4
              Left = 0
              Top = 0
              Width = 107
              Height = 17
              Align = alTop
              Caption = 'Filename'
              Checked = True
              State = cbChecked
              TabOrder = 0
            end
          end
          object Panel26: TPanel
            AlignWithMargins = True
            Left = 3
            Top = 48
            Width = 107
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 5
            object editProductionProductMajorVersion: TEdit
              Tag = 6
              Left = 0
              Top = 17
              Width = 107
              Height = 23
              Align = alTop
              TabOrder = 1
              OnChange = editProductionMajorVersionChange
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
            object CheckBox1: TCheckBox
              Tag = 6
              Left = 0
              Top = 0
              Width = 107
              Height = 17
              Align = alTop
              Caption = 'Product major'
              Checked = True
              State = cbChecked
              TabOrder = 0
            end
          end
          object Panel27: TPanel
            AlignWithMargins = True
            Left = 116
            Top = 48
            Width = 108
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 6
            object editProductionProductMinorVersion: TEdit
              Tag = 7
              Left = 0
              Top = 17
              Width = 108
              Height = 23
              Align = alTop
              TabOrder = 1
              OnChange = editProductionMajorVersionChange
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
            object CheckBox2: TCheckBox
              Tag = 7
              Left = 0
              Top = 0
              Width = 108
              Height = 17
              Align = alTop
              Caption = 'Product minor'
              Checked = True
              State = cbChecked
              TabOrder = 0
            end
          end
          object Panel28: TPanel
            AlignWithMargins = True
            Left = 230
            Top = 48
            Width = 107
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 7
            object editProductionProductReleaseVersion: TEdit
              Tag = 8
              Left = 0
              Top = 17
              Width = 107
              Height = 23
              Align = alTop
              TabOrder = 1
              OnChange = editProductionMajorVersionChange
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
            object CheckBox3: TCheckBox
              Tag = 8
              Left = 0
              Top = 0
              Width = 107
              Height = 17
              Align = alTop
              Caption = 'Product release'
              Checked = True
              State = cbChecked
              TabOrder = 0
            end
          end
          object Panel29: TPanel
            AlignWithMargins = True
            Left = 343
            Top = 48
            Width = 108
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 8
            object editProductionProductBuildVersion: TEdit
              Tag = 9
              Left = 0
              Top = 17
              Width = 108
              Height = 23
              Align = alTop
              TabOrder = 1
              OnChange = editProductionMajorVersionChange
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
            object CheckBox4: TCheckBox
              Tag = 9
              Left = 0
              Top = 0
              Width = 108
              Height = 17
              Align = alTop
              Caption = 'Product build'
              Checked = True
              State = cbChecked
              TabOrder = 0
            end
          end
        end
      end
      object Panel3: TPanel
        Left = 0
        Top = 105
        Width = 567
        Height = 104
        Align = alClient
        BevelOuter = bvNone
        Caption = 'Panel3'
        ShowCaption = False
        TabOrder = 1
        object Label2: TLabel
          Left = 0
          Top = 0
          Width = 40
          Height = 15
          Align = alTop
          Caption = 'Staging'
        end
        object GridPanel3: TGridPanel
          Left = 0
          Top = 15
          Width = 567
          Height = 89
          Align = alClient
          BevelOuter = bvNone
          Caption = 'GridPanel2'
          ColumnCollection = <
            item
              Value = 20.000000000000000000
            end
            item
              Value = 20.000000000000000000
            end
            item
              Value = 20.000000000000000000
            end
            item
              Value = 20.000000000000000000
            end
            item
              Value = 20.000000000000000000
            end>
          ControlCollection = <
            item
              Column = 0
              Control = Panel5
              Row = 0
            end
            item
              Column = 1
              Control = Panel6
              Row = 0
            end
            item
              Column = 2
              Control = Panel7
              Row = 0
            end
            item
              Column = 3
              Control = Panel8
              Row = 0
            end
            item
              Column = 4
              Control = Panel24
              Row = 0
            end
            item
              Column = 0
              Control = Panel35
              Row = 1
            end
            item
              Column = 1
              Control = Panel36
              Row = 1
            end
            item
              Column = 2
              Control = Panel37
              Row = 1
            end
            item
              Column = 3
              Control = Panel38
              Row = 1
            end
            item
              Column = 4
              Control = Panel39
              Row = 1
            end>
          RowCollection = <
            item
              Value = 50.000000000000000000
            end
            item
              Value = 50.000000000000000000
            end>
          ShowCaption = False
          TabOrder = 0
          object Panel5: TPanel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 107
            Height = 38
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object Label4: TLabel
              Left = 0
              Top = 0
              Width = 52
              Height = 15
              Align = alTop
              Caption = 'File major'
            end
            object editStagingMajorVersion: TEdit
              Tag = 1
              Left = 0
              Top = 15
              Width = 107
              Height = 23
              Align = alTop
              TabOrder = 0
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
          end
          object Panel6: TPanel
            AlignWithMargins = True
            Left = 116
            Top = 3
            Width = 108
            Height = 38
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            object Label5: TLabel
              Left = 0
              Top = 0
              Width = 53
              Height = 15
              Align = alTop
              Caption = 'File minor'
            end
            object editStagingMinorVersion: TEdit
              Tag = 2
              Left = 0
              Top = 15
              Width = 108
              Height = 23
              Align = alTop
              TabOrder = 0
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
          end
          object Panel7: TPanel
            AlignWithMargins = True
            Left = 230
            Top = 3
            Width = 107
            Height = 38
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 2
            object Label6: TLabel
              Left = 0
              Top = 0
              Width = 57
              Height = 15
              Align = alTop
              Caption = 'File release'
            end
            object editStagingReleaseVersion: TEdit
              Tag = 3
              Left = 0
              Top = 15
              Width = 107
              Height = 23
              Align = alTop
              TabOrder = 0
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
          end
          object Panel8: TPanel
            AlignWithMargins = True
            Left = 343
            Top = 3
            Width = 108
            Height = 38
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 3
            object Label7: TLabel
              Left = 0
              Top = 0
              Width = 48
              Height = 15
              Align = alTop
              Caption = 'File build'
            end
            object editStagingBuildVersion: TEdit
              Tag = 4
              Left = 0
              Top = 15
              Width = 108
              Height = 23
              Align = alTop
              TabOrder = 0
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
          end
          object Panel24: TPanel
            AlignWithMargins = True
            Left = 457
            Top = 3
            Width = 107
            Height = 38
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 4
            object Label17: TLabel
              Left = 0
              Top = 0
              Width = 48
              Height = 15
              Align = alTop
              Caption = 'Filename'
            end
            object editStagingFilename: TEdit
              Tag = 5
              Left = 0
              Top = 15
              Width = 107
              Height = 23
              Align = alTop
              TabOrder = 0
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
          end
          object Panel35: TPanel
            AlignWithMargins = True
            Left = 3
            Top = 47
            Width = 107
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 5
            object Label24: TLabel
              Left = 0
              Top = 0
              Width = 76
              Height = 15
              Align = alTop
              Caption = 'Product major'
            end
            object editStagingProductMajorVersion: TEdit
              Tag = 6
              Left = 0
              Top = 15
              Width = 107
              Height = 23
              Align = alTop
              TabOrder = 0
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
          end
          object Panel36: TPanel
            AlignWithMargins = True
            Left = 116
            Top = 47
            Width = 108
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 6
            object Label25: TLabel
              Left = 0
              Top = 0
              Width = 77
              Height = 15
              Align = alTop
              Caption = 'Product minor'
            end
            object editStagingProductMinorVersion: TEdit
              Tag = 7
              Left = 0
              Top = 15
              Width = 108
              Height = 23
              Align = alTop
              TabOrder = 0
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
          end
          object Panel37: TPanel
            AlignWithMargins = True
            Left = 230
            Top = 47
            Width = 107
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 7
            object Label26: TLabel
              Left = 0
              Top = 0
              Width = 81
              Height = 15
              Align = alTop
              Caption = 'Product release'
            end
            object editStagingProductReleaseVersion: TEdit
              Tag = 8
              Left = 0
              Top = 15
              Width = 107
              Height = 23
              Align = alTop
              TabOrder = 0
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
          end
          object Panel38: TPanel
            AlignWithMargins = True
            Left = 343
            Top = 47
            Width = 108
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 8
            object Label27: TLabel
              Left = 0
              Top = 0
              Width = 72
              Height = 15
              Align = alTop
              Caption = 'Product build'
            end
            object editStagingProductBuildVersion: TEdit
              Tag = 9
              Left = 0
              Top = 15
              Width = 108
              Height = 23
              Align = alTop
              TabOrder = 0
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
          end
          object Panel39: TPanel
            AlignWithMargins = True
            Left = 457
            Top = 47
            Width = 107
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 9
          end
        end
      end
      object Panel4: TPanel
        Left = 0
        Top = 209
        Width = 567
        Height = 105
        Align = alClient
        BevelOuter = bvNone
        Caption = 'Panel3'
        ShowCaption = False
        TabOrder = 2
        object Label3: TLabel
          Left = 0
          Top = 0
          Width = 71
          Height = 15
          Align = alTop
          Caption = 'Development'
        end
        object GridPanel4: TGridPanel
          Left = 0
          Top = 15
          Width = 567
          Height = 90
          Align = alClient
          BevelOuter = bvNone
          Caption = 'GridPanel2'
          ColumnCollection = <
            item
              Value = 20.000000000000000000
            end
            item
              Value = 20.000000000000000000
            end
            item
              Value = 20.000000000000000000
            end
            item
              Value = 20.000000000000000000
            end
            item
              Value = 20.000000000000000000
            end>
          ControlCollection = <
            item
              Column = 0
              Control = Panel9
              Row = 0
            end
            item
              Column = 1
              Control = Panel10
              Row = 0
            end
            item
              Column = 2
              Control = Panel18
              Row = 0
            end
            item
              Column = 3
              Control = Panel19
              Row = 0
            end
            item
              Column = 4
              Control = Panel25
              Row = 0
            end
            item
              Column = 0
              Control = Panel30
              Row = 1
            end
            item
              Column = 1
              Control = Panel31
              Row = 1
            end
            item
              Column = 2
              Control = Panel32
              Row = 1
            end
            item
              Column = 3
              Control = Panel33
              Row = 1
            end
            item
              Column = 4
              Control = Panel34
              Row = 1
            end>
          RowCollection = <
            item
              Value = 49.943788850927060000
            end
            item
              Value = 50.056211149072940000
            end>
          ShowCaption = False
          TabOrder = 0
          object Panel9: TPanel
            AlignWithMargins = True
            Left = 3
            Top = 3
            Width = 107
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 0
            object Label8: TLabel
              Left = 0
              Top = 0
              Width = 52
              Height = 15
              Align = alTop
              Caption = 'File major'
            end
            object editDevelopmentMajorVersion: TEdit
              Tag = 1
              Left = 0
              Top = 15
              Width = 107
              Height = 23
              Align = alTop
              TabOrder = 0
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
          end
          object Panel10: TPanel
            AlignWithMargins = True
            Left = 116
            Top = 3
            Width = 108
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 1
            object Label9: TLabel
              Left = 0
              Top = 0
              Width = 51
              Height = 15
              Align = alTop
              Caption = 'file minor'
            end
            object editDevelopmentMinorVersion: TEdit
              Tag = 2
              Left = 0
              Top = 15
              Width = 108
              Height = 23
              Align = alTop
              TabOrder = 0
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
          end
          object Panel18: TPanel
            AlignWithMargins = True
            Left = 230
            Top = 3
            Width = 107
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 2
            object Label15: TLabel
              Left = 0
              Top = 0
              Width = 57
              Height = 15
              Align = alTop
              Caption = 'File release'
            end
            object editDevelopmentReleaseVersion: TEdit
              Tag = 3
              Left = 0
              Top = 15
              Width = 107
              Height = 23
              Align = alTop
              TabOrder = 0
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
          end
          object Panel19: TPanel
            AlignWithMargins = True
            Left = 343
            Top = 3
            Width = 108
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 3
            object Label16: TLabel
              Left = 0
              Top = 0
              Width = 48
              Height = 15
              Align = alTop
              Caption = 'File build'
            end
            object editDevelopmentBuildVersion: TEdit
              Tag = 4
              Left = 0
              Top = 15
              Width = 108
              Height = 23
              Align = alTop
              TabOrder = 0
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
          end
          object Panel25: TPanel
            AlignWithMargins = True
            Left = 457
            Top = 3
            Width = 107
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 4
            object Label18: TLabel
              Left = 0
              Top = 0
              Width = 48
              Height = 15
              Align = alTop
              Caption = 'Filename'
            end
            object editDevelopmentFilename: TEdit
              Tag = 5
              Left = 0
              Top = 15
              Width = 107
              Height = 23
              Align = alTop
              TabOrder = 0
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
          end
          object Panel30: TPanel
            AlignWithMargins = True
            Left = 3
            Top = 48
            Width = 107
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 5
            object Label19: TLabel
              Left = 0
              Top = 0
              Width = 76
              Height = 15
              Align = alTop
              Caption = 'Product major'
            end
            object editDevelopmentProductMajorVersion: TEdit
              Tag = 6
              Left = 0
              Top = 15
              Width = 107
              Height = 23
              Align = alTop
              TabOrder = 0
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
          end
          object Panel31: TPanel
            AlignWithMargins = True
            Left = 116
            Top = 48
            Width = 108
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 6
            object Label20: TLabel
              Left = 0
              Top = 0
              Width = 77
              Height = 15
              Align = alTop
              Caption = 'Product minor'
            end
            object editDevelopmentProductMinorVersion: TEdit
              Tag = 7
              Left = 0
              Top = 15
              Width = 108
              Height = 23
              Align = alTop
              TabOrder = 0
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
          end
          object Panel32: TPanel
            AlignWithMargins = True
            Left = 230
            Top = 48
            Width = 107
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 7
            object Label21: TLabel
              Left = 0
              Top = 0
              Width = 81
              Height = 15
              Align = alTop
              Caption = 'Product release'
            end
            object editDevelopmentProductReleaseVersion: TEdit
              Tag = 8
              Left = 0
              Top = 15
              Width = 107
              Height = 23
              Align = alTop
              TabOrder = 0
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
          end
          object Panel33: TPanel
            AlignWithMargins = True
            Left = 343
            Top = 48
            Width = 108
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 8
            object Label22: TLabel
              Left = 0
              Top = 0
              Width = 72
              Height = 15
              Align = alTop
              Caption = 'Product build'
            end
            object editDevelopmentProductBuildVersion: TEdit
              Tag = 9
              Left = 0
              Top = 15
              Width = 108
              Height = 23
              Align = alTop
              TabOrder = 0
              OnDragDrop = editProductNameDragDrop
              OnDragOver = editProductNameDragOver
            end
          end
          object Panel34: TPanel
            AlignWithMargins = True
            Left = 457
            Top = 48
            Width = 107
            Height = 39
            Align = alClient
            BevelOuter = bvNone
            TabOrder = 9
          end
        end
      end
    end
    object GridPanel5: TGridPanel
      Left = 0
      Top = 0
      Width = 573
      Height = 94
      Align = alTop
      BevelOuter = bvNone
      Caption = 'GridPanel5'
      ColumnCollection = <
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
      ControlCollection = <
        item
          Column = 0
          Control = Panel11
          Row = 0
        end
        item
          Column = 1
          Control = Panel12
          Row = 0
        end
        item
          Column = 2
          Control = Panel13
          Row = 0
        end
        item
          Column = 3
          Control = Panel14
          Row = 0
        end
        item
          Column = 0
          Control = Panel15
          Row = 1
        end
        item
          Column = 1
          Control = Panel40
          Row = 1
        end>
      RowCollection = <
        item
          Value = 50.000000000000000000
        end
        item
          Value = 50.000000000000000000
        end>
      ShowCaption = False
      TabOrder = 0
      object Panel11: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 137
        Height = 41
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 0
        object Label1: TLabel
          Left = 0
          Top = 0
          Width = 75
          Height = 15
          Align = alTop
          Caption = 'Product name'
        end
        object editProductName: TEdit
          Left = 0
          Top = 15
          Width = 137
          Height = 23
          Align = alTop
          TabOrder = 0
          OnDragDrop = editProductNameDragDrop
          OnDragOver = editProductNameDragOver
        end
      end
      object Panel12: TPanel
        AlignWithMargins = True
        Left = 146
        Top = 3
        Width = 138
        Height = 41
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 1
        object Label10: TLabel
          Left = 0
          Top = 0
          Width = 80
          Height = 15
          Align = alTop
          Caption = 'File description'
        end
        object editFileDescription: TEdit
          Left = 0
          Top = 15
          Width = 138
          Height = 23
          Align = alTop
          TabOrder = 0
          OnDragDrop = editProductNameDragDrop
          OnDragOver = editProductNameDragOver
        end
      end
      object Panel13: TPanel
        AlignWithMargins = True
        Left = 290
        Top = 3
        Width = 137
        Height = 41
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 2
        object Label11: TLabel
          Left = 0
          Top = 0
          Width = 73
          Height = 15
          Align = alTop
          Caption = 'Internal name'
        end
        object editInternalName: TEdit
          Left = 0
          Top = 15
          Width = 137
          Height = 23
          Align = alTop
          TabOrder = 0
          OnDragDrop = editProductNameDragDrop
          OnDragOver = editProductNameDragOver
        end
      end
      object Panel14: TPanel
        AlignWithMargins = True
        Left = 433
        Top = 3
        Width = 137
        Height = 41
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 3
        object Label12: TLabel
          Left = 0
          Top = 0
          Width = 85
          Height = 15
          Align = alTop
          Caption = 'Company name'
        end
        object editCompanyName: TEdit
          Left = 0
          Top = 15
          Width = 137
          Height = 23
          Align = alTop
          TabOrder = 0
          OnDragDrop = editProductNameDragDrop
          OnDragOver = editProductNameDragOver
        end
      end
      object Panel15: TPanel
        AlignWithMargins = True
        Left = 3
        Top = 50
        Width = 137
        Height = 41
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 4
        object Label13: TLabel
          Left = 0
          Top = 0
          Width = 82
          Height = 15
          Align = alTop
          Caption = 'Legal copyright'
        end
        object editLegalCopyright: TEdit
          Left = 0
          Top = 15
          Width = 137
          Height = 23
          Align = alTop
          TabOrder = 0
          OnDragDrop = editProductNameDragDrop
          OnDragOver = editProductNameDragOver
        end
      end
      object Panel40: TPanel
        AlignWithMargins = True
        Left = 146
        Top = 50
        Width = 138
        Height = 41
        Align = alClient
        BevelOuter = bvNone
        TabOrder = 5
        object Label23: TLabel
          Left = 0
          Top = 0
          Width = 76
          Height = 15
          Align = alTop
          Caption = 'Company URL'
        end
        object editCompanyURL: TEdit
          Left = 0
          Top = 15
          Width = 138
          Height = 23
          Align = alTop
          TabOrder = 0
          OnDragDrop = editProductNameDragDrop
          OnDragOver = editProductNameDragOver
        end
      end
    end
  end
  object listboxVariables: TListBox
    AlignWithMargins = True
    Left = 576
    Top = 3
    Width = 187
    Height = 397
    Align = alRight
    BorderStyle = bsNone
    ItemHeight = 15
    TabOrder = 1
    OnMouseDown = listboxVariablesMouseDown
  end
  object ActionList: TActionList
    Images = dmResources.ImageListCommon
    Left = 676
    Top = 60
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
  end
end
