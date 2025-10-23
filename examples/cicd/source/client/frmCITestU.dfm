object frmCITest: TfrmCITest
  Left = 0
  Top = 0
  Caption = 'Person'
  ClientHeight = 177
  ClientWidth = 619
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 15
  object GridPanel1: TGridPanel
    Left = 0
    Top = 0
    Width = 619
    Height = 177
    Align = alClient
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
        Control = Panel1
        Row = 0
      end
      item
        Column = 1
        Control = Panel2
        Row = 0
      end
      item
        Column = 0
        Control = Panel3
        Row = 1
      end
      item
        Column = 1
        Control = Panel4
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
    ExplicitLeft = 144
    ExplicitTop = 76
    ExplicitWidth = 185
    ExplicitHeight = 41
    object Panel1: TPanel
      AlignWithMargins = True
      Left = 4
      Top = 4
      Width = 302
      Height = 82
      Align = alClient
      ShowCaption = False
      TabOrder = 0
      ExplicitLeft = 56
      ExplicitTop = 40
      ExplicitWidth = 185
      ExplicitHeight = 41
      object Label1: TLabel
        Left = 1
        Top = 1
        Width = 300
        Height = 15
        Align = alTop
        Caption = 'First name'
        ExplicitWidth = 55
      end
      object editFirstName: TEdit
        Left = 1
        Top = 16
        Width = 300
        Height = 23
        Align = alTop
        TabOrder = 0
        OnChange = dateOfBirthChange
        ExplicitLeft = 36
        ExplicitTop = 52
        ExplicitWidth = 121
      end
    end
    object Panel2: TPanel
      AlignWithMargins = True
      Left = 312
      Top = 4
      Width = 303
      Height = 82
      Align = alClient
      ShowCaption = False
      TabOrder = 1
      ExplicitLeft = 56
      ExplicitTop = 40
      ExplicitWidth = 185
      ExplicitHeight = 41
      object Label2: TLabel
        Left = 1
        Top = 1
        Width = 301
        Height = 15
        Align = alTop
        Caption = 'Last name'
        ExplicitWidth = 54
      end
      object editLastName: TEdit
        Left = 1
        Top = 16
        Width = 301
        Height = 23
        Align = alTop
        TabOrder = 0
        OnChange = dateOfBirthChange
        ExplicitWidth = 183
      end
    end
    object Panel3: TPanel
      AlignWithMargins = True
      Left = 4
      Top = 92
      Width = 302
      Height = 81
      Align = alClient
      ShowCaption = False
      TabOrder = 2
      ExplicitLeft = 56
      ExplicitTop = 40
      ExplicitWidth = 185
      ExplicitHeight = 41
      object Label3: TLabel
        Left = 1
        Top = 1
        Width = 300
        Height = 15
        Align = alTop
        Caption = 'Date of birth'
        ExplicitWidth = 66
      end
      object dateOfBirth: TDateTimePicker
        Left = 1
        Top = 16
        Width = 300
        Height = 23
        Align = alTop
        Date = 44862.000000000000000000
        Time = 0.614443599537480600
        TabOrder = 0
        OnChange = dateOfBirthChange
        ExplicitLeft = 36
        ExplicitTop = 48
        ExplicitWidth = 186
      end
    end
    object Panel4: TPanel
      AlignWithMargins = True
      Left = 312
      Top = 92
      Width = 303
      Height = 81
      Align = alClient
      ShowCaption = False
      TabOrder = 3
      ExplicitLeft = 56
      ExplicitTop = 40
      ExplicitWidth = 185
      ExplicitHeight = 41
      object Label4: TLabel
        Left = 1
        Top = 1
        Width = 301
        Height = 15
        Align = alTop
        Caption = 'Age'
        ExplicitWidth = 21
      end
      object lblAgeInformation: TLabel
        Left = 1
        Top = 39
        Width = 301
        Height = 41
        Align = alClient
        Alignment = taCenter
        Layout = tlCenter
        ExplicitWidth = 3
        ExplicitHeight = 15
      end
      object editAge: TEdit
        Left = 1
        Top = 16
        Width = 301
        Height = 23
        Align = alTop
        ReadOnly = True
        TabOrder = 0
        ExplicitWidth = 183
      end
    end
  end
end
