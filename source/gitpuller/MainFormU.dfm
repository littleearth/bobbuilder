object MainForm: TMainForm
  Left = 0
  Top = 0
  Caption = 'Git Repository Puller'
  ClientHeight = 409
  ClientWidth = 694
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  TextHeight = 13
  object pnlTop: TPanel
    Left = 0
    Top = 0
    Width = 694
    Height = 81
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 0
    ExplicitWidth = 700
    object lblPath: TLabel
      Left = 16
      Top = 16
      Width = 99
      Height = 13
      Caption = 'Base Directory Path:'
    end
    object edtPath: TEdit
      Left = 16
      Top = 35
      Width = 489
      Height = 21
      TabOrder = 2
    end
    object btnBrowse: TButton
      Left = 520
      Top = 33
      Width = 75
      Height = 25
      Caption = 'Browse...'
      TabOrder = 0
      OnClick = btnBrowseClick
    end
    object btnPull: TButton
      Left = 608
      Top = 33
      Width = 75
      Height = 25
      Caption = 'Pull All'
      TabOrder = 1
      OnClick = btnPullClick
    end
  end
  object pnlMain: TPanel
    Left = 0
    Top = 81
    Width = 694
    Height = 287
    Align = alClient
    BevelOuter = bvNone
    TabOrder = 1
    ExplicitWidth = 700
    ExplicitHeight = 378
    DesignSize = (
      694
      287)
    object memoLog: TMemo
      Left = 8
      Top = 8
      Width = 678
      Height = 271
      Anchors = [akLeft, akTop, akRight, akBottom]
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Consolas'
      Font.Style = []
      ParentFont = False
      ReadOnly = True
      ScrollBars = ssVertical
      TabOrder = 0
      ExplicitWidth = 684
      ExplicitHeight = 362
    end
  end
  object pnlBottom: TPanel
    Left = 0
    Top = 368
    Width = 694
    Height = 41
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 2
    ExplicitTop = 459
    ExplicitWidth = 700
    object btnExit: TButton
      AlignWithMargins = True
      Left = 622
      Top = 3
      Width = 75
      Height = 35
      Align = alRight
      Caption = 'Exit'
      TabOrder = 0
      OnClick = btnExitClick
      ExplicitLeft = 608
      ExplicitTop = 8
      ExplicitHeight = 25
    end
  end
  object TimerLog: TTimer
    Interval = 2000
    OnTimer = TimerLogTimer
    Left = 60
    Top = 193
  end
end
