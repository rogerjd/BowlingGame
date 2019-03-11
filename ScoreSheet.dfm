object ScoreSheetForm: TScoreSheetForm
  Left = 0
  Top = 0
  ActiveControl = Button0
  Caption = 'Bowling Score Sheet'
  ClientHeight = 293
  ClientWidth = 917
  Color = clBtnFace
  TransparentColor = True
  TransparentColorValue = clMoneyGreen
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Button0: TButton
    Left = 32
    Top = 40
    Width = 33
    Height = 25
    Caption = '0'
    TabOrder = 0
    OnClick = Button0Click
  end
  object Button1: TButton
    Tag = 1
    Left = 64
    Top = 40
    Width = 33
    Height = 25
    Caption = '1'
    TabOrder = 1
    OnClick = Button0Click
  end
  object Button2: TButton
    Tag = 2
    Left = 96
    Top = 40
    Width = 33
    Height = 25
    Caption = '2'
    TabOrder = 2
    OnClick = Button0Click
  end
  object Button3: TButton
    Tag = 3
    Left = 128
    Top = 40
    Width = 33
    Height = 25
    Caption = '3'
    TabOrder = 3
    OnClick = Button0Click
  end
  object Button4: TButton
    Tag = 4
    Left = 160
    Top = 40
    Width = 33
    Height = 25
    Caption = '4'
    TabOrder = 4
    OnClick = Button0Click
  end
  object Button5: TButton
    Tag = 5
    Left = 192
    Top = 40
    Width = 33
    Height = 25
    Caption = '5'
    TabOrder = 5
    OnClick = Button0Click
  end
  object Button6: TButton
    Tag = 6
    Left = 224
    Top = 40
    Width = 33
    Height = 25
    Caption = '6'
    TabOrder = 6
    OnClick = Button0Click
  end
  object Button7: TButton
    Tag = 7
    Left = 253
    Top = 40
    Width = 33
    Height = 25
    Caption = '7'
    TabOrder = 7
    OnClick = Button0Click
  end
  object Button8: TButton
    Tag = 8
    Left = 288
    Top = 40
    Width = 33
    Height = 25
    Caption = '8'
    TabOrder = 8
    OnClick = Button0Click
  end
  object Button9: TButton
    Tag = 9
    Left = 320
    Top = 40
    Width = 33
    Height = 25
    Caption = '9'
    TabOrder = 9
    OnClick = Button0Click
  end
  object Button10: TButton
    Tag = 10
    Left = 352
    Top = 40
    Width = 33
    Height = 25
    Caption = '10'
    TabOrder = 10
    OnClick = Button0Click
  end
  object Panel1: TPanel
    Left = 32
    Top = 104
    Width = 81
    Height = 81
    BevelOuter = bvNone
    TabOrder = 11
    object Label1: TLabel
      Left = 0
      Top = 0
      Width = 7
      Height = 13
      Align = alTop
      Alignment = taCenter
      Caption = '1'
      Color = clScrollBar
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = False
    end
    object Panel1a: TPanel
      Left = 48
      Top = 13
      Width = 17
      Height = 26
      BevelOuter = bvNone
      TabOrder = 0
    end
    object Panel1b: TPanel
      Left = 64
      Top = 13
      Width = 17
      Height = 26
      BevelKind = bkTile
      BevelOuter = bvNone
      TabOrder = 1
    end
    object Panel1c: TPanel
      Left = 0
      Top = 45
      Width = 81
      Height = 36
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
    end
  end
  object Panel4: TPanel
    Left = 292
    Top = 104
    Width = 81
    Height = 81
    BevelOuter = bvNone
    TabOrder = 12
    object Label4: TLabel
      Left = 0
      Top = 0
      Width = 7
      Height = 13
      Align = alTop
      Alignment = taCenter
      Caption = '4'
      Color = clScrollBar
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = False
    end
    object Panel4a: TPanel
      Left = 48
      Top = 13
      Width = 17
      Height = 24
      BevelOuter = bvNone
      TabOrder = 0
    end
    object Panel4b: TPanel
      Left = 64
      Top = 13
      Width = 17
      Height = 24
      BevelKind = bkTile
      BevelOuter = bvNone
      TabOrder = 1
    end
    object Panel4c: TPanel
      Left = 0
      Top = 47
      Width = 81
      Height = 34
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
    end
  end
  object Panel3: TPanel
    Left = 206
    Top = 104
    Width = 81
    Height = 81
    BevelOuter = bvNone
    TabOrder = 13
    object Label5: TLabel
      Left = 0
      Top = 0
      Width = 7
      Height = 13
      Align = alTop
      Alignment = taCenter
      Caption = '3'
      Color = clScrollBar
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = False
    end
    object Panel3c: TPanel
      Left = 0
      Top = 47
      Width = 81
      Height = 34
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 0
    end
    object Panel3a: TPanel
      Left = 48
      Top = 13
      Width = 17
      Height = 24
      BevelOuter = bvNone
      TabOrder = 1
    end
    object Panel3b: TPanel
      Left = 64
      Top = 13
      Width = 17
      Height = 24
      BevelKind = bkTile
      BevelOuter = bvNone
      TabOrder = 2
    end
  end
  object Panel2: TPanel
    Left = 119
    Top = 104
    Width = 81
    Height = 81
    BevelOuter = bvNone
    TabOrder = 14
    object Label6: TLabel
      Left = 0
      Top = 0
      Width = 7
      Height = 13
      Align = alTop
      Alignment = taCenter
      Caption = '2'
      Color = clScrollBar
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = False
    end
    object Panel2a: TPanel
      Left = 48
      Top = 13
      Width = 17
      Height = 26
      BevelOuter = bvNone
      TabOrder = 0
    end
    object Panel2b: TPanel
      Left = 64
      Top = 13
      Width = 17
      Height = 26
      BevelKind = bkTile
      BevelOuter = bvNone
      TabOrder = 1
    end
    object Panel2c: TPanel
      Left = 0
      Top = 45
      Width = 81
      Height = 36
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
    end
  end
  object Panel7: TPanel
    Left = 553
    Top = 104
    Width = 81
    Height = 81
    BevelOuter = bvNone
    TabOrder = 15
    object Label7: TLabel
      Left = 0
      Top = 0
      Width = 7
      Height = 13
      Align = alTop
      Alignment = taCenter
      Caption = '7'
      Color = clScrollBar
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = False
    end
    object Panel7b: TPanel
      Left = 64
      Top = 13
      Width = 17
      Height = 24
      BevelKind = bkTile
      BevelOuter = bvNone
      TabOrder = 0
    end
    object Panel7a: TPanel
      Left = 48
      Top = 13
      Width = 17
      Height = 24
      BevelOuter = bvNone
      TabOrder = 1
    end
    object Panel7c: TPanel
      Left = 0
      Top = 47
      Width = 81
      Height = 34
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
    end
  end
  object Panel6: TPanel
    Left = 466
    Top = 104
    Width = 81
    Height = 81
    BevelOuter = bvNone
    TabOrder = 16
    object Label8: TLabel
      Left = 0
      Top = 0
      Width = 7
      Height = 13
      Align = alTop
      Alignment = taCenter
      Caption = '6'
      Color = clScrollBar
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = False
    end
    object Panel6b: TPanel
      Left = 64
      Top = 13
      Width = 17
      Height = 24
      BevelKind = bkTile
      BevelOuter = bvNone
      TabOrder = 0
    end
    object Panel6a: TPanel
      Left = 48
      Top = 13
      Width = 17
      Height = 24
      BevelOuter = bvNone
      TabOrder = 1
    end
    object Panel6c: TPanel
      Left = 0
      Top = 47
      Width = 81
      Height = 34
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
    end
  end
  object Panel5: TPanel
    Left = 379
    Top = 104
    Width = 81
    Height = 81
    BevelOuter = bvNone
    TabOrder = 17
    object Label9: TLabel
      Left = 0
      Top = 0
      Width = 7
      Height = 13
      Align = alTop
      Alignment = taCenter
      Caption = '5'
      Color = clScrollBar
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = False
    end
    object Panel5a: TPanel
      Left = 48
      Top = 13
      Width = 17
      Height = 24
      BevelOuter = bvNone
      TabOrder = 0
    end
    object Panel5b: TPanel
      Left = 64
      Top = 13
      Width = 17
      Height = 24
      BevelKind = bkTile
      BevelOuter = bvNone
      TabOrder = 1
    end
    object Panel5c: TPanel
      Left = 0
      Top = 47
      Width = 81
      Height = 34
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
    end
  end
  object Panel10: TPanel
    Left = 813
    Top = 104
    Width = 81
    Height = 81
    BevelOuter = bvNone
    TabOrder = 18
    object Label10: TLabel
      Left = 0
      Top = 0
      Width = 14
      Height = 13
      Align = alTop
      Alignment = taCenter
      Caption = '10'
      Color = clScrollBar
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = False
    end
    object Panel10b2: TPanel
      Left = 64
      Top = 13
      Width = 17
      Height = 24
      BevelKind = bkTile
      BevelOuter = bvNone
      TabOrder = 0
    end
    object Panel10a: TPanel
      Left = 30
      Top = 13
      Width = 17
      Height = 24
      BevelOuter = bvNone
      TabOrder = 1
    end
    object Panel10c: TPanel
      Left = 0
      Top = 47
      Width = 81
      Height = 34
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
    end
    object Panel10b: TPanel
      Left = 48
      Top = 13
      Width = 17
      Height = 24
      BevelOuter = bvNone
      TabOrder = 3
    end
  end
  object Panel9: TPanel
    Left = 726
    Top = 104
    Width = 81
    Height = 81
    BevelOuter = bvNone
    TabOrder = 19
    object Label11: TLabel
      Left = 0
      Top = 0
      Width = 7
      Height = 13
      Align = alTop
      Alignment = taCenter
      Caption = '9'
      Color = clScrollBar
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = False
    end
    object Panel9b: TPanel
      Left = 64
      Top = 13
      Width = 17
      Height = 24
      BevelKind = bkTile
      BevelOuter = bvNone
      TabOrder = 0
    end
    object Panel9a: TPanel
      Left = 48
      Top = 13
      Width = 17
      Height = 24
      BevelOuter = bvNone
      TabOrder = 1
    end
    object Panel9c: TPanel
      Left = 0
      Top = 47
      Width = 81
      Height = 34
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
    end
  end
  object Panel8: TPanel
    Left = 639
    Top = 104
    Width = 81
    Height = 81
    BevelOuter = bvNone
    TabOrder = 20
    object Label12: TLabel
      Left = 0
      Top = 0
      Width = 7
      Height = 13
      Align = alTop
      Alignment = taCenter
      Caption = '8'
      Color = clScrollBar
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
      Transparent = False
    end
    object Panel8b: TPanel
      Left = 64
      Top = 13
      Width = 17
      Height = 24
      BevelKind = bkTile
      BevelOuter = bvNone
      TabOrder = 0
    end
    object Panel8a: TPanel
      Left = 48
      Top = 13
      Width = 17
      Height = 24
      BevelOuter = bvNone
      TabOrder = 1
    end
    object Panel8c: TPanel
      Left = 0
      Top = 47
      Width = 81
      Height = 34
      Align = alBottom
      BevelOuter = bvNone
      TabOrder = 2
    end
  end
  object btnNewGame: TButton
    Left = 720
    Top = 40
    Width = 75
    Height = 25
    Caption = 'New Game'
    TabOrder = 21
    OnClick = btnNewGameClick
  end
end
