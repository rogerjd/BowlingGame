object Form1: TForm1
  Left = 0
  Top = 0
  ActiveControl = btnScoreSheet
  Caption = 'Bowling'
  ClientHeight = 243
  ClientWidth = 472
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object btnClose: TButton
    Left = 8
    Top = 210
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Close'
    ModalResult = 2
    TabOrder = 0
    OnClick = btnCloseClick
  end
  object btnScoreSheet: TButton
    Left = 198
    Top = 109
    Width = 75
    Height = 25
    Caption = 'Score Sheet'
    TabOrder = 1
    OnClick = btnScoreSheetClick
  end
end
