unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  BowlingGame, Generics.Collections, ScoreSheet;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
begin
  Close();
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  bg: TBowlingGame;
  sbf: TList<TScoreByFrame>;
begin
  bg := TBowlingGame.Create();
  bg.Start();
  bg.Roll(3);
  bg.Roll(4);
  bg.Roll(10);
  bg.Roll(7);
  bg.Roll(2);

  sbf := bg.ScoreByFrame();

  bg.Free();
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  frm: TScoreSheetForm;
begin
  frm := TScoreSheetForm.Create(nil);
  frm.ShowModal();
  frm.Free();
end;

end.
