unit ScoreSheet;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, BowlingGame,
  Generics.Collections;

type
  TForm2 = class(TForm)
    Button0: TButton;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Button5: TButton;
    Button6: TButton;
    Button7: TButton;
    Button8: TButton;
    Button9: TButton;
    Button10: TButton;
    Panel1: TPanel;
    Label1: TLabel;
    Panel2: TPanel;
    Panel3: TPanel;
    Label2: TLabel;
    Panel4: TPanel;
    Panel7: TPanel;
    Label4: TLabel;
    Panel8: TPanel;
    Panel9: TPanel;
    Label5: TLabel;
    Panel10: TPanel;
    Panel11: TPanel;
    Label6: TLabel;
    Panel12: TPanel;
    Panel13: TPanel;
    Label7: TLabel;
    Panel14: TPanel;
    Panel15: TPanel;
    Label8: TLabel;
    Panel16: TPanel;
    Panel17: TPanel;
    Label9: TLabel;
    Panel18: TPanel;
    Panel19: TPanel;
    Label10: TLabel;
    Panel20: TPanel;
    Panel21: TPanel;
    Label11: TLabel;
    Panel22: TPanel;
    Panel23: TPanel;
    Label12: TLabel;
    Panel24: TPanel;
    procedure Button0Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    { Private declarations }
    Buttons: array [0 .. 10] of TButton;
    Game: TBowlingGame;
    CurrentFrame: Integer;
    PendingFrames: TList<Integer>;

    procedure UpdateFrame();
  public
    { Public declarations }
  end;

var
  Form2: TForm2;

implementation


{$R *.dfm}

procedure TForm2.Button0Click(Sender: TObject);
var
  n: Integer;
  sbfl: TList<TScoreByFrame>;
  sbf: TScoreByFrame;
begin
  n := (Sender as TComponent).Tag;
  Game.Roll(n);

  sbfl := Game.ScoreByFrame();


end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  PendingFrames := TList<Integer>.Create();
  Game := TBowlingGame.Create();
  Game.Start();
end;

procedure TForm2.FormDestroy(Sender: TObject);
begin
  PendingFrames.Free();
  Game.Free();
end;

procedure TForm2.UpdateFrame;
begin

end;

end.
