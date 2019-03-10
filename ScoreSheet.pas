unit ScoreSheet;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.ExtCtrls, BowlingGame,
  Generics.Collections;

type
  TScoreSheetForm = class(TForm)
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
    Panel1a: TPanel;
    Panel4: TPanel;
    Label4: TLabel;
    Panel4a: TPanel;
    Panel3: TPanel;
    Label5: TLabel;
    Panel3a: TPanel;
    Panel2: TPanel;
    Label6: TLabel;
    Panel12: TPanel;
    Panel7: TPanel;
    Label7: TLabel;
    Panel14: TPanel;
    Panel6: TPanel;
    Label8: TLabel;
    Panel16: TPanel;
    Panel5: TPanel;
    Label9: TLabel;
    Panel18: TPanel;
    Panel10: TPanel;
    Label10: TLabel;
    Panel20: TPanel;
    Panel9: TPanel;
    Label11: TLabel;
    Panel22: TPanel;
    Panel8: TPanel;
    Label12: TLabel;
    Panel24: TPanel;
    Panel1b: TPanel;
    Panel1c: TPanel;
    procedure Button0Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MakeArrays();
  private
    { Private declarations }
    Buttons: array [0 .. 10] of TButton;
    Frames: array [1 .. 10] of TPanel;
    Game: TBowlingGame;
    CurrentFrame: Integer;
    PendingFrames: TList<Integer>;
    sbf: TScoreByFrame;

    procedure UpdateFrame(Number: Integer);
    procedure UpdateValidButtons();
  public
    { Public declarations }
  end;

var
  ScoreSheetForm: TScoreSheetForm;

implementation

{$R *.dfm}

procedure TScoreSheetForm.Button0Click(Sender: TObject);
var
  n: Integer;
  sbfl: TList<TScoreByFrame>;

  procedure UpdatePending();
  var
    tmp: TScoreByFrame;
    i: Integer;
  begin
    for i := PendingFrames.Count - 1 downto 0 do
    begin
      if sbfl[PendingFrames[i]].Status = 'Scored' then begin
        UpdateFrame(PendingFrames[i]);
        PendingFrames.Delete(i);
      end;
    end;
  end;

begin
  n := (Sender as TComponent).Tag;
  Game.Roll(n);
  sbfl := Game.ScoreByFrame();
  UpdatePending();

  sbf := sbfl[CurrentFrame];
  if sbf.Status = 'Pending' then
    PendingFrames.Add(CurrentFrame);

  UpdateFrame(CurrentFrame);

  UpdateValidButtons();
end;

procedure TScoreSheetForm.FormCreate(Sender: TObject);
begin
  PendingFrames := TList<Integer>.Create();
  CurrentFrame := 1;
  MakeArrays();
  Game := TBowlingGame.Create();
  Game.Start();
end;

procedure TScoreSheetForm.FormDestroy(Sender: TObject);
begin
  PendingFrames.Free();
  Game.Free();
end;

procedure TScoreSheetForm.MakeArrays;

  procedure MakeButtonsArray();
  var
    i: Integer;
  begin
    Buttons[0] := Button0;
    Buttons[1] := Button1;
    Buttons[2] := Button2;
    Buttons[3] := Button3;
    Buttons[4] := Button4;
    Buttons[5] := Button5;
    Buttons[6] := Button6;
    Buttons[7] := Button7;
    Buttons[8] := Button8;
    Buttons[9] := Button9;
    Buttons[10] := Button10;
  end;

  procedure MakeFramesArray();
  begin
    Frames[1] := Panel1;
    Frames[2] := Panel2;
    Frames[3] := Panel3;
    Frames[4] := Panel4;
    Frames[5] := Panel5;
    Frames[6] := Panel6;
    Frames[7] := Panel7;
    Frames[8] := Panel8;
    Frames[9] := Panel9;
    Frames[10] := Panel10;
  end;

begin
  MakeButtonsArray();
  MakeFramesArray();
end;

procedure TScoreSheetForm.UpdateFrame(Number: Integer);
var
  boxA, boxB, boxTot: TPanel;

  function Get(box: char): TPanel;
  begin
    Result := TPanel(self.FindComponent('Panel' +
      IntToStr(CurrentFrame) + box));
  end;

begin
  boxA := Get('a');
  boxB := Get('b');

  if length(sbf.FrameScore) = 1 then
  begin
    if sbf.FrameScore = 'X' then
      boxB.Caption := sbf.FrameScore[1]
    else
      boxA.Caption := sbf.FrameScore[1];
  end
  else
  begin
    boxA.Caption := sbf.FrameScore[1];
    boxB.Caption := sbf.FrameScore[2];
  end;
end;

procedure TScoreSheetForm.UpdateValidButtons;
var
  i: Integer;
begin
  for i := 0 to 10 do
    Buttons[i].Enabled := False;
  for i := 0 to Game.FramesCtrl.GetCurrent.NumPinsStanding do
    Buttons[i].Enabled := True;
end;

end.
