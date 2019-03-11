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
    Panel3: TPanel;
    Label5: TLabel;
    Panel2: TPanel;
    Label6: TLabel;
    Panel7: TPanel;
    Label7: TLabel;
    Panel6: TPanel;
    Label8: TLabel;
    Panel5: TPanel;
    Label9: TLabel;
    Panel10: TPanel;
    Label10: TLabel;
    Panel9: TPanel;
    Label11: TLabel;
    Panel8: TPanel;
    Label12: TLabel;
    Panel1b: TPanel;
    Panel1c: TPanel;
    Panel2a: TPanel;
    Panel2b: TPanel;
    Panel2c: TPanel;
    Panel3c: TPanel;
    Panel3a: TPanel;
    Panel3b: TPanel;
    btnNewGame: TButton;
    Panel4a: TPanel;
    Panel4b: TPanel;
    Panel4c: TPanel;
    Panel5a: TPanel;
    Panel5b: TPanel;
    Panel5c: TPanel;
    Panel6b: TPanel;
    Panel7b: TPanel;
    Panel8b: TPanel;
    Panel9b: TPanel;
    Panel10b2: TPanel;
    Panel6a: TPanel;
    Panel7a: TPanel;
    Panel8a: TPanel;
    Panel9a: TPanel;
    Panel10a: TPanel;
    Panel6c: TPanel;
    Panel7c: TPanel;
    Panel8c: TPanel;
    Panel9c: TPanel;
    Panel10c: TPanel;
    Panel10b: TPanel;
    Button11: TButton;
    procedure Button0Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure MakeArrays();
    procedure NewGame();
    procedure ClearFrames();
    procedure btnNewGameClick(Sender: TObject);
    procedure GameOver();
    procedure DisableButtons();
  private
    { Private declarations }
    Buttons: array [0 .. 10] of TButton;
    Frames: array [1 .. 10] of TPanel;
    Game: TBowlingGame;
    CurrentFrame: Integer;
    PendingFrames: TList<Integer>;
    sbfl: TList<TScoreByFrame>;

    procedure UpdateFrame(FrameNumber: Integer);
    procedure UpdateValidButtons();
    function GetBox(box: string; frame: Integer): TPanel;
  public
    { Public declarations }
  end;

var
  ScoreSheetForm: TScoreSheetForm;

implementation

{$R *.dfm}

procedure TScoreSheetForm.btnNewGameClick(Sender: TObject);
begin
  NewGame();
end;

procedure TScoreSheetForm.Button0Click(Sender: TObject);
var
  n: Integer;
  sbf: TScoreByFrame;

  procedure UpdatePending();
  var
    i: Integer;
  begin
    for i := PendingFrames.Count - 1 downto 0 do
    begin
      if sbfl[PendingFrames[i] - 1].Status = 'Scored' then
      begin
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

  sbf := sbfl[CurrentFrame - 1];
  if sbf.Status = 'Pending' then
    PendingFrames.Add(CurrentFrame);

  UpdateFrame(CurrentFrame);

  UpdateValidButtons();

  if (sbf.Status = 'Scored') or (sbf.Status = 'Pending') then
    Inc(CurrentFrame);
  if CurrentFrame > 10 then
    GameOver();
end;

procedure TScoreSheetForm.ClearFrames;
var
  i: Integer;
  pnl: TPanel;
begin
  for i := 1 to 10 do
  begin
    pnl := GetBox('a', i);
    pnl.Caption := '';

    pnl := GetBox('b', i);
    pnl.Caption := '';
    if i = 10 then
    begin
      pnl := GetBox('b2', i);
      pnl.Caption := '';
    end;

    pnl := GetBox('c', i);
    pnl.Caption := '';
  end;
end;

procedure TScoreSheetForm.DisableButtons;
var
  i: Integer;
begin
  for i := 0 to 10 do
    Buttons[i].Enabled := False;
end;

procedure TScoreSheetForm.FormCreate(Sender: TObject);
begin
  PendingFrames := TList<Integer>.Create();
  MakeArrays();
  Game := TBowlingGame.Create();
  Game.Start();
  NewGame();
end;

procedure TScoreSheetForm.FormDestroy(Sender: TObject);
var
  i: Integer;
begin
  for i := PendingFrames.Count - 1 downto 0 do
    PendingFrames.Delete(i);

  PendingFrames.Free();
  Game.Free();
end;

procedure TScoreSheetForm.GameOver;
begin
  DisableButtons();
end;

function TScoreSheetForm.GetBox(box: string; frame: Integer): TPanel;
begin
  Result := TPanel(self.FindComponent('Panel' + IntToStr(frame) + box));
end;

procedure TScoreSheetForm.MakeArrays;

  procedure MakeButtonsArray();
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

procedure TScoreSheetForm.NewGame;
begin
  Game.Start();
  CurrentFrame := 1;
  ClearFrames();
  PendingFrames.Clear();
  UpdateValidButtons();
end;

procedure TScoreSheetForm.UpdateFrame(FrameNumber: Integer);
var
  boxA, boxB, boxB2, boxTot: TPanel;
  sbfTemp: TScoreByFrame;

  procedure RegFrame();
  begin
    boxA := GetBox('a', FrameNumber);
    boxB := GetBox('b', FrameNumber);

    if length(sbfTemp.FrameScore) = 1 then
    begin
      if sbfTemp.FrameScore = 'X' then
        boxB.Caption := sbfTemp.FrameScore[1]
      else
        boxA.Caption := sbfTemp.FrameScore[1];
    end
    else
    begin
      boxA.Caption := sbfTemp.FrameScore[1];
      boxB.Caption := sbfTemp.FrameScore[3];
    end;

    if sbfTemp.Status = 'Scored' then
    begin
      boxTot := GetBox('c', FrameNumber);
      boxTot.Caption := IntToStr(sbfTemp.GameScore);
    end;
  end;

  procedure FinalFrame();
  begin
    boxA := GetBox('a', FrameNumber);
    boxB := GetBox('b', FrameNumber);
    boxB2 := GetBox('b2', FrameNumber);

    if length(sbfTemp.FrameScore) = 1 then
      boxA.Caption := sbfTemp.FrameScore[1]
    else if length(sbfTemp.FrameScore) = 3 then
    begin
      boxA.Caption := sbfTemp.FrameScore[1];
      boxB.Caption := sbfTemp.FrameScore[3];
    end
    else if length(sbfTemp.FrameScore) = 5 then
    begin
      boxA.Caption := sbfTemp.FrameScore[1];
      boxB.Caption := sbfTemp.FrameScore[3];
      boxB2.Caption := sbfTemp.FrameScore[5];
    end;

    if sbfTemp.Status = 'Scored' then
    begin
      boxTot := GetBox('c', FrameNumber);
      boxTot.Caption := IntToStr(sbfTemp.GameScore);
    end;
  end;

begin
  sbfTemp := sbfl[FrameNumber - 1];
  if sbfTemp.Number < 10 then
    RegFrame()
  else
    FinalFrame();
end;

procedure TScoreSheetForm.UpdateValidButtons;
var
  i: Integer;
begin
  DisableButtons();
  for i := 0 to Game.FramesCtrl.GetCurrent.NumPinsStanding do
    Buttons[i].Enabled := True;
end;

end.
