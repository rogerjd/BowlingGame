unit BowlingGame;

interface

type
  TRollTotal = (Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine,
    Strike, Spare);

  // TFrame = record;

  TFrameRolls = array [1 .. 3] of TRollTotal;

  TFrameRollsCtrl = record
    // ownr: TFrame;
    Over: Boolean;
    FrameRolls: TFrameRolls;
    CurrentRoll: Integer;
    procedure RecordRoll(NumPins: Integer);
  end;

  TLastTwoRolls = record
    Roll1, Roll2: Integer;
  end;

  TFrame = record
  private
    FCurrentRoll: Integer;
    procedure SetCurrentRoll(const Value: Integer);
  public
    FrameRollsCtrl: TFrameRollsCtrl;
    //over, see FRC
    Number: Integer;
    Score: Integer;
    Scored: Boolean;
    RunningTotal: Integer;
    // property CurrentRoll: integer read FCurrentRoll write SetCurrentRoll;
  end;

  TFrames = array [1 .. 10] of TFrame;

  TFramesCtrl = record
  private
    Frames: TFrames;
    CurrentFrame: Integer;
  public
    function GetCurrent(): TFrame;
    procedure Next();
    procedure Init();
  end;

  TScoreCtrl = record
    procedure Score();
  end;

  TBowlingGame = record
  private
    FTotalScore: Integer;
    procedure SetTotalScore(const Value: Integer);
    procedure CalculateScore();
  public
    FramesCtrl: TFramesCtrl;
    ScoreCtrl: TScoreCtrl;
    GameOver: Boolean;
    procedure Start();
    procedure Roll(NumOfPins: Integer);
    function ScoreByFrame(): Integer;
    property TotalScore: Integer read FTotalScore write SetTotalScore;
    (*
      constructor Create();
      destructor Destroy(); override;
    *)
  end;

  (*
    TFrame = class
    public
    Over: Boolean;
    Number: Integer;
    Score: Integer;
    RunningTotal: Integer;
    end;

    TRegularFrame = class(TFrame)
    RollResult1, RollResult2: TRollResult;
    end;

    TFinalFrame = class(TRegularFrame)
    RollResult3: TRollResult;
    end;

    TFrames = array [1 .. 10] of TFrame;

    TBowlingGame = class
    private
    FTotalScore: Integer;
    procedure SetTotalScore(const Value: Integer);
    public
    Frames: TFrames;
    CurrentFrame: Integer;
    procedure Start();
    procedure Roll(NumOfPins: Integer);
    function ScoreByFrame(): Integer;
    property TotalScore: Integer read FTotalScore write SetTotalScore;
    constructor Create();
    destructor Destroy(); override;
    end;
  *)

implementation

{ TBowlingGame }

(*
  constructor TBowlingGame.Create;
  var
  i: Integer;
  begin
  for i := 1 to 10 do
  begin
  Frames[i] := TFrame.Create();
  end;
  end;

  destructor TBowlingGame.Destroy;
  var
  i: Integer;
  begin
  for i := 1 to 10 do
  begin
  Frames[i].Free();
  end;

  inherited Destroy();
  end;
*)

procedure TBowlingGame.CalculateScore;
begin

end;

procedure TBowlingGame.Roll(NumOfPins: Integer);
var
  frame: TFrame;
begin
  frame := FramesCtrl.GetCurrent();
  frame.FrameRollsCtrl.RecordRoll(NumOfPins);

  ScoreCtrl.Score();
  FramesCtrl.Next();
end;

function TBowlingGame.ScoreByFrame: Integer;
begin

end;

procedure TBowlingGame.SetTotalScore(const Value: Integer);
begin
  FTotalScore := Value;
end;

procedure TBowlingGame.Start;
begin
  GameOver := False;
  FramesCtrl.Init();
end;

{ TFrame }

procedure TFrame.SetCurrentRoll(const Value: Integer);
begin
  FCurrentRoll := Value;
end;

{ TFramesCtrl }

function TFramesCtrl.GetCurrent(): TFrame;
begin
  Inc(CurrentFrame);
  Result := Frames[CurrentFrame];
end;

procedure TFramesCtrl.Init;
var
  i: Integer;
begin
  FillChar(self, SizeOf(self), 0);
  for i := 1 to 10 do
    Frames[i].Number := i;

  CurrentFrame := 1;
end;

procedure TFramesCtrl.Next;
begin
  if CurrentFrame < 10 then
    Inc(CurrentFrame);
end;

{ TFrameRollsCtrl }

(*
procedure TFrameRollsCtrl.Next;
begin
  if CurrentFrame < 10 then
  begin
    if frame.CurrentRoll = 2 then
    begin
      frame.Over := True;
      Inc(CurrentFrame);
    end;
  end
  else
  begin
    if frame.CurrentRoll = 3 then
    begin
      frame.Over := True;
      GameOver := True;
    end;
  end;

end;
*)

procedure TFrameRollsCtrl.RecordRoll(NumPins: Integer);
begin
  Inc(CurrentRoll);
  FrameRolls[CurrentRoll] := TRollTotal(NumPins);
end;

{ TScoreCtrl }

procedure TScoreCtrl.Score;
begin

end;

end.
