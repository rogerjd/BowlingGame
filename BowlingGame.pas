unit BowlingGame;

interface

type
  TRollResult = (Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine,
    Strike, Spare);

  TFrameRolls = array[1..3] of TRollResult;

  TFrame = record
    FrameRolls: TFrameRolls;
    CurrentRoll: Integer;
    Over: Boolean;
    Number: Integer;
    Score: Integer;
    RunningTotal: Integer;
  end;

  TFrames = array [1 .. 10] of TFrame;

  TBowlingGame = record
  private
    FTotalScore: Integer;
    procedure SetTotalScore(const Value: Integer);
  public
    Frames: TFrames;
    CurrentFrame: Integer;
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

procedure TBowlingGame.Roll(NumOfPins: Integer);
var
  frame: TFrame;
begin
  frame := Frames[CurrentFrame];
  Inc(frame.CurrentRoll);
  frame.FrameRolls[frame.CurrentRoll] := TRollResult(NumOfPins);
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
  FillChar(Frames, SizeOf(Frames), 0);
  CurrentFrame := 1;
end;

end.
