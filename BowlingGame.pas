unit BowlingGame;

interface

type
  TRollResult = (Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine,
    Strike, Spare);

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

implementation

{ TBowlingGame }

constructor TBowlingGame.Create;
var
  i: Integer;
begin
  for i := 1 to 9 do
  begin
    Frames[i] := TRegularFrame.Create();
  end;
  Frames[10] := TFinalFrame.Create();
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

procedure TBowlingGame.Roll(NumOfPins: Integer);
begin

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

end;

end.
