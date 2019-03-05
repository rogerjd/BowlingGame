unit BowlingGame;

interface

type
  TFrame = record
  public
    Number: Integer;
    Score: Integer;
    RunningTotal: Integer;
  end;

  TRollResult = (Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine,
    Strike, Spare);


  TRegularFrame = class(TFrame)
    RollResult1, RollResult2: TRollResult;
  end;

  TFinalFrame = class(TRegularFrame)
    RollResult3: TRollResult;
  end;

  TBowlingGame = class
  private
    FTotalScore: Integer;
    procedure SetTotalScore(const Value: Integer);
  public
    procedure Start();
    procedure Roll(NumOfPins: Integer);
    function ScoreByFrame(): Integer;
    property TotalScore: Integer read FTotalScore write SetTotalScore;
  end;

implementation

{ TBowlingGame }

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
