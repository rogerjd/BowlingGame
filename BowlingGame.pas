unit BowlingGame;

interface

type
  TFrame = class
  public
    Number: Integer;
    Score: Integer;
    RunningTotal: Integer;
  end;

  TBowlingGame = class
  public
    procedure Start();
    procedure Roll(NumOfPins: Integer);
  end;

implementation

end.
