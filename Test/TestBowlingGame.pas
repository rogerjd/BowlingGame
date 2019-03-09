unit TestBowlingGame;
{

  Delphi DUnit Test Case
  ----------------------
  This unit contains a skeleton test case class generated by the Test Case Wizard.
  Modify the generated code to correctly setup and call the methods from the unit
  being tested.

}

interface

uses
  TestFramework, Generics.Collections, BowlingGame;

type
  // Test methods for class TFrameRollsCtrl

  TestTBowlingGame = class(TTestCase)
  strict private
    FBowlingGame: TBowlingGame;
  public
    procedure SetUp; override;
    procedure TearDown; override;
  published
    procedure TestStart;
    procedure TestRoll;
    procedure TestPerfectGame();
    procedure TestScoreByFrame;
  end;

implementation

procedure TestTBowlingGame.SetUp;
begin
  FBowlingGame := TBowlingGame.Create;
  FBowlingGame.Start;
  Assert(FBowlingGame.TotalScore = 0);
end;

procedure TestTBowlingGame.TearDown;
begin
  FBowlingGame.Free;
  FBowlingGame := nil;
end;

procedure TestTBowlingGame.TestStart;
begin
  // TODO: Validate method results
end;

procedure TestTBowlingGame.TestPerfectGame;
begin
  FBowlingGame.Roll(10);
  FBowlingGame.Roll(10);
  FBowlingGame.Roll(10);
  FBowlingGame.Roll(10);
  FBowlingGame.Roll(10);
  FBowlingGame.Roll(10);
  FBowlingGame.Roll(10);
  FBowlingGame.Roll(10);
  FBowlingGame.Roll(10);
  FBowlingGame.Roll(10);
  FBowlingGame.Roll(10);
  FBowlingGame.Roll(10);
  Assert(FBowlingGame.TotalScore = 300);
end;

procedure TestTBowlingGame.TestRoll;
var
  NumOfPins: Integer;
begin
  // TODO: Setup method call parameters
  FBowlingGame.Roll(4);
  Assert(FBowlingGame.TotalScore = 0);
  FBowlingGame.Roll(3);
  Assert(FBowlingGame.TotalScore = 7);
  FBowlingGame.Roll(10);
  Assert(FBowlingGame.TotalScore = 7);
  FBowlingGame.Roll(7);
  FBowlingGame.Roll(2);
  Assert(FBowlingGame.TotalScore = 35);

  // TODO: Validate method results
end;

procedure TestTBowlingGame.TestScoreByFrame;
var
  ReturnValue: Integer;
begin
  ReturnValue := FBowlingGame.ScoreByFrame;
  // TODO: Validate method results
end;

initialization

// Register any test cases with the test runner
RegisterTest(TestTBowlingGame.Suite);

end.
