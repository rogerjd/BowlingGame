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
    procedure TestAllGutterGame();
    procedure TestStrikeSpare();
    procedure TestAllSpareAndStrike();
    procedure TestGame1();
    procedure TestGame2();
    procedure TestScoreByFrame;
    procedure TestScoreByFrame2;
    procedure TestNewGame();
  end;

implementation

procedure TestTBowlingGame.SetUp;
begin
  FBowlingGame := TBowlingGame.Create;
  FBowlingGame.Start;
  Assert(FBowlingGame.TotalScore = 0);
  Assert(not FBowlingGame.GameOver);
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

procedure TestTBowlingGame.TestStrikeSpare;
begin
  Assert(not FBowlingGame.GameOver);
  FBowlingGame.Roll(10);
  FBowlingGame.Roll(7);
  FBowlingGame.Roll(3);
  FBowlingGame.Roll(10);
  FBowlingGame.Roll(7);
  FBowlingGame.Roll(3);
  FBowlingGame.Roll(10);
  FBowlingGame.Roll(7);
  FBowlingGame.Roll(3);
  FBowlingGame.Roll(10);
  FBowlingGame.Roll(7);
  FBowlingGame.Roll(3);
  FBowlingGame.Roll(10);
  FBowlingGame.Roll(7);
  FBowlingGame.Roll(3);
  FBowlingGame.Roll(10);
  Assert(FBowlingGame.TotalScore = 200);
  Assert(FBowlingGame.GameOver);
end;

procedure TestTBowlingGame.TestAllGutterGame;
begin
  FBowlingGame.Roll(0);
  FBowlingGame.Roll(0);
  FBowlingGame.Roll(0);
  FBowlingGame.Roll(0);
  FBowlingGame.Roll(0);
  FBowlingGame.Roll(0);
  FBowlingGame.Roll(0);
  FBowlingGame.Roll(0);
  FBowlingGame.Roll(0);
  FBowlingGame.Roll(0);
  FBowlingGame.Roll(0);
  FBowlingGame.Roll(0);
  Assert(FBowlingGame.TotalScore = 0);
end;

procedure TestTBowlingGame.TestAllSpareAndStrike;
begin
  FBowlingGame.Roll(7);
  FBowlingGame.Roll(3);

  FBowlingGame.Roll(7);
  FBowlingGame.Roll(3);

  FBowlingGame.Roll(8);
  FBowlingGame.Roll(2);

  FBowlingGame.Roll(7);
  FBowlingGame.Roll(3);

  FBowlingGame.Roll(5);
  FBowlingGame.Roll(5);

  FBowlingGame.Roll(7);
  FBowlingGame.Roll(3);

  FBowlingGame.Roll(9);
  FBowlingGame.Roll(1);

  FBowlingGame.Roll(7);
  FBowlingGame.Roll(3);

  FBowlingGame.Roll(8);
  FBowlingGame.Roll(2);

  FBowlingGame.Roll(7);
  FBowlingGame.Roll(3);
  FBowlingGame.Roll(10);
  Assert(FBowlingGame.TotalScore = 175);
  Assert(FBowlingGame.GameOver);
end;

procedure TestTBowlingGame.TestGame1;
begin
  FBowlingGame.Roll(9);
  FBowlingGame.Roll(0);

  FBowlingGame.Roll(2);
  FBowlingGame.Roll(7);

  FBowlingGame.Roll(10);

  FBowlingGame.Roll(3);
  FBowlingGame.Roll(6);

  FBowlingGame.Roll(8);
  FBowlingGame.Roll(1);

  FBowlingGame.Roll(7);
  FBowlingGame.Roll(3);

  FBowlingGame.Roll(5);
  FBowlingGame.Roll(3);

  FBowlingGame.Roll(6);
  FBowlingGame.Roll(2);

  FBowlingGame.Roll(7);
  FBowlingGame.Roll(3);

  FBowlingGame.Roll(6);
  FBowlingGame.Roll(3);
  Assert(FBowlingGame.TotalScore = 111);
  Assert(FBowlingGame.GameOver);
end;

procedure TestTBowlingGame.TestGame2;
begin
  FBowlingGame.Roll(9);
  FBowlingGame.Roll(0);

  FBowlingGame.Roll(10);

  FBowlingGame.Roll(0);
  FBowlingGame.Roll(0);

  FBowlingGame.Roll(3);
  FBowlingGame.Roll(6);

  FBowlingGame.Roll(0);
  FBowlingGame.Roll(0);

  FBowlingGame.Roll(10);

  FBowlingGame.Roll(0);
  FBowlingGame.Roll(10);

  FBowlingGame.Roll(6);
  FBowlingGame.Roll(4);

  FBowlingGame.Roll(0);
  FBowlingGame.Roll(0);

  FBowlingGame.Roll(6);
  FBowlingGame.Roll(3);
  Assert(FBowlingGame.TotalScore = 83);
  Assert(FBowlingGame.GameOver);
end;

procedure TestTBowlingGame.TestNewGame;
var
  sbfl: TList<TScoreByFrame>;
  sbf: TScoreByFrame;
begin
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 0);

  FBowlingGame.Roll(10);
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 1);
  sbfl := FBowlingGame.ScoreByFrame();
  sbf := sbfl[0];
  Assert(sbf.Status = 'Pending');
  Assert(sbf.Number = 1);
  Assert(sbf.FrameScore = 'X');
  Assert(sbf.FrameScoreInPoints = 0);
  Assert(sbf.GameScore = 0);

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
  Assert(FBowlingGame.GameOver);

  FBowlingGame.Start();
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 0);

  FBowlingGame.Roll(10);
  sbfl := FBowlingGame.ScoreByFrame();
  sbf := sbfl[0];
  Assert(sbf.Status = 'Pending');
  Assert(sbf.Number = 1);
  Assert(sbf.FrameScore = 'X');
  Assert(sbf.FrameScoreInPoints = 0);
  Assert(sbf.GameScore = 0);

  FBowlingGame.Roll(10);
  FBowlingGame.Roll(10);
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 3);
  sbf := sbfl[0];
  Assert(sbf.Status = 'Scored');
  Assert(sbf.Number = 1);
  Assert(sbf.FrameScore = 'X');
  Assert(sbf.FrameScoreInPoints = 30);
  Assert(sbf.GameScore = 30);
  sbf := sbfl[2];
  Assert(sbf.Status = 'Pending');

  sbf := sbfl[2];
  Assert(sbf.Status = 'Pending');
  Assert(sbf.Number = 3);
  Assert(sbf.FrameScore = 'X');
  Assert(sbf.FrameScoreInPoints = 0);
  Assert(sbf.GameScore = 30);

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
  Assert(FBowlingGame.GameOver);
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
  Assert(FBowlingGame.GameOver);
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
  sbfl: TList<TScoreByFrame>;
  sbf: TScoreByFrame;
begin
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 0);

  FBowlingGame.Roll(4);
  Assert(FBowlingGame.TotalScore = 0);

  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 1);
  Assert(sbfl[0].Status = 'In Play');
  Assert(sbfl[0].Number = 1);

  FBowlingGame.Roll(5);
  Assert(FBowlingGame.TotalScore = 9);
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 1);
  sbf := sbfl[0];
  Assert(sbf.Status = 'Scored');
  Assert(sbfl[0].Number = 1);
  // 1

  FBowlingGame.Roll(2);
  Assert(FBowlingGame.TotalScore = 9);
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 2);
  sbf := sbfl[1];
  Assert(sbf.Status = 'In Play');
  Assert(sbf.Number = 2);

  FBowlingGame.Roll(3);
  Assert(FBowlingGame.TotalScore = 14);
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 2);
  sbf := sbfl[1];
  Assert(sbf.Status = 'Scored');
  Assert(sbf.Number = 2);
  // 2

  FBowlingGame.Roll(10);
  Assert(FBowlingGame.TotalScore = 14);
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 3);
  sbf := sbfl[2];
  Assert(sbf.Status = 'Pending');
  Assert(sbf.Number = 3);
  // 3

  FBowlingGame.Roll(10);
  Assert(FBowlingGame.TotalScore = 14);
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 4);
  sbf := sbfl[3];
  Assert(sbf.Status = 'Pending');
  Assert(sbf.Number = 4);
  // 4

  FBowlingGame.Roll(10);
  Assert(FBowlingGame.TotalScore = 44);
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 5);
  sbf := sbfl[4];
  Assert(sbf.Status = 'Pending');
  Assert(sbf.Number = 5);
  // 5

  FBowlingGame.Roll(10);
  Assert(FBowlingGame.TotalScore = 74);
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 6);
  sbf := sbfl[5];
  Assert(sbf.Status = 'Pending');
  Assert(sbf.Number = 6);
  // 6

  FBowlingGame.Roll(9);
  Assert(FBowlingGame.TotalScore = 103);
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 7);
  sbf := sbfl[6];
  Assert(sbf.Status = 'In Play');
  Assert(sbf.Number = 7);

  FBowlingGame.Roll(1);
  Assert(FBowlingGame.TotalScore = 123);
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 7);
  sbf := sbfl[6];
  Assert(sbf.Status = 'Pending');
  Assert(sbf.Number = 7);
  // 7

  FBowlingGame.Roll(10);
  Assert(FBowlingGame.TotalScore = 143);
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 8);
  sbf := sbfl[7];
  Assert(sbf.Status = 'Pending');
  Assert(sbf.Number = 8);
  // 8

  FBowlingGame.Roll(10);
  Assert(FBowlingGame.TotalScore = 143);
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 9);
  sbf := sbfl[8];
  Assert(sbf.Status = 'Pending');
  Assert(sbf.Number = 9);
  // 9

  FBowlingGame.Roll(4);
  Assert(FBowlingGame.TotalScore = 167);
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 10);
  sbf := sbfl[9];
  Assert(sbf.Status = 'In Play');
  Assert(sbf.Number = 10);

  FBowlingGame.Roll(3);
  Assert(FBowlingGame.TotalScore = 191);
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 10);
  sbf := sbfl[9];
  Assert(sbf.Status = 'Scored');
  Assert(sbf.Number = 10);
  // 10

  sbfl := FBowlingGame.ScoreByFrame();
  sbf := sbfl[0];
  Assert(sbf.Status = 'Scored');
  Assert(sbf.Number = 1);
  Assert(sbf.FrameScore = '4 5');
  Assert(sbf.FrameScoreInPoints = 9);
  Assert(sbf.GameScore = 9);

  sbf := sbfl[1];
  Assert(sbf.Status = 'Scored');
  Assert(sbf.Number = 2);
  Assert(sbf.FrameScore = '2 3');
  Assert(sbf.FrameScoreInPoints = 5);
  Assert(sbf.GameScore = 14);

  sbf := sbfl[2];
  Assert(sbf.Status = 'Scored');
  Assert(sbf.Number = 3);
  Assert(sbf.FrameScore = 'X');
  Assert(sbf.FrameScoreInPoints = 30);
  Assert(sbf.GameScore = 44);

  sbf := sbfl[3];
  Assert(sbf.Status = 'Scored');
  Assert(sbf.Number = 4);
  Assert(sbf.FrameScore = 'X');
  Assert(sbf.FrameScoreInPoints = 30);
  Assert(sbf.GameScore = 74);

  sbf := sbfl[4];
  Assert(sbf.Status = 'Scored');
  Assert(sbf.Number = 5);
  Assert(sbf.FrameScore = 'X');
  Assert(sbf.FrameScoreInPoints = 29);
  Assert(sbf.GameScore = 103);

  sbf := sbfl[5];
  Assert(sbf.Status = 'Scored');
  Assert(sbf.Number = 6);
  Assert(sbf.FrameScore = 'X');
  Assert(sbf.FrameScoreInPoints = 20);
  Assert(sbf.GameScore = 123);

  sbf := sbfl[6];
  Assert(sbf.Status = 'Scored');
  Assert(sbf.Number = 7);
  Assert(sbf.FrameScore = '9 /');
  Assert(sbf.FrameScoreInPoints = 20);
  Assert(sbf.GameScore = 143);

  sbf := sbfl[7];
  Assert(sbf.Status = 'Scored');
  Assert(sbf.Number = 8);
  Assert(sbf.FrameScore = 'X');
  Assert(sbf.FrameScoreInPoints = 24);
  Assert(sbf.GameScore = 167);

  sbf := sbfl[8];
  Assert(sbf.Status = 'Scored');
  Assert(sbf.Number = 9);
  Assert(sbf.FrameScore = 'X');
  Assert(sbf.FrameScoreInPoints = 17);
  Assert(sbf.GameScore = 184);

  sbf := sbfl[9];
  Assert(sbf.Status = 'Scored');
  Assert(sbf.Number = 10);
  Assert(sbf.FrameScore = '4 3');
  Assert(sbf.FrameScoreInPoints = 7);
  Assert(sbf.GameScore = 191);

end;

procedure TestTBowlingGame.TestScoreByFrame2;
var
  sbfl: TList<TScoreByFrame>;
  sbf: TScoreByFrame;
begin
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 0);

  FBowlingGame.Roll(9);
  Assert(FBowlingGame.TotalScore = 0);

  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 1);
  Assert(sbfl[0].Status = 'In Play');
  Assert(sbfl[0].Number = 1);

  FBowlingGame.Roll(1);
  Assert(FBowlingGame.TotalScore = 0);
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 1);
  sbf := sbfl[0];
  Assert(sbf.Status = 'Pending');
  Assert(sbfl[0].Number = 1);
  // 1

  FBowlingGame.Roll(2);
  Assert(FBowlingGame.TotalScore = 12);
  sbfl := FBowlingGame.ScoreByFrame();
  Assert(sbfl.Count = 2);
  sbf := sbfl[1];
  Assert(sbf.Status = 'In Play');
  Assert(sbf.Number = 2);

end;

initialization

// Register any test cases with the test runner
RegisterTest(TestTBowlingGame.Suite);

end.
