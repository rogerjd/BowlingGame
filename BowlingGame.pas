unit BowlingGame;

interface

uses
  Generics.Collections;

type
  TRollTotal = (rtZero, rtOne, rtTwo, rtThree, rtFour, rtFive, rtSix, rtSeven,
    rtEight, rtNine, rtStrike, rtSpare); // if 10 is rolled it will be rtStrike.

  TFrame = class;

  TFrameRollsCtrl = class
    frame: TFrame;
    Over: Boolean;
    FrameRolls: TList<TRollTotal>;
  private
    function GetScore: integer;
    function NumPinsToRollTotal(NumPins: integer): TRollTotal;
    function RollTotalToNumberOfPins(RollTotal: TRollTotal): integer;
    function RollTotalAsString(RollTotal: TRollTotal): string;
  public
    property Score: integer read GetScore;
    procedure RecordRoll(NumPins: integer);
    constructor Create(xownr: TFrame);
    destructor Destroy(); override;
  end;

  TFramesCtrl = class;
  TBowlingGame = class;

  TFrame = class
  private
    FScore: integer;
    FRunningTotal: integer;
    function GetOpenFrame: Boolean;
    procedure SetScore(const Value: integer);
    procedure SetRunningTotal(const Value: integer);
  public
    StrikeCount, SpareCount: integer;
    FramesCtrl: TFramesCtrl;
    FrameRollsCtrl: TFrameRollsCtrl;
    Game: TBowlingGame;

    Number: integer;
    Scored: Boolean;
    function NumPinsStanding(): integer;
    procedure Reset();
    function CheckRollInput(NumOfPins: integer): Boolean;
    property OpenFrame: Boolean read GetOpenFrame;
    function NeedRollsRecordedInFutureFrame: Boolean;
    property Score: integer read FScore write SetScore;
    property RunningTotal: integer read FRunningTotal write SetRunningTotal;
    Constructor Create(xFrameNum: integer; xFramesCtrl: TFramesCtrl;
      xGame: TBowlingGame);
    destructor Destroy(); override;
  end;

  TFrames = array [1 .. 10] of TFrame;

  TFramesCtrl = class
  private
    Frames: TFrames;
    CurrentFrame: integer;
  public
    function GetCurrent(): TFrame;
    function Next(): Boolean;
    procedure Init();
    constructor Create(xGame: TBowlingGame);
    destructor Destroy(); override;
  end;

  TPendingScoreFrame = class
    FrameNum: integer;
    FramesCtrl: TFramesCtrl;
    BonusPoints: TList<integer>;
  private
    function ReadyToScore(): Boolean;
    procedure Score();
  public
    constructor Create(xFrameNum: integer; xFramesCtrl: TFramesCtrl);
    destructor Destroy; override;
  end;

  // frames with strike/spare; they need a roll in a  future frame in order to score
  TPendingFrames = class
    FramesPending: TList<TPendingScoreFrame>;
  private
    function Any(): Boolean;
    procedure Add(FrameNum: integer; xFramesCtrl: TFramesCtrl);
    procedure AddBonusPoints(pts: integer);
  public
    constructor Create();
    destructor Destroy; override;
  end;

  TScoreByFrame = record
    Number: integer;
    Status: string;
    GameScore: integer;
    FrameScore: string;
    FrameScoreInPoints: integer;
  end;

  TScoreCtrl = class
    FramesCtrl: TFramesCtrl;
    Pending: TPendingFrames;
    ScoreByFrames: TList<TScoreByFrame>;
    procedure Score();
    procedure Init();
    function GetScoreByFrame(): TList<TScoreByFrame>;
    constructor Create(xFramesCtrl: TFramesCtrl);
    destructor Destroy(); override;
  end;

  TBowlingGame = class
  private
    FTotalScore: integer;
  public
    FramesCtrl: TFramesCtrl;
    ScoreCtrl: TScoreCtrl;
    GameOver: Boolean;

    // starts a new game of bowling
    procedure Start();

    // takes the number of pins knocked down for each roll
    procedure Roll(NumOfPins: integer);

    // score for each frame that has occurred so far
    function ScoreByFrame(): TList<TScoreByFrame>;

    // returns the total score for that game up to the given point
    property TotalScore: integer read FTotalScore write FTotalScore;

    constructor Create();
    destructor Destroy(); override;
  end;

implementation

uses
  Dialogs, SysUtils, System.UITypes;

{ TBowlingGame }

constructor TBowlingGame.Create;
begin
  FramesCtrl := TFramesCtrl.Create(self);
  ScoreCtrl := TScoreCtrl.Create(FramesCtrl);
end;

destructor TBowlingGame.Destroy;
begin
  FramesCtrl.Free();
  ScoreCtrl.Free();
  inherited;
end;

procedure TBowlingGame.Roll(NumOfPins: integer);
var
  frame: TFrame;
  IsNextFrame: Boolean;

  function ValidInput(): Boolean;
  begin
    Result := NumOfPins in [0 .. 10];
  end;

begin
  if GameOver then
  begin
    MessageDlg('Game Over', mtInformation, [mbOK], 0);
    Exit;
  end;

  if not ValidInput() then
  begin
    MessageDlg('Invalid input, Roll = ' + IntToStr(NumOfPins), mtError,
      [mbOK], 0);
    Exit;
  end;

  frame := FramesCtrl.GetCurrent();
  if not frame.CheckRollInput(NumOfPins) then
  begin
    MessageDlg('Possible invalid input, roll > pins standing = ' +
      IntToStr(NumOfPins), mtInformation, [mbOK], 0);
  end;
  frame.FrameRollsCtrl.RecordRoll(NumOfPins);

  if ScoreCtrl.Pending.Any then
    ScoreCtrl.Pending.AddBonusPoints(NumOfPins);

  ScoreCtrl.Score(); // current and pending

  if frame.FrameRollsCtrl.Over then
  begin
    IsNextFrame := FramesCtrl.Next();
    GameOver := not IsNextFrame;
  end;
end;

function TBowlingGame.ScoreByFrame: TList<TScoreByFrame>;
begin
  Result := ScoreCtrl.GetScoreByFrame();
end;

procedure TBowlingGame.Start;
begin
  GameOver := False;
  ScoreCtrl.Init();
  FramesCtrl.Init();
end;

{ TFrame }

// input: NumOfPins int, the number of pins knocked down in this roll
// output: boolean, True if NumOfPins is <= pins standing in the current frame, else false
function TFrame.CheckRollInput(NumOfPins: integer): Boolean;
begin
  Result := NumOfPins <= NumPinsStanding();
end;

constructor TFrame.Create(xFrameNum: integer; xFramesCtrl: TFramesCtrl;
  xGame: TBowlingGame);
begin
  Number := xFrameNum;
  FramesCtrl := xFramesCtrl;
  FrameRollsCtrl := TFrameRollsCtrl.Create(self);
  Game := xGame;
end;

destructor TFrame.Destroy;
begin
  FrameRollsCtrl.Destroy();
  inherited;
end;

function TFrame.NeedRollsRecordedInFutureFrame: Boolean;
begin
  if Number = 10 then
    Result := False
  else
    Result := not OpenFrame;
end;

function TFrame.NumPinsStanding: integer;
begin
  // 10th frame can have strike/spare and still be 'active frame'
  Result := 10 - (FrameRollsCtrl.GetScore() mod 10);
end;

procedure TFrame.Reset;
begin
  Score := 0;
  Scored := False;
  StrikeCount := 0;
  SpareCount := 0;

  RunningTotal := 0;
  FrameRollsCtrl.FrameRolls.Clear();
  FrameRollsCtrl.Over := False;
end;

function TFrame.GetOpenFrame: Boolean;
begin
  Result := (StrikeCount + SpareCount) = 0;
end;

procedure TFrame.SetScore(const Value: integer);
begin
  FScore := Value;
  Scored := True;

  if Number = 1 then
    RunningTotal := Value
  else
    RunningTotal := FramesCtrl.Frames[Number - 1].RunningTotal + Value;
end;

procedure TFrame.SetRunningTotal(const Value: integer);
begin
  FRunningTotal := Value;
  Game.TotalScore := Value;
end;

{ TFramesCtrl }

constructor TFramesCtrl.Create(xGame: TBowlingGame);
var
  i: integer;
begin
  for i := 1 to 10 do
  begin
    Frames[i] := TFrame.Create(i, self, xGame);
  end;
end;

destructor TFramesCtrl.Destroy;
var
  i: integer;
begin
  for i := 1 to 10 do
  begin
    Frames[i].Free();
  end;

  inherited;
end;

function TFramesCtrl.GetCurrent(): TFrame;
begin
  Result := Frames[CurrentFrame];
end;

procedure TFramesCtrl.Init;
var
  i: integer;
begin
  CurrentFrame := 1;

  for i := 1 to 10 do
  begin
    Frames[i].Reset();
    Frames[i].Scored := False;
  end;
end;

function TFramesCtrl.Next: Boolean;
begin
  if CurrentFrame < 10 then
  begin
    Inc(CurrentFrame);
    Result := True;
  end
  else
  begin
    Result := False;
  end;
end;

{ TFrameRollsCtrl }

constructor TFrameRollsCtrl.Create(xownr: TFrame);
begin
  frame := xownr;
  FrameRolls := TList<TRollTotal>.Create();
  // CurrentRoll := 0;
end;

destructor TFrameRollsCtrl.Destroy;
begin
  FrameRolls.Free();
  inherited;
end;

function TFrameRollsCtrl.GetScore: integer;

  function SumFrameRolls(): integer;
  var
    i: integer;
  begin
    Result := 0;

    for i := 0 to FrameRolls.Count - 1 do
    begin
      if FrameRolls[i] = rtStrike then
        Inc(Result, 10)
      else if RollTotalToNumberOfPins(FrameRolls[i]) in [0 .. 9] then
        Inc(Result, RollTotalToNumberOfPins(FrameRolls[i]))
      else if FrameRolls[i] = rtSpare then
      begin
        Dec(Result, RollTotalToNumberOfPins(FrameRolls[i - 1]));
        Inc(Result, 10);
      end;
    end;
  end;

begin
  Result := SumFrameRolls(); // todo and 1 + 2 + 3 if 10th
end;

function TFrameRollsCtrl.NumPinsToRollTotal(NumPins: integer): TRollTotal;

  function IsSpare(): Boolean;
  begin
    Result := (FrameRolls.Count > 0) and (NumPins <> 0) and
      (RollTotalToNumberOfPins(FrameRolls[FrameRolls.Count - 1]) in [0 .. 9])
      and (RollTotalToNumberOfPins(FrameRolls[FrameRolls.Count - 1]) +
      NumPins = 10);
  end;

  function IsStrike(): Boolean;
  begin
    Result := (not IsSpare()) and (NumPins = 10);
  end;

begin
  if IsStrike() then
  begin
    Result := rtStrike;
    Inc(frame.StrikeCount);
    Exit;
  end;

  if IsSpare() then
  begin
    Result := rtSpare;
    Inc(frame.SpareCount);
    Exit;
  end;

  Result := TRollTotal(NumPins);
end;

procedure TFrameRollsCtrl.RecordRoll(NumPins: integer);
begin
  FrameRolls.Add(NumPinsToRollTotal(NumPins));

  // is frame done
  if frame.Number < 10 then
  begin
    if ((frame.StrikeCount + frame.SpareCount) > 0) or (FrameRolls.Count = 2)
    then
      Over := True;
  end
  else
    Over := (frame.OpenFrame and (FrameRolls.Count = 2)) or
      ((not frame.OpenFrame) and (FrameRolls.Count = 3));
end;

function TFrameRollsCtrl.RollTotalAsString(RollTotal: TRollTotal): string;
begin
  case RollTotal of
    rtZero .. rtNine:
      Result := IntToStr(ord(RollTotal));
    rtStrike:
      Result := 'X';
    rtSpare:
      Result := '/';
  end;
end;

function TFrameRollsCtrl.RollTotalToNumberOfPins(RollTotal: TRollTotal)
  : integer;
begin
  Result := -1;
  case RollTotal of
    rtZero .. rtNine:
      Result := ord(RollTotal);
    rtStrike, rtSpare:
      Result := 10;
  end;
end;

{ TScoreCtrl }

constructor TScoreCtrl.Create(xFramesCtrl: TFramesCtrl);
begin
  FramesCtrl := xFramesCtrl;
  Pending := TPendingFrames.Create();
  ScoreByFrames := TList<TScoreByFrame>.Create();
end;

destructor TScoreCtrl.Destroy;
begin
  Pending.Free();
  ScoreByFrames.Free();
  inherited;
end;

function TScoreCtrl.GetScoreByFrame: TList<TScoreByFrame>;
var
  frame: TFrame;
  i: integer;
  sbf: TScoreByFrame;
  TotalScore: integer;

  function InitScoreByFrame(): TScoreByFrame;
  var
    i: integer;

    function GetFrameStaus(): string;
    begin
      if frame.FrameRollsCtrl.Over then
      begin
        if frame.Scored then
          Result := 'Scored'
        else
          Result := 'Pending';
      end
      else
      begin
        if frame.FrameRollsCtrl.FrameRolls.Count > 0 then
          Result := 'In Play'
        else
          Result := '';
      end;
    end;

  begin
    Result.Number := frame.Number;
    Result.Status := GetFrameStaus();
    Result.FrameScoreInPoints := frame.Score;
    Inc(TotalScore, frame.Score);
    Result.GameScore := TotalScore;

    Result.FrameScore := '';
    if Result.Status <> '' then
    begin
      for i := 0 to frame.FrameRollsCtrl.FrameRolls.Count - 1 do
      begin
        Result.FrameScore := Result.FrameScore +
          frame.FrameRollsCtrl.RollTotalAsString(frame.FrameRollsCtrl.FrameRolls
          [i]) + ' ';
      end;
      Delete(Result.FrameScore, Length(Result.FrameScore), 1);
    end;
  end;

begin
  ScoreByFrames.Clear();
  TotalScore := 0;

  // get frames scored so far
  for i := 1 to 10 do
  begin
    frame := FramesCtrl.Frames[i];
    if frame.Scored then
      ScoreByFrames.Add(InitScoreByFrame())
    else
      Break;
  end;

  // get pending frames
  for i := 0 to Pending.FramesPending.Count - 1 do
  begin
    frame := Pending.FramesPending[i].FramesCtrl.Frames
      [Pending.FramesPending[i].FrameNum];
    ScoreByFrames.Add(InitScoreByFrame())
  end;

  // current in progress
  frame := FramesCtrl.Frames[FramesCtrl.CurrentFrame];
  sbf := InitScoreByFrame();
  if sbf.Status = 'In Play' then
    ScoreByFrames.Add(sbf);

  Result := ScoreByFrames;
end;

procedure TScoreCtrl.Init;
begin
  FramesCtrl.Init();
  Pending.FramesPending.Clear();
  ScoreByFrames.Clear();
end;

procedure TScoreCtrl.Score;
var
  frame: TFrame;
  i: integer;
begin
  frame := FramesCtrl.GetCurrent();

  // process pending score
  if Pending.Any then
  begin
    for i := Pending.FramesPending.Count - 1 downto 0 do
    begin
      if Pending.FramesPending[i].ReadyToScore() then
      begin
        Pending.FramesPending[i].Score();
        Pending.FramesPending[i].Free();
        Pending.FramesPending.Delete(i);
      end;
    end;
  end;

  if frame.FrameRollsCtrl.Over then
  begin
    if (not frame.NeedRollsRecordedInFutureFrame()) then
    begin
      frame.Score := frame.FrameRollsCtrl.GetScore();
    end
    else
      Pending.Add(frame.Number, FramesCtrl);
  end;
end;

{ TPendingScoreFrame }

constructor TPendingScoreFrame.Create(xFrameNum: integer;
  xFramesCtrl: TFramesCtrl);
begin
  FrameNum := xFrameNum;
  FramesCtrl := xFramesCtrl;
  BonusPoints := TList<integer>.Create();
end;

destructor TPendingScoreFrame.Destroy;
begin
  BonusPoints.Free();
  inherited;
end;

function TPendingScoreFrame.ReadyToScore: Boolean;
begin
  Result := False;
  if FramesCtrl.Frames[FrameNum].StrikeCount = 1 then
    Result := BonusPoints.Count = 2
  else if FramesCtrl.Frames[FrameNum].SpareCount = 1 then
    Result := BonusPoints.Count = 1;
end;

procedure TPendingScoreFrame.Score;
var
  frame: TFrame;
begin
  frame := FramesCtrl.Frames[FrameNum];
  if frame.StrikeCount = 1 then
  begin
    frame.Score := frame.FrameRollsCtrl.GetScore() + BonusPoints[0] +
      BonusPoints[1];
  end
  else if frame.SpareCount = 1 then
  begin
    frame.Score := frame.FrameRollsCtrl.GetScore() + BonusPoints[0];
  end;
end;

{ TPendingFrames }

procedure TPendingFrames.Add(FrameNum: integer; xFramesCtrl: TFramesCtrl);
var
  PendingScoreFrame: TPendingScoreFrame;
begin
  PendingScoreFrame := TPendingScoreFrame.Create(FrameNum, xFramesCtrl);
  FramesPending.Add(PendingScoreFrame);
end;

procedure TPendingFrames.AddBonusPoints(pts: integer);
var
  i: integer;
begin
  for i := 0 to FramesPending.Count - 1 do
  begin
    FramesPending[i].BonusPoints.Add(pts);
  end;
end;

function TPendingFrames.Any: Boolean;
begin
  Result := FramesPending.Count > 0;
end;

constructor TPendingFrames.Create;
begin
  FramesPending := TList<TPendingScoreFrame>.Create();
end;

destructor TPendingFrames.Destroy;
var
  i: integer;
begin
  for i := FramesPending.Count - 1 downto 0 do
    FramesPending[i].Free();

  FramesPending.Free();
  inherited;
end;

end.
