unit BowlingGame;

interface

uses
  Generics.Collections;

type
  TRollTotal = (rtZero, rtOne, rtTwo, rtThree, rtFour, rtFive, rtSix, rtSeven,
    rtEight, rtNine, rtStrike, rtSpare); // todo: never 10?

  TFrameRolls2 = array [1 .. 3] of TRollTotal;

  TFrame = class;

  (*
    TFrameRolls = class
    ownr: TFrame;
    Over: Boolean;
    Rolls: TList<TRollTotal>;
    function GetScore: integer;
    procedure RecordRoll(NumPins: integer);
    constructor Create(xownr: TFrame);
    destructor Destroy(); override;
    end;
  *)

  TFrameRollsCtrl = class
    frame: TFrame;
    Over: Boolean;
    FrameRolls: TList<TRollTotal>; // TFrameRolls
    // FrameRolls: TFrameRolls2;
    // FrameRolls2: TList<integer>;
  private
    CurrentRoll: integer; // todo: dont need w/list?
    function GetScore: integer;
    function NumPinsToRollTotal(NumPins: integer): TRollTotal;
    function RollTotalToNumberOfPins(RollTotal: TRollTotal): integer;
  public
    property Score: integer read GetScore;
    procedure RecordRoll(NumPins: integer);
    constructor Create(xownr: TFrame);
    destructor Destroy(); override;
  end;

  TFramesCtrl = class;

  TFrame = class
  private
    FCurrentRoll: integer;
    FScore: Integer;
    procedure SetCurrentRoll(const Value: integer);
    function GetOpenFrame: Boolean;
    procedure SetScore(const Value: Integer);
    // procedure SetIsScoreableByItself(const Value: Boolean);
  public
    StrikeCount, SpareCount: integer; // only 1 spare
    FramesCtrl: TFramesCtrl;
    FrameRollsCtrl: TFrameRollsCtrl;

    // over, see FRC
    Number: integer;
//    Score: integer;
    Scored: Boolean;
    RunningTotal: integer; // todo: cumulative, set along with Score (prop)?
    procedure Reset();
    function CheckRollInput(NumOfPins: integer): Boolean;
    property OpenFrame: Boolean read GetOpenFrame;
    function NeedRollsRecordedInFutureFrame: Boolean;
    property Score: Integer read FScore write SetScore;
    Constructor Create(xFrameNum: integer; xFramesCtrl: TFramesCtrl);
    destructor Destroy(); override;
    // property CurrentRoll: integer read FCurrentRoll write SetCurrentRoll;
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
    constructor Create();
    destructor Destroy(); override;
  end;

  TPendingScoreFrame = class
    FrameNum: integer;
    FramesCtrl: TFramesCtrl;
    BonusPoints: TList<integer>;
    // Bonus1, Bonus2: Integer;
  private
    function ReadyToScore(): Boolean;
    procedure Score();
    constructor Create(xFrameNum: integer; xFramesCtrl: TFramesCtrl);
    destructor Destroy; override;
  end;

  TPendingFrames = class
    FramesPending: TList<TPendingScoreFrame>;
  private
    function Any(): Boolean;
    procedure Add(FrameNum: integer; xFramesCtrl: TFramesCtrl);
    procedure AddBonusPoints(pts: integer);
    constructor Create();
    destructor Destroy; override;
  end;

  TScoreCtrl = class
    FramesCtrl: TFramesCtrl;
    Pending: TPendingFrames;
    procedure Score();
    constructor Create(xFramesCtrl: TFramesCtrl);
    destructor Destroy(); override;
  end;

  TBowlingGame = class
  private
    FTotalScore: integer;
    procedure SetTotalScore(const Value: integer);
    procedure CalculateScore();
  public
    FramesCtrl: TFramesCtrl;
    ScoreCtrl: TScoreCtrl;
    GameOver: Boolean;
    procedure Start();
    procedure Roll(NumOfPins: integer);
    function ScoreByFrame(): integer;
    property TotalScore: integer read FTotalScore write SetTotalScore;
    // todo: cumulative
    constructor Create();
    destructor Destroy(); override;
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

uses
  Dialogs, SysUtils;

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

constructor TBowlingGame.Create;
begin
  FramesCtrl := TFramesCtrl.Create();
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
    MessageDlg('Invalid input, Roll = ' + IntToStr(NumOfPins), mtInformation,
      [mbOK], 0);
    Exit;
  end;

  frame := FramesCtrl.GetCurrent();
  if not frame.CheckRollInput(NumOfPins) then
  begin
    MessageDlg('Possible invalid input, roll > pins standing = ' +
      IntToStr(NumOfPins), mtInformation, [mbOK], 0);
  end;
  frame.FrameRollsCtrl.RecordRoll(NumOfPins); // over < 10

  if ScoreCtrl.Pending.Any then
    ScoreCtrl.Pending.AddBonusPoints(NumOfPins);

  ScoreCtrl.Score(); // current and pending

  if frame.FrameRollsCtrl.Over then
  begin
    IsNextFrame := FramesCtrl.Next();
    GameOver := not IsNextFrame;
  end;
end;

function TBowlingGame.ScoreByFrame: integer;
begin

end;

procedure TBowlingGame.SetTotalScore(const Value: integer);
begin
  FTotalScore := Value;
end;

procedure TBowlingGame.Start;
begin
  GameOver := False;
  FramesCtrl.Init();
end;

{ TFrame }

// input: NumOfPins int, the number of pins knocked down in this roll
// output: boolean, True if NumOfPins is <= pins standing in the current frame, else false
function TFrame.CheckRollInput(NumOfPins: integer): Boolean;
var
  PinsStanding: integer;
begin
  // 10th frame can have strike/spare and still be 'active frame'
  PinsStanding := 10 - (FrameRollsCtrl.GetScore() mod 10);
  Result := NumOfPins <= PinsStanding;
end;

constructor TFrame.Create(xFrameNum: integer; xFramesCtrl: TFramesCtrl);
begin
  Number := xFrameNum;
  FramesCtrl := xFramesCtrl;
  FrameRollsCtrl := TFrameRollsCtrl.Create(self);
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

procedure TFrame.Reset;
begin
  Score := 0;
  Scored := False;
  RunningTotal := 0;
end;

function TFrame.GetOpenFrame: Boolean;
begin
  Result := (StrikeCount + SpareCount) = 0;
end;

procedure TFrame.SetScore(const Value: Integer);
var
  PrevFrame: TFrame;
begin
  FScore := Value;
  Scored := True;

  if FramesCtrl.CurrentFrame = 1 then
    RunningTotal := Value
  else
    RunningTotal := FramesCtrl.Frames[FramesCtrl.CurrentFrame - 1].RunningTotal + Value;
end;

procedure TFrame.SetCurrentRoll(const Value: integer);
begin
  FCurrentRoll := Value;
end;

{ TFramesCtrl }

constructor TFramesCtrl.Create;
var
  i: integer;
begin
  for i := 1 to 10 do
  begin
    Frames[i] := TFrame.Create(i, self);
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
  // ??  Inc(CurrentFrame);
  Result := Frames[CurrentFrame];
end;

procedure TFramesCtrl.Init;
var
  i: integer;
begin
  for i := 1 to 10 do
  begin
    Frames[i].Reset();
  end;

  CurrentFrame := 1;
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

constructor TFrameRollsCtrl.Create(xownr: TFrame);
begin
  frame := xownr;
  FrameRolls := TList<TRollTotal>.Create();
  CurrentRoll := 0;
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

      // Inc(Result, RollTotalToNumberOfPins(FrameRolls[i])); // todo: bug
    end;

    (*
      for i := 0 to FrameRolls.Count - 1 do
      Inc(Result, RollTotalToNumberOfPins(FrameRolls[i])); // todo: bug
    *)
  end;

begin
  Result := SumFrameRolls(); // todo and 1 + 2 + 3 if 10th

  (*
    if frame.Number < 10 then
    begin
    if not frame.OpenFrame then
    Result := 10;
    Exit;
    end
    else
    begin
    Result := SumFrameRolls();; // todo and 1 + 2 + 3 if 10th
    end;
  *)

  (*
    if not ownr.OpenFrame then
    Result := 10
    else
    Result := SumFrameRolls();; // todo and 1 + 2 + 3 if 10th
  *)
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
  if IsStrike() then // todo:
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

  (*
    if FrameRolls.Count = 0 then
    begin
    if NumPins = 10 then
    Result := rtStrike
    else
    Result := TRollTotal(NumPins);
    end
    else
    begin
    if (ord(FrameRolls[FrameRolls.Count]) + NumPins) = 10 then
    Result := rtSpare
    else
    Result := TRollTotal(NumPins);
    end;
  *)
end;

procedure TFrameRollsCtrl.RecordRoll(NumPins: integer);
begin
  Inc(CurrentRoll); // todo: use List.count
  FrameRolls.Add(NumPinsToRollTotal(NumPins));
  // TRollTotal(NumPins);
  // todo: SetValue also Strike/Spare in ownr0

  // is frame done
  if frame.Number < 10 then
  begin // todo: List.Count
    if ((frame.StrikeCount + frame.SpareCount) > 0) or (CurrentRoll = 2) then
      Over := True;
  end
  else
    Over := (frame.OpenFrame and (CurrentRoll = 2)) or
      ((not frame.OpenFrame) and (CurrentRoll = 3));
end;

function TFrameRollsCtrl.RollTotalToNumberOfPins(RollTotal: TRollTotal)
  : integer;
begin
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
end;

destructor TScoreCtrl.Destroy;
begin
  Pending.Free();
  inherited;
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
    for i := 0 to Pending.FramesPending.Count - 1 do
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
    //  frame.RunningTotal :=

//todo: remv      frame.Scored := True;
    end
    else
      Pending.Add(frame.Number, FramesCtrl);
  end
  else
  begin
    // score := frame score + bonus
  end;

  (*
    if frame.FrameRollsCtrl.Over then begin
    //todo: strike/spare
    if frame.OpenFrame then
    frame.Score := frame.FrameRollsCtrl.GetScore()
    else ; //todo
    end;
  *)
end;

{ TPendingScoreFrame }

constructor TPendingScoreFrame.Create(xFrameNum: integer;
  xFramesCtrl: TFramesCtrl);
begin
  FrameNum := xFrameNum;
  FramesCtrl := xFramesCtrl;
  BonusPoints := TList<integer>.Create();
  (*
    Bonus1 := -1;
    Bonus2 := -2;
  *)
end;

destructor TPendingScoreFrame.Destroy;
begin
  BonusPoints.Free();
  inherited;
end;

function TPendingScoreFrame.ReadyToScore: Boolean;
begin
  if FramesCtrl.Frames[FrameNum].StrikeCount = 1 then
    Result := BonusPoints.Count = 2 // (Bonus1 > -1) and (Bonus2 > -1)
  else if FramesCtrl.Frames[FrameNum].SpareCount = 1 then
    Result := BonusPoints.Count = 1; // (Bonus1 > -1);
end;

procedure TPendingScoreFrame.Score;
var
  frame: TFrame;
begin
  frame := FramesCtrl.Frames[FrameNum]; // todo: bug?
  if frame.StrikeCount = 1 then
  begin
    frame.Score := frame.FrameRollsCtrl.GetScore() + BonusPoints[0] +
      BonusPoints[1];
//    frame.Scored := True;
  end
  else if frame.SpareCount = 1 then
  begin
    frame.Score := frame.FrameRollsCtrl.GetScore() + BonusPoints[0];
//    frame.Scored := True;
  end;
end;

{ TPendingFrames }

procedure TPendingFrames.Add(FrameNum: integer; xFramesCtrl: TFramesCtrl);
var
  PendingScoreFrame: TPendingScoreFrame;
begin
  PendingScoreFrame := TPendingScoreFrame.Create(FrameNum, xFramesCtrl);
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
begin
  FramesPending.Free();
  inherited;
end;

(*
  { TFrameRolls }

  constructor TFrameRolls.Create(xownr: TFrame);
  begin
  ownr := xownr;
  end;

  destructor TFrameRolls.Destroy;
  begin

  inherited;
  end;

  function TFrameRolls.GetScore: integer;
  begin
  if not ownr.OpenFrame then
  Result := 10
  else
  Result := 3 + 4; // todo and 1 + 2 + 3 if 10th
  end;

  procedure TFrameRolls.RecordRoll(NumPins: integer);
  begin
  Inc(CurrentRoll);
  FrameRolls[CurrentRoll] := TRollTotal(NumPins);
  // todo: SetValue also Strike/Spare in ownr0

  if ownr.Number < 10 then
  begin
  if (ownr.Strike or ownr.Spare) or (CurrentRoll = 2) then
  Over := True;
  end
  else
  Over := (ownr.OpenFrame and (CurrentRoll = 2)) or
  ((not ownr.OpenFrame) and (CurrentRoll = 3));
  end;
*)

end.
