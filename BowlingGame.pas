unit BowlingGame;

interface

uses
  Generics.Collections;

type
  TRollTotal = (rtZero, rtOne, rtTwo, rtThree, rtFour, rtFive, rtSix, rtSeven,
    rtEight, rtNine, rtTen, rtStrike, rtSpare); // todo: never 10?

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
    CurrentRoll: integer;
  private
    function GetScore: integer;
    function NumPinsToRollTotal(NumPins: integer): TRollTotal;
    function RollTotalToNumberOfPins(RollTotal: TRollTotal): integer;
  public
    property Score: integer read GetScore;
    procedure RecordRoll(NumPins: integer);
    constructor Create(xownr: TFrame);
    destructor Destroy(); override;
  end;

  TFrame = class
  private
    FCurrentRoll: integer;
    procedure SetCurrentRoll(const Value: integer);
    function GetOpenFrame: Boolean;
    // procedure SetIsScoreableByItself(const Value: Boolean);
  public
    Strike, Spare: Boolean;
    FrameRollsCtrl: TFrameRollsCtrl;

    // over, see FRC
    Number: integer;
    Score: integer;
    Scored: Boolean;
    RunningTotal: integer;
    property OpenFrame: Boolean read GetOpenFrame;
    function NeedRollsRecordedInFutureFrame: Boolean;
    Constructor Create();
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
  Dialogs;

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
begin
  if GameOver then
  begin
    MessageDlg('Game Over', mtInformation, [mbOK], 0);
    Exit;
  end;

  frame := FramesCtrl.GetCurrent();
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

constructor TFrame.Create;
begin
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

function TFrame.GetOpenFrame: Boolean;
begin
  Result := not(Strike or Spare);
end;

procedure TFrame.SetCurrentRoll(const Value: integer);
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
  i: integer;
begin
  FillChar(self, SizeOf(self), 0);
  for i := 1 to 10 do
    Frames[i].Number := i;

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
end;

destructor TFrameRollsCtrl.Destroy;
begin

  inherited;
end;

function TFrameRollsCtrl.GetScore: integer;

  function SumFrameRolls(): integer;
  var
    i: integer;
  begin
    Result := 0;
    for i := 0 to FrameRolls.Count do
      Inc(Result, RollTotalToNumberOfPins(FrameRolls[i])); // todo:
  end;

begin
  (* todo:
    if frame.Number < 10 then
    begin
    if not frame.OpenFrame then
    Result := 10;
    Exit;
    end;
  *)

  Result := SumFrameRolls();; // todo and 1 + 2 + 3 if 10th
  (*
    if not ownr.OpenFrame then
    Result := 10
    else
    Result := SumFrameRolls();; // todo and 1 + 2 + 3 if 10th
  *)
end;

function TFrameRollsCtrl.NumPinsToRollTotal(NumPins: integer): TRollTotal;

// only 2nd roll can result in Spare
  function IsSpare(): Boolean;
  begin
    Result := (FrameRolls.Count = 1) and (NumPins <> 0) and
      (RollTotalToNumberOfPins(FrameRolls[0]) in [0 .. 9]) and
      (ord(FrameRolls[1]) + NumPins = 10);
  end;

  function IsStrike(): Boolean;
  begin
    Result := (not IsSpare()) and (NumPins = 10);
    (*
      if frame.Number < 10 then
      Result := (FrameRolls.Count = 0) and (NumPins = 10)
      else
      Result := (FrameRolls.Count = 0) and (NumPins = 10)

      // todo: bug 0, 10 = spare
    *)
  end;

begin
  if IsStrike() then // todo:
  begin
    Result := rtStrike;
    frame.Strike := True;
    Exit;
  end;

  if IsSpare() then
  begin
    Result := rtSpare;
    frame.Spare := True;
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
  Inc(CurrentRoll);
  FrameRolls[CurrentRoll] := NumPinsToRollTotal(NumPins);
  // TRollTotal(NumPins);
  // todo: SetValue also Strike/Spare in ownr0

  if frame.Number < 10 then
  begin
    if (frame.Strike or frame.Spare) or (CurrentRoll = 2) then
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
      frame.Score := frame.FrameRollsCtrl.GetScore()
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
  if FramesCtrl.Frames[FrameNum].Strike then
    Result := BonusPoints.Count = 2 // (Bonus1 > -1) and (Bonus2 > -1)
  else if FramesCtrl.Frames[FrameNum].Spare then
    Result := BonusPoints.Count = 1; // (Bonus1 > -1);
end;

procedure TPendingScoreFrame.Score;
var
  frame: TFrame;
begin
  frame := FramesCtrl.Frames[FrameNum];
  if frame.Strike then
    frame.Score := frame.FrameRollsCtrl.GetScore() + BonusPoints[0] +
      BonusPoints[1]
  else if frame.Spare then
    frame.Score := frame.FrameRollsCtrl.GetScore() + BonusPoints[0];
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
