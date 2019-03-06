unit BowlingGame;

interface

uses
  Generics.Collections;

type
  TRollTotal = (Zero, One, Two, Three, Four, Five, Six, Seven, Eight, Nine, Ten,
    Strike, Spare);

  TFrameRolls = array [1 .. 3] of TRollTotal;

  TFrame = class;

  TFrameRollsCtrl = class
    ownr: TFrame;
    Over: Boolean;
    FrameRolls: TFrameRolls;
    CurrentRoll: Integer;
  private
    function GetScore: Integer;
  public
    property Score: Integer read GetScore;
    procedure RecordRoll(NumPins: Integer);
    constructor Create(xownr: TFrame);
    destructor Destroy(); override;
  end;

  TLastTwoRolls = record
    Roll1, Roll2: Integer;
  end;

  TFrame = class
  private
    FCurrentRoll: Integer;
    procedure SetCurrentRoll(const Value: Integer);
    function GetOpenFrame: Boolean;
    // procedure SetIsScoreableByItself(const Value: Boolean);
  public
    Strike, Spare: Boolean;
    FrameRollsCtrl: TFrameRollsCtrl;

    // over, see FRC
    Number: Integer;
    Score: Integer;
    Scored: Boolean;
    RunningTotal: Integer;
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
    CurrentFrame: Integer;
  public
    function GetCurrent(): TFrame;
    procedure Next();
    procedure Init();
  end;


  TPendingScoreFrame = class
    FrameNum: Integer;
//    FrameRegScore: Integer;
    Bonus1,
    Bonus2: Integer;
  private
    function GetIsStrike: Integer;
    property IsStrike: Integer read GetIsStrike;
    function ReadyToScore(): Boolean;
    procedure Score();
  end;

  TPendingFrames = class
    Frames: TList<TPendingScoreFrame>;
  private
    function Any(): Boolean;
    procedure Add(FrameNum: Integer);
    procedure AddBonusPoints(pts: Integer);
    procedure Score();
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

procedure TBowlingGame.Roll(NumOfPins: Integer);
var
  frame: TFrame;
begin
  frame := FramesCtrl.GetCurrent();
  frame.FrameRollsCtrl.RecordRoll(NumOfPins); // over < 10

  if ScoreCtrl.Pending.Any then
    ScoreCtrl.Pending.AddBonusPoints(NumOfPins);

  ScoreCtrl.Score(); // current and pending

  if frame.FrameRollsCtrl.Over then
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

constructor TFrameRollsCtrl.Create(xownr: TFrame);
begin

end;

destructor TFrameRollsCtrl.Destroy;
begin

  inherited;
end;

function TFrameRollsCtrl.GetScore: Integer;
begin
  if not ownr.OpenFrame then
    Result := 10
  else
    Result := 3 + 4; // todo and 1 + 2 + 3 if 10th
end;

procedure TFrameRollsCtrl.RecordRoll(NumPins: Integer);
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
    Over := (ownr.OpenFrame and (CurrentRoll = 2)) or ((not ownr.OpenFrame) and (CurrentRoll = 3));
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
  i: Integer;
begin
  frame := FramesCtrl.GetCurrent();

  //process pending score
  if Pending.Any then begin
    for i := 0 to Pending.Frames.Count - 1 do
    begin
      if Pending.Frames[i].ReadyToScore() then
        Pending.Frames[i].Score();
    end;
  end;

  if frame.FrameRollsCtrl.Over then
  begin
    if (not frame.NeedRollsRecordedInFutureFrame()) then
      frame.Score := frame.FrameRollsCtrl.GetScore()
    else
      Pending.Add(frame.Number);
  end
  else begin
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

function TPendingScoreFrame.GetIsStrike: Integer;
begin
//  Result := fr
end;

function TPendingScoreFrame.ReadyToScore: Boolean;
begin

end;

procedure TPendingScoreFrame.Score;
begin

end;

{ TPendingFrames }

procedure TPendingFrames.Add(FrameNum: Integer);
begin

end;

procedure TPendingFrames.AddBonusPoints(pts: Integer);
begin

end;

function TPendingFrames.Any: Boolean;
begin
  Result := Frames.Count > 0;
end;

procedure TPendingFrames.Score;
begin

end;

end.
