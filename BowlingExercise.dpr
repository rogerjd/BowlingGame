program BowlingExercise;

uses
  Vcl.Forms,
  MainForm in 'MainForm.pas' {Form1} ,
  BowlingGame in 'BowlingGame.pas',
  ScoreSheet in 'ScoreSheet.pas' {Form2};

{$R *.res}

begin
  ReportMemoryLeaksOnShutdown := DebugHook <> 0;
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;

end.
