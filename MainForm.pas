unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    btnClose: TButton;
    btnScoreSheet: TButton;
    procedure btnCloseClick(Sender: TObject);
    procedure btnScoreSheetClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  BowlingGame, Generics.Collections, ScoreSheet;

{$R *.dfm}

procedure TForm1.btnCloseClick(Sender: TObject);
begin
  Close();
end;

procedure TForm1.btnScoreSheetClick(Sender: TObject);
var
  frm: TScoreSheetForm;
begin
  frm := TScoreSheetForm.Create(nil);
  frm.ShowModal();
  frm.Free();
end;

end.
