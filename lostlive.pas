unit LostLive;

{$mode objfpc}{$H+}

interface     {NIEDOKONCZONE}

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls;

type

  { TForm4 }

  TForm4 = class(TForm)
    Label1: TLabel;
    LostLiveTimer: TTimer;
    procedure FormShow(Sender: TObject);
    procedure LostLiveTimerTimer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form4: TForm4;

implementation

{$R *.lfm}

{ TForm4 }

procedure TForm4.LostLiveTimerTimer(Sender: TObject);
begin
  LostLiveTimer.Enabled := false;
  self.close;
end;

procedure TForm4.FormShow(Sender: TObject);
begin
  LostLiveTimer.Enabled := true;
end;

end.

