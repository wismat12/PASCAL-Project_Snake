unit Main;

{$mode objfpc}{$H+}
{Mateusz Wisniewski Poniedzialek 17:15 284660}
interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, Game, Menu, Option;

type

  { TForm1 }

  TForm1 = class(TForm)
    lblGameInfo: TLabel;
    lblInfo2: TLabel;
    lblInfo1: TLabel;
    lblTarget: TLabel;
    lblLevel: TLabel;
    lblPause: TLabel;
    lblPoints: TLabel;
    lblLives: TLabel;
    lblSnakeTimer: TLabel;
    PaintBox1: TPaintBox;
    GameTimer: TTimer;
    SnakeTimer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormPaint(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure GameTimerTimer(Sender: TObject);
    procedure SnakeTimerTimer(Sender: TObject);
  private
    procedure RefrehGameInfo();
    procedure SetPause(state: boolean);
    procedure ShowMenu();
    procedure SetFormWidthAndHeight(newOptions: TOption);
  public
    Game: TGame;
    MenuForm: TForm2;
    bg_main: TPortableNetworkGraphic;
  end;

var
  Form1: TForm1;
  Options: TOption;

implementation
{$R *.lfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
     self.Visible:=false;
     BoundsRect := Screen.MonitorFromWindow(Handle).BoundsRect;

     Randomize();
     Form1.DoubleBuffered := True;
     form1.KeyPreview := True;

     Options := TOption.Create();
     Options := Options.LoadOptions();

     bg_main := TPortableNetworkGraphic.Create();
     bg_main.LoadFromFile('images\bg_main.png');

     Game := TGame.Create(Options);
     SetFormWidthAndHeight(Options);

     GameTimer.Enabled := false;
     SnakeTimer.Enabled := false;

     // stop plaing
     // BASS_ChannelStop(BassBackgroundMusicHandle);

     lblPoints.Left := 10;
     lblPoints.Top := 10;

     lblLives.Left := lblPoints.Left + lblPoints.Width + 10;
     lblLives.Top:= lblPoints.Top;

     lblLevel.Left:= lblLives.Left + lblLives.Width + 10;
     lblLevel.Top:= lblLives.Top;

     lblTarget.Top:=10;
     lblTarget.Left:= Trunc(Form1.Width / 2.0 - lblTarget.Width / 2.0);

     lblSnakeTimer.Top:=10;
     lblSnakeTimer.Left:= Form1.Width - 10 - lblSnakeTimer.Width;

     LblPause.Left:=trunc(Form1.Width / 2.0 - lblPause.Width/ 2.0);
     LblPause.top:= Form1.Height - 10 - LblPause.Height;

     lblInfo1.Top:= Form1.Height - 10 - lblInfo1.Height;
     lblInfo1.Left:= 10;

     lblInfo2.Top:= Form1.Height - 10 - lblInfo2.Height;
     lblInfo2.Left:= Form1.Width - 10 - lblInfo2.Width;

     lblGameInfo.Top := lblPoints.Top + lblPoints.Height + 10;
     lblGameInfo.Left := 10;


     ShowMenu();
end;

procedure TForm1.SetFormWidthAndHeight(newOptions: TOption);
begin
  Form1.PaintBox1.Width := newOptions.TileWidth * newOptions.BoardWidth;
  Form1.PaintBox1.Height := newOptions.TileHeight * newOptions.BoardHeight;
  Form1.PaintBox1.Left := Trunc(Form1.Width / 2.0 - Form1.PaintBox1.Width / 2.0);
  Form1.PaintBox1.Top := Trunc(Form1.Height / 2.0 - Form1.PaintBox1.Height / 2.0);
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if(Key = 37) then   // GetKeyState(VK_LEFT)
  begin
    Game.ChangeSnakeMoveDirection('left');
  end
  else if(Key = 38) then   // GetKeyState(VK_UP)
  begin
    Game.ChangeSnakeMoveDirection('up');
  end
  else if(Key = 39) then   // GetKeyState(VK_Right)
  begin
    Game.ChangeSnakeMoveDirection('right');
  end
  else if(Key = 40) then   // GetKeyState(VK_DOWN)
  begin
    Game.ChangeSnakeMoveDirection('down');
  end
  else if((Key = ord('P')) and (Game.GameOverFlag = false)) then
  begin
    SetPause(not(Game.PauseFlag));
    Game.NextLvl := false;
    Game.LostLive:= false;
  end
  else if(Key = ord('N')) then
  begin

    Game.NewGame(Options);
    GameTimer.Enabled := true;
    SnakeTimer.Enabled := true;
    Game.PauseFlag := false;
  end

  else if(Key = 112) then     // F1
  begin
    SetPause(true);
    ShowMenu();
  end
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
     Form1.Canvas.Draw(0,0,bg_main);
end;

procedure TForm1.PaintBox1Paint(Sender: TObject);
begin
   Game.DrawGame(Form1.PaintBox1.Canvas);
end;

procedure TForm1.GameTimerTimer(Sender: TObject);
begin
   Form1.PaintBox1.Repaint();

   if(Game.PauseFlag = true) then
   begin
     SetPause(true);
     if (Game.NextLvl = true) then begin
     lblTarget.Caption := 'Congratulations! Next LVL! ';
     lblTarget.Left:= Trunc(Form1.Width / 2.0 - lblTarget.Width / 2.0);
     end;
     if(Game.LostLive = true) then begin
     lblTarget.Caption := 'Be Careful!';
     lblTarget.Left:= Trunc(Form1.Width / 2.0 - lblTarget.Width / 2.0);
     end;
   end;

   if(Game.GameOverFlag = true) then
   begin
     lblTarget.Caption := ' GAME OVER ';
      SetPause(true);
     GameTimer.Enabled:=false;
     ShowMenu();
   end;

   if(Game.EndOfTheGameFlag = true) then
   begin
     lblTarget.Caption := 'Congratulations!!!Game is finished';
     lblTarget.Left:= Trunc(Form1.Width / 2.0 - lblTarget.Width / 2.0);
     SetPause(true);
     GameTimer.Enabled:=false;
     ShowMenu();
   end;

   SnakeTimer.Interval := Game.GetSnakeSpeed();

   if(Game.PauseFlag = false) then
   begin
     Game.FruitRefresh();
   end;
end;

procedure TForm1.SnakeTimerTimer(Sender: TObject); // nigdy nie może byc szybszy niż GameTimer
begin
  Game.SnakeMove();
  Game.CheckColisions();
  Form1.RefrehGameInfo();
end;

procedure TForm1.RefrehGameInfo();
var points, lives, level, apples: integer;
begin
  points := Game.GetPoints();
  lives := Game.GetLives();
  level := Game.GetLevel();
  apples := Game.GetCollectedApples();

  lblPoints.Caption := 'Points: ' + IntToStr(points);
  lblLives.Caption := 'Lives: ' + IntToStr(lives);
  lblLevel.Caption := 'Level: ' + IntToStr(level);
  if(Game.LostLive <> true) then lblTarget.Caption := 'Target: ' + IntToStr(Game.MaxApples - apples) + ' Red Apples';
  lblSnakeTimer.Caption := 'Snake Timer ' + IntToStr(SnakeTimer.Interval);
     lblGameInfo.Caption := Game.InfoString;
     lblPoints.Left := 10;
     lblPoints.Top := 10;
     lblLives.Left := lblPoints.Left + lblPoints.Width + 10;
     lblLives.Top:= lblPoints.Top;
     lblLevel.Left:= lblLives.Left + lblLives.Width + 10;
     lblLevel.Top:= lblLives.Top;
     lblTarget.Top:=10;
     lblTarget.Left:= Trunc(Form1.Width / 2.0 - lblTarget.Width / 2.0);
     lblSnakeTimer.Top:=10;
     lblSnakeTimer.Left:= Form1.Width - 10 - lblSnakeTimer.Width;
     lblTarget.Left:= Trunc(Form1.Width / 2.0 - lblTarget.Width / 2.0);
end;

procedure TForm1.SetPause(state: boolean);
begin
  Game.PauseFlag := state;
  SnakeTimer.Enabled := not(state);
  lblPause.Visible := state;
end;

procedure TForm1.ShowMenu();
var newOptions: TOption;
begin
  self.MenuForm := TForm2.Create(Form1);
  newOptions := self.MenuForm.Execute();
  self.Visible:=true;
  if(self.MenuForm.ExitGame = true) then
  begin
    //self.Close();
    application.terminate;
    Exit;
  end;
  if( newOptions <> nil ) then
  begin
    Game.NewGame(newOptions);
    SetFormWidthAndHeight(newOptions);
  end;
  SetPause(false);
  GameTimer.Enabled := true;
  Game.EndOfTheGameFlag := false;

end;

end.

