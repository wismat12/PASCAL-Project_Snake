unit Game;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Dialogs,
  Tile, Option, Level, Snake, Fruit, HighScore, LostLive, BASS;

type

  { TGame }

  TGame = class
    private
    Points, Lives, GameLevel, SnakeSpeed, CollectedApples: integer;
    Background:  TPortableNetworkGraphic;
    HighScoreClass: THighScoreClass;
    public
    GameOverFlag: boolean;
    PauseFlag: boolean;
    NextLvl: boolean;
    LostLive: boolean;
    HighScore: boolean;
    EndOfTheGameFlag: boolean;
    InfoString: string;
    Option: TOption;
    Level: TLevel;
    Snake: TSnake;
    LostLiveForm: TForm4;
    MaxApples: Integer;
    BassMainHandle: Cardinal;
    BassBackgroundMusicHandle: Cardinal;
    BassAppleeffectHandle: Cardinal;

    constructor Create(newOptions: TOption);
    procedure NewGame(newOptions: TOption);
    procedure CreateSnake();
    procedure DrawGame(Canvas: TCanvas);
    procedure DrawWalls(Canvas: TCanvas);
    procedure DrawFruits(Canvas: TCanvas);
    procedure DrawSnake(Canvas: TCanvas);
    procedure SnakeMove();
    procedure ChangeSnakeMoveDirection(direction: string);
    procedure CheckColisions();
    function GetPoints() : integer;
    function GetLives() : integer;
    function GetLevel() : integer;
    function GetSnakeSpeed() : integer;
    function GetCollectedApples() : integer;
    procedure NextLevel();
    procedure FruitRefresh();
    procedure GameOver();
    procedure EndOfTheGame();
    procedure LiveDown();
    procedure SaveHighScores();
end;

implementation

constructor TGame.Create(newOptions: TOption);
begin
  NewGame(newOptions);
  HighScoreClass := THighScoreClass.Create();

  BASS_Init(1, 48000, 0, BassMainHandle, nil);
  BASS_Start;

  BassBackgroundMusicHandle := BASS_StreamCreateFile(False, pChar('music\background1.mp3'), 0, 0, 0);
  Bass_ChannelPlay(BassBackgroundMusicHandle, false);

  BassAppleeffectHandle := BASS_StreamCreateFile(False, pChar('music\apple.mp3'), 0, 0, 0);


end;

procedure TGame.NewGame(newOptions: TOption);
begin
    GameOverFlag := false;
    PauseFlag := false;
    Option := newOptions;
    Level := Level.LoadLevel(1, Option);
    self.MaxApples := Option.Target;
    self.Points := 0;

    self.Lives := Option.StartLivesAmount;
    self.SnakeSpeed := Level.StartSpeed;
    self.GameLevel := Level.LevelNumber;

    self.CollectedApples := 0;

    self.NextLvl := false;
    self.LostLive := false;
    self.HighScore:=false;

    Background := TPortableNetworkGraphic.Create();
    Background.LoadFromFile(Level.BackgroundPath);

    if (Option.PenetrateBorders = true) then  self.InfoString:= 'Borders are safe!'
       else self.InfoString := 'Borders are lethal!';

    CreateSnake();
end;

procedure TGame.CreateSnake();
var
    i: integer;
    body: array of TTile;
    bodyStepX, bodyStepY: integer;
begin
    if(Level.InitialDrawDirection = 'left') then
    begin
      bodyStepX := -1;
      bodyStepY := 0;
    end
    else if(Level.InitialDrawDirection = 'right') then
    begin
      bodyStepX := 1;
      bodyStepY := 0;
    end
    else if(Level.InitialDrawDirection = 'up') then
    begin
      bodyStepX := 0;
      bodyStepY := -1;
    end
    else if(Level.InitialDrawDirection = 'down') then
    begin
      bodyStepX := 0;
      bodyStepY := 1;
    end;

    SetLength(body, Level.StartLength);

      if(Level.InitialDrawDirection = 'left') then
      begin
        body[0] :=    TTile.Create(Level.StartLocationX, Level.StartLocationY, 'Images\headR.png');
      end;
      if(Level.InitialDrawDirection = 'right') then
      begin
        body[0] := TTile.Create(Level.StartLocationX, Level.StartLocationY, 'Images\headL.png');
      end;
      if(Level.InitialDrawDirection = 'up') then
      begin
        body[0] := TTile.Create(Level.StartLocationX, Level.StartLocationY, 'Images\headD.png');
      end;
      if(Level.InitialDrawDirection = 'down') then
      begin
        body[0] := TTile.Create(Level.StartLocationX, Level.StartLocationY, 'Images\headU.png');
      end;
    for i := 1 to Level.StartLength - 1 do
      begin
        body[i] := TTile.Create(Level.StartLocationX + (bodyStepX * i),
                                Level.StartLocationY + (bodyStepY * i),
                                'Images\' + IntToStr(i mod 10) + '.png');
      end;
    Snake := TSnake.Create(body, Option, Level);
end;

procedure TGame.DrawGame(Canvas: TCanvas);
begin
  Canvas.Draw(0, 0, self.Background);

  DrawWalls(Canvas);
  DrawFruits(Canvas);
  DrawSnake(Canvas);

end;

procedure TGame.DrawWalls(Canvas: TCanvas);
var i:integer;
begin
  for i := 0 to Level.MaxWall - 1 do
    begin
      Level.Walls[i].Draw(Canvas, Option);
    end;
end;

procedure TGame.DrawFruits(Canvas: TCanvas);
var i:integer;
begin
  for i := 0 to Level.MaxFruit - 1 do
    begin
      Level.Fruits[i].Draw(Canvas, Option);
    end;
end;

procedure TGame.DrawSnake(Canvas: TCanvas);
begin
  Snake.Draw(Canvas, Option);
end;

procedure TGame.SnakeMove();
var colisionWithBoarders: integer;
begin
    colisionWithBoarders := Snake.Move();
  if(colisionWithBoarders = 2) then
  begin
    self.InfoString := 'Borders!!!';
    self.Points := self.Points - 44;
    LiveDown();
  end;
end;

procedure TGame.ChangeSnakeMoveDirection(direction: string);
begin
  Snake.ChangeSnakeMoveDirection(direction);
end;

procedure TGame.CheckColisions();
var snakeColidedWithItself, snakeColidedWithWalls : boolean;
    fruitIndex: integer;
    colidedFruit: TFruit;
    visibleFruitLeft: boolean;
    i : integer;
begin
   snakeColidedWithItself := Snake.IsSnakeColidedWithItself();
   snakeColidedWithWalls := Snake.IsSnakeColidedWithWalls(Level.Walls, Level.MaxWall - 1);
   if(snakeColidedWithItself = true) then
   begin
     self.InfoString := 'Snake!!!';
     self.Points := self.Points - 30;
     LiveDown();
   end;
   if(snakeColidedWithWalls = true) then  begin
     self.InfoString := 'Obstacle!!!';
     self.Points := self.Points - 50;
     LiveDown();
   end;

   fruitIndex := Snake.IsSnakeColidedWithFruits(Level.Fruits, Level.MaxFruit - 1);
   if(fruitIndex >= 0) then
   begin
     colidedFruit := Level.Fruits[fruitIndex];
     colidedFruit.Visible := false;
     if(colidedFruit.Kind = 'IncPoints') then
     begin
       self.InfoString := 'Red Apple!';
       self.Points := self.Points + 3;
       Snake.IncLength();
       CollectedApples := CollectedApples + 1;
       Bass_ChannelPlay(BassAppleeffectHandle, false);
     end
     else if(colidedFruit.Kind = 'IncSpeed') then
     begin
       if(self.SnakeSpeed > 300) then
       begin
         self.InfoString := 'Yellow Apple!';
         self.SnakeSpeed := self.SnakeSpeed - 60; // SnakeTimer in ms
         self.Points := self.Points + 25;
         Bass_ChannelPlay(BassAppleeffectHandle, false);
       end;
     end
     else if(colidedFruit.Kind = 'IncLives') then
     begin
       self.InfoString := 'Life Apple!';
       self.Lives := self.Lives + 1;
       self.Points := self.Points + 10;
       Bass_ChannelPlay(BassAppleeffectHandle, false);
     end
     else if(colidedFruit.Kind = 'IncLength') then
     begin
       self.InfoString := 'Green Apple!';
       Snake.IncLength();
       self.Points := self.Points + 5;
       Bass_ChannelPlay(BassAppleeffectHandle, false);
     end
     else if(colidedFruit.Kind = 'DecPoints') then
     begin
       self.InfoString := 'Grapes!!!';
       self.Points := self.Points - 20;
     end
     else if(colidedFruit.Kind = 'DecSpeed') then
     begin
       self.InfoString := 'Onion!!!';
       self.SnakeSpeed := self.SnakeSpeed + 60; // SnakeTimer in ms
       self.Points := self.Points - 25;
     end
     else if(colidedFruit.Kind = 'DecLives') then
     begin
       self.InfoString := 'Skull!!!';
       if(self.Lives > 1) then
       begin
         LiveDown();
         self.Points := self.Points - 20;
       end;
     end
     else if(colidedFruit.Kind = 'DecLength') then
     begin
       self.InfoString := 'Grenade!!!';
       Snake.DecLength();
       self.Points := self.Points - 10;
     end;
     visibleFruitLeft := false;
     for i := 0 to Level.MaxFruit - 1 do
     begin
       if((Level.Fruits[i].Visible = true) and (Level.Fruits[i].Kind <> 'DecLives') and (Level.Fruits[i].Kind <> 'DecPoints') and (Level.Fruits[i].Kind <> 'DecSpeed') and (Level.Fruits[i].Kind <> 'DecLength')) then
       begin
         visibleFruitLeft := true;
         break;
       end;
     end;
     if(CollectedApples = MaxApples) then
     begin
       if(visibleFruitLeft = false) then begin
       self.InfoString := 'Additional 50 points!';
       self.Points := self.Points +50;
       end;
       if(self.GameLevel < 6) then
       begin
            NextLevel();
       end
       else
       begin
          EndOfTheGame();
       end;
     end;
   end;
end;

procedure TGame.NextLevel();
begin

  GameOverFlag := false;
  PauseFlag := true;
  NextLvL := true;

  self.GameLevel := self.GameLevel + 1;
  Level := Level.LoadLevel(self.GameLevel, Option);

  self.Points := self.Points + Level.StartPoints;
  self.Lives := self.Lives + Level.StartLives;
  self.SnakeSpeed := Level.StartSpeed;
  self.CollectedApples:=0;

  Background.LoadFromFile(Level.BackgroundPath);

  CreateSnake();
  Bass_ChannelPlay(BassBackgroundMusicHandle, false);
end;

procedure TGame.GameOver();
begin
  GameOverFlag := true;
  SaveHighScores();
end;

procedure TGame.EndOfTheGame();
begin

 // ShowMessage('Congratulations!!');
  self.PauseFlag:= True;
  SaveHighScores();

  EndOfTheGameFlag := true;
end;


procedure TGame.LiveDown();
begin
  self.Lives := self.Lives - 1;
  self.PauseFlag := True;
  CreateSnake();
  self.LostLive := true;

  if(self.Lives = 0) then
  begin
    GameOver();
  end
end;

procedure TGame.FruitRefresh();
var i, j: integer;
    colided: boolean;
begin
  for i := 0 to Level.MaxFruit - 1 do
  begin

    Level.Fruits[i].Refresh();

    repeat
      colided := false;

      for j := 0 to Level.MaxFruit - 1 do
      begin
        if((i <> j) and (Level.Fruits[i].X = Level.Fruits[j].X) and (Level.Fruits[i].Y = Level.Fruits[j].Y)) then
        begin
          colided := true;
        end;
      end;

      for j := 0 to Level.MaxWall - 1 do
      begin
        if((Level.Fruits[i].X = Level.Walls[j].X) and (Level.Fruits[i].Y = Level.Walls[j].Y)) then
        begin
          colided := true;
        end;
      end;

      if(Snake.IsSnakeColidedWithFruit(Level.Fruits[i])) then
      begin
        colided := true;
      end;

      if(colided = true) then
      begin
           Level.Fruits[i].RefreshPosotion();
      end;

    until colided = false;

  end;
end;

function TGame.GetPoints() : integer;
begin
  Result := self.Points;
end;

function TGame.GetLives() : integer;
begin
  Result := self.Lives;
end;

function TGame.GetLevel() : integer;
begin
  Result := self.GameLevel;
end;

function TGame.GetSnakeSpeed() : integer;
begin
  Result := self.SnakeSpeed;
end;

function TGame.GetCollectedApples: integer;
begin
  Result := self.CollectedApples;
end;



procedure TGame.SaveHighScores();
var highScores: THighScoreArray;
    i, posToInsert: integer;
    NewHighScore: THighScore;
    NewHighScoreArray: array[0..9] of THighScore;
    userName: string;
begin
  posToInsert := -1;
  // wczytać highscores
  highScores := HighScoreClass.LoadHighScores();
  // znaleźc pozycję na ktorej można dodać
  for i := 0 to 9 do
  begin
    if(highScores[i].Score < self.Points) then
    begin
      posToInsert := i;
      Break;
    end;
  end;

  if(posToInsert > -1) then
  begin
    // dodac wiersz - pamiętac o dlugosci poszczegolnych itemow

    userName := InputBox('High Scores', 'Enter your name', '');

    userName := LeftStr(userName, 16);

    NewHighScore := THighScore.Create(userName, DateToStr(Date), self.Points);
    for i := 0 to posToInsert - 1 do
    begin
      NewHighScoreArray[i] := highScores[i];
    end;
    NewHighScoreArray[posToInsert] :=  NewHighScore;
    for i := posToInsert + 1 to 9 do
    begin
      NewHighScoreArray[i] := highScores[i - 1];
    end;
    // zapisac xml
    HighScoreClass.SaveHighScores(NewHighScoreArray);
  end;
end;

end.

