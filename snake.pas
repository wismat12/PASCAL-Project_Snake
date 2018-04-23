unit Snake;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Tile, Option, Level, Wall, Fruit;

type
  TSnake = class
    private
      Option: TOption;
      Level: TLevel;
      MaxX, MaxY: integer;  // board size
      LastTailX, LastTailY : integer;
    public
      SnakeLength: integer; // including head
      Direction: string; // of movment
      Speed: integer; // in ms

      Body: array of TTile;

      headL, headR, headU, headD: TPortableNetworkGraphic;

      constructor Create(bbody: array of TTile; ooption: TOption; llevel: TLevel);
      procedure Draw(Canvas: TCanvas; ooption: TOption);
      function Move(): integer;
      procedure ChangeSnakeMoveDirection(ddirection: string);
      function IsSnakeColidedWithItself() : boolean;
      function IsSnakeColidedWithWalls(Walls: array of TWall; MaxWallIndex: integer ) : boolean;
      function IsSnakeColidedWithFruits(Fruits: array of TFruit; MaxFruitIndex: integer) : integer;
      function IsSnakeColidedWithFruit(Fruit: TFruit) : boolean;
      procedure IncLength();
      procedure DecLength();
end;

implementation

constructor TSnake.Create(bbody: array of TTile; ooption: TOption; llevel: TLevel);
var i: integer;
begin

  headL := TPortableNetworkGraphic.Create();
  headL.LoadFromFile('images\headL.png');
  headR := TPortableNetworkGraphic.Create();
  headR.LoadFromFile('images\headR.png');
  headU := TPortableNetworkGraphic.Create();
  headU.LoadFromFile('images\headU.png');
  headD := TPortableNetworkGraphic.Create();
  headD.LoadFromFile('images\headD.png');

  MaxX := ooption.BoardWidth; //sstartMaxX;
  MaxY := ooption.BoardHeight; //sstartMaxY;
  SetLength(Body, Length(bbody));
  for i := 0 to Length(bbody) - 1 do
  begin
    Body[i] := bbody[i];
  end;

  Option := ooption;
  Level := llevel;
  SnakeLength := Level.StartLength;
  Direction := Level.StartMoveDirection;

  LastTailX := -1;
  LastTailY := -1;
end;

procedure TSnake.Draw(Canvas: TCanvas; ooption: TOption);
  var i: integer;
  begin
    for i := 0 to SnakeLength - 1 do  // -1 <=  -1 ZeroBasedArrays
        begin
          Canvas.Draw(Body[i].X * ooption.TileWidth, Body[i].Y * ooption.TileHeight, Body[i].Pic);
        end;
  end;

function TSnake.Move() : integer;
  var i: integer;
  begin
    LastTailX := Body[SnakeLength - 1].X; // For IncLength fruit
    LastTailY := Body[SnakeLength - 1].Y;

    for i := SnakeLength - 1 downto 1 do
    begin
      Body[i].X := Body[i - 1].X;
      Body[i].Y := Body[i - 1].Y;
    end;

    if(Direction = 'left') then
    begin
      Body[0].X := Body[0].X - 1;
    end
    else if(Direction = 'right') then
    begin
      Body[0].X := Body[0].X + 1;
    end
    else if(Direction = 'up') then
    begin
      Body[0].Y := Body[0].Y - 1;
    end
    else if(Direction = 'down') then
    begin
      Body[0].Y := Body[0].Y + 1;
    end;

    // jezeli Option.PenetrateBorders = true to przeniesc glowe na druga strone
    if(Option.PenetrateBorders = true) then
    begin
      for i := 0 to SnakeLength - 1 do
      begin
        if(Body[i].X < 0) then   // rownoczesnie moze przekroczyc tylko jeden wymiar => else if
        begin
          Body[i].X := Body[i].X + Option.BoardWidth;
          Result := 5; // not colided with boaard borders
          exit;
        end
        else if(Body[i].X >= Option.BoardWidth) then
        begin
          Body[i].X := Body[i].X - Option.BoardWidth;
          Result := 5; // not colided with boaard borders
          exit;
        end
        else if(Body[i].Y < 0) then
        begin
          Body[i].Y := Body[i].Y + Option.BoardHeight;
          Result := 5; // not colided with boaard borders
          exit;
        end
        else if(Body[i].Y >= Option.BoardHeight) then
        begin
          Body[i].Y := Body[i].Y - Option.BoardHeight;
          Result := 5; // not colided with boaard borders
          exit;
        end
        else
        begin
          Result := 5;
          exit;
        end;
      end;
    end
    // jezeli Option.PenetrateBorders = false to skucie
    else
    begin
      for i := 0 to SnakeLength - 1 do
      begin
        if((Body[i].X < 0) or (Body[i].X >= Option.BoardWidth) or (Body[i].Y < 0) or (Body[i].Y >= Option.BoardHeight))then   // rownoczesnie moze przekroczyc tylko jeden wymiar => else if
        begin
          Result:= 2; // colided with boaard borders
          exit;
        end;
      end;
    end;
  end;

procedure TSnake.ChangeSnakeMoveDirection(ddirection: string);
begin
  Direction := ddirection;
  if(Direction = 'left') then
  begin
    Body[0].Pic:=headL;
  end;
  if(Direction = 'right') then
  begin
    Body[0].Pic:=headR;
  end;
  if(Direction = 'up') then
  begin
    Body[0].Pic:=headU;
  end;
  if(Direction = 'down') then
  begin
    Body[0].Pic:=headD;
  end;
end;

function TSnake.IsSnakeColidedWithItself() : boolean;
var i: integer;
begin
  for i := 1 to SnakeLength - 1 do
  begin
    if((Body[0].X = Body[i].X) and (Body[0].Y = Body[i].Y)) then
    begin
      IsSnakeColidedWithItself := true;
      exit;
    end;
  end;

  IsSnakeColidedWithItself := false;
end;

function TSnake.IsSnakeColidedWithWalls(Walls: array of TWall; MaxWallIndex: integer ) : boolean;
var i: integer;
begin
  for i := 0 to MaxWallIndex do
  begin
    if((Body[0].X = Walls[i].X) and (Body[0].Y = Walls[i].Y)) then
    begin
      IsSnakeColidedWithWalls := true;
      exit;
    end;
  end;

  IsSnakeColidedWithWalls := false;
end;

function TSnake.IsSnakeColidedWithFruits(Fruits: array of TFruit; MaxFruitIndex: integer) : integer;
var i: integer;
begin
  for i := 0 to MaxFruitIndex do
  begin
    if((Fruits[i].Visible = true) and (Fruits[i].X = Body[0].X) and (Fruits[i].Y = Body[0].Y)) then
    begin
      IsSnakeColidedWithFruits := i;
      exit;
    end;
  end;
  IsSnakeColidedWithFruits := -1;
end;

function TSnake.IsSnakeColidedWithFruit(Fruit: TFruit) : boolean;
var i: integer;
begin
  for i := 0 to SnakeLength - 1 do
  begin
    if((Body[i].X = Fruit.X) and (Body[i].Y = Fruit.Y)) then
    begin
      IsSnakeColidedWithFruit := true;
      exit;
    end;
  end;
  IsSnakeColidedWithFruit := false;
end;

procedure TSnake.IncLength();
begin
  SnakeLength := SnakeLength + 1;
  SetLength(Body, SnakeLength); // SnakeLength include head
  Body[SnakeLength - 1] := TTile.Create(LastTailX, LastTailY, 'Images\' + IntToStr((SnakeLength - 1) mod 10) + '.png');
end;

procedure TSnake.DecLength();
begin
  SnakeLength := SnakeLength - 1;
  SetLength(Body, SnakeLength); // SnakeLength include head
end;

end.

