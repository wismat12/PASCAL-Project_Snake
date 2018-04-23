unit Level;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, Wall, Fruit, Option, laz2_XMLRead, laz2_DOM;

type
  TLevel = class
    private
    public
    StartPoints, StartLives, LevelNumber: Integer;
    MaxWall, MaxFruit: Integer;

    Walls : array of TWall;
    Fruits : array of TFruit;
    StartSpeed, StartLength, StartLocationX, StartLocationY : Integer;
    StartMoveDirection : string; // up down left right
    InitialDrawDirection: string; // up down left right
    BackgroundPath: string;


    constructor Create(LevNum, SPoints, SSpeed, SLength, SLocationX, SLocationY, mmaxWall, mmaxFruit, SMoveDirection, InitDrawDirection, BkgPath: string; wwalls: array of TWall; ffruits: array of TFruit);
    function LoadLevel(levNumToLoad: integer; Option: TOption): TLevel;
end;

implementation

  constructor TLevel.Create(LevNum, SPoints, SSpeed, SLength, SLocationX, SLocationY, mmaxWall, mmaxFruit, SMoveDirection, InitDrawDirection, BkgPath: string; wwalls: array of TWall; ffruits: array of TFruit);
  var wallArrayLen, fruitArrayLen, i: integer;
  begin
    LevelNumber := StrToInt(LevNum);
    StartPoints := StrToInt(SPoints);

    StartSpeed := StrToInt(SSpeed); // speed is in ms - Timer1 tick
    StartLength := StrToInt(SLength);
    StartLocationX := StrToInt(SLocationX); //27;
    StartLocationY := StrToInt(SLocationY); //29;
    StartMoveDirection := SMoveDirection;
    InitialDrawDirection := InitDrawDirection;
    BackgroundPath := BkgPath;

    MaxWall := StrToInt(mmaxWall);
    MaxFruit := StrToInt(mmaxFruit);
    SetLength(Walls, MaxWall);
    SetLength(Fruits, MaxFruit);

    wallArrayLen := Length(wwalls);
    fruitArrayLen := Length(ffruits);

    for i:= 0 to wallArrayLen - 1 do
    begin
      Walls[i] := wwalls[i]; //TWall.Create(wwalls[i].X, wwalls[i].Y, wwalls[i].Pic.);
    end;

    for i:= 0 to fruitArrayLen - 1 do
    begin
      Fruits[i] := ffruits[i]; //TFruit.Create(ffruits[i].Kind, ffruits[i].Visible, ffruits[i].DisplaytimeLeft, ffruits[i].StartMaxX, ffruits[i].StartMaxY, ffruits[i].X, ffruits[i].Y, ffruits[i].Pic);
    end;

  end;

  function TLevel.LoadLevel(levNumToLoad: integer; Option: TOption): TLevel;
    var
      Doc: TXMLDocument;
      LevNum, SPoints, SSpeed, SLength, SLocationX, SLocationY, mmaxWall, mmaxFruit, SMoveDirection, InitDrawDirection, BkgPath: string;
      wwalls: TDOMNode;
      wwall: TDOMNode;
      wwwalls: array of TWall;
      wwwallX, wwwallY, wwwallPic: string;
      ffruits: TDOMNode;
      ffruit: TDOMNode;
      fffruits: array of TFruit;
      fffruitX, fffruitY, fffruitPic, fffruitKind, fffruitVisible, fffruitDisplayTimeLeft: string;
      counter: integer;
    begin
    try
      ReadXMLFile(Doc, 'levels\level' + IntToStr(levNumToLoad) + '.xml');

      LevNum := Doc.DocumentElement.FindNode('LevelNumber').TextContent;
      SPoints := Doc.DocumentElement.FindNode('StartPoints').TextContent;
      SSpeed := Doc.DocumentElement.FindNode('StartSpeed').TextContent;
      SLength := Doc.DocumentElement.FindNode('StartLength').TextContent;
      SLocationX := Doc.DocumentElement.FindNode('StartLocationX').TextContent;
      SLocationY := Doc.DocumentElement.FindNode('StartLocationY').TextContent;
      SMoveDirection := Doc.DocumentElement.FindNode('StartMoveDirection').TextContent;
      InitDrawDirection := Doc.DocumentElement.FindNode('InitialDrawDirection').TextContent;
      mmaxWall := Doc.DocumentElement.FindNode('MaxWall').TextContent;
      mmaxFruit := Doc.DocumentElement.FindNode('MaxFruit').TextContent;
      BkgPath := Doc.DocumentElement.FindNode('BackgroundPath').TextContent;

      wwalls := Doc.DocumentElement.FindNode('Walls');
      counter := 0;
      repeat
        wwall := wwalls.FindNode('Wall' + IntToStr(counter));
        if(wwall <> nil) then
        begin
          wwwallX := wwall.FindNode('X').TextContent;
          wwwallY := wwall.FindNode('Y').TextContent;
          wwwallPic := wwall.FindNode('Pic').TextContent;

          counter := counter + 1;
          SetLength(wwwalls, counter);
          wwwalls[counter - 1] := TWall.Create(StrToInt(wwwallX), StrToInt(wwwallY), wwwallPic);
        end;
      until wwall = nil;

      ffruits := Doc.DocumentElement.FindNode('Fruits');
      counter := 0;
      repeat
        ffruit := ffruits.FindNode('Fruit' + IntToStr(counter));
        if(ffruit <> nil) then
        begin
          fffruitX := ffruit.FindNode('X').TextContent;
          fffruitY := ffruit.FindNode('Y').TextContent;
          fffruitPic := ffruit.FindNode('Pic').TextContent;
          fffruitKind := ffruit.FindNode('Kind').TextContent;
          fffruitVisible := ffruit.FindNode('visible').TextContent;
          fffruitDisplayTimeLeft := ffruit.FindNode('DispTimeLeft').TextContent;

          counter := counter + 1;
          SetLength(fffruits, counter);
          fffruits[counter - 1] := TFruit.Create(fffruitKind, StrToBool(fffruitVisible), StrToInt(fffruitDisplayTimeLeft), Option.BoardWidth, Option.BoardHeight, StrToInt(fffruitX), StrToInt(fffruitY), fffruitPic);
        end;
      until ffruit = nil;

      Result := TLevel.Create(LevNum, SPoints, SSpeed, SLength, SLocationX, SLocationY, mmaxWall, mmaxFruit, SMoveDirection, InitDrawDirection, BkgPath, wwwalls, fffruits);

    finally
      Doc.Free;
    end;
  end;

end.

