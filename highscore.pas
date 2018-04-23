unit HighScore;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, laz2_XMLRead, laz2_XMLWrite, laz2_DOM;

type

  THighScore = class
    private
    public
    Name, Date: String;
    Score:  Integer;
    constructor Create(nname, ddate: string; sscore: Integer);
  end;

  THighScoreArray = array of THighScore;

  TStringArray = array of string;

  THighScoreClass = class
    private
    function PrepareHighScoreRow(hsScores, hsDate, hsName: string): string;
    public
    function LoadHighScores(): THighScoreArray;
    function LoadHighScoresStringArray(): TStringArray;
    procedure SaveHighScores(NewHighScoreArray: array of THighScore);
  end;

implementation

constructor THighScore.Create(nname, ddate: string; sscore: Integer);
begin
  Name := nname;
  Date := ddate;
  Score := sscore;
end;

function THighScoreClass.LoadHighScores(): THighScoreArray;
var
    HighScoresArray: array[0..9] of THighScore;
    Doc: TXMLDocument;
    highscores: TDOMNode;
    highscore: TDOMNode;
    hsDate, hsName, hsScores: string;
    counter, hsScoresInt: integer;
begin
    try
      ReadXMLFile(Doc, 'HighScores.xml');

     // highscores := Doc.DocumentElement.FindNode('HIGHSCORES');
     highscores := Doc.FindNode('HIGHSCORES');
      counter := 0;
      repeat
        highscore := highscores.FindNode('HS' + IntToStr(counter));
        if(highscore <> nil) then
        begin
          hsDate := highscore.FindNode('DATE').TextContent;
          hsName := highscore.FindNode('NAME').TextContent;
          hsScores := highscore.FindNode('SCORES').TextContent;
          hsScoresInt := StrToInt(hsScores);

          //highscoreArray[counter] := hsScores + ' ' + hsDate + ' ' + hsName;
          //highscoreArray[counter] := PrepareHighScoreRow(hsScores, hsDate, hsName);
          HighScoresArray[counter] := THighScore.Create(hsName, hsDate, hsScoresInt);

          counter := counter + 1;
        end;
      until highscore = nil;

      Result := HighScoresArray;

    finally
      Doc.Free;
    end;
end;

procedure THighScoreClass.SaveHighScores(NewHighScoreArray: array of THighScore);
var
    Doc: TXMLDocument;
    RootNode, Node, NameNode, DateNode, ScoreNode: TDOMNode;
    i: Integer;
begin
  try
    Doc := TXMLDocument.Create;

    RootNode := Doc.CreateElement('HIGHSCORES');
    Doc.Appendchild(RootNode);

    for i := 0 to 9 do
    begin
      Node := Doc.CreateElement('HS' + IntToStr(i));

      NameNode := Doc.CreateElement('NAME');
      TDOMElement(NameNode).TextContent := NewHighScoreArray[i].Name;
      Node.Appendchild(NameNode);

      DateNode := Doc.CreateElement('DATE');
      TDOMElement(DateNode).TextContent := NewHighScoreArray[i].Date;
      Node.Appendchild(DateNode);

      ScoreNode := Doc.CreateElement('SCORES');
      TDOMElement(ScoreNode).TextContent := IntToStr(NewHighScoreArray[i].Score);
      Node.Appendchild(ScoreNode);

      RootNode.Appendchild(Node);
    end;

    WriteXMLFile(Doc, 'HighScores.xml');

    finally
      Doc.Free;
    end;
end;

function THighScoreClass.PrepareHighScoreRow(hsScores, hsDate, hsName: string): string;
var newRow: string;
    i: integer;
begin
  // 16 __ 4 __ 10
  newRow := hsName;
  for i := Length(hsName) to 16 + 1 do
  begin
    newRow := newRow + ' ';
  end;

  newRow := newRow + hsScores;
  for i := Length(hsScores) to 4 + 1 do
  begin
    newRow := newRow + ' ';
  end;

  newRow := newRow + hsDate;
  for i := Length(hsDate) to 10 do
  begin
    newRow := newRow + ' ';
  end;

  Result := newRow;
end;

function THighScoreClass.LoadHighScoresStringArray(): TStringArray;
var
    HighScoresArray: array[0..9] of string;
    Doc: TXMLDocument;
    highscores: TDOMNode;
    highscore: TDOMNode;
    hsDate, hsName, hsScores: string;
    counter: integer;
begin
    try
      ReadXMLFile(Doc, 'HighScores.xml');

     // highscores := Doc.DocumentElement.FindNode('HIGHSCORES');
     highscores := Doc.FindNode('HIGHSCORES');
      counter := 0;
      repeat
        highscore := highscores.FindNode('HS' + IntToStr(counter));
        if(highscore <> nil) then
        begin
          hsDate := highscore.FindNode('DATE').TextContent;
          hsName := highscore.FindNode('NAME').TextContent;
          hsScores := highscore.FindNode('SCORES').TextContent;

          //highscoreArray[counter] := hsScores + ' ' + hsDate + ' ' + hsName;
          HighScoresArray[counter] := PrepareHighScoreRow(hsScores, hsDate, hsName);

          counter := counter + 1;
        end;
      until highscore = nil;

      Result := HighScoresArray;

    finally
      Doc.Free;
    end;
end;

end.

