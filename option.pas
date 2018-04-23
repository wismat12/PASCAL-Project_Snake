unit Option;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, laz2_XMLRead, laz2_XMLWrite, laz2_DOM;

type
  TOption = class
    private
    public
    Target: integer;
    PenetrateBorders: boolean;
    StartLivesAmount: integer;
    TileWidth: integer;
    TileHeight: integer;
    BoardWidth: integer;
    BoardHeight: integer;
    constructor Create();
    function LoadOptions(): TOption;
    procedure SaveOptions(o: TOption);
end;

implementation

  constructor TOption.Create(); // default values - real will be read from file
  begin
    Target := 10;
    PenetrateBorders := true;
    StartLivesAmount := 3;
    TileWidth := 24;
    TileHeight := 24;
    BoardWidth := 43;
    BoardHeight := 28;
  end;

  function TOption.LoadOptions(): TOption;
  var
      Doc: TXMLDocument;
      ttarget, ppenetrateBoarders, sstartLivesAmount, bboardWidth, bboardHeight, ttileWidth, ttileHeight: string;
      o: TOption;
  begin
    try
       ReadXMLFile(Doc, 'Options.xml');
       ttarget := Doc.DocumentElement.FindNode('Target').TextContent;
       ppenetrateBoarders := Doc.DocumentElement.FindNode('PenetrateBoarders').TextContent;
       sstartLivesAmount := Doc.DocumentElement.FindNode('StartLivesAmount').TextContent;
       bboardWidth := Doc.DocumentElement.FindNode('BoardWidth').TextContent;
       bboardHeight := Doc.DocumentElement.FindNode('BoardHeight').TextContent;
       ttileWidth := Doc.DocumentElement.FindNode('TileWidth').TextContent;
       ttileHeight := Doc.DocumentElement.FindNode('TileHeight').TextContent;

       o := TOption.Create();

       o.Target := StrToInt(ttarget);
       o.PenetrateBorders := StrToBool(ppenetrateBoarders);
       o.StartLivesAmount := StrToInt(sstartLivesAmount);
       o.BoardWidth := StrToInt(bboardWidth);
       o.BoardHeight := StrToInt(bboardHeight);
       o.TileWidth := StrToInt(ttileWidth);
       o.TileHeight := StrToInt(ttileHeight);

       Result := o;
      finally
        Doc.Free;
    end;
  end;

  procedure TOption.SaveOptions(o: TOption);
  var
      Doc: TXMLDocument;
      RootNode, Node: TDOMNode;
  begin
    try
      Doc := TXMLDocument.Create;

      RootNode := Doc.CreateElement('OPTIONS');
      Doc.Appendchild(RootNode);

      Node := Doc.CreateElement('Target');
      TDOMElement(Node).TextContent := IntToStr(o.Target);
      RootNode.Appendchild(Node);

      Node := Doc.CreateElement('PenetrateBoarders');
      TDOMElement(Node).TextContent := BoolToStr(o.PenetrateBorders, true);
      RootNode.Appendchild(Node);

      Node := Doc.CreateElement('StartLivesAmount');
      TDOMElement(Node).TextContent := IntToStr(o.StartLivesAmount);
      RootNode.Appendchild(Node);

      Node := Doc.CreateElement('BoardWidth');
      TDOMElement(Node).TextContent := IntToStr(o.BoardWidth);
      RootNode.Appendchild(Node);

      Node := Doc.CreateElement('BoardHeight');
      TDOMElement(Node).TextContent := IntToStr(o.BoardHeight);
      RootNode.Appendchild(Node);

      Node := Doc.CreateElement('TileWidth');
      TDOMElement(Node).TextContent := IntToStr(o.TileWidth);
      RootNode.Appendchild(Node);

      Node := Doc.CreateElement('TileHeight');
      TDOMElement(Node).TextContent := IntToStr(o.TileHeight);
      RootNode.Appendchild(Node);

      WriteXMLFile(Doc, 'Options.xml');

      finally
        Doc.Free;
      end;
  end;

end.

