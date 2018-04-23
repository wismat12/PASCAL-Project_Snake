unit DialogHelp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs;

type

  TForm3 = class(TForm)
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    helpImg: TPortableNetworkGraphic;
  public
    { public declarations }
  end;

var
  Form3: TForm3;

implementation

{$R *.lfm}

procedure TForm3.FormCreate(Sender: TObject);
begin
  helpImg := TPortableNetworkGraphic.Create();
  helpImg.LoadFromFile('images\helpImg.png');
end;

procedure TForm3.FormPaint(Sender: TObject);
begin
  self.Canvas.Draw(0, 0, helpImg);
end;

end.

