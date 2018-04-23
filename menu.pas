unit Menu;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls, Option, laz2_XMLRead, laz2_XMLWrite, laz2_DOM, HighScore, DialogHelp;

type

  { TForm2 }

  TForm2 = class(TForm)
    btnHelp: TButton;
    btnExit: TButton;
    btnSave: TButton;
    btnCancel: TButton;
    cbAllowPassingThroughBoarders: TCheckBox;
    ebStartLivesAmount: TEdit;
    ebBoardWidth: TEdit;
    ebBoardHeight: TEdit;
    ebTileWidth: TEdit;
    ebTileHeight: TEdit;
    ebTarget: TEdit;
    LblSnake: TLabel;
    LblTarget: TLabel;
    lblStartLivesAmount: TLabel;
    lblHighScores: TLabel;
    lbHighScores: TListBox;
    lblBoardWidth: TLabel;
    lblBoardHeight: TLabel;
    lblTileWidth: TLabel;
    lblTileHeight: TLabel;
    procedure btnCancelClick(Sender: TObject);
    procedure btnExitClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    menuImg: TPortableNetworkGraphic;
    function PrepareOptions: TOption;

  public
    ExitGame: boolean;
    function Execute: TOption;
  end;

var
  HelpForm: TForm3;
  Options: TOption;
  HighScoreClass: THighScoreClass;

implementation
{$R *.lfm}

procedure TForm2.FormCreate(Sender: TObject);
begin
  HighScoreClass := THighScoreClass.Create();
  Options := TOption.Create();
  HelpForm := TForm3.Create(self);
  menuImg := TPortableNetworkGraphic.Create();
  menuImg.LoadFromFile('images\bg_menu.png');
end;

procedure TForm2.FormPaint(Sender: TObject);
begin
  canvas.Draw(0,0,menuImg);
end;

function TForm2.Execute: TOption;
var highScoreString: TStringArray;
begin
  ExitGame := false;
  Options := Options.LoadOptions();

  cbAllowPassingThroughBoarders.checked := Options.PenetrateBorders;
  ebTarget.Text := IntToStr(Options.Target);
  ebStartLivesAmount.Text := IntToStr(Options.StartLivesAmount);
  ebTileWidth.Text := IntToStr(Options.TileWidth);
  ebTileHeight.Text := IntToStr(Options.TileHeight);
  ebBoardWidth.Text := IntToStr(Options.BoardWidth);
  ebBoardHeight.Text := IntToStr(Options.BoardHeight);

  highScoreString := HighScoreClass.LoadHighScoresStringArray();
  lbHighScores.Items.AddStrings(highScoreString);
  self.ShowModal();
  Result := Options;
end;

function TForm2.PrepareOptions: TOption;
var o: TOption;
begin
  o := TOption.Create();
  o.Target := StrToInt(ebTarget.Text);
  o.PenetrateBorders := cbAllowPassingThroughBoarders.Checked;
  o.StartLivesAmount := StrToInt(ebStartLivesAmount.Text);
  o.TileWidth := StrToInt(ebTileWidth.Text);
  o.TileHeight := StrToInt(ebTileHeight.Text);
  o.BoardWidth := StrToInt(ebBoardWidth.Text);
  o.BoardHeight := StrToInt(ebBoardHeight.Text);
  Result := o;
end;

procedure TForm2.btnSaveClick(Sender: TObject);
begin
  Options := PrepareOptions();
  Options.SaveOptions(Options);
  self.Close();
end;

procedure TForm2.btnCancelClick(Sender: TObject);
begin
  Options := nil;
  self.Close();
end;

procedure TForm2.btnExitClick(Sender: TObject);
begin
  ExitGame := true;
  self.Close();
end;

procedure TForm2.btnHelpClick(Sender: TObject);
begin
  HelpForm.ShowModal();
end;

end.

