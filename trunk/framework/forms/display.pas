{-------------------------------------------------------------------------------
Copyright (c) 2006 Lawrence-Albert Zemour. All rights reserved.

This file is part of Sofia.

Sofia is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.

Sofia is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with Sofia; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301  USA
-------------------------------------------------------------------------------}

unit display;

interface

uses Forms, Classes, Controls, ExtCtrls, ComCtrls, Grids, Types, Graphics,
  StdCtrls;

type
  TDisplayForm = class(TForm)
    Panel2: TPanel;
    pnlPlugin: TPanel;
    Panel1: TPanel;
    pbPages: TPaintBox;
    Panel3: TPanel;
    pnlScrollRight: TPanel;
    pnlScrollLeft: TPanel;
    sgPages: TStringGrid;
    Panel5: TPanel;
    Image1: TImage;
    Panel7: TPanel;
    lblUser: TLabel;
    lblAide: TLabel;
    lblQuitter: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Edit1: TEdit;
    Button1: TButton;
    TimerBug: TTimer;
    procedure sgPagesDrawCell(Sender: TObject; ACol, ARow: Integer;
      Rect: TRect; State: TGridDrawState);
    procedure pbPagesPaint(Sender: TObject);
    procedure lblMouseEnter(Sender: TObject);
    procedure lblMouseLeave(Sender: TObject);
    procedure TimerBugTimer(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FPagesCount: Integer;
    FPageIndex: Integer;
    { Déclarations privées }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AddPage(AName, ACaption: string): Integer;
    { Déclarations publiques }
  end;

var
  DisplayForm: TDisplayForm;

const
  clBleu = $00C09A6C;
  clVert = $000FA089;
  clGris = $00D7E8EB;

implementation

uses DateUtils, Dialogs, app, plugmgr, plugintf, SysUtils, Windows, Messages;

{$R *.dfm}

constructor TDisplayForm.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPagesCount := 0;
end;

destructor TDisplayForm.Destroy;
begin
  inherited Destroy;
end;

function TDisplayForm.AddPage(AName, ACaption: string): Integer;
var
  Plug: IPlugDisplay;
  Panel: TPanel;
begin
  Inc(FPagesCount);
  sgPages.ColCount := FPagesCount;
  sgPages.Cells[0, FPagesCount - 1] := ACaption;

  Panel := TPanel.Create(Self);
  Plug := AppForm.PluginCnt.Display[AName];
  if Assigned(Plug) then
  begin
    sgPages.Cols[FPagesCount - 1].Objects[0] := Panel;
    Panel.BevelOuter := bvNone;
    Panel.Align := alClient;
    Panel.Parent := pnlPlugin;
    Plug.Parent := Panel;
    Plug.Show;
  end
  else
    Panel.Free;

  Result := FPagesCount - 1;
end;

procedure TDisplayForm.sgPagesDrawCell(Sender: TObject; ACol,
  ARow: Integer; Rect: TRect; State: TGridDrawState);
var
  ACanvas: TCanvas;
  AGrid: TStringGrid;
  TextHeight: Integer;
  TextWidth: Integer;
  Text: string;
  TextX: Integer;
  TextY: Integer;

  procedure EraseBackground;
  begin
    with ACanvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := clWindow;
      FillRect(Rect);
    end;
  end;

  procedure DrawActiveBackground;
  begin
    with ACanvas do
    begin
      Brush.Style := bsSolid;
      Brush.Color := clGris;
      FillRect(Rect);
      Pen.Color := clVert;
      PenPos := Point(Rect.Left, Rect.Bottom);
      LineTo(Rect.Left, Rect.Top);
      LineTo(Rect.Right - 1, Rect.Top);
      LineTo(Rect.Right - 1, Rect.Bottom);
      pbPages.Repaint;
    end;
  end;

  procedure DrawInactiveBackground;
  begin
    with ACanvas do
    begin
      Rect.Top := Rect.Top + 2;
      Brush.Color := clVert;
      FillRect(Rect);
    end;
  end;

  procedure DrawCaption;
  begin
    InflateRect(Rect, -1, -1);
    with ACanvas do
    begin
      Brush.Style := bsClear;
      Font.Name := 'Verdana';
      Font.Size := 8;
      Font.Style := [fsBold];
      TextRect(Rect, TextX, TextY, Text);
    end;
  end;

  procedure DrawActiveCaption;
  begin
    with ACanvas do
    begin
      Font.Color := clBlack;
    end;
    DrawCaption;
  end;

  procedure DrawInactiveCaption;
  begin
    with ACanvas do
    begin
      Font.Color := clWhite;
    end;
    DrawCaption;
  end;

begin
  if not (Sender is TStringGrid) then
    Exit;
  AGrid := Sender as TStringGrid;
  ACanvas := AGrid.Canvas;

  Text := AGrid.Cells[ARow, ACol];
  TextHeight := ACanvas.TextHeight(Text);
  TextWidth := ACanvas.TextWidth(Text);
  AGrid.ColWidths[ACol] := TextWidth + 20;
  TextX := ((Rect.Right - Rect.Left) div 2) - (TextWidth div 2) + Rect.Left;
  TextY := ((Rect.Bottom - Rect.Top) div 2) - (TextHeight div 2) + Rect.Top;

  EraseBackground;
  if (gdSelected in State) then
  begin
    FPageIndex := ACol;
    DrawActiveBackground;
    DrawActiveCaption;
  end
  else
  begin
    DrawInactiveBackground;
    DrawInactiveCaption
  end;
end;

procedure TDisplayForm.pbPagesPaint(Sender: TObject);
var
  APaintBox: TPaintBox;
begin
  if not (Sender is TPaintBox) then
    Exit;
  APaintBox := Sender as TPaintBox;

  with APaintBox.Canvas do
  begin
    Brush.Color := clGris;
    FillRect(APaintBox.ClientRect);
    Pen.Color := clVert;
    PenPos := Point(sgPages.CellRect(FPageIndex, 0).Left + sgPages.Left, 0);
    LineTo(0, 0);
    LineTo(0, APaintBox.Height - 1);
    LineTo(APaintBox.Width - 1, APaintBox.Height - 1);
    LineTo(APaintBox.Width - 1, 0);
    LineTo(sgPages.CellRect(FPageIndex, 0).Right + sgPages.Left - 2, 0);
  end;
end;

procedure TDisplayForm.lblMouseEnter(Sender: TObject);
var
  ALabel: TLabel;
begin
  if not (Sender is TLabel) then
    Exit;
  ALabel := Sender as TLabel;
  ALabel.Font.Style := ALabel.Font.Style + [fsUnderline]
end;

procedure TDisplayForm.lblMouseLeave(Sender: TObject);
var
  ALabel: TLabel;
begin
  if not (Sender is TLabel) then
    Exit;
  ALabel := Sender as TLabel;
  ALabel.Font.Style := ALabel.Font.Style - [fsUnderline];
end;

procedure TDisplayForm.TimerBugTimer(Sender: TObject);
begin
  if FPagesCount = 1 then
  begin
    sgPagesDrawCell(sgPages, 0, 0, sgPages.CellRect(0, 0), [gdSelected]);
    sgPages.Invalidate;
    Application.ProcessMessages;
    TimerBug.Enabled := False;
  end;
end;

procedure TDisplayForm.Button1Click(Sender: TObject);
var
  Res: IPlugIO;
  Qry: IPlugDatabaseObject;
  Dts: IPlugDataset;
  desc: string;
begin
  AddPage('recherche', 'Résultats');

  Qry := AppForm.PluginCnt.DatabaseObject['dbobj'];
  Dts := AppForm.PluginCnt.Dataset['dbuib'];
  Res := AppForm.PluginCnt.IO['recherche'];

  desc := 'Résultats dans la catégorie "%s"';

  Dts.Add(Qry.GetQueryPersonnes('contact', Format(desc, ['Contact'])));
  Dts.Add(Qry.GetQueryPersonnes('client', Format(desc, ['Clients'])));
  Dts.Add(Qry.GetQueryPersonnes('organisation', Format(desc, ['Organisations'])));
  Res.XML := Dts.XML;
end;

end.

