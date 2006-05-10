{*****************************************************************************}
{                                                                             }
{    GDI + Controls                                                           }
{      http://lummie.co.uk                                                    }
{        Version: 1.0.2                                                       }
{                                                                             }
{    Copyright (c) 2005 Matt harrison (http://www.lummie.co.uk)               }
{                                                                             }
{*****************************************************************************}
unit ColorSelectorDialogForm;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, GDIPAPI, GDIPOBJ, GDIPUTIL, StdCtrls, ColorSelectorFrame;

type
  TColorSelectorDialog = class(TForm)
    GDIPColorSelectorFrame: TGDIPColorSelector;
    Button1: TButton;
    Button2: TButton;
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    function GetColor: TGPColor;
    procedure SetColor(const Value: TGPColor);
    function GetChanged: TColorChangeEvent;
    procedure SetChanged(const Value: TColorChangeEvent);
    function GetAlphaEnabled: boolean;
    procedure SetAlphaEnabled(const Value: boolean);
    { Private declarations }
  public
    { Public declarations }
    property AlphaColor : TGPColor read GetColor write SetColor;
    property AlphaEnabled : boolean read GetAlphaEnabled write SetAlphaEnabled;
    property OnChange : TColorChangeEvent read GetChanged write SetChanged;
  end;

var
  ColorSelectorDialog: TColorSelectorDialog;

implementation

{$R *.dfm}

function TColorSelectorDialog.GetChanged: TColorChangeEvent;
begin
  result := GDIPColorSelectorFrame.OnChange;
end;

function TColorSelectorDialog.GetColor: TGPColor;
begin
  result := GDIPColorSelectorFrame.AlphaColor;
end;

procedure TColorSelectorDialog.SetChanged(const Value: TColorChangeEvent);
begin
  GDIPColorSelectorFrame.OnChange := value;
end;

procedure TColorSelectorDialog.SetColor(const Value: TGPColor);
begin
  GDIPColorSelectorFrame.AlphaColor := Value;
end;

procedure TColorSelectorDialog.FormCloseQuery(Sender: TObject;
  var CanClose: Boolean);
begin
  if assigned(ActiveControl) and ActiveControl.Focused then
    DefocusControl(ActiveControl,false);
  GDIPColorSelectorFrame.TimerUpdateTimer(Sender);
end;

function TColorSelectorDialog.GetAlphaEnabled: boolean;
begin
  result := GDIPColorSelectorFrame.AlphaEnabled;
end;

procedure TColorSelectorDialog.SetAlphaEnabled(const Value: boolean);
begin
  GDIPColorSelectorFrame.AlphaEnabled := value;
end;

end.
