{*****************************************************************************}
{                                                                             }
{    GDI + Controls                                                           }
{      http://lummie.co.uk                                                    }
{        Version: 1.0.3                                                       }
{                                                                             }
{    Copyright (c) 2005 Matt harrison (http://www.lummie.co.uk)               }
{                                                                             }
{*****************************************************************************}
unit GDIPControlsReg;

interface

uses
  SysUtils, Classes, GPWinControl, GDIPAPI, GDIPOBJ, GDIPUTIL, GDIPFontCombo, ColorSelectorFrame, GDIPColorDisplay;

procedure Register;

implementation

uses
  dialogs;

procedure Register;
begin
  RegisterComponents('GDI+ Controls', [TGDIPFontCombo, TGDIPColorSelector, TGDIPColorDisplay, TGDIPCanvas {,TGDIPDropDownColorSelector, TGDIPImageBrowser}]);
end;

end.
