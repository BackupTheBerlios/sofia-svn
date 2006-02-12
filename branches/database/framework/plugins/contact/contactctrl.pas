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

unit contactctrl;

interface

uses
  Forms, Classes, Controls, StdCtrls, ExtCtrls, contactclasses, ComCtrls,
  Mask, Graphics;

type
  TContactFrame = class(TFrame)
    pcMain: TPageControl;
    tsNom: TTabSheet;
    tsDomicile: TTabSheet;
    tsBureau: TTabSheet;
    tsPersonnel: TTabSheet;
    tsAutre: TTabSheet;
    edtNom: TLabeledEdit;
    edtPrenom: TLabeledEdit;
    LabeledEdit1: TLabeledEdit;
    ListView1: TListView;
    btnAjouterAddr: TButton;
    btnModifierAddr: TButton;
    btnSupprimer: TButton;
    btnDefaultAddr: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    BevelTop: TBevel;
    Image1: TImage;
    Label2: TLabel;
    Bevel1: TBevel;
    Label3: TLabel;
    Image2: TImage;
    Bevel2: TBevel;
    MaskEdit1: TMaskEdit;
    MaskEdit2: TMaskEdit;
    MaskEdit3: TMaskEdit;
    MaskEdit4: TMaskEdit;
    Label4: TLabel;
    Label5: TLabel;
    Edit1: TEdit;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Bevel3: TBevel;
    Edit2: TEdit;
    Label9: TLabel;
  private
    { Déclarations privées }
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { Déclarations publiques }
  end;

  TController = class(TInterfacedObject, IController)
    function GetNomContact: string; stdcall;
    procedure SetNomContact(const Value: string); stdcall;
  private
    FContainer: TContactFrame;
  public
    constructor Create(AContainer: TWinControl);
    property Container: TContactFrame read FContainer write FContainer;
  end;

function NewController(AContainer: TWinControl): IController;

implementation

{$R *.dfm}


function NewController(AContainer: TWinControl): IController;
begin
  Result := TController.Create(AContainer);
end;

constructor TContactFrame.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TContactFrame.Destroy;
begin
  inherited;
end;

constructor TController.Create(AContainer: TWinControl);
begin
  FContainer := AContainer as TContactFrame;
end;

function TController.GetNomContact: string;
begin
  Result := Container.edtNom.Text;
end;

procedure TController.SetNomContact(const Value: string);
begin
  Container.edtNom.Text := Value;
end;

end.

