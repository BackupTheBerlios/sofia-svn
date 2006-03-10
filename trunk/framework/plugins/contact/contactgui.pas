unit contactgui;

interface

uses
  Classes, Controls, Forms, contactclasses, Graphics, ExtCtrls, ImgList,
  StdCtrls;

type
  TContainer = class(TFrame)
    ImageList_9: TImageList;
    Panel3: TPanel;
    Panel4: TPanel;
    pnlClient: TPanel;
    Image1: TImage;
    Panel1: TPanel;
    procedure Image1Click(Sender: TObject);
  private
    { Déclarations privées }
  public
    { Déclarations publiques }
  end;

  TController = class(TInterfacedObject, IController)
    function GetNomContact: string; stdcall;
    procedure SetNomContact(const Value: string); stdcall;
  private
    FContainer: TContainer;
  public
    constructor Create(AContainer: TWinControl);
  end;

function NewController(AContainer: TWinControl): IController;

implementation

uses TypInfo;

{$R *.dfm}

function NewController(AContainer: TWinControl): IController;
begin
  Result := TController.Create(AContainer);
end;

constructor TController.Create(AContainer: TWinControl);
begin
  FContainer := AContainer as TContainer;
end;

function TController.GetNomContact: string;
begin
  // TODO -cMM: TController.GetNomContact default body inserted
end;

procedure TController.SetNomContact(const Value: string);
begin
  // TODO -cMM: TController.SetNomContact default body inserted
end;

procedure TContainer.Image1Click(Sender: TObject);
var
  btmp: TBitmap;
begin
  if not (Sender is TImage) then
    Exit;

  with Sender as TImage do
  begin
    Tag := (Tag + 1) mod 2;
    pnlClient.Visible := Boolean(Tag);
    btmp := TBitmap.Create;
    try
      ImageList_9.GetBitmap(Tag, btmp);
      Picture.Bitmap.Assign(btmp);
    finally
      btmp.Free;
    end;
  end;
end;

end.

