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

implementation

uses TypInfo;

{$R *.dfm}

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

