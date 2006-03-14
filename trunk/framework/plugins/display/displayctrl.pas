unit displayctrl;

interface

uses Controls, displaygui;

type

  IController = interface(IInterface)
    ['{B0122448-88BA-44DF-9B33-8198AF276DF6}']
    function AddPage(AName, ACaption: string): TWinControl;
    procedure Search(Categories: string); stdcall;
  end;

  TController = class(TInterfacedObject, IController)
    function AddPage(AName, ACaption: string): TWinControl;
    procedure Search(Categories: string); stdcall;
  private
    FContainer: TContainer;
  public
    constructor Create(AContainer: TWinControl);
  end;

function NewController(AContainer: TWinControl): IController;

implementation

function NewController(AContainer: TWinControl): IController;
begin
  Result := TController.Create(AContainer);
end;

constructor TController.Create(AContainer: TWinControl);
begin
  FContainer := AContainer as TContainer;
end;

function TController.AddPage(AName, ACaption: string): TWinControl;
begin
  Result := FContainer.AddPage(AName, ACaption);
end;

procedure TController.Search(Categories: string);
begin
  // TODO -cMM: TController.Search default body inserted
end;


end.
