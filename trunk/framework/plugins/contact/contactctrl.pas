unit contactctrl;

interface

uses Controls, contactclasses, contactgui;

type

  TController = class(TInterfacedObject, IController)
    procedure Activate; stdcall;
    function GetNomContact: string; stdcall;
    procedure SetNomContact(const Value: string); stdcall;
  private
    FContainer: TContainer;
    FPlugin: TPlugin;
  public
    constructor Create(APlugin: TPlugin; AContainer: TWinControl);
  end;

function NewController(APlugin: TPlugin; AContainer: TWinControl): IController;

implementation

function NewController(APlugin: TPlugin; AContainer: TWinControl): IController;
begin
  Result := TController.Create(APlugin, AContainer);
end;

constructor TController.Create(APlugin: TPlugin; AContainer: TWinControl);
begin
  FContainer := AContainer as TContainer;
  FPlugin := APlugin;
end;

procedure TController.Activate;
begin
  FContainer.edtCivilite.SetFocus;
end;

function TController.GetNomContact: string;
begin
  // TODO -cMM: TController.GetNomContact default body inserted
end;

procedure TController.SetNomContact(const Value: string);
begin
  // TODO -cMM: TController.SetNomContact default body inserted
end;


end.
