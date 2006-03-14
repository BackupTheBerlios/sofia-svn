unit welcomectrl;

interface

uses Controls, welcomegui, welcomeclasses;

type
  TController = class(TInterfacedObject, IController)
  private
    FContainer: TContainer;
    FPlugin: TPlugin;
  public
    constructor Create(APlugin: TPlugin; AContainer: TWinControl);
    destructor Destroy; override;
    property Container: TContainer read FContainer write FContainer;
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

destructor TController.Destroy;
begin
  inherited;
end;


end.
