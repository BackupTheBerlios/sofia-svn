unit searchctrl;

interface

uses Controls, searchclasses, searchgui;

type

  TController = class(TInterfacedObject, IController)
    procedure SetXMLData(const Value: string); stdcall;
    property XMLData: string write SetXMLData;
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

procedure TController.SetXMLData(const Value: string);
begin
  FContainer.ClientDataSet.XMLData := Value;
  FContainer.ClientDataSet.Open;
end;


end.
