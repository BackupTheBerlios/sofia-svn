unit mainviewctrl;

interface

uses Controls, mainviewgui, mainviewclasses, plugintf, cmdintf;

type

  TSearchCommand = class(TInterfacedObject, IPluginCommand)
    procedure Cancel; stdcall;
    procedure Execute; stdcall;
  private
    FController: ILocalController;
  public
    constructor Create(Controller: ILocalController);
  end;

  TOpenViewCommand = class(TInterfacedObject, IPluginCommand)
    procedure Cancel; stdcall;
    procedure Execute; stdcall;
  private
    FController: ILocalController;
  public
    constructor Create(Controller: ILocalController);
  end;



  TLocalController = class(TInterfacedObject, ILocalController)
    function OpenView(const PluginName: string; InstanceName: string; const
        Caption: string): TWinControl;
    procedure SetPluginManager(const Value: IPluginManager); stdcall;
  private
    FContainer: TContainer;
    FPluginManager: IPluginManager;
    procedure ExecuteSearchAction;
    procedure ExecuteNewContactAction;
  public
    constructor Create(AContainer: TWinControl);
  end;


function NewLocalController(APlugin: TPlugin; AContainer: TWinControl):
    ILocalController;

implementation

function NewLocalController(APlugin: TPlugin; AContainer: TWinControl):
    ILocalController;
begin
  Result := TLocalController.Create(APlugin, AContainer);
end;

constructor TLocalController.Create(AContainer: TWinControl);
begin
  FContainer := AContainer as TContainer;

  FContainer.btnGo.OnClick := DoSearch;
  FContainer.lblNouveauContact.OnClick := DoNewContact;
end;

function TLocalController.OpenView(const PluginName: string; InstanceName:
    string; const Caption: string): TWinControl;
var
  Ctrl: TWinControl;
  InstanceName: string;
begin
  with FPluginManager[PluginName].NamedInstance[InstanceName].AsView do
  begin
    if Supports(FPluginManager[PluginName].LastPluginInstance, INamedPluginInstance) then
      InstanceName := FPluginManager[PluginName].AsNamedPluginInstance.InstanceName
    else
      InstanceName := PluginName;
    Ctrl := FContainer.AddPage(InstanceName, Caption);
    if Assigned(Ctrl) then
    begin
      Parent := Ctrl;
      Show;
    end;
  end;
  Result := Ctrl;
end;

procedure TLocalController.ExecuteSearchAction;
{
var
  XMLResult: ISerializable;
  BusinessObject: IBusinessObject;
  Dataset: IDatasetAdapter;
}
begin
{
  BusinessObject := FPluginManager['model'].AsBusinessObject;
  Dataset := FPluginManager['dbuib'].AsDatasetAdapter;
  XMLResult := FPluginManager['search'].AsSerializable;
  }

  //Dataset.AddEntity(BusinessObject.GetPersonnes(Categories));
  //XMLResult.XML := Dataset.XML;

  //AddPage('search', '', 'Résultats de la recherche');
end;

procedure TLocalController.ExecuteNewContactAction;
begin
  AddPage('contact', 'Nouveau contact');
end;

procedure TLocalController.SetPluginManager(const Value: IPluginManager);
begin
  FPluginManager := Value;
end;

{------------------------------------------------------------------------------}

constructor TSearchCommand.Create(Controller: ILocalController);
begin
  inherited;
  FController := Controller;
end;

procedure TSearchCommand.Cancel;
begin
  //Impossible d'annuler la recherche
end;

procedure TSearchCommand.Execute;
begin
  FController.ExecuteSearchAction;
end;

{------------------------------------------------------------------------------}

constructor TOpenViewCommand.Create(Controller: ILocalController);
begin
  inherited;
  FController := Controller;
end;

procedure TOpenViewCommand.Cancel;
begin
  //Impossible d'annuler la recherche
end;

procedure TOpenViewCommand.Execute;
begin
  FController.ExecuteSearchAction;
end;



end.
