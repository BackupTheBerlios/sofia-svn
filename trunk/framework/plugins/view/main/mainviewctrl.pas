unit mainviewctrl;

interface

uses Controls, mainviewgui, mainviewclasses, plugintf, cmdintf;

type

  TSearchCommand = class(TPluginCommand)
  private
  public
    procedure Execute; override; stdcall;
  end;

  TNewContactCommand = class(TPluginCommand)
  private
  public
    procedure Execute; override; stdcall;
  end;

  TModelReceiver = class(TInterfacedObject, IPluginCommandReceiver)
  private
    FPluginManager: IPluginManager;
  public
    constructor Create(APluginManager: IPluginManager);
  end;

  TViewReceiver = class(TInterfacedObject, IPluginCommandReceiver)
  private
    FContainer: TWinControl;
    FPluginManager: IPluginManager;
  public
    constructor Create(APluginManager: IPluginManager; AContainer: TWinControl);
    procedure OpenView(const PluginName: string; InstanceName: string; const
        Caption: string);
  end;

  TLocalController = class(TInterfacedObject, ILocalController)
    procedure SetPluginManager(const Value: IPluginManager); stdcall;
  private
    FContainer: TContainer;
    FPluginManager: IPluginManager;
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

  //instancier ici les TableEntity
  

  //instancier ici les receivers
  //instancier ici les commandes
  //instancier ici les macros
  //affecter les commandes/macros aux controles graphiques

  //FContainer.btnGo.OnClick := DoSearch;
  //FContainer.lblNouveauContact.OnClick := DoNewContact;
end;

procedure TLocalController.SetPluginManager(const Value: IPluginManager);
begin
  FPluginManager := Value;
end;

procedure TSearchCommand.Execute;
begin
  Receiver.ExecuteSearchAction;
end;

procedure TNewContactCommand.Execute;
begin
  TViewReceiver(Receiver).OpenView('contact', '', 'Nouveau contact');
end;

constructor TViewReceiver.Create(APluginManager: IPluginManager; AContainer:
    TWinControl);
begin
  FPluginManager := APluginManager;
  FContainer := AContainer;
end;

procedure TViewReceiver.OpenView(const PluginName: string; InstanceName:
    string; const Caption: string);
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
  //Result := Ctrl;
end;

constructor TModelReceiver.Create(APluginManager: IPluginManager);
begin
  FPluginManager := APluginManager;
end;

{
procedure TLocalController.ExecuteSearchAction;
var
  XMLResult: ISerializable;
  BusinessObject: IBusinessObject;
  Dataset: IDatasetAdapter;
begin
  BusinessObject := FPluginManager['model'].AsBusinessObject;
  Dataset := FPluginManager['dbuib'].AsDatasetAdapter;
  XMLResult := FPluginManager['search'].AsSerializable;
  Dataset.AddEntity(BusinessObject.GetPersonnes(Categories));
  XMLResult.XML := Dataset.XML;

  AddPage('search', '', 'Résultats de la recherche');
end;
}

end.
