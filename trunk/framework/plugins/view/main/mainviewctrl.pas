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

  TModelReceiver = class(TInterfacedObject, IPluginCommandReceiver)
  private
    FPluginManager: IPluginManager;
  public
    constructor Create(APluginManager: IPluginManager);
    procedure GetPersonnes(Categorie: string);
  end;

function NewLocalController(AContainer: TWinControl): ILocalController;

implementation

uses dbintf;

function NewLocalController(AContainer: TWinControl): ILocalController;
begin
  Result := TLocalController.Create(AContainer);
end;

constructor TLocalController.Create(AContainer: TWinControl);
begin
  FContainer := AContainer as TContainer;

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
  TModelReceiver(Receiver).GetPersonnes('contact');
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

procedure TViewReceiver.OpenView(const PluginName: string; InstanceName: string; const Caption: string);
var
  Ctrl: TWinControl;
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
  FContainer := AContainer;
end;

procedure TModelReceiver.GetPersonnes(Categorie: string);
var
  DataTable: IDataTable;
begin
  DataTable := FPluginManager['uib'].AsDataset.AddDataTable('contactmodel.xml', 'personnes');
  DataTable.DataAdapter.SelectCommand.Params.Select(Format('/Param[@Name=%s]', ['prs_categorie'])).SetAttributeValue('Value', 'contact');
  FPluginManager['search'].AsSerializable.XML := FPluginManager['uib'].AsDataset.DataReader['personnes'].XMLData;
end;

end.
