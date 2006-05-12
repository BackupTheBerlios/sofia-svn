unit mainviewctrl;

interface

uses Controls, mainviewclasses, plugintf, cmdintf;

type
  IContainerActions = interface(IInterface)
  ['{515D874A-2735-4536-B859-C77A53D9ECEA}']
    function AddPage(const AName, ACaption: string): TWinControl;
  end;

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
    FContainer: IContainerActions;
    FPluginManager: IPluginManager;
  public
    constructor Create(APluginManager: IPluginManager; AContainer: IContainerActions);
    procedure OpenView(const PluginName: string; InstanceName: string; const
        Caption: string);
  end;

  TLocalController = class(TInterfacedObject, ILocalController)
    procedure SetPluginManager(const Value: IPluginManager); stdcall;
  private
    FContainerActions: IContainerActions;
    FPluginManager: IPluginManager;
  public
    constructor Create(AContainerActions: IContainerActions);
  end;

  TModelReceiver = class(TInterfacedObject, IPluginCommandReceiver)
  private
    FPluginManager: IPluginManager;
  public
    constructor Create(APluginManager: IPluginManager);
    procedure GetPersonnes(Categorie: string);
  end;

implementation

uses dbintf, SysUtils;


constructor TLocalController.Create(AContainerActions: IContainerActions);
begin
  FContainerActions := AContainerActions;

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
    IContainerActions);
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
