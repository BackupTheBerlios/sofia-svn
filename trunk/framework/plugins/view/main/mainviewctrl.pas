unit mainviewctrl;

interface

uses Controls, mainviewclasses, plugintf, cmdintf, classes;

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

  TShowSearchResultsCommand = class(TPluginCommand)
  private
  public
    procedure Execute; override; stdcall;
  end;

  TContainerReceiver = class(TInterfacedObject, IPluginCommandReceiver)
    procedure SetPluginManager(const Value: IPluginManager); stdcall;
  private
    FContainerActions: IContainerActions;
    FPluginManager: IPluginManager;
  public
    constructor Create(AContainer: IContainerActions);
    procedure AddPage(const PluginName: string; InstanceName: string; const
        Caption: string);
  end;

  TModelReceiver = class(TInterfacedObject, IPluginCommandReceiver)
    procedure SetPluginManager(const Value: IPluginManager); stdcall;
  private
    FPluginManager: IPluginManager;
  public
    procedure GetPersonnes(Categorie: string);
  end;

  TLocalController = class(TInterfacedObject, ILocalController)
    procedure SetPluginManager(const Value: IPluginManager); stdcall;
  private
    FCommandList: IInterfaceList;
    FContainerActions: IContainerActions;
    FModelReceiver: TModelReceiver;
    FNewContactCommand: IPluginCommand;
    FPluginManager: IPluginManager;
    FSearchAndDisplayCommand: IPluginCommand;
    FSearchCommand: IPluginCommand;
    FShowSearchResultsCommand: IPluginCommand;
    FContainerReceiver: TContainerReceiver;
  public
    constructor Create(AContainerActions: IContainerActions);
    property CommandList: IInterfaceList read FCommandList write FCommandList;
    property ContainerActions: IContainerActions read FContainerActions;
  end;

implementation

uses dbintf, SysUtils;


constructor TLocalController.Create(AContainerActions: IContainerActions);
begin
  //instancier les recepteurs
  FModelReceiver := TModelReceiver.Create;
  FContainerReceiver := TContainerReceiver.Create(FContainerActions);

  //instancier les commandes
  FSearchCommand := TSearchCommand.Create(FModelReceiver);
  FNewContactCommand := TNewContactCommand.Create(FContainerReceiver);
  FShowSearchResultsCommand := TShowSearchResultsCommand.Create(FContainerReceiver);

  //instancier les macros
  FSearchAndDisplayCommand := TPluginCommandList.Create;
  with (FSearchAndDisplayCommand as IPluginMacro) do
  begin
    Commands.Add(FSearchCommand);
    Commands.Add(FShowSearchResultsCommand);
  end;

  //affectation des commandes


end;

procedure TLocalController.SetPluginManager(const Value: IPluginManager);
begin
  FPluginManager := Value;
  FModelReceiver.SetPluginManager(Value);
  FContainerReceiver.SetPluginManager(Value);
end;

procedure TSearchCommand.Execute;
begin
  TModelReceiver(Receiver).GetPersonnes('contact');
end;

procedure TNewContactCommand.Execute;
begin
  TContainerReceiver(Receiver).AddPage('contact', '', 'Nouveau contact');
end;

constructor TContainerReceiver.Create(AContainer: IContainerActions);
begin
  FContainerActions := AContainer;
end;

procedure TContainerReceiver.AddPage(const PluginName: string; InstanceName:
    string; const Caption: string);
var
  Ctrl: TWinControl;
begin
  with FPluginManager[PluginName].NamedInstance[InstanceName].AsView do
  begin
    if Supports(FPluginManager[PluginName].LastPluginInstance, INamedPluginInstance) then
      InstanceName := FPluginManager[PluginName].AsNamedPluginInstance.InstanceName
    else
      InstanceName := PluginName;
    Ctrl := FContainerActions.AddPage(InstanceName, Caption);
    if Assigned(Ctrl) then
    begin
      Parent := Ctrl;
      Show;
    end;
  end;
end;

procedure TContainerReceiver.SetPluginManager(const Value: IPluginManager);
begin
  FPluginManager := Value;
end;

procedure TModelReceiver.GetPersonnes(Categorie: string);
var
  DataTable: IDataTable;
begin
  DataTable := FPluginManager['uib'].AsDataset.AddDataTable('contactmodel.xml', 'personnes');
  DataTable.DataAdapter.SelectCommand.Params.Select(Format('/Param[@Name=%s]', ['prs_categorie'])).SetAttributeValue('Value', 'contact');
  FPluginManager['search'].AsSerializable.XML := FPluginManager['uib'].AsDataset.DataReader['personnes'].XMLData;
end;

procedure TModelReceiver.SetPluginManager(const Value: IPluginManager);
begin
  FPluginManager := Value;
end;

procedure TShowSearchResultsCommand.Execute;
begin
  TContainerReceiver(Receiver).AddPage('search', '', 'Résultat de la recherche');
end;


end.
