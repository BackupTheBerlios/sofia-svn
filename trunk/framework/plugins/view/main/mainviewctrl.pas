unit mainviewctrl;

interface

uses Controls, mainviewgui, mainviewclasses, plugintf;

type

  TLocalController = class(TInterfacedObject, ILocalController)
    function AddPage(const PluginName: string; InstanceName: string; const Caption:
        string): TWinControl;
    procedure Search(Categories: string); stdcall;
    procedure SetPluginManager(const Value: IPluginManager); stdcall;
  private
    FContainer: TContainer;
    FPlugin: TPlugin;
    FPluginManager: IPluginManager;
    procedure DoSearch(Sender: TObject);
    procedure DoNewContact(Sender: TObject);
  public
    constructor Create(APlugin: TPlugin; AContainer: TWinControl);
    procedure NewContact; stdcall;
  end;

function NewLocalController(APlugin: TPlugin; AContainer: TWinControl):
    ILocalController;

implementation

function NewLocalController(APlugin: TPlugin; AContainer: TWinControl):
    ILocalController;
begin
  Result := TLocalController.Create(APlugin, AContainer);
end;

constructor TLocalController.Create(APlugin: TPlugin; AContainer: TWinControl);
begin
  FContainer := AContainer as TContainer;
  FPlugin := APlugin;

  FContainer.btnGo.OnClick := DoSearch;
  FContainer.lblNouveauContact.OnClick := DoNewContact;
end;

function TLocalController.AddPage(const PluginName: string; InstanceName:
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

procedure TLocalController.DoSearch(Sender: TObject);
begin
  FPlugin.Search('contact');
end;

procedure TLocalController.DoNewContact(Sender: TObject);
begin
  FPlugin.NewContact;
end;

procedure TLocalController.NewContact;
begin
  FContainer.AddPage('contact', 'Nouveau contact');
end;

procedure TLocalController.Search(Categories: string);
begin
  // TODO -cMM: TController.Search default body inserted
end;

procedure TLocalController.SetPluginManager(const Value: IPluginManager);
begin
  FPluginManager := Value;
end;


end.
