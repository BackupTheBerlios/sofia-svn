unit displayctrl;

interface

uses Controls, displaygui, displayclasses;

type

  TController = class(TInterfacedObject, IController)
    function AddPage(const AName, ACaption: string): TWinControl;
    procedure Search(Categories: string); stdcall;
  private
    FContainer: TContainer;
    FPlugin: TPlugin;
    procedure DoSearch(Sender: TObject);
    procedure DoNewContact(Sender: TObject);
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

  FContainer.btnGo.OnClick := DoSearch;
  FContainer.lblNouveauContact.OnClick := DoNewContact;
end;

function TController.AddPage(const AName, ACaption: string): TWinControl;
begin
  Result := FContainer.AddPage(AName, ACaption);
end;

procedure TController.DoSearch(Sender: TObject);
begin
  FPlugin.Search('contact;client;organisation');
end;

procedure TController.DoNewContact(Sender: TObject);
begin
  FPlugin.NewContact;
end;

procedure TController.Search(Categories: string);
begin
  // TODO -cMM: TController.Search default body inserted
end;


end.
