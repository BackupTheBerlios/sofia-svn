unit plugdef;

interface

uses Classes, Controls, DB;

type
  TPlugContainer = TWinControl;
  TPlugDataRoot = TComponent;
  TPlugDataItem = TCollectionItem;
  TPlugDataCollection = TCollection;
  TPlugDataStream = TStringStream;
  TPlugConnection = TCustomConnection;

  TDataItemClass = class of TPlugDataItem;
  TPlugDataComponent = class(TPlugDataRoot)
  private
    FCollection: TPlugDataCollection;
    FDataItemClass: TDataItemClass;
    function GetItems(Index: Integer): TPlugDataItem;
  public
    constructor Create(AOwner: TComponent; ADataItemClass: TDataItemClass);
        reintroduce; overload;
    destructor Destroy; override;
    property DataItemClass: TDataItemClass read FDataItemClass write FDataItemClass;
    property Items[Index: Integer]: TPlugDataItem read GetItems; default;
  published
    property Collection: TPlugDataCollection read FCollection write FCollection;
  end;

implementation

{ TPlugDataComponent }

constructor TPlugDataComponent.Create(AOwner: TComponent; ADataItemClass:
    TDataItemClass);
begin
  inherited Create(AOwner);
  FCollection := TCollection.Create(ADataItemClass);
end;

destructor TPlugDataComponent.Destroy;
begin
  inherited;
  FCollection.Free;
end;

function TPlugDataComponent.GetItems(Index: Integer): TPlugDataItem;
begin
  Result := TPlugDataItem(FCollection.Items[0]);
end;

end.



