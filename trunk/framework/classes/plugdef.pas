unit plugdef;

interface

uses Classes, Controls, DB;

type
  TPlugContainer = TWinControl;
  TPlugConnection = TCustomConnection;

  TSerializeStream = TStringStream;
  TSerializable = class(TComponent)
  private
    FCollection: TCollection;
    function GetItems(Index: Integer): TCollectionItem;
  public
    constructor Create(AOwner: TComponent; AItemClass: TCollectionItemClass);
        reintroduce; overload;
    destructor Destroy; override;
    property Items[Index: Integer]: TCollectionItem read GetItems; default;
  published
    property Collection: TCollection read FCollection write FCollection;
  end;

implementation

{ TPlugDataComponent }

constructor TSerializable.Create(AOwner: TComponent; AItemClass:
    TCollectionItemClass);
begin
  inherited Create(AOwner);
  FCollection := TCollection.Create(AItemClass);
end;

destructor TSerializable.Destroy;
begin
  inherited;
  FCollection.Free;
end;

function TSerializable.GetItems(Index: Integer): TCollectionItem;
begin
  Result := FCollection.Items[Index];
end;

end.



