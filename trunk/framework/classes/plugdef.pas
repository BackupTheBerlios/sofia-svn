unit plugdef;

interface

uses Classes, Controls, DB;

type
  TPlugContainer = TWinControl;
  TPlugDataBase = TComponent;
  TPlugDataItem = TCollectionItem;
  TPlugDataStream = TStringStream;
  TPlugConnection = TCustomConnection;

  TDataItemClass = class of TPlugDataItem;
  TPlugData = class(TPlugDataBase)
  private
    FCollection: TCollection;
    FDataItemClass: TDataItemClass;
    function GetItems(Index: Integer): TPlugDataItem;
  public
    constructor Create(AOwner: TComponent; ADataItemClass: TDataItemClass);
        overload;
    destructor Destroy; override;
    property DataItemClass: TDataItemClass read FDataItemClass write FDataItemClass;
    property Items[Index: Integer]: TPlugDataItem read GetItems; default;
  published
    property Collection: TCollection read FCollection write FCollection;
  end;

implementation

{ TPlugData }

constructor TPlugData.Create(AOwner: TComponent; ADataItemClass:
    TDataItemClass);
begin
  inherited Create(AOwner);
  FCollection := TCollection.Create(ADataItemClass);
end;

destructor TPlugData.Destroy;
begin
  inherited;
  FCollection.Free;
end;

function TPlugData.GetItems(Index: Integer): TPlugDataItem;
begin
  Result := TPlugDataItem(FCollection.Items[0]);
end;

end.



