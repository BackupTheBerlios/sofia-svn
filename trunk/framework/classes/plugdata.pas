unit plugdata;

interface

uses Classes, plugdef;

type
  TDataItemClass = class of TPlugDataItem;
  TPlugDataComponent = class(TPlugDataRoot)
  private
    FCollection: TPlugDataCollection;
    FDataItemClass: TDataItemClass;
    function GetItems(Index: Integer): TPlugDataItem;
  public
    constructor Create(AOwner: TComponent; ADataItemClass: TDataItemClass);
        overload;
    destructor Destroy; override;
    property DataItemClass: TDataItemClass read FDataItemClass write FDataItemClass;
    property Items[Index: Integer]: TPlugDataItem read GetItems; default;
  published
    property Collection: TPlugDataCollection read FCollection write FCollection;
  end;

  TSerializer = class(TObject)
  public
    procedure Deserialize(Stream: TPlugDataStream; PlugDataComponent:
        TPlugDataComponent);
    procedure Serialize(Data: TPlugDataComponent; Stream: TPlugDataStream);
  end;

implementation

{ TPlugData }

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

procedure TSerializer.Deserialize(Stream: TPlugDataStream; PlugDataComponent:
    TPlugDataComponent);
var
  BinStream: TMemoryStream;
begin
  BinStream := TMemoryStream.Create;
  try
    ObjectTextToBinary(Stream, BinStream);
    BinStream.Position := 0;
    BinStream.ReadComponent(PlugDataComponent);
  finally
    BinStream.Free;
  end;
end;

procedure TSerializer.Serialize(Data: TPlugDataComponent; Stream:
    TPlugDataStream);
var
  BinStream: TMemoryStream;
begin
  BinStream := TMemoryStream.Create;
  try
    BinStream.WriteComponent(Data);
    BinStream.Position := 0;
    ObjectBinaryToText(BinStream, Stream);
  finally
    BinStream.Free;
  end;
end;

end.
