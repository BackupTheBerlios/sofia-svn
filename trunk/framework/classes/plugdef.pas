unit plugdef;

interface

uses Classes, Controls, DB;

type
  TPlugContainer = TWinControl;
  TPlugDataRoot = TComponent;
  TPlugDataComponent = class(TPlugDataRoot);
  TPlugDataItem = TCollectionItem;
  TPlugDataCollection = TCollection;
  TPlugDataStream = TStringStream;
  TPlugConnection = TCustomConnection;

implementation

end.



