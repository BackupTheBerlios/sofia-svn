unit XMLCursor;

interface

{$IFNDEF MSWINDOWS}
	{$IFDEF WIN32}
		{$DEFINE MSWINDOWS}
  {$ENDIF}
{$ENDIF}

uses Sysutils, Classes,
{$IFDEF MSWINDOWS}
  Windows, ComObj,
{$ENDIF}
  StdXML_TLB,
  OraXML;

type
	IORAXMLContext = interface
  ['{829A31EE-6A27-D511-969D-000102D8B2A6}']
    procedure AddBuffer(Buffer: PChar);
  	function GetContext: Pxmlctx;
  end;
  TORAXMLContext = class(TInterfacedObject, IORAXMLContext)
  private
    Fxmlctx: Pxmlctx;
    FBuffers: TList;
	protected
    procedure AddBuffer(Buffer: PChar);
  	function GetContext: Pxmlctx;
  protected
    procedure DisposeBuffers;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;
  IXMLCursorInternal = interface
  ['{D9329CA1-C252-11D4-8896-0060087D03E0}']
    function Get_CurrentNodeList: TList;
  	function  ORAXMLContext: Pointer; safecall; // !!! Do not use this function for multi-platform applications
    function  ORAXMLNode: Pointer; safecall; // !!! Do not use this function for multi-platform applications
  end;
{$IFDEF MSWINDOWS}
  TXMLCursor = class(TAutoIntfObject, IXMLCursor, IXMLCursorInternal)
{$ENDIF}
{$IFDEF LINUX}
  TXMLCursor = class(TInterfacedObject, IXMLCursor, IXMLCursorInternal)
	protected
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
{$ENDIF}
  private
    FCurrentNode: Pxmlnode;
    FCurrentNodeList: TList;
//    FDocumentNode: Pxmlnode;
    FEOF: Boolean;
    FPosition: Integer;
    FNodeList: Pxmlnodes;
    FORAXMLContext: IORAXMLContext;
    FXPath: string;
  protected
    procedure Assign(const CursorSource: TXMLCursor);
    procedure CopyElementNodeList(Nodes: Pxmlnodes; NodeList: TList);
    procedure CopyNodeList(Nodes: Pxmlnodes; NodeList: TList);
    procedure Filter(XPath: string);
		procedure FindFirstElement;
    function InternalCloneNode(Node: Pxmlnode; Deep: Boolean): Pxmlnode;
    function InternalGetValue(CurrentNode: Pxmlnode; XPath: string): string;
		function MoveInAbsolutePath(ParentNodePath: string): Boolean;
		function MoveInRelativePath(ParentNodePath: string): Boolean;
    function xmlctx: Pxmlctx;
	protected
  	{ IXMLCursorInternal }
    function Get_CurrentNodeList: TList;
  	function  ORAXMLContext: Pointer; safecall; // !!! Do not use this function for multi-platform applications
    function  ORAXMLNode: Pointer; safecall; // !!! Do not use this function for multi-platform applications
  public
  	{ IXMLCursor }
    function  AppendChild(const ElementName, Value: WideString): IXMLCursor; safecall;
    procedure AppendXMLCursor(const XMLCursor: IXMLCursor); safecall;
    function  ContainerXML: WideString; safecall;
    function	Count: Integer; safecall;
    procedure Delete; safecall;
    function  Document: IXMLCursor; safecall;
    function	EOF: WordBool; safecall;
    procedure First; safecall;
    function GetName: WideString; safecall;
    function  GetValue(const XPath: WideString): WideString; safecall;
    function Get_Values(const Name: WideString): WideString; safecall;
    function InsertBefore(const ElementName, Value: WideString): IXMLCursor; safecall;
    function InsertAfter(const ElementName, Value: WideString): IXMLCursor; safecall;
    procedure Last; safecall;
    procedure	Load(const FileName: WideString); safecall;
    procedure	LoadXML(const SourceXML: WideString); safecall;
    procedure MoveTo(Index: Integer); safecall;
    procedure	Next; safecall;
    function  RecNo: Integer; safecall;
    procedure ReplaceWithXMLCursor(const XMLCursor: IXMLCursor); safecall;
    procedure	Save(const FileName: WideString); safecall;
    function	Select(const XPath: WideString): IXMLCursor; safecall;
    procedure	SetAttributeValue(const AttributeName, Value: WideString); safecall;
    procedure	SetCValue(const ElementName, Value: WideString); safecall;
    procedure	SetValue(const ElementName, Value: WideString); safecall;
    procedure Set_Values(const Name: WideString; const Value: WideString); safecall;
    function  XML: WideString; safecall;
    function XMLDOMDocument: IUnknown; safecall;
    function XMLDOMNode: IUnknown; safecall;
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

{$IFDEF MSWINDOWS}
  TXSLProc = class(TAutoIntfObject, IXSLProc)
{$ENDIF}
{$IFDEF LINUX}
  TXSLProc = class(TInterfacedObject, IXSLProc)
  protected
    function GetTypeInfoCount(out Count: Integer): HResult; stdcall;
    function GetTypeInfo(Index, LocaleID: Integer; out TypeInfo): HResult; stdcall;
    function GetIDsOfNames(const IID: TGUID; Names: Pointer;
      NameCount, LocaleID: Integer; DispIDs: Pointer): HResult; stdcall;
    function Invoke(DispID: Integer; const IID: TGUID; LocaleID: Integer;
      Flags: Word; var Params; VarResult, ExcepInfo, ArgErr: Pointer): HResult; stdcall;
{$ENDIF}
 	protected
  	FFileName: string;
    FFiles: TStrings;
    FXSLContext: IORAXMLContext;
 	protected
  	// IXSLProc
    function IsUpToDate: WordBool; safecall;
  	procedure Load(const AFileName: WideString); safecall;
  	function Process(const Document: IXMLCursor): WideString; safecall;
  public
  	constructor Create; virtual;
    destructor Destroy; override;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
	ActiveX, FileCtrl;
{$ENDIF}
{$IFDEF LINUX}
  Libc;
{$ENDIF}

procedure reraise(Message: string);
var
	E, NewE: Exception;
begin
	E := ExceptObject as Exception;
	NewE := E.ClassType.Create as Exception;
  NewE.Message := Message+#13#10+E.Message;
  raise NewE;
end;

procedure AssignList(Source, Destination: TList);
var
	I: Integer;
begin
  Destination.Clear;
  Destination.Capacity := Source.Capacity;
  for I := 0 to Source.Count-1 do
  	Destination.Add(Source[I]);
end;

procedure TORAXMLContext.AddBuffer(Buffer: PChar);
begin
	FBuffers.Add(Buffer);
end;

constructor TORAXMLContext.Create;
var
	Err: uword;
begin
	inherited;
	Fxmlctx := OraXML.xmlinit(@Err, nil, nil, nil, nil, nil, nil, nil, nil);
  FBuffers := TList.Create;
end;

destructor TORAXMLContext.Destroy;
begin
	DisposeBuffers;
  FBuffers.Free;
	OraXML.xmlterm(Fxmlctx);
  Fxmlctx := nil;
	inherited;
end;

procedure TORAXMLContext.DisposeBuffers;
var
	I: Integer;
begin
	for I := 0 to FBuffers.Count - 1 do
  	StrDispose(FBuffers[I]);
end;

function TORAXMLContext.GetContext: Pxmlctx;
begin
	Result := Fxmlctx;
end;

{ TXMLCursor }

constructor TXMLCursor.Create;
resourcestring
	StdXMLTLB = 'StdXML.tlb';
{$IFDEF MSWINDOWS}
var
  StdXMLTypeLib: ITypeLib;
{$ENDIF}
begin
	try
{$IFDEF MSWINDOWS}
	  OleCheck(LoadTypeLib(PWideChar(WideString(StdXMLTLB)), StdXMLTypeLib));
    inherited Create(StdXMLTypeLib, IXMLCursor);
{$ENDIF}
{$IFDEF LINUX}
		inherited;
{$ENDIF}
	  FCurrentNodeList := TList.Create;
    FPosition := -1;
    FEOF := True;
  except
    reraise('TXMLCursor.Create');
  end;
end;

destructor TXMLCursor.Destroy;
begin
	FORAXMLContext := nil;
  FCurrentNode := nil;
  FNodeList := nil;
  FCurrentNodeList.Free;
  inherited;
end;

{$IFDEF LINUX}
function TXMLCursor.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
	Result := E_NOTIMPL;
end;

function TXMLCursor.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
	Result := E_NOTIMPL;
end;

function TXMLCursor.GetTypeInfoCount(out Count: Integer): HResult;
begin
	Result := E_NOTIMPL;
end;

function TXMLCursor.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
begin
	Result := E_NOTIMPL;
end;
{$ENDIF}

function TXMLCursor.AppendChild(const ElementName, Value: WideString): IXMLCursor;
var
	BufferElement, BufferValue: PChar;
  DocumentNode: Pxmlnode;
  Element: Pxmlnode;
  NewCursor: TXMLCursor;
  NodeList: Pxmlnodes;
  SElementName: string;
  SValue: string;
begin
	Result := nil;
  if FORAXMLContext = nil then
  begin
    FORAXMLContext := TORAXMLContext.Create;
    SElementName := ElementName;
    BufferElement := StrAlloc(Length(ElementName) + 1);
    StrPCopy(BufferElement, ElementName);
    FORAXMLContext.AddBuffer(BufferElement);
    SValue := Value;
    Element := OraXML.createElement(xmlctx, BufferElement);
		DocumentNode := OraXML.getDocument(xmlctx);
    if DocumentNode = nil then
    	raise Exception.Create('TXMLCursor.AppendChild - DocumentNode is nil');
    OraXML.appendChild(xmlctx, DocumentNode, Element);
    if SValue <> '' then
    begin
	  	BufferValue := StrAlloc(Length(SValue) + 1);
  	  FORAXMLContext.AddBuffer(BufferValue);
    	StrPCopy(BufferValue, SValue);
    	OraXML.setNodeValue(Element, BufferValue);
    end;
  	FNodeList := OraXML.getChildNodes(DocumentNode);
    CopyElementNodeList(FNodeList, FCurrentNodeList);
  	FindFirstElement;
    NodeList := FNodeList;
  end else begin
    SElementName := ElementName;
    BufferElement := StrAlloc(Length(ElementName) + 1);
    StrPCopy(BufferElement, ElementName);
    FORAXMLContext.AddBuffer(BufferElement);
    SValue := Value;
    if FCurrentNode = nil then
      Exit;
    Element := OraXML.createElement(xmlctx, BufferElement);
    OraXML.appendChild(xmlctx, FCurrentNode, Element);
    if SValue <> '' then
    	OraXML.setNodeValue(Element, PChar(SValue));
    NodeList := OraXML.getChildNodes(FCurrentNode);
    FCurrentNodeList.Add(Element);
  end;
  NewCursor := TXMLCursor.Create;
  NewCursor.FORAXMLContext := FORAXMLContext;
  NewCursor.FNodeList := NodeList;
  AssignList(FCurrentNodeList, NewCursor.FCurrentNodeList);
  NewCursor.Last;
  Result := NewCursor;
end;

procedure TXMLCursor.AppendXMLCursor(const XMLCursor: IXMLCursor);
var
  Container: Pxmlnode;
  I: Integer;
  Node: Pxmlnode;
  ClonedNode: Pxmlnode;
  NodeList: TList;
  XMLCursorInternal: IXMLCursorInternal;
begin
  if XMLCursor = nil then
    Exit;
  if XMLCursor.QueryInterface(IXMLCursorInternal, XMLCursorInternal) <> S_OK then
    Exit;
  if XMLCursorInternal = nil then
    Exit;
  NodeList := XMLCursorInternal.Get_CurrentNodeList;
  if NodeList = nil then
    Exit;
  if NodeList.Count = 0 then
    Exit;
  if FORAXMLContext = nil then
  begin
//    raise Exception.Create('TXMLCursor.AppendXMLCursor - FXMLDOMDocument = nil'); // !!! should handle this case
    // In this case XMLCursor should have only one element
    if NodeList.Count > 1 then
      raise Exception.Create('TXMLCursor.AppendXMLCursor - Cannot add XMLCursor to current since it has more than one element.');
    FORAXMLContext := TORAXMLContext.Create;
    Node := NodeList[0];
    ClonedNode := OraXML.cloneNode(xmlctx, Node, True);
    OraXML.appendChild(xmlctx, FCurrentNode, ClonedNode);
    Exit;
  end;
  if FCurrentNode = nil then
    Exit;
  Container := FCurrentNode;
  for I := 0 to NodeList.Count-1 do
  begin
    Node := NodeList[I];
    ClonedNode := OraXML.cloneNode(xmlctx, Node, True);
    OraXML.appendChild(xmlctx, Container, ClonedNode);
  end;
end;

procedure TXMLCursor.Assign(const CursorSource: TXMLCursor);
begin
  FCurrentNode := CursorSource.FCurrentNode;
  FNodeList := CursorSource.FNodeList;
  AssignList(CursorSource.FCurrentNodeList, FCurrentNodeList);
  FPosition := CursorSource.FPosition;
  FORAXMLContext := CursorSource.FORAXMLContext;
end;

procedure TXMLCursor.CopyNodeList(Nodes: Pxmlnodes;
  NodeList: TList);
var
	Count: Integer;
	I: Integer;
  Node: Pxmlnode;
begin
  if Nodes = nil then
  	Exit;
	if NodeList = nil then
  	Exit;
  NodeList.Clear;
  Count := OraXML.numChildNodes(Nodes);
	for I := 0 to Count - 1 do
  begin
  	Node := OraXML.getChildNode(Nodes, I);
  	if OraXML.getNodeType(Node) = OraXML.ELEMENT_NODE then
	    NodeList.Add(Node);
  end;
end;

procedure TXMLCursor.CopyElementNodeList(Nodes: Pxmlnodes; NodeList: TList);
var
	Count: Integer;
	I: Integer;
begin
	if NodeList = nil then
  	Exit;
  NodeList.Clear;
  if Nodes = nil then
  	Exit;
  Count := OraXML.numChildNodes(Nodes);
	for I := 0 to Count - 1 do
    NodeList.Add(OraXML.getChildNode(Nodes, I));
end;

function TXMLCursor.ContainerXML: WideString;
var
	ParentNode: Pxmlnode;
begin
  Result := '';
  if FCurrentNode = nil then
    Exit;
  ParentNode := OraXML.getParentNode(FCurrentNode);
  if ParentNode = nil then
    Result := XML
  else
    Result := OraXML.FormatXML(ParentNode, True);
end;

function TXMLCursor.Count: Integer;
begin
  Result := 0;
  if FCurrentNodeList = nil then
    Exit;
  Result := FCurrentNodeList.Count;
end;

procedure TXMLCursor.Delete;
var
  ParentNode: Pxmlnode;
begin
  if FCurrentNode = nil then
    Exit;
  ParentNode := OraXML.getParentNode(FCurrentNode);;
  if ParentNode = nil then
    Exit;
  OraXML.removeChild(FCurrentNode);
  FNodeList := OraXML.getChildNodes(ParentNode);
  // ????
  CopyElementNodeList(FNodeList, FCurrentNodeList);
  if FNodeList = nil then
  begin
    FPosition := -1;
    FCurrentNode := nil;
    FEOF := True;
    Exit;
  end;
  if FPosition >= Count then
    FPosition := Count-1;
  FCurrentNode := FCurrentNodeList[FPosition];
end;

function TXMLCursor.Document: IXMLCursor;
var
  NewCursor: TXMLCursor;
  DocumentNode: Pxmlnode;
begin
  if FORAXMLContext = nil then
    Exit;
  NewCursor := TXMLCursor.Create;
  NewCursor.FORAXMLContext := FORAXMLContext;
	DocumentNode := OraXML.getDocument(xmlctx);
  if DocumentNode <> nil then
  begin
	  NewCursor.FNodeList := OraXML.getChildNodes(DocumentNode);
    CopyElementNodeList(NewCursor.FNodeList, NewCursor.FCurrentNodeList);
  end;
  NewCursor.FindFirstElement;
  Result := NewCursor;
end;

function TXMLCursor.EOF: WordBool;
begin
  Result := FEOF or (FPosition = -1);
end;

procedure TXMLCursor.Filter(XPath: string);

	function LastPos(const C: Char; const S: string): Integer;
  var
  	I: Integer;
  begin
  	Result := 0;
		for I := Length(S) downto 1 do
    	if S[I] = C then
      begin
      	Result := I;
      	Exit;
      end;
  end;

  procedure FilterOnNodeName(NodeName: string);
  var
  	Count: Integer;
  	I: Integer;
    Node: Pxmlnode;
  begin
  	NodeName := Trim(NodeName);
  	FCurrentNodeList.Clear;
		Count := OraXML.numChildNodes(FNodeList);
    for I := 0 to Count - 1 do
    begin
    	Node := OraXML.getChildNode(FNodeList, I);
    	if AnsiCompareText(OraXML.getNodeName(Node), NodeName) = 0 then
      	FCurrentNodeList.Add(Node);
    end;
  end;

  procedure FilterOnValue(PosBracket: Integer; XPath: string);
  	function UnquoteStr(S: string): string;
    var
    	PosQuote: Integer;
    begin
			PosQuote := Pos('''', S);
    	while PosQuote > 0 do
      begin
      	System.Delete(S, PosQuote, 1);
				PosQuote := Pos('''', S);
      end;
			PosQuote := Pos('"', S);
    	while PosQuote > 0 do
      begin
      	System.Delete(S, PosQuote, 1);
				PosQuote := Pos('"', S);
      end;
      Result := Trim(S);
    end;
  var
  	Count: Integer;
  	I: Integer;
    FilterName: string;
    FilterValue: string;
    Node: Pxmlnode;
    ElementName: string;
    PosOperator: Integer;
    NodeName: string;
    NodeValue: string;
  begin
  	FCurrentNodeList.Clear;
    ElementName := Trim(Copy(XPath, 1, PosBracket - 1));
    PosOperator := Pos('=', XPath);
    FilterName := Trim(Copy(XPath, PosBracket + 1, PosOperator - PosBracket - 1));
    if FilterName = '' then
    	Exit;
    FilterValue := Trim(Copy(XPath, PosOperator + 1, Pos(']', XPath) - PosOperator - 1));
    FilterValue := UnquoteStr(FilterValue);
    if FilterValue = '' then
    	Exit;
		Count := OraXML.numChildNodes(FNodeList);
    for I := 0 to Count - 1 do
    begin
    	Node := OraXML.getChildNode(FNodeList, I);
      NodeName := OraXML.getNodeName(Node);
    	if (AnsiCompareText(NodeName, ElementName) = 0) then
      begin
      	NodeValue := InternalGetValue(Node, FilterName);
      	if (AnsiCompareText(NodeValue, FilterValue) = 0) then
	      	FCurrentNodeList.Add(Node);
    	end;
    end;
  end;

var
	Res: Boolean;
	ParentNodePath: string;
	PosLastSlash: Integer;
  PosBracket: Integer;
  temp: string;
begin
  if FCurrentNode = nil then
  begin
    First;
    Exit;
  end;
  temp := OraXML.getNodeName(FCurrentNode);
	// 1. Search Parent Node
  PosLastSlash := LastPos('/', XPath);
	if PosLastSlash > 0 then
  begin
  	// go to the parentNode
    ParentNodePath := Copy(XPath, 1, PosLastSlash - 1);
    if Pos('/', XPath) = 1 then
    	Res := MoveInAbsolutePath(ParentNodePath)
    else
    	Res := MoveInRelativePath(ParentNodePath);
    if not Res then // bad Xpath
    begin
    	FNodeList := nil;
      FCurrentNodeList.Clear;
      Exit;
    end;
    System.Delete(XPath, 1, PosLastSlash);
	end;
  FNodeList := OraXML.getChildNodes(FCurrentNode);
  // 2. filter XPath
  if Pos('*', XPath) = 1 then
  begin
	  CopyElementNodeList(FNodeList, FCurrentNodeList);
    Exit;
  end;
  PosBracket := Pos('[', XPath);
  if PosBracket = 0 then
  begin
  	FilterOnNodeName(XPath);
  	Exit;
  end;
  FilterOnValue(PosBracket, XPath);
end;

procedure TXMLCursor.FindFirstElement;
var
	ChildNode: Pxmlnode;
  NodeType: Integer;
	I, Count: Integer;
begin
	if FCurrentNodeList = nil then
  	Exit;
  Count := FCurrentNodeList.Count;
  FPosition := -1;
  FEOF := True;
  for I := 0 to Count-1 do
  begin
		ChildNode := FCurrentNodeList[I];
    NodeType := OraXML.getNodeType(ChildNode);
    if (NodeType = ELEMENT_NODE) then
    begin
    	FPosition := I;
      FEOF := False;
		  FCurrentNode := ChildNode;
      Exit;
    end;
  end;
end;

function TXMLCursor.InternalCloneNode(Node: Pxmlnode; Deep: Boolean): Pxmlnode;
	function CopyNode(Node: Pxmlnode): Pxmlnode;
  var
    NodeName: PChar;
    SNodeName: string;
  begin
    SNodeName := OraXML.getNodeName(Node);
    NodeName := Stralloc(Length(SNodeName) + 1);
    StrPCopy(NodeName, SNodeName);
    FORAXMLContext.AddBuffer(NodeName);
    Result := OraXML.createElement(xmlctx, NodeName);
  end;

  procedure CloneChildren(SourceNode, RootNode: Pxmlnode);
  var
  	ChildNode: Pxmlnode;
    ClonedNode: Pxmlnode;
    Count: Integer;
    I: Integer;
  	NodeList: Pxmlnodes;
    SValue: string;
    Value: PChar;
  begin
    NodeList := OraXML.getChildNodes(SourceNode);
    Count := OraXML.numChildNodes(NodeList);
		for I := 0 to Count - 1 do
    begin
      ChildNode := OraXML.getChildNode(NodeList, I);
    	if OraXML.getNodeType(ChildNode) = TEXT_NODE then
			begin
        SValue := OraXML.getNodeValue(ChildNode);
        if SValue <> '' then
        begin
          Value := StrAlloc(Length(SValue) + 1);
          StrPCopy(Value, SValue);
          FORAXMLContext.AddBuffer(Value);
          ClonedNode := OraXML.createTextNode(xmlctx, Value);
          OraXML.appendChild(xmlctx, RootNode, ClonedNode);
        end;
      end;
    	if OraXML.getNodeType(ChildNode) = ELEMENT_NODE then
      begin
        ClonedNode := CopyNode(ChildNode);
        OraXML.appendChild(xmlctx, RootNode, ClonedNode);
        CloneChildren(ChildNode, ClonedNode);
      end;
    end;
  end;

begin
	Result := CopyNode(Node);
  if Deep then
  	CloneChildren(Node, Result);
end;

function TXMLCursor.InternalGetValue(CurrentNode: Pxmlnode; XPath: string): string;

	function LastPos(const C: Char; const S: string): Integer;
  var
  	I: Integer;
  begin
  	Result := 0;
		for I := Length(S) downto 1 do
    	if S[I] = C then
      begin
      	Result := I;
      	Exit;
      end;
  end;

var
	Index: Integer;
	Node: Pxmlnode;
	NodeName: string;
  ParentNodePath: string;
	PosLastSlash: Integer;
  Res: Boolean;
  TextNode: Pxmlnode;
begin
	// 1. Search Parent Node
  PosLastSlash := LastPos('/', XPath);
	if PosLastSlash > 0 then
  begin
  	// go to the parentNode
    ParentNodePath := Copy(XPath, 1, PosLastSlash - 1);
    if Pos('/', XPath) = 1 then
    	Res := MoveInAbsolutePath(ParentNodePath)
    else
    	Res := MoveInRelativePath(ParentNodePath);
    if not Res then // bad Xpath
    begin
    	FNodeList := nil;
      FCurrentNodeList.Clear;
      Exit;
    end;
    System.Delete(XPath, 1, PosLastSlash);
    CurrentNode := FCurrentNode;
	end;
	Result := '';
  if XPath = '.' then
  begin
    TextNode := OraXML.getFirstChild(CurrentNode);
    if TextNode = nil then
      Exit;
    Result := OraXML.getNodeValue(TextNode);
  	Exit;
  end;
	if Pos('@', XPath) = 1 then
  begin
  	System.Delete(XPath, 1, 1);
	  Result := OraXML.getAttribute(CurrentNode, PChar(XPath));
    Exit;
  end;

  NodeName := OraXML.getNodeName(CurrentNode);
	Node := OraXML.getNamedItem(OraXML.getChildNodes(CurrentNode), PChar(XPath), @Index);
  if Node = nil then
    Exit;
  TextNode := OraXML.getFirstChild(Node);
  if TextNode = nil then
  	Exit;
  Result := OraXML.getNodeValue(TextNode);
end;

function TXMLCursor.MoveInRelativePath(ParentNodePath: string): Boolean;
var
  Index: Integer;
  CurrentNode: Pxmlnode;
  Node: Pxmlnode;
  NodeList: Pxmlnodes;
  temp: string;
begin
  NodeList := OraXML.getChildNodes(FCurrentNode);;
  while Pos('/', ParentNodePath) > 0 do
  begin
    temp := Copy(ParentNodePath, 1, Pos('/', ParentNodePath) - 1);
    Node := OraXML.getNamedItem(NodeList, PChar(temp), @Index);
    if Node = nil then // Bad Xpath
    begin
      Result := False;
      Exit;
    end;
    CurrentNode := Node;
    NodeList := OraXML.getChildNodes(CurrentNode);
    System.Delete(ParentNodePath, 1, Pos('/', ParentNodePath));  // with '/'
  end;
  // only one node name
  if ParentNodePath <> '' then
  begin
    Node := OraXML.getNamedItem(NodeList, PChar(ParentNodePath), @Index);
    if Node = nil then // Bad Xpath
    begin
      Result := False;
      Exit;
    end;
    FCurrentNode := Node;
  end;
  Result := True;
end;

function TXMLCursor.MoveInAbsolutePath(ParentNodePath: string): Boolean;
var
  CurrentNode: Pxmlnode;
  NodeList: Pxmlnodes;
begin
  CurrentNode := FCurrentNode;
  NodeList := FNodeList;
  FCurrentNode := OraXML.getDocument(xmlctx);
  FNodeList := OraXML.getChildNodes(FCurrentNode);
  System.Delete(ParentNodePath, 1, 1);
  if not MoveInRelativePath(ParentNodePath) then
  begin
    FCurrentNode := CurrentNode;
    FNodeList := NodeList;
    Result := False;
  end
  else
    Result := True;
end;

procedure TXMLCursor.First;
begin
  MoveTo(0);
end;

function TXMLCursor.Get_CurrentNodeList: TList;
begin
//  Result := FCurrentNodeList;
	Result := FCurrentNodeList;
end;

function TXMLCursor.GetName: WideString;
begin
  Result := '';
  if FCurrentNode = nil then
    Exit;
  Result := OraXML.getNodeName(FCurrentNode);
end;

function GetElement(Node: Pxmlnode; const Name: string; Index: Integer): Pxmlnode;
var
	Elements: Pxmlnodes;
  ChildNode: Pxmlnode;
  Count: Integer;
  FoundIndex: Integer;
  I: Integer;
  NodeType: Integer;
  NodeName: string;
begin
  FoundIndex := -1;
  Elements := OraXML.getChildNodes(Node);
  Count := OraXML.numChildNodes(Elements);
  for I := 0 to Count-1 do
  begin
		ChildNode := OraXML.getChildNode(Elements, I);
    NodeType := OraXML.getNodeType(ChildNode);
    NodeName := OraXML.getNodeName(ChildNode);
    if (NodeType = ELEMENT_NODE) and (NodeName = Name) then
    begin
      Inc(FoundIndex);
      if FoundIndex >= Index then
      begin
        Result := ChildNode;
        Exit;
      end;
    end;
  end;
  Result := nil;
end;


function TXMLCursor.GetValue(const XPath: WideString): WideString;
begin
  Result := '';
  if FCurrentNode = nil then
    Exit;
  // Filter on XPath
  Result := InternalGetValue(FCurrentNode, XPath);
end;

function TXMLCursor.Get_Values(const Name: WideString): WideString;
begin
	Result := GetValue(Name);  
end;

function TXMLCursor.InsertBefore(const ElementName, Value: WideString): IXMLCursor;
var
  BufferElement, BufferValue: PChar;
  Element: Pxmlnode;
  NewCursor: TXMLCursor;
  Node, ParentNode: Pxmlnode;
  NodeList: Pxmlnodes;
  Position: Integer;
	SElementName: string;
  SValue: string;
begin
	Result := nil;
	SElementName := ElementName;
  SValue := Value;
  if FORAXMLContext = nil then
  begin
    Result := AppendChild(ElementName, Value);
    Exit;
  end;
  if FCurrentNode = nil then
    Exit;
  ParentNode := OraXML.getParentNode(FCurrentNode);
  if ParentNode = nil then
    Exit;
  BufferElement := StrAlloc(Length(ElementName) + 1);
  FORAXMLContext.AddBuffer(BufferElement);
  StrPCopy(BufferElement, SElementName);
  Element := OraXML.createElement(xmlctx, BufferElement);
  OraXML.insertBefore(xmlctx, ParentNode, Element, FCurrentNode);
  if SValue <> '' then
  begin
    BufferValue := StrAlloc(Length(SValue) + 1);
    FORAXMLContext.AddBuffer(BufferValue);
    StrPCopy(BufferValue, SValue);
    OraXML.setNodeValue(Element, BufferValue);
  end;
  NewCursor := TXMLCursor.Create;
  NewCursor.FORAXMLContext := FORAXMLContext;
  NodeList := OraXML.getChildNodes(ParentNode);
  NewCursor.FNodeList := NodeList;
  CopyElementNodeList(NodeList, NewCursor.FCurrentNodeList);
  Node := FCurrentNode;
  // retrieve the absolute position of FCurrentNode in parentNode.childNodes
  Position := 0;
  while True do
  begin
    Node := OraXML.getPreviousSibling(Node);
    if Node = nil then
      Break;
    Inc(Position);
  end;
  if Position > 0 then // except when creation of the document
    Dec(Position);
  NewCursor.MoveTo(Position);
  Result := NewCursor;
end;

function TXMLCursor.InsertAfter(const ElementName, Value: WideString): IXMLCursor;
var
  BufferElement, BufferValue: PChar;
  NewCursor: TXMLCursor;
  Element: Pxmlnode;
  Node, ParentNode: Pxmlnode;
  NodeList: Pxmlnodes;
  Position: Integer;
	SElementName: string;
  SValue: string;
begin
	Result := nil;
	SElementName := ElementName;
  SValue := Value;
  if FORAXMLContext = nil then
  begin
    Result := AppendChild(ElementName, Value);
    Exit;
  end;
  if FCurrentNode = nil then
    Exit;
  ParentNode := OraXML.getParentNode(FCurrentNode);
  if ParentNode = nil then
    Exit;
  BufferElement := StrAlloc(Length(ElementName) + 1);
  FORAXMLContext.AddBuffer(BufferElement);
  StrPCopy(BufferElement, SElementName);
  Element := OraXML.createElement(xmlctx, BufferElement);
  Node := OraXML.getNextSibling(FCurrentNode);;
  if Node = nil then
  	OraXML.appendChild(xmlctx, ParentNode, Element)
  else
  	OraXML.insertBefore(xmlctx, ParentNode, Element, Node);
  if SValue <> '' then
  begin
    BufferValue := StrAlloc(Length(SValue) + 1);
    FORAXMLContext.AddBuffer(BufferValue);
    StrPCopy(BufferValue, SValue);
    OraXML.setNodeValue(Element, BufferValue);
  end;
  NewCursor := TXMLCursor.Create;
  NewCursor.FORAXMLContext := FORAXMLContext;
  NodeList := OraXML.getChildNodes(ParentNode);
  NewCursor.FNodeList := NodeList;
  CopyElementNodeList(NodeList, NewCursor.FCurrentNodeList);
  Node := FCurrentNode;
  // retrieve the absolute position of FCurrentNode in parentNode.childNodes
  Position := 0;
  while True do
  begin
    Node := OraXML.getPreviousSibling(Node);
    Inc(Position);
    if Node = nil then
      Break;
  end;
  NewCursor.MoveTo(Position);
  Result := NewCursor;
end;

procedure TXMLCursor.Last;
begin
  MoveTo(Count-1);
end;

procedure ReadStringFromFile(var S: string; const FileName: string);
var
  Stream: TStream;
begin
  try
    Stream := TFileStream.Create(FileName, fmOpenRead);
    try
      SetLength(S, Stream.Size);
      Stream.ReadBuffer(PChar(S)^, Stream.Size);
    finally
      Stream.Free;
    end;
  except
    reraise('XMLUtils.ReadStringFromFile - FileName='+FileName);
    raise;
  end;
end;

procedure TXMLCursor.Load(const FileName: WideString);
var
	XMLBuffer: string;
begin 
  ReadStringFromFile(XMLBuffer, FileName);
  LoadXML(XMLBuffer);
end;

procedure TXMLCursor.LoadXML(const SourceXML: WideString);
var
	Err: uword;
  SourceString: string;
  DocumentNode: Pxmlnode;
begin
  if FORAXMLContext = nil then
  begin
  	FORAXMLContext := TORAXMLContext.Create;
  end;

  SourceString := SourceXML;
  Err := OraXML.xmlparsebuf(xmlctx, PChar(SourceString), Length(SourceString), nil,
  						XML_FLAG_VALIDATE or XML_FLAG_DISCARD_WHITESPACE);
  OraXML.XMLCheck(Err, 'TXMLCursor.LoadXML - xmlparsebuf');

  FCurrentNode := nil;
  DocumentNode := OraXML.getDocument(xmlctx);
  if DocumentNode <> nil then
  begin
  	FNodeList := OraXML.getChildNodes(DocumentNode);
  	CopyElementNodeList(FNodeList, FCurrentNodeList);
  	FindFirstElement;
  end;
end;

procedure TXMLCursor.MoveTo(Index: Integer);
var
  VCount: Integer;
begin
  FPosition := -1;
  FEOF := True;
  FCurrentNode := nil;
  if FNodeList = nil then
    Exit;
  VCount := Count;
  if VCount = 0 then
    Exit;
  if (Index < 0) or (Index >= VCount) then
    raise Exception.Create('TXMLCursor.MoveTo - Index out of range: '+IntToStr(Index)+'/'+IntToStr(VCount));
  FPosition := Index;
  FEOF := False;
  FCurrentNode := FCurrentNodeList[FPosition];
end;

procedure TXMLCursor.Next;
begin
  if (Count-1 = FPosition) or (FNodeList = nil) then
  begin
    FEOF := True;
    Exit;
  end;
  Inc(FPosition);
  FCurrentNode := FCurrentNodeList[FPosition];
end;

function TXMLCursor.RecNo: Integer;
begin
  Result := FPosition;
end;

procedure TXMLCursor.ReplaceWithXMLCursor(const XMLCursor: IXMLCursor);
var
  ClonedNode: Pxmlnode;
  I: Integer;
  NodeList: TList;
  Node, ParentNode: Pxmlnode;
  XMLCursorInternal: IXMLCursorInternal;
begin
  if FORAXMLContext = nil then
    raise Exception.Create('TXMLCursor.ReplaceWithXMLCursor - FXMLDOMDocument = nil');
  if FCurrentNode = nil then
    Exit;
  ParentNode := OraXML.getParentNode(FCurrentNode);
  if ParentNode = nil then
    Exit;
  if XMLCursor = nil then
    Exit;
  if XMLCursor.QueryInterface(IXMLCursorInternal, XMLCursorInternal) <> S_OK then
    Exit;
  if XMLCursorInternal = nil then
    Exit;
  NodeList := XMLCursorInternal.Get_CurrentNodeList;
  for I := 0 to NodeList.Count-1 do
  begin
  	Node := NodeList[I];
    ClonedNode := InternalCloneNode(Node, True);
    OraXML.insertBefore(xmlctx, ParentNode, ClonedNode, FCurrentNode);
  end;
  OraXML.removeChild(FCurrentNode);
  FNodeList := OraXML.getChildNodes(ParentNode);
  CopyElementNodeList(FNodeList, FCurrentNodeList);
  FCurrentNode := nil;
  First; // Reset FPosition
end;

procedure WriteStringToFile(const S: string; const FileName: string);
var
  Stream: TStream;

  procedure CheckDir(DirName: string);
  begin
    if DirName = '' then
      Exit;
    if not DirectoryExists(DirName) then
      ForceDirectories(DirName);
  end;

begin
  try
    CheckDir(ExtractFileDir(FileName));
    if FileExists(FileName) then
    begin
      Stream := TFileStream.Create(FileName, fmOpenReadWrite);  // !!! Need to lock for exclusive access fmShareExclusive
      Stream.Size := 0;
    end
    else
      Stream := TFileStream.Create(FileName, fmCreate);
    try
      Stream.WriteBuffer(PChar(S)^, Length(S));
    finally
      Stream.Free;
    end;
  except     
    reraise('XMLUtils.WriteStringToFile - FileName='+FileName);
  end;
end;

procedure TXMLCursor.Save(const FileName: WideString);
var
  Path: string;
  XMLBuffer: string;
begin
  if FORAXMLContext <> nil then
  begin
    Path := ExtractFilePath(FileName);
    ForceDirectories(Path);
  	XMLBuffer := FormatXML(OraXML.getDocument(xmlctx), True);
//    WriteXMLToFile(XMLBuffer, FileName);
    WriteStringToFile(XMLBuffer, FileName);
  end;
end;

function TXMLCursor.Select(const XPath: WideString): IXMLCursor;
var
  XMLCursor: TXMLCursor;
begin
  XMLCursor := TXMLCursor.Create;
  Result := XMLCursor;
  XMLCursor.FORAXMLContext := FORAXMLContext;
  XMLCursor.FXPath := XPath;
  XMLCursor.FCurrentNode := FCurrentNode;
  XMLCursor.FNodeList := FNodeList;
  XMLCursor.Filter(XPath);
  XMLCursor.First;
end;

procedure TXMLCursor.SetAttributeValue(const AttributeName, Value: WideString);
var
  BufferAttributeName: PChar;
  BufferAttributeValue: PChar;
  SAttributeName: string;
  SValue: string;
begin
  if FCurrentNode = nil then
    Exit;
  SAttributeName := AttributeName;
  SValue := Value;
  BufferAttributeName := StrAlloc(Length(SAttributeName) + 1);
  FORAXMLContext.AddBuffer(BufferAttributeName);
  StrPCopy(BufferAttributeName, SAttributeName);

  BufferAttributeValue := StrAlloc(Length(SValue) + 1);
  FORAXMLContext.AddBuffer(BufferAttributeValue);
  StrPCopy(BufferAttributeValue, SValue);
  OraXML.setAttribute(xmlctx, FCurrentNode, BufferAttributeName, BufferAttributeValue);
end;

procedure TXMLCursor.SetValue(const ElementName, Value: WideString);
var
  Buffer: PChar;
  Index: Integer;
  Node: Pxmlnode;
	SElementName: string;
  SValue: string;

  procedure SetText(Node: Pxmlnode);
  var
	  Buffer: PChar;
  	TextNode: Pxmlnode;
  begin
    TextNode := OraXML.getFirstChild(Node);
    Buffer := StrAlloc(Length(SValue) + 1);
    FORAXMLContext.AddBuffer(Buffer);
    StrPCopy(Buffer, SValue);
    if TextNode = nil then
    begin
      TextNode := OraXML.createTextNode(xmlctx, Buffer);
      OraXML.appendChild(xmlctx, Node, TextNode);
    end else begin
      OraXML.setNodeValue(TextNode, Buffer);
    end;
  end;

begin
  if FCurrentNode = nil then
    Exit;
  SValue := Value;
	SElementName := ElementName;
  if Pos('@', SElementName) = 1 then
  begin
  	System.Delete(SElementName, 1, 1);
	  SetAttributeValue(SElementName, Value);
    Exit;
  end;
	if Pos('.', SElementName) = 1 then
  begin
  	SetText(FCurrentNode);
  	Exit;
  end;
	Node := OraXML.getNamedItem(OraXML.getChildNodes(FCurrentNode), PChar(SElementName), @Index);
  if Node = nil then
  begin
  	Buffer := StrAlloc(Length(SElementName) + 1);
    FORAXMLContext.AddBuffer(Buffer);
    StrPCopy(Buffer, SElementName);
    Node := OraXML.createElement(xmlctx, Buffer);
    OraXML.appendChild(xmlctx, FCurrentNode, Node);
  end;
  SetText(Node);
end;

procedure	TXMLCursor.SetCValue(const ElementName, Value: WideString);
begin
end;

function TXMLCursor.XML: WideString;
begin
	Result := '';
  if FCurrentNode = nil then
  	Exit;
  Result := OraXML.FormatXML(FCurrentNode, True);
end;

procedure TXMLCursor.Set_Values(const Name: WideString; const Value: WideString);
begin
	SetValue(Name, Value);
end;

function TXMLCursor.xmlctx: Pxmlctx;
begin
	Result := nil;
  if FORAXMLContext = nil then
  	Exit;
	Result := FORAXMLContext.GetContext;
end;

function TXMLCursor.XMLDOMDocument: IUnknown;
begin
	Result := nil;
end;

function TXMLCursor.XMLDOMNode: IUnknown;
begin
	Result := nil;
end;

function TXMLCursor.ORAXMLContext: Pointer;
begin
	Result := xmlctx;
end;

// !!! Do not use this function for multi-platform applications
function TXMLCursor.ORAXMLNode: Pointer;
begin
	Result := FCurrentNode;
end;

////////////////////////////////////////////////////////////////////////////////
// TXSLProc
////////////////////////////////////////////////////////////////////////////////


constructor TXSLProc.Create;
resourcestring
	StdXMLTLB = 'StdXML.tlb';
{$IFDEF MSWINDOWS}
var
  StdXMLTypeLib: ITypeLib;
{$ENDIF}
begin
	try
{$IFDEF MSWINDOWS}
	  OleCheck(LoadTypeLib(PWideChar(WideString(StdXMLTLB)), StdXMLTypeLib));
    inherited Create(StdXMLTypeLib, IXSLProc);
{$ENDIF}
{$IFDEF LINUX}
		inherited;
{$ENDIF}
    FFiles := TStringList.Create;
  except
    reraise('TXSLProc.Create');
  end;
end;

destructor TXSLProc.Destroy;
begin
  FXSLContext := nil;
	FFiles.Free;
	inherited;
end;

{$IFDEF LINUX}
function TXSLProc.GetIDsOfNames(const IID: TGUID; Names: Pointer;
  NameCount, LocaleID: Integer; DispIDs: Pointer): HResult;
begin
	Result := E_NOTIMPL;
end;

function TXSLProc.GetTypeInfo(Index, LocaleID: Integer;
  out TypeInfo): HResult;
begin
	Result := E_NOTIMPL;
end;

function TXSLProc.GetTypeInfoCount(out Count: Integer): HResult;
begin
	Result := E_NOTIMPL;
end;

function TXSLProc.Invoke(DispID: Integer; const IID: TGUID;
  LocaleID: Integer; Flags: Word; var Params; VarResult, ExcepInfo,
  ArgErr: Pointer): HResult;
begin
	Result := E_NOTIMPL;
end;
{$ENDIF}

function TXSLProc.IsUpToDate: WordBool;
var
  CurrentAge, PreviousAge: Integer;
  I: Integer;
begin
	Result := True;
  try
    for I := 0 to FFiles.Count-1 do
    begin
      CurrentAge := FileAge(FFiles[I]);
      PreviousAge := Integer(FFiles.Objects[I]);
      Result := CurrentAge = PreviousAge;
      if Result = False then
        Exit;
    end;
  except on E: Exception do
    raise Exception.Create('TXSLProc.IsUpToDate - FileName='+FFileName+#13#10+E.Message);
  end;
end;

procedure TXSLProc.Load(const AFileName: WideString);

  // adds the list of imported files to watch to FilesToWatch
  {
  procedure ProcessImports(Source: IXMLDOMDocument);
  var
    ChildCount: Integer;
    ChildNode: IXMLDOMNode;
    ChildNodes: IXMLDOMNodeList;
    CurrentAge: Integer;
    HrefNode: IXMLDOMNode;
    I: Integer;
    ImportedDocument: IXMLDOMDocument;
    ImportFileName: string;
  begin
    try
      ChildNodes := Source.documentElement.childNodes;
      ChildCount := ChildNodes.length;

      for I := 0 to ChildCount-1 do
      begin
        ChildNode := ChildNodes.item[I];
        if (ChildNode.nodeName <> 'xsl:import') and
            (ChildNode.nodeName <> 'xsl:include') then
          Continue;

        HrefNode := ChildNode.attributes.getNamedItem('href');
        if HrefNode = nil then
          raise Exception.Create('href attribute missing on xsl:import/xsl:include');
        ImportFileName := HrefNode.text;
        if ImportFileName = '' then
          raise Exception.Create('href attribute empty on xsl:import / xsl:include');
        if ImportFileName[1] <> '/' then
          ImportFileName := ExtractFilePath(AFileName)+ImportFileName;
        ImportFileName := UnixPathToDosPath(ImportFileName);
        ImportedDocument := CoFreeThreadedDOMDocument.Create;
        ImportedDocument.Load(ImportFileName);
		    CurrentAge := FileAge(ImportFileName);
        FFiles.AddObject(ImportFileName, pointer(CurrentAge));
        ProcessImports(ImportedDocument);
      end;
    except on E: Exception do
	    raise Exception.Create('TXSLProc.Load.ProcessImports - XSL processing - '+ImportFileName+#13#10+E.Message);
    end;
  end;
  }
resourcestring
  SEmptyXSLDocument = 'XSL document is empty.';
  SXSLError = 'Error loading XSL file.';
var
	CurrentAge: Integer;
  Err: uword;
begin
	try
		FFileName := AFileName;
    if FXSLContext <> nil then
    	FXSLContext := nil;
    FXSLContext := TORAXMLContext.Create;
    Err := OraXML.xmlparse(FXSLContext.GetContext, PChar(FFileName), nil, XML_FLAG_VALIDATE or XML_FLAG_DISCARD_WHITESPACE);
    if Err <> 0 then
    	raise Exception.Create(SXSLError);
    CurrentAge := FileAge(AFileName);
    FFiles.AddObject(AFileName, Pointer(CurrentAge));
//    ProcessImports(XSLDocument);
	except on E: Exception do
    raise Exception.Create('TXSLProc.Load - '+FFileName+#13#10+E.Message);
  end;
end;

function TXSLProc.Process(const Document: IXMLCursor): WideString;
var
	SResult: string;

  procedure PurgeResult;
  const
    SUTF16 = '<META http-equiv="Content-Type" content="text/html; charset=UTF-16">';
  var
    Index: Integer;
  begin
    Index := Pos(SUTF16, SResult);
    if Index = 0 then
      Exit;
    Delete(SResult, Index, Length(SUTF16));
  end;

resourcestring
	SXSLProcessError = 'Process XSL Error';
var
	Buffer: WideString;
  Cursor: IXMLCursor;
  Err: uword;
  ResultNode: Pxmlnode;
  XSLResult: IORAXMLContext;
  XMLCursorInternal: IXMLCursorInternal;
begin
  try
    Buffer := Document.Document.XML;
		Cursor := TXMLCursor.Create;
    Cursor.LoadXML(Buffer);
    XSLResult := TORAXMLContext.Create;
    if Cursor.QueryInterface(IXMLCursorInternal, XMLCursorInternal) <> S_OK then
      raise Exception.Create('Cursor does not support interface XMLCursorInternal');
    if XMLCursorInternal = nil then
      raise Exception.Create('XMLCursorInternal is nil');
    Err := xslprocess(XMLCursorInternal.ORAXMLContext, FXSLContext.GetContext, XSLResult.GetContext, @ResultNode);
    if Err <> 0 then
    	raise Exception.Create(SXSLProcessError);
    PurgeResult;
    Result := FormatXML(ResultNode, True);
  except on E: Exception do
    raise Exception.Create('TXSLProc.Process - XSL processing - '+FFileName+#13#10+E.Message);
  end;
end;

end.


