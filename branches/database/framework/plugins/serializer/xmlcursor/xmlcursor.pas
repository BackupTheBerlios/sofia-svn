unit XMLCursor;

interface

uses Sysutils, Classes, ComObj, 
  StdXML_TLB, MSXML2_TLB;

type
  IXMLCursorInternal = interface
  ['{D9329CA1-C252-11D4-8896-0060087D03E0}']
    function Get_CurrentNodeList: IXMLDOMNodeList;
  end;
  TXMLCursor = class(TAutoIntfObject, IXMLCursor, IXMLCursorInternal)
  public
    FCurrentNode: IXMLDOMNode;
    FCurrentNodeList: IXMLDOMNodeList;
    FEOF: Boolean;
    FPosition: Integer;
    FXMLDOMDocument: IXMLDOMDocument;
    function  ElementContainer: IXMLDOMNode;
  protected
    procedure Assign(const CursorSource: TXMLCursor);
  protected
    function Get_CurrentNodeList: IXMLDOMNodeList;
  protected
  	{ IXMLCursor }
    function  AppendChild(const ElementName: WideString; const Value: WideString): IXMLCursor; safecall;
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
  	function  XMLDOMDocument: IUnknown; safecall; // !!! Do not use this function for multi-platform applications
    function  XMLDOMNode: IUnknown; safecall; // !!! Do not use this function for multi-platform applications
  public
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  TXSLProc = class(TAutoIntfObject, IXSLProc)
 	protected
  	FFileName: string;
    FFiles: TStrings;
	  FXSLTemplate: IXSLTemplate;
  	FXSLProcessor: IXSLProcessor;
 	protected
  	// IXSLProc
    function IsUpToDate: WordBool; safecall;
  	procedure Load(const AFileName: WideString);  safecall;
  	function Process(const Document: IXMLCursor): WideString;  safecall;
  public
  	constructor Create; virtual;
    destructor Destroy; override;
  end;
  
implementation

uses
  FileCtrl,
  Windows, ActiveX, 
  HTTPApp;


procedure ReportDOMParseError(Error: IXMLDOMParseError);
resourcestring
	SErrorMsg = 'Error %d on line %d, char %d in "%s"'#13#10+
			'%s'#13#10+
			'%s';
begin
	if Error.errorCode <> 0 then
 		raise Exception.Create(Format(SErrorMsg, [Error.errorCode, Error.line,
				Error.linePos, Error.url, Error.srcText, Error.reason]));
end;

procedure reraise(Message: string);
var
	E, NewE: Exception;
begin
	E := ExceptObject as Exception;
	NewE := E.ClassType.Create as Exception;
  NewE.Message := Message+#13#10+E.Message;
  raise NewE;
end;


{ TXMLCursor }
constructor TXMLCursor.Create;
resourcestring
	StdXMLTLB = 'StdXML.tlb';
var
  StdXMLTypeLib: ITypeLib;
begin
	try
	  OleCheck(LoadTypeLib(PWideChar(WideString(StdXMLTLB)), StdXMLTypeLib));
    inherited Create(StdXMLTypeLib, IXMLCursor);
    FPosition := -1;
    FEOF := True;
  except
    reraise('TXMLCursor.Create');
  end;
end;

destructor TXMLCursor.Destroy;
begin
  FCurrentNode := nil;
  FCurrentNodeList := nil;
//  if (FXMLDOMDocument <> nil) and (FXMLDOMDocument.documentElement = FCurrentNode) then
      FXMLDOMDocument := nil;
  inherited;
end;

function TXMLCursor.AppendChild(const ElementName, Value: WideString): IXMLCursor;
var
  Element: IXMLDOMElement;
  NewCursor: TXMLCursor;
  NodeList: IXMLDOMNodeList;
begin
	Result := nil;
  if FXMLDOMDocument = nil then
  begin
    FXMLDOMDocument := CoFreeThreadedDOMDocument.Create;
    Element := FXMLDOMDocument.createElement(ElementName);
    FXMLDOMDocument.appendChild(Element);
    if Value <> '' then
      Element.text := Value;
    FCurrentNodeList := FXMLDOMDocument.childNodes;
    NodeList := FCurrentNodeList;
    First;
  end else begin
    if FCurrentNode = nil then
      Exit;
    Element := FXMLDOMDocument.createElement(ElementName);
    FCurrentNode.appendChild(Element);
    if Value <> '' then
      Element.text := Value;
    NodeList := FCurrentNode.childNodes;
  end;
  NewCursor := TXMLCursor.Create;
  NewCursor.FXMLDOMDocument := FXMLDOMDocument;
//  NewCursor.FCurrentNodeList := FCurrentNode.childNodes; // FCurrentNode.selectNodes(ElementName); // !!! Need optimization // !!! Buggy in the case of Document is nil
  NewCursor.FCurrentNodeList := NodeList;
  NewCursor.Last;
  Result := NewCursor;
end;

procedure TXMLCursor.AppendXMLCursor(const XMLCursor: IXMLCursor);
var
  Container: IXMLDOMNode;
  I: Integer;
  Node: IXMLDOMNode;
  NodeList: IXMLDOMNodeList;
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
  if NodeList.length = 0 then
    Exit;
  if FXMLDOMDocument = nil then
  begin
//    raise Exception.Create('TXMLCursor.AppendXMLCursor - FXMLDOMDocument = nil'); // !!! should handle this case
    // In this case XMLCursor should have only one element
    if NodeList.length > 1 then
      raise Exception.Create('TXMLCursor.AppendXMLCursor - Cannot add XMLCursor to current since it has more than one element.');
    FXMLDOMDocument := CoFreeThreadedDOMDocument.Create;
    FXMLDOMDocument.appendChild(NodeList.item[0].cloneNode(True));
    Exit;
  end;
  if FCurrentNode = nil then
    Exit;
  Container := FCurrentNode;
  for I := 0 to NodeList.length-1 do
  begin
    Node := NodeList.item[I].cloneNode(True);
    Container.appendChild(Node);
  end;
end;

procedure TXMLCursor.Assign(const CursorSource: TXMLCursor);
begin
  FCurrentNode := CursorSource.FCurrentNode;
  FCurrentNodeList := CursorSource.FCurrentNodeList;
  FPosition := CursorSource.FPosition;
  FXMLDOMDocument := CursorSource.FXMLDOMDocument;
end;

function TXMLCursor.ContainerXML: WideString;
begin
  Result := '';
  if FCurrentNode = nil then
    Exit;
  if FCurrentNode.parentNode = nil then
    Result := XML
  else
    Result := FCurrentNode.parentNode.xml;
end;

function TXMLCursor.Count: Integer;
begin
  Result := 0;
  if FCurrentNodeList = nil then
    Exit;
  Result := FCurrentNodeList.length;
end;

procedure TXMLCursor.Delete;
var
  ParentNode: IXMLDOMNode;
begin
  if FCurrentNode = nil then
    Exit;
  ParentNode := FCurrentNode.parentNode;
  if ParentNode = nil then
    Exit;
  ParentNode.removeChild(FCurrentNode);
  if FCurrentNodeList = nil then
  begin
    FPosition := -1;
    FCurrentNode := nil;
    FEOF := True;
    Exit;
  end;
  if FPosition >= FCurrentNodeList.length then
    FPosition := FCurrentNodeList.length-1;
  FCurrentNode := FCurrentNodeList.item[FPosition];
end;

function TXMLCursor.Document: IXMLCursor;
var
  NewCursor: TXMLCursor;
begin
  if FXMLDOMDocument = nil then
    Exit;
  NewCursor := TXMLCursor.Create;
  NewCursor.FXMLDOMDocument := FXMLDOMDocument;
  NewCursor.FCurrentNodeList := FXMLDOMDocument.childNodes;
  NewCursor.First;
  Result := NewCursor;
end;

function TXMLCursor.ElementContainer: IXMLDOMNode;
begin
  if FCurrentNode = nil then
    Result := nil
  else
    Result := FCurrentNode.parentNode;
end;

function TXMLCursor.EOF: WordBool;
begin
  Result := FEOF or (FPosition = -1);
end;

procedure TXMLCursor.First;
begin
  MoveTo(0);
end;

function TXMLCursor.Get_CurrentNodeList: IXMLDOMNodeList;
begin
  Result := FCurrentNodeList;
end;

function TXMLCursor.GetName: WideString;
begin
  Result := '';
  if FCurrentNode = nil then
    Exit;
  Result := FCurrentNode.nodeName;
end;

function TXMLCursor.GetValue(const XPath: WideString): WideString;
var
  Node: IXMLDOMNode;
  XML: string;
begin
  Result := '';
  if FCurrentNode = nil then
    Exit;
  XML := FCurrentNode.XML;
  Node := FCurrentNode.selectSingleNode(XPath);
  if Node = nil then
    Exit;
  Result := Node.text;
end;

function TXMLCursor.Get_Values(const Name: WideString): WideString;
begin
	Result := GetValue(Name);
end;

function TXMLCursor.InsertBefore(const ElementName, Value: WideString): IXMLCursor;
var
  NewCursor: TXMLCursor;
  Element: IXMLDOMElement;
  Node, ParentNode: IXMLDOMNode;
  Position: Integer;
begin
	Result := nil;
  if FXMLDOMDocument = nil then
  begin
    Result := AppendChild(ElementName, Value);
    Exit;
  end;
  if FCurrentNode = nil then
    Exit;
  ParentNode := FCurrentNode.parentNode;
  if ParentNode = nil then
    Exit;
  Element := FXMLDOMDocument.createElement(ElementName);
  ParentNode.insertBefore(Element, FCurrentNode);
  if Value <> '' then
    Element.text := Value;
  NewCursor := TXMLCursor.Create;
  NewCursor.FXMLDOMDocument := FXMLDOMDocument;
  NewCursor.FCurrentNodeList := ParentNode.childNodes;
  Node := FCurrentNode;
  // retrieve the absolute position of FCurrentNode in parentNode.childNodes
  Position := 0;
  while True do
  begin
    Node := Node.previousSibling;
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
  NewCursor: TXMLCursor;
  Element: IXMLDOMElement;
  Node, ParentNode: IXMLDOMNode;
  Position: Integer;
begin
	Result := nil;
  if FXMLDOMDocument = nil then
  begin
    Result := AppendChild(ElementName, Value);
    Exit;
  end;
  if FCurrentNode = nil then
    Exit;
  ParentNode := FCurrentNode.parentNode;
  if ParentNode = nil then
    Exit;
  Element := FXMLDOMDocument.createElement(ElementName);
  Node := FCurrentNode.nextSibling;
  if Node = nil then
    ParentNode.appendChild(Element)
  else
    ParentNode.insertBefore(Element, Node);
  if Value <> '' then
    Element.text := Value;
  NewCursor := TXMLCursor.Create;
  NewCursor.FXMLDOMDocument := FXMLDOMDocument;
  NewCursor.FCurrentNodeList := ParentNode.childNodes;
  Node := FCurrentNode;
  // retrieve the absolute position of FCurrentNode in parentNode.childNodes
  Position := 0;
  while True do
  begin
    Node := Node.previousSibling;
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

procedure TXMLCursor.Load(const FileName: WideString);
begin
  if FXMLDOMDocument = nil then
    FXMLDOMDocument := CoFreeThreadedDOMDocument.Create;
  if not FXMLDOMDocument.load(FileName) then
    ReportDOMParseError(FXMLDOMDocument.parseError);
  if FXMLDOMDocument.documentElement = nil then
    FCurrentNodeList := nil
  else
    FCurrentNodeList := FXMLDOMDocument.selectNodes(FXMLDOMDocument.documentElement.nodeName);
  First; // To be sure to be on the document element
end;

procedure TXMLCursor.LoadXML(const SourceXML: WideString);
begin
  if FXMLDOMDocument = nil then
    FXMLDOMDocument := CoFreeThreadedDOMDocument.Create;
  if not FXMLDOMDocument.loadXML(SourceXML) then
    ReportDOMParseError(FXMLDOMDocument.parseError);
  if FXMLDOMDocument.documentElement = nil then
    FCurrentNodeList := nil
  else
    FCurrentNodeList := FXMLDOMDocument.selectNodes(FXMLDOMDocument.documentElement.nodeName);
  First;
end;

procedure TXMLCursor.MoveTo(Index: Integer);
var
  VCount: Integer;
begin
  FPosition := -1;
  FEOF := True;
  FCurrentNode := nil;
  if FCurrentNodeList = nil then
    Exit;
  VCount := Count;
  if VCount = 0 then
    Exit;
  if (Index < 0) or (Index >= VCount) then
    raise Exception.Create('TXMLCursor.MoveTo - Index out of range: '+IntToStr(Index)+'/'+IntToStr(VCount));
  if FCurrentNodeList = nil then
    Exit;
  FPosition := Index;
  FEOF := False;
  FCurrentNode := FCurrentNodeList.item[FPosition];
end;

procedure TXMLCursor.Next;
begin
  if (Count-1 = FPosition) or (FCurrentNodeList = nil) then
  begin
    FEOF := True;
    Exit;
  end;
  Inc(FPosition);
  FCurrentNode := FCurrentNodeList.item[FPosition];
end;

function TXMLCursor.RecNo: Integer;
begin
  Result := FPosition;
end;

procedure TXMLCursor.ReplaceWithXMLCursor(const XMLCursor: IXMLCursor);
var
  ClonedNode: IXMLDOMNode;
  Count: Integer;
  I: Integer;
  NodeList: IXMLDOMNodeList;
  ParentNode: IXMLDOMNode;
  XMLCursorInternal: IXMLCursorInternal;
begin
  if FXMLDOMDocument = nil then
    raise Exception.Create('TXMLCursor.ReplaceWithXMLCursor - FXMLDOMDocument = nil');
  if FCurrentNode = nil then
    Exit;
  ParentNode := FCurrentNode.parentNode;
  if ParentNode = nil then
    Exit;
  if XMLCursor = nil then
    Exit;
  if XMLCursor.QueryInterface(IXMLCursorInternal, XMLCursorInternal) <> S_OK then
    Exit;
  if XMLCursorInternal = nil then
    Exit;
  NodeList := XMLCursorInternal.Get_CurrentNodeList;
  Count := NodeList.length;
  for I := 0 to Count-1 do
  begin
    ClonedNode := NodeList.item[I].cloneNode(true);
    ParentNode.insertBefore(ClonedNode, FCurrentNode);
  end;
  FCurrentNodeList := ParentNode.childNodes;
  ParentNode.removeChild(FCurrentNode);
  FCurrentNode := nil;
  First; // Reset FPosition
end;

procedure TXMLCursor.Save(const FileName: WideString);
var
  Path: string;
begin
  if FXMLDOMDocument <> nil then
  begin
    Path := ExtractFilePath(FileName);
    ForceDirectories(Path);
    FXMLDOMDocument.Save(FileName);
  end;
end;

function TXMLCursor.Select(const XPath: WideString): IXMLCursor;
var
  NodeList: IXMLDOMNodeList;
  XMLCursor: TXMLCursor;
begin
  XMLCursor := TXMLCursor.Create;
  Result := XMLCursor;
  if FCurrentNode = nil then
  begin
    XMLCursor.First;
    Exit;
  end;
  if XPath = '*' then
  begin
	  NodeList := FCurrentNode.childNodes;
  end else begin
	  NodeList := FCurrentNode.selectNodes(XPath);
  end;
  XMLCursor.FXMLDOMDocument := FXMLDOMDocument;
  XMLCursor.FCurrentNodeList := NodeList;
  XMLCursor.First;
end;

procedure TXMLCursor.SetAttributeValue(const AttributeName, Value: WideString);
var
  Element: IXMLDOMElement;
begin
  if FCurrentNode = nil then
    Exit;
  Element := FCurrentNode as IXMLDOMElement;
  Element.setAttribute(AttributeName, Value);
end;

procedure TXMLCursor.SetValue(const ElementName, Value: WideString);
var
	AttributeName: string;
  Node: IXMLDOMNode;
begin
  if FCurrentNode = nil then
    Exit;
  if Pos('@', ElementName) = 1 then
  begin
		AttributeName := ElementName;
    System.Delete(AttributeName, 1, 1);
		SetAttributeValue(AttributeName, Value);
    Exit;
  end;
  Node := FCurrentNode.selectSingleNode(ElementName);
  if Node = nil then
  begin
    Node := FXMLDOMDocument.createElement(ElementName);
    FCurrentNode.appendChild(Node);
  end;
  // ??? Should be a CData Section to handle special characters ???
  Node.text := Value;
end;

procedure TXMLCursor.Set_Values(const Name: WideString; const Value: WideString);
begin
	SetValue(Name, Value);
end;

procedure	TXMLCursor.SetCValue(const ElementName, Value: WideString);
var
  Node: IXMLDOMNode;
  CDATA: IXMLDOMNode;
begin
  if FCurrentNode = nil then
    Exit;
  Node := FCurrentNode.selectSingleNode(ElementName);
  if Node = nil then
  begin
    Node := FXMLDOMDocument.createElement(ElementName);
    FCurrentNode.appendChild(Node);
  end;
  CDATA := Node.Get_firstChild;
  if CDATA = nil then
  begin
  	// Should protect CDATA against ]]>
    CDATA := FXMLDOMDocument.createCDATASection(Value);
    Node.appendChild(CDATA);
    Exit;
  end else begin
  	// Should protect CDATA against ]]>
    CDATA.text := Value;
  end;
end;

function TXMLCursor.XML: WideString;
begin
  Result := '';
  if FCurrentNode = nil then
    Exit;
  if (FCurrentNode.parentNode = nil) or (FCurrentNode.parentNode.parentNode = nil) then
    Result := FXMLDOMDocument.xml
  else
    Result := FCurrentNode.xml;
end;

function TXMLCursor.XMLDOMDocument: IUnknown;
begin
  Result := FXMLDOMDocument;
end;

// !!! Do not use this function for multi-platform applications
function TXMLCursor.XMLDOMNode: IUnknown;
begin
  Result := FCurrentNode;
end;

////////////////////////////////////////////////////////////////////////////////
// TXSLProc
////////////////////////////////////////////////////////////////////////////////

constructor TXSLProc.Create;
resourcestring
	StdXMLTLB = 'StdXML.tlb';
var
  StdXMLTypeLib: ITypeLib;
begin
	try
	  OleCheck(LoadTypeLib(PWideChar(WideString(StdXMLTLB)), StdXMLTypeLib));
    inherited Create(StdXMLTypeLib, IXSLProc);
    FFiles := TStringList.Create;
  except
    reraise('TXSLProc.Create');
  end;
end;

destructor TXSLProc.Destroy;
begin
	FFiles.Free;
  FXSLProcessor := nil;
  FXSLTemplate := nil;
	inherited;
end;

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

resourcestring
  SEmptyXSLDocument = 'XSL document is empty.';
var
	CurrentAge: Integer;
	XSLDocument: IXMLDOMDocument;
begin
	try
		FFileName := AFileName;
    XSLDocument := CoFreeThreadedDOMDocument.Create;
    XSLDocument.load(AFileName);
    if XSLDocument.documentElement = nil then
      raise Exception.Create(SEmptyXSLDocument);
    // imports/includes xmldom_msxml
    CurrentAge := FileAge(AFileName);
    FFiles.AddObject(AFileName, Pointer(CurrentAge));
    ProcessImports(XSLDocument);
    FXSLTemplate := CoXSLTemplate.Create;
    FXSLTemplate.stylesheet := XSLDocument;
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

begin
  try
  	if FXSLProcessor = nil then
	    FXSLProcessor := FXSLTemplate.createProcessor;
    FXSLProcessor.input := IXMLDOMdocument(Document.XMLDOMDocument);
    FXSLProcessor.transform;
    SResult := FXSLProcessor.output;
    PurgeResult;
    Result := SResult;
  except on E: Exception do
    raise Exception.Create('TXSLProc.Process - XSL processing - '+FFileName+#13#10+E.Message);
  end;
end;

end.
