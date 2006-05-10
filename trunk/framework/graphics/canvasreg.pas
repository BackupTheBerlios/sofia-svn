unit canvasreg;

interface

uses Classes, DesignIntf, DesignEditors;

type

  TCanvasItemPropertiesProperty = class(TClassProperty)
  protected
    function HasSubProperties: Boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  TBrushPropertiesProperty = class(TClassProperty)
  protected
    function HasSubProperties: Boolean;
  public
    function GetAttributes: TPropertyAttributes; override;
    function GetValue: string; override;
    procedure GetValues(Proc: TGetStrProc); override;
    procedure SetValue(const Value: string); override;
  end;

  //----------------------------------------------------------------------------
  //  Auteur        : AZemour
  //  Description   : Editeur de composant, TaxiLayerPanel
  //----------------------------------------------------------------------------
  TCanvasItemsEditor = class(TDefaultEditor)
  public
    procedure EditProperty(const Prop: IProperty; var Continue: Boolean); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
    procedure ExecuteVerb(Index: Integer); override;

  end;

procedure Register;

implementation

uses
  canvas, Windows, Messages, SysUtils;

function EnumChildProc(WND: HWND; LParam: Integer): BOOL; stdcall
var
  AName: array[0..255] of Char;
const
  S: string = 'TPropSelection';
begin
  Result := True;
  if (GetClassName(WND, @AName[0], 255) <> 0) and (AName = S) then
  begin
    SendMessage(WND, WM_CHAR, $2D, $4A0001);
    InvalidateRect(WND, nil, True);
    SendMessage(GetParent(WND), WM_SIZE, 0, 0);
  end;
end;

function EnumWnd(WND: HWND; LParam: Integer): BOOL; stdcall;
begin
  Result := True;
  EnumChildWindows(WND, @EnumChildProc, 0);
end;

procedure ObjectInspectorCollapseProperty;
begin
  EnumWindows(@EnumWnd, 0);
end;

function TCanvasItemPropertiesProperty.HasSubProperties: Boolean;
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
  begin
    Result := TCanvasItem(GetComponent(I)).Properties <> nil;
    if not Result then
      Exit;
  end;
  Result := True;
end;

function TCanvasItemPropertiesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, paSubProperties);
  Result := Result - [paReadOnly] + [paValueList, paSortList, paRevertable];
end;

function TCanvasItemPropertiesProperty.GetValue: string;
begin
  if HasSubProperties then
    Result := GetRegisteredCanvasItemProperties.GetDescriptionByClass(TCanvasItemProperties(GetOrdValue).ClassType)
  else
    Result := '';
end;

procedure TCanvasItemPropertiesProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to GetRegisteredCanvasItemProperties.Count - 1 do
    Proc(GetRegisteredCanvasItemProperties.Descriptions[I]);
end;

procedure TCanvasItemPropertiesProperty.SetValue(const Value: string);
var
  ACanvasItemPropertiesClass: TCanvasItemPropertiesClass;
  I: Integer;
begin
  ACanvasItemPropertiesClass := TCanvasItemPropertiesClass(GetRegisteredCanvasItemProperties.FindByClassName(Value));
  if ACanvasItemPropertiesClass = nil then
    ACanvasItemPropertiesClass := TCanvasItemPropertiesClass(GetRegisteredCanvasItemProperties.FindByDescription(Value));
  if GetValue <> Value then
    ObjectInspectorCollapseProperty;
  for I := 0 to PropCount - 1 do
    TCanvasItem(GetComponent(I)).PropertiesClass := ACanvasItemPropertiesClass;
  Modified;
end;

procedure Register;
begin
  RegisterComponents('Sofia', [TGraphicsCanvas]);
  RegisterPropertyEditor(TypeInfo(string), TCanvasItem, 'PropertiesClassName', nil);
  RegisterPropertyEditor(TypeInfo(string), TPenProperties, 'BrushPropertiesClassName', nil);
  RegisterPropertyEditor(TypeInfo(string), TDrawStringProperties, 'BrushPropertiesClassName', nil);
  RegisterPropertyEditor(TypeInfo(string), TFillRectangleProperties, 'BrushPropertiesClassName', nil);
  RegisterPropertyEditor(TypeInfo(string), TFillRoundedRectangleProperties, 'BrushPropertiesClassName', nil);
  RegisterPropertyEditor(TypeInfo(TCanvasItemProperties), TCanvasItem, 'Properties', TCanvasItemPropertiesProperty);
  RegisterPropertyEditor(TypeInfo(TBrushProperties), TDrawStringProperties, 'BrushProperties', TBrushPropertiesProperty);
  RegisterPropertyEditor(TypeInfo(TBrushProperties), TFillRectangleProperties, 'BrushProperties', TBrushPropertiesProperty);
  RegisterPropertyEditor(TypeInfo(TBrushProperties), TFillRoundedRectangleProperties, 'BrushProperties', TBrushPropertiesProperty);
  RegisterPropertyEditor(TypeInfo(TBrushProperties), TPenProperties, 'BrushProperties', TBrushPropertiesProperty);
  RegisterComponentEditor(TGraphicsCanvas, TCanvasItemsEditor);
end;

function TBrushPropertiesProperty.GetAttributes: TPropertyAttributes;
begin
  Result := inherited GetAttributes;
  if not HasSubProperties then
    Exclude(Result, paSubProperties);
  Result := Result - [paReadOnly] + [paValueList, paSortList, paRevertable];
end;

function TBrushPropertiesProperty.GetValue: string;
begin
  if HasSubProperties then
    Result := GetRegisteredBrushProperties.GetDescriptionByClass(TBrushProperties(GetOrdValue).ClassType)
  else
    Result := '';
end;

procedure TBrushPropertiesProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := 0 to GetRegisteredBrushProperties.Count - 1 do
    Proc(GetRegisteredBrushProperties.Descriptions[I]);
end;

function TBrushPropertiesProperty.HasSubProperties: Boolean;
var
  I: Integer;
begin
  for I := 0 to PropCount - 1 do
  begin
    Result := False;
    if GetComponent(I) is TDrawStringProperties then
      Result := TDrawStringProperties(GetComponent(I)).BrushProperties <> nil;
    if GetComponent(I) is TFillRectangleProperties then
      Result := TFillRectangleProperties(GetComponent(I)).BrushProperties <> nil;
    if GetComponent(I) is TFillRoundedRectangleProperties then
      Result := TFillRoundedRectangleProperties(GetComponent(I)).BrushProperties <> nil;
    if GetComponent(I) is TPenProperties then
      Result := TPenProperties(GetComponent(I)).BrushProperties <> nil;
    if not Result then
      Exit;
  end;
  Result := True;
end;

procedure TBrushPropertiesProperty.SetValue(const Value: string);
var
  ABrushPropertiesClass: TBrushPropertiesClass;
  I: Integer;
begin
  ABrushPropertiesClass := TBrushPropertiesClass(GetRegisteredBrushProperties.FindByClassName(Value));
  if ABrushPropertiesClass = nil then
    ABrushPropertiesClass := TBrushPropertiesClass(GetRegisteredBrushProperties.FindByDescription(Value));
  if GetValue <> Value then
    ObjectInspectorCollapseProperty;
  for I := 0 to PropCount - 1 do
  begin
    if GetComponent(I) is TDrawStringProperties then
      TDrawStringProperties(GetComponent(I)).BrushPropertiesClass := ABrushPropertiesClass;
    if GetComponent(I) is TFillRectangleProperties then
      TFillRectangleProperties(GetComponent(I)).BrushPropertiesClass := ABrushPropertiesClass;
    if GetComponent(I) is TFillRoundedRectangleProperties then
      TFillRoundedRectangleProperties(GetComponent(I)).BrushPropertiesClass := ABrushPropertiesClass;
    if GetComponent(I) is TPenProperties then
      TPenProperties(GetComponent(I)).BrushPropertiesClass := ABrushPropertiesClass;
  end;
  Modified;
end;

procedure TCanvasItemsEditor.EditProperty(const Prop: IProperty; var Continue: Boolean);
begin
  if SameText(Prop.GetName, 'CanvasItems') then
  begin
    Prop.Edit;
    Continue := False;
  end
  else
    inherited;
end;

procedure TCanvasItemsEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: Edit;
  end;
end;

function TCanvasItemsEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Editer les éléments du canvas';
  else
    Result := '';
  end;
end;

function TCanvasItemsEditor.GetVerbCount: Integer;
begin
  Result := 1;
end;


end.

