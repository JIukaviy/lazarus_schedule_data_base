unit UFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, umetadata, Controls, StdCtrls, DB, Buttons, Graphics,
  ExtCtrls, Math;

type

  TCondition = object
    FormatStr: String;
    SQLStr: String;
    Caption: String;
  end;

  TConditions = array of TCondition;

  { TFilterInfo }

  TFilterInfo = object
    Column: TColumnInfo;
    Condition: TCondition;
    Value: String;
    function SQLValue(): String;
  end;

  TFilterInfos = array of TFilterInfo;

  { TFilter }

  TFilter = class
  private
    FOnChange: TNotifyEvent;
    FPanel: TPanel;
    FField: TCombobox;
    FConditionCmbBox: TCombobox;
    FEdit: TEdit;
    FButton: TSpeedButton;
    FTable: TTableInfo;
    FLeft: integer;
    FTop: integer;
    FWidth: integer;
    FHeight: integer;
    FOwner: TWinControl;
    FParent: TWinControl;
    FAlign: TAlign;
    FColumns: TColumnInfos;
    procedure SetLeft(aLeft: integer);
    procedure SetTop(aTop: integer);
    procedure SetWidth(aWidth: integer);
    procedure SetParent(aParent: TWinControl);
    procedure SetButton(aButton: TSpeedButton);
    procedure SetAlign(aAlign: TAlign);
    procedure SetOnChange(aEvent: TNotifyEvent);
    function GetCurrCondition(): TCondition;
    procedure OnColSelect(Sender: TObject);
  public
    constructor Create(aOwner: TWinControl; aTableInfo: TTableInfo);
    destructor Destroy(); override;
    function GetFilterInfo(): TFilterInfo;
    function CheckFields(): boolean;
    procedure DelButton();
    procedure ChangeTag(aTag: integer);
    property Left: integer read FLeft write SetLeft;
    property Top: integer read FTop write SetTop;
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight;
    property Parent: TWinControl read FParent write SetParent;
    property Button: TSpeedButton read FButton write SetButton;
    property Align: TAlign read FAlign write SetAlign;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;

  TFilters = array of TFilter;

  { TFilterList }

  TFilterList = class
  private
    FFilters: TFilters;
    FPanel: TPanel;
    FParent: TWinControl;
    FTable: TTableInfo;
    FLeft: integer;
    FTop: integer;
    FWidth: integer;
    FHeight: integer;
    FSpace: integer;
    FAlign: TAlign;
    FOnHeightChange: TNotifyEvent;
    FOnFilterDel: TNotifyEvent;
    FOnFilterAdd: TNotifyEvent;
    FOnFilterChange: TNotifyEvent;
    procedure SetLeft(aLeft: integer);
    procedure SetTop(aTop: integer);
    procedure SetWidth(aWidth: integer);
    procedure SetParent(aParent: TWinControl);
    procedure SetAlign(aAlign: TAlign);
    procedure SetOnFilterChange(aEvent: TNotifyEvent);
    procedure UpdatePos();
    function CreateButton(ImageName: string; aOnClick: TNotifyEvent;
      aTag: integer): TSpeedButton;
    procedure OnDelClick(Sender: TObject);
    procedure OnAddClick(Sender: TObject);
    procedure OnPanelResize(Sender: TObject);
    function GetFiltersCount(): Integer;
  public
    constructor Create(aParent: TWinControl; aTable: TTableInfo);
    destructor Destroy(); override;
    procedure Add();
    procedure Del(Index: integer);
    function GetFilterInfos(): TFilterInfos;
    function CheckFields(): boolean;
    property Left: integer read FLeft write SetLeft;
    property Top: integer read FTop write SetTop;
    property Width: integer read FWidth write SetWidth;
    property Height: integer read FHeight;
    property Parent: TWinControl read FParent write SetParent;
    property OnHeightChange: TNotifyEvent read FOnHeightChange write FOnHeightChange;
    property OnFilterDel: TNotifyEvent read FOnFilterDel write FOnFilterDel;
    property OnFilterAdd:  TNotifyEvent read FOnFilterAdd write FOnFilterAdd;
    property OnFilterChange: TNotifyEvent read FOnFilterChange write SetOnFilterChange;
    property Align: TAlign read FAlign write SetAlign;
    property Count: Integer read GetFiltersCount;
  end;

  function CreateIdFilter(aTable: TTableInfo; aID: Integer): TFilterInfo;


implementation
var
  Conditions: array of TConditions;

procedure AddCondition(aType: TFieldType; aCondition, aCaption: String; aFormatStr: String = '%s');
var
  i: Integer;
begin
  i := Integer(aType);
  SetLength(Conditions[i], Length(Conditions[i]) + 1);
  with Conditions[i][High(Conditions[i])] do begin
    SQLStr := aCondition;
    Caption := aCaption;
    FormatStr := aFormatStr;
  end;
end;

procedure CopyConditions(aTo, aFrom: TFieldType);
begin
  Conditions[Integer(aTo)] := Conditions[Integer(aFrom)];
end;

function GetConditions(aFieldType: TFieldType): TConditions;
begin
  if Length(Conditions[Integer(aFieldType)]) = 0 then
    Result := Conditions[Integer(ftUnknown)]
  else
    Result := Conditions[Integer(aFieldType)];
end;

function GetConditionCaptions(aFieldType: TFieldType): Strings;
var
  Conditions: TConditions;
  i: Integer;
begin
  Conditions := GetConditions(aFieldType);
  SetLength(Result, Length(Conditions));
  for i := 0 to High(Conditions) do
    Result[i] := Conditions[i].Caption;
end;

function CreateIdFilter(aTable: TTableInfo; aID: Integer): TFilterInfo;
begin
  with Result do begin
    Column := aTable.Columns[COL_ID];
    Condition := Conditions[Integer(ftInteger)][0];
    Value := IntToStr(aID);
  end;
end;

{ TFilterInfo }

function TFilterInfo.SQLValue: String;
begin
  Result := Format(Condition.FormatStr, [Value]);
end;

constructor TFilter.Create(aOwner: TWinControl; aTableInfo: TTableInfo);
const
  FieldTypeCount = 41;
  defConditionFormat = '%s';
begin
  FOwner := aOwner;
  FTable := aTableInfo;
  FHeight := 30;
  FColumns := aTableInfo.GetCols(cstAll, [coVisible]);

  FPanel := TPanel.Create(aOwner);
  with FPanel do begin
    Height := FHeight;
    BevelOuter := bvNone;
    BorderSpacing.Top := 3;
    BorderSpacing.Bottom := 3;
    with ChildSizing do
    begin
      ControlsPerLine := 3;
      Layout := cclLeftToRightThenTopToBottom;
      HorizontalSpacing := 3;
      LeftRightSpacing := 3;
    end;
  end;

  FConditionCmbBox := TCombobox.Create(FPanel);
  with FConditionCmbBox do begin
    Parent := FPanel;
    ReadOnly := True;
    Align := alLeft;
    Constraints.MinWidth := 120;
  end;

  FField := TCombobox.Create(FPanel);
  with FField do begin
    Parent := FPanel;
    Items.AddStrings(GetColCaptions(FColumns));
    ReadOnly := True;
    Align := alLeft;
    OnSelect := @Self.OnColSelect;
    Constraints.MinWidth := 130;
  end;

  FEdit := TEdit.Create(FPanel);
  with FEdit do begin
    Parent := FPanel;
    AnchorToNeighbour(akLeft, 3, FConditionCmbBox);
  end;

  FPanel.Height := FField.Height;
end;

function TFilter.GetFilterInfo(): TFilterInfo;
begin
  with Result do begin
    Column := FColumns[FField.ItemIndex];
    Condition := GetCurrCondition();
    Value := FEdit.Text;
  end;
end;

function TFilter.CheckFields(): boolean;
begin
  Result := (Length(FEdit.Text) > 0) and
            (FConditionCmbBox.ItemIndex >= 0) and
            (FField.ItemIndex >= 0);
end;

procedure TFilter.SetLeft(aLeft: integer);
begin
  FPanel.Left := aLeft;
  FLeft := aLeft;
end;

procedure TFilter.SetTop(aTop: integer);
begin
  FPanel.Top := aTop;
  FTop := aTop;
end;

procedure TFilter.SetWidth(aWidth: integer);
const
  Space = 5;
var
  SWidth: integer;
  ButtonWidth: integer;
begin
  FWidth := aWidth;
  FPanel.Width := aWidth;
end;

procedure TFilter.SetParent(aParent: TWinControl);
begin
  FParent := aParent;
  FPanel.Parent := aParent;
end;

procedure TFilter.SetButton(aButton: TSpeedButton);
begin
  DelButton();
  FButton := aButton;
  with FButton do begin
    Parent := FPanel;
    Left := 1000;
    Constraints.MaxWidth := FHeight;
    Height := FHeight;
    Width := FHeight;
    Align := alRight;
    //Anchors := [akRight];
  end;
  FPanel.ChildSizing.ControlsPerLine := 4;
  FEdit.AnchorToNeighbour(akRight, 3, FButton);
end;

procedure TFilter.SetAlign(aAlign: TAlign);
begin
  FAlign := aAlign;
  FPanel.Align := aAlign;
end;

procedure TFilter.SetOnChange(aEvent: TNotifyEvent);
begin
  FOnChange := aEvent;
  FEdit.OnChange := aEvent;
  FConditionCmbBox.OnChange := aEvent;
end;

function TFilter.GetCurrCondition: TCondition;
var
  Condtions: TConditions;
begin
  Condtions := GetConditions(FColumns[FField.ItemIndex].FieldType);
  Result := Condtions[FConditionCmbBox.ItemIndex];
end;

procedure TFilter.OnColSelect(Sender: TObject);
begin
  with FConditionCmbBox.Items do begin
    Clear;
    AddStrings(GetConditionCaptions(FColumns[TComboBox(Sender).ItemIndex].FieldType));
  end;
  if (FOnChange <> nil) and (CheckFields()) then FOnChange(Self);
end;

procedure TFilter.DelButton();
begin
  FreeAndNil(FButton);
  FPanel.ChildSizing.ControlsPerLine := 3;
end;

procedure TFilter.ChangeTag(aTag: integer);
begin
  if FButton <> nil then
    FButton.Tag := aTag;
end;

destructor TFilter.Destroy();
begin
  FreeAndNil(FEdit);
  FreeAndNil(FConditionCmbBox);
  FreeAndNil(FField);
  FreeAndNil(FButton);
  FreeAndNil(FPanel);
  inherited;
end;

//FilterList

constructor TFilterList.Create(aParent: TWinControl; aTable: TTableInfo);
begin
  FParent := aParent;
  FTable := aTable;
  FHeight := 0;
  FSpace := 5;
  FOnHeightChange := nil;
  FOnFilterDel := nil;

  FPanel := TPanel.Create(aParent);
  with FPanel do begin
    Parent := aParent;
    AutoSize := true;
    OnResize := @OnPanelResize;
    with ChildSizing do begin
      ControlsPerLine := 1;
      Layout := cclLeftToRightThenTopToBottom;
    end;
  end;
end;

procedure TFilterList.Add();
begin
  SetLength(FFilters, Length(FFilters) + 1);
  FFilters[High(FFilters)] := TFilter.Create(FParent, FTable);

  with FFilters[High(FFilters)] do
  begin
    Parent := Self.FPanel;
    //SetButton(CreateButton('Images\AddFilter.png', @OnAddClick, High(FFilters)));
    SetButton(CreateButton('Images\DelFilter.png', @OnDelClick, High(FFilters)));
    Align := alTop;
    OnChange := FOnFilterChange;
  end;

  {if ArrLen > 0 then
  begin
    Filters[ArrHigh - 1].SetButton(CreateButton('Images\DelFilter.png', @OnDelClick, ArrHigh - 1));
  end;}
  if FOnFilterAdd <> nil then FOnFilterAdd(Self);
  UpdatePos();
end;

procedure TFilterList.Del(Index: integer);
var
  i: integer;
begin
  if not InRange(Index, 0, High(FFilters)) then
    Exit;
  FreeAndNil(FFilters[Index]);
  for i := Index to High(FFilters) - 1 do begin
    FFilters[i] := FFilters[i + 1];
    FFilters[i].ChangeTag(i);
  end;
  SetLength(FFilters, Length(FFilters) - 1);
  if FOnFilterDel <> nil then FOnFilterDel(Self);
  UpdatePos();
end;

function TFilterList.GetFilterInfos(): TFilterInfos;
var
  i: integer;
begin
  SetLength(Result, Length(FFilters));
  for i := 0 to High(FFilters) do
    Result[i] := FFilters[i].GetFilterInfo();
end;

function TFilterList.CheckFields(): boolean;
var
  i: integer;
begin
  for i := 0 to High(FFilters) do
    if not FFilters[i].CheckFields() then
      Exit(False);
  Exit(True);
end;

function TFilterList.CreateButton(ImageName: string; aOnClick: TNotifyEvent;
  aTag: integer): TSpeedButton;
var
  Picture: TPicture;
begin
  Picture := TPicture.Create();
  Picture.LoadFromFile(ImageName);
  Result := TSpeedButton.Create(FParent);
  with Result do
  begin
    Glyph := Picture.Bitmap;
    OnClick := aOnClick;
    Flat := True;
    Tag := aTag;
  end;
end;

procedure TFilterList.UpdatePos();
begin
  if FOnHeightChange <> nil then FOnHeightChange(Self);
end;

procedure TFilterList.OnAddClick(Sender: TObject);
begin
  Add();
end;

procedure TFilterList.OnDelClick(Sender: TObject);
begin
  Del(TSpeedButton(Sender).Tag);
end;

procedure TFilterList.OnPanelResize(Sender: TObject);
begin
  FHeight := FPanel.Height;
end;

function TFilterList.GetFiltersCount: Integer;
begin
  Result := Length(FFilters);
end;

procedure TFilterList.SetLeft(aLeft: integer);
begin
  FLeft := aLeft;
  FPanel.Left := aLeft;
  //for i := 0 to High(FFilters) do
  //  FFilters[i].Left := aLeft;
end;

procedure TFilterList.SetTop(aTop: integer);
var
  i: integer;
begin
  FTop := aTop;
  FPanel.Top := aTop;
  //for i := 0 to High(FFilters) do
  //  FFilters[i].Top := aTop;
end;

procedure TFilterList.SetWidth(aWidth: integer);
begin
  FWidth := aWidth;
  FPanel.Width := aWidth;
  //for i := 0 to High(FFilters) do
  //  FFilters[i].Width := aWidth;
end;

procedure TFilterList.SetParent(aParent: TWinControl);
begin
  FParent := aParent;
  FPanel.Parent := aParent;
end;

procedure TFilterList.SetAlign(aAlign: TAlign);
begin
  FAlign := aAlign;
  FPanel.Align := aAlign;
end;

procedure TFilterList.SetOnFilterChange(aEvent: TNotifyEvent);
var
  i: Integer;
begin
  FOnFilterChange := aEvent;
  for i := 0 to High(FFilters) do
    FFilters[i].OnChange := aEvent;
end;

destructor TFilterList.Destroy();
var
  i: Integer;
begin
  for i := 0 to High(FFilters) do
    FreeAndNil(FFilters[i]);
  FreeAndNil(FPanel);
  inherited;
end;

initialization
  SetLength(Conditions, Integer(High(TFieldType)) + 1);
  AddCondition(ftUnknown, '=', 'Равно');
  AddCondition(ftUnknown, '<=', 'Меньше или равно');
  AddCondition(ftUnknown, '>=', 'Больше или равно');
  AddCondition(ftUnknown, '<', 'Меньше');
  AddCondition(ftUnknown, '>', 'Больше');

  CopyConditions(ftString, ftUnknown);
  AddCondition(ftString, 'Like', 'Начинается с:', '%s%%');
  AddCondition(ftString, 'Like', 'Заканчивается на:', '%%%s');
  AddCondition(ftString, 'Like', 'Содержит:', '%%%s%%');

end.
