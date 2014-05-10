unit UFilters;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, umetadata, Controls, StdCtrls, DB, Buttons, Graphics,
  ExtCtrls, Math;

type

  TCondition = object
    FormatStr: String;
    Condition: String;
    Caption: String;
  end;

  TConditions = array of TCondition;

  TFilterInfo = record
    FieldName: String;
    ParamName: String;
    Condition: String;
    FieldType: TFieldType;
    Value: String;
  end;

  TFilterInfos = array of TFilterInfo;

  { TFilter }

  TFilter = class
  private
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
    FConditions: array of TConditions;
    procedure SetLeft(aLeft: integer);
    procedure SetTop(aTop: integer);
    procedure SetWidth(aWidth: integer);
    procedure SetParent(aParent: TWinControl);
    procedure SetButton(aButton: TSpeedButton);
    procedure SetAlign(aAlign: TAlign);
    function GetConditions(aFieldType: TFieldType): TConditions;
    function GetConditionCaptions(aFieldType: TFieldType): Strings;
    function GetCurrCondition(): TCondition;
    procedure OnItemSelect(Sender: TObject);
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
    procedure SetLeft(aLeft: integer);
    procedure SetTop(aTop: integer);
    procedure SetWidth(aWidth: integer);
    procedure SetParent(aParent: TWinControl);
    procedure SetAlign(aAlign: TAlign);
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
    property Align: TAlign read FAlign write SetAlign;
    property Count: Integer read GetFiltersCount;
  end;

implementation

constructor TFilter.Create(aOwner: TWinControl; aTableInfo: TTableInfo);
const
  FieldTypeCount = 41;
  defConditionFormat = '%s';

  procedure AddCondition(aType: TFieldType; aCondition, aCaption, aFromatStr: String);
  var
    i: Integer;
  begin
    i := Integer(aType);
    SetLength(FConditions[i], Length(FConditions[i]) + 1);
    with FConditions[i][High(FConditions[i])] do begin
      Condition := aCondition;
      Caption := aCaption;
      FormatStr := aFromatStr;
    end;
  end;

  procedure AddCondition(aType: TFieldType; aCondition, aCaption: String);
  begin
    AddCondition(aType, aCondition, aCaption, defConditionFormat);
  end;

  procedure CopyConditions(aTo, aFrom: TFieldType);
  begin
    FConditions[Integer(aTo)] := FConditions[Integer(aFrom)];
  end;

begin
  FOwner := aOwner;
  FTable := aTableInfo;
  FHeight := 30;

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
    Items.AddStrings(aTableInfo.GetColumnCaptions);
    ReadOnly := True;
    Align := alLeft;
    OnSelect := @Self.OnItemSelect;
    Constraints.MinWidth := 130;
  end;

  FEdit := TEdit.Create(FPanel);
  with FEdit do begin
    Parent := FPanel;
    AnchorToNeighbour(akLeft, 3, FConditionCmbBox);
  end;

  FPanel.Height := FField.Height;
  SetLength(FConditions, Integer(High(TFieldType)) + 1);

  AddCondition(ftUnknown, '=', 'Равно');
  AddCondition(ftUnknown, '<=', 'Меньше или равно');
  AddCondition(ftUnknown, '>=', 'Больше или равно');
  AddCondition(ftUnknown, '<', 'Меньше');
  AddCondition(ftUnknown, '>', 'Больше');

  CopyConditions(ftString, ftUnknown);
  AddCondition(ftString, 'Like', 'Начинается с:', '%s%%');
  AddCondition(ftString, 'Like', 'Заканчивается на:', '%%%s');
  AddCondition(ftString, 'Like', 'Содержит:', '%%%s%%');
end;

function TFilter.GetFilterInfo(): TFilterInfo;
var
  Column: TColumnInfo;
begin
  Column := FTable.Columns[FField.ItemIndex];
  with Result do
  begin
    with Column do
    begin
      FieldName := GetColFullName(Column);
      Condition := GetCurrCondition.Condition;
      ParamName := Name;
      Value := Format(GetCurrCondition.FormatStr, [FEdit.Text]);
      Result.FieldType := FieldType;
    end;
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

function TFilter.GetConditionCaptions(aFieldType: TFieldType): Strings;
var
  Conditions: TConditions;
  i: Integer;
begin
  Conditions := GetConditions(aFieldType);
  SetLength(Result, Length(Conditions));
  for i := 0 to High(Conditions) do
    Result[i] := Conditions[i].Caption;
end;

function TFilter.GetCurrCondition: TCondition;
var
  Condtions: TConditions;
begin
  Condtions := GetConditions(FTable.Columns[FField.ItemIndex].FieldType);
  Result := Condtions[FConditionCmbBox.ItemIndex];
end;

function TFilter.GetConditions(aFieldType: TFieldType): TConditions;
begin
  if Length(FConditions[Integer(aFieldType)]) = 0 then
    Result := FConditions[Integer(ftUnknown)]
  else
    Result := FConditions[Integer(aFieldType)];
end;

procedure TFilter.OnItemSelect(Sender: TObject);
begin
  with TComboBox(Sender) do begin
    FConditionCmbBox.Items.Clear;
    FConditionCmbBox.Items.AddStrings(GetConditionCaptions(FTable.Columns[ItemIndex].FieldType));
  end;
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
  DelButton();
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
  end;

  {if ArrLen > 0 then
  begin
    Filters[ArrHigh - 1].SetButton(CreateButton('Images\DelFilter.png', @OnDelClick, ArrHigh - 1));
  end;}

  UpdatePos();
end;

procedure TFilterList.Del(Index: integer);
var
  i: integer;
begin
  if not InRange(Index, 0, High(FFilters)) then
    Exit;
  FreeAndNil(FFilters[Index]);
  for i := Index to High(FFilters) - 1 do
  begin
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
  begin
    Result[i] := FFilters[i].GetFilterInfo();
    Result[i].ParamName += IntToStr(i);
  end;
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
  if FOnHeightChange <> nil then
    FOnHeightChange(Self);
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

destructor TFilterList.Destroy();
var
  i: Integer;
begin
  for i := 0 to High(FFilters) do
    FFilters[i].Destroy();
  FreeAndNil(FPanel);
  inherited;
end;

end.
