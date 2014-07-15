unit UScheduleForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqldb, mysql55conn, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, Grids, DbCtrls, StdCtrls, ExtCtrls, Buttons, Math, UDBData,
  umetadata, UFilters, LCLIntf, LCLProc, ComCtrls, EditBtn,
  UExpandPanel, uanimatedgrid, USQLQueryCreator, UEditForm, USchConflicts;

type

  { TScheduleForm }

  TPoint = record
    X: Integer;
    y: Integer;
  end;

  TLookupRecord = record
    Value: String;
    ID: Integer;
  end;

  TCellIndex = record
    Col: Integer;
    Row: Integer;
  end;

  TItemIndex = record
    Cell: TCellIndex;
    Index: Integer;
  end;

  TVisibleColIDs = array of boolean;

  TRects = array of TRect;

  { TScheduleItem }

  TStringsArr = array of Strings;

  { TEditCellBtns }

  //TEditClickEvent: procedure(Sender: TObject; ItemIndex: Integer) of object;

  TEditCellBtns = class
  private
    FInsertBtn: TSpeedButton;
    FEditBtn: TSpeedButton;
    FDeleteBtn: TSpeedButton;
    FOwner: TWinControl;
    FTable: TTableInfo;
    FOnInsertClick: TNotifyEvent;
    FOnEditClick: TNotifyEvent;
    FOnDeleteClick: TNotifyEvent;
    FRect: TRect;
    FVisible: boolean;
    procedure SetInsertClick(aEvent: TNotifyEvent);
    procedure SetEditClick(aEvent: TNotifyEvent);
    procedure SetDeleteClick(aEvent: TNotifyEvent);
    procedure SetRect(aRect: TRect);
    procedure SetVisible(aVal: boolean);
  public
    constructor Create(aOwner: TWinControl);
    destructor Destroy(); override;
    property OnInsertClick: TNotifyEvent read FOnInsertClick write SetInsertClick;
    property OnEditClick: TNotifyEvent read FOnEditClick write SetEditClick;
    property OnDeleteClick: TNotifyEvent read FOnDeleteClick write SetDeleteClick;
    property Visible: boolean read FVisible write SetVisible;
    property Rect: TRect read FRect write SetRect;
  end;

  TScheduleItem = class
  private
    FOwner: TWinControl;
    FEditCellBtns: TEditCellBtns;
    FItemID: Integer;
    FCellIndex: TCellIndex;
    FBorderRect: TRect;
    FHeight: Integer;
    FFill: boolean;
    FOnEditClick: TNotifyEvent;
    FOnInsertClick: TNotifyEvent;
    FOnDeleteClick: TNotifyEvent;
    FXColVal: TColumnVal;
    FYColVal: TColumnVal;
    procedure EditClick(Sender: TObject);
    procedure InsertClick(Sender: TObject);
    procedure DeleteClick(Sender: TObject);
    procedure SetRect(aRect: TRect);
    procedure SetDrawBtn(aVal: boolean);
    function GetDrawBtn(): boolean;
  public
    VisibleColIDs: ^TVisibleColIDs;
    Fields: Strings;
    Columns: TColumnInfos;
    constructor Create(aOwner: TWinControl; aItemID: Integer);
    destructor Destroy(); override;
    procedure Draw(aCanvas: TCanvas);
    property Height: Integer read FHeight write FHeight;
    property BorderRect: TRect read FBorderRect write SetRect;
    property ItemID: Integer read FItemID;
    property DrawBtn: boolean read GetDrawBtn write SetDrawBtn;
    property XColVal: TColumnVal read FXColVal write FXColVal;
    property YColVal: TColumnVal read FYColVal write FYColVal;
    property OnEditClick: TNotifyEvent read FOnEditClick write FOnEditClick;
    property OnInsertClick: TNotifyEvent read FOnInsertClick write FOnInsertClick;
    property OnDeleteClick: TNotifyEvent read FOnDeleteClick write FOnDeleteClick;
  end;

  TScheduleItems = array of TScheduleItem;

  TScheduleItemList = class
  private
    FItems: TScheduleItems;
    FCanvas: TCanvas;
    FStringsArr: TStringsArr;
    FScrollBar: TScrollBar;
    FHeight: Integer;
    FVertOffset: Integer;
    FRect: TRect;
    procedure InitItems();
    procedure Draw(Sender: TObject);
    function GetElemHeight(): Integer;
    procedure SetElemHeight(aHeight: Integer);
    function GetElemWidth(): Integer;
    procedure SetElemWidth(aWidth: Integer);
    procedure SetStringsArr(aStringsArr: TStringsArr);
  public
    constructor Create(aOwner: TWinControl);
    destructor Destroy(); override;
    procedure ShowItems();
    property Items: TScheduleItems read FItems write FItems;
    property StringsArr: TStringsArr read FStringsArr write SetStringsArr;
    property ElemHeight: Integer read GetElemHeight write SetElemHeight;
    property ElemWidth: Integer read GetElemWidth write SetElemWidth;
    property ElemRect: TRect read FRect write FRect;
    property Canvas: TCanvas read FCanvas write FCanvas;
  end;

  TLookupResult = array of TLookupRecord;
  TScheduleData = array of array of array of TScheduleItem;

  TScheduleForm = class(TForm)
    AddFilterButton: TBitBtn;
    ButtonExport: TButton;
    RefreshButton: TBitBtn;
    Grid: TAnimatedGrid;
    ImageList: TImageList;
    Label3: TLabel;
    Label4: TLabel;
    MenuPanel: TPanel;
    OpenFilterPanelButton: TSpeedButton;
    OpenHideColumnsPanel: TSpeedButton;
    XColumnBox: TComboBox;
    YColumnBox: TComboBox;
    procedure AddFilterButtonClick(Sender: TObject);
    procedure ButtonExportClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure GridDblClick(Sender: TObject);
    procedure GridClick(Sender: TObject);
    procedure GridDrawCell(Sender: TObject; aCol, aRow: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure OpenHideColumnsPanelClick(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GridMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure GridMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure GridSelectCell(Sender: TObject; aCol, aRow: Integer;
      var CanSelect: Boolean);
    procedure CheckGroupClick(Sender: TObject; Index: Integer);
    procedure OpenFilterPanelButtonClick(Sender: TObject);
    procedure OnInsertBtnClick(Sender: TObject);
    procedure OnEditBtnClick(Sender: TObject);
    procedure OnDeleteBtnClick(Sender: TObject);
    procedure OnFilterAdd(Sender: TObject);
    procedure OnFilterDel(Sender: TObject);
    procedure OnFilterHeightChange(Sender: TObject);
    procedure OnFilterChange(Sender: TObject);
    procedure XColumnBoxChange(Sender: TObject);
    procedure YColumnBoxChange(Sender: TObject);
    procedure SetDataIsActual();
    procedure SetDataIsNotActual();
    procedure SetCurrElemHeight();
  private
    FilterPanel: TExpandPanel;
    VisibleColsPanel: TExpandPanel;
    XTitles: TLookupResult;
    YTitles: TLookupResult;
    FilterList: TFilterList;
    Items: TScheduleData;
    EditCellBtns: TEditCellBtns;
    LastSelectedCell: TCellIndex;
    LastSelItem: TItemIndex;
    LastOpenedCell: TCellIndex;
    VisibleClmnsChkBox: TCheckGroup;
    VisibleColumns: TVisibleColIDs;
    FSomethingChanged: boolean;
    Query: TSQLQueryCreator;
    FColumns: TColumnInfos;
    MousePos: TPoint;
    procedure SetCaptions();
    procedure SetColumnCaptions();
    procedure SetRowCaptions();
    procedure SendQuery();
    procedure ClearItems();
    procedure FillItems();
    procedure FillXTitles();
    procedure FillYTitles();
    procedure FillLookupResults();
    function GetListHeight(aRow, aCol: Integer): Integer;
    function GetLookupResult(aColIndex: Integer): TLookupResult;
    function GetXColInfo(aColIndex: Integer): TCOlumnInfo;
    function GetYColInfo(aRowIndex: Integer): TCOlumnInfo;
    function ItemByID(aItemID: TItemIndex): TScheduleItem;
    function CreateLookupQuery(ColumnIndex: Integer): TSQLQueryCreator;
  public
    { public declarations }
  end;

var
  ScheduleForm: TScheduleForm;

implementation

uses
  UExport;

var
  SchTable: TTableInfo;
  ElementHeight: Integer;

const
  RowHeight = 18;
  ColCaptionsHeight = 22;

function InRect(aPoint: TPoint; aRect: TRect): boolean;
begin
  Result := InRange(aPoint.X, aRect.Left, aRect.Right) and
            InRange(aPoint.Y, aRect.Top,  aRect.Bottom);
end;

{ TEditCellBtns }

procedure TEditCellBtns.SetInsertClick(aEvent: TNotifyEvent);
begin
  FInsertBtn.OnClick := aEvent;
end;

procedure TEditCellBtns.SetEditClick(aEvent: TNotifyEvent);
begin
  FEditBtn.OnClick := aEvent;
end;

procedure TEditCellBtns.SetDeleteClick(aEvent: TNotifyEvent);
begin
  FDeleteBtn.OnClick := aEvent;
end;

procedure TEditCellBtns.SetRect(aRect: TRect);
const
  Space = 7;
begin
  FRect := aRect;
  with FInsertBtn do begin
    Left := aRect.Right - (Width + Space)*3;
    Top := aRect.Top;
  end;

  with FEditBtn do begin
    Left := aRect.Right - (Width + Space)*2;
    Top := aRect.Top;
  end;

  with FDeleteBtn do begin
    Left := aRect.Right - (Width + Space);
    Top := aRect.Top;
  end;
end;

procedure TEditCellBtns.SetVisible(aVal: boolean);
begin
  FInsertBtn.Visible := aVal;
  FEditBtn.Visible := aVal;
  FDeleteBtn.Visible := aVal;
end;

constructor TEditCellBtns.Create(aOwner: TWinControl);
const
  BtnSize = 20;
var
  Picture: TPicture;

  procedure InitBtn(var aBtn: TSpeedButton; aImgName: String);
  begin
    aBtn := TSpeedButton.Create(aOwner);
    Picture.LoadFromFile(aImgName);
    with aBtn do begin
      Parent := aOwner;
      Height := BtnSize;
      Width := BtnSize;
      Visible := false;
      Glyph := Picture.Bitmap;
      Flat := true;
    end;
  end;

begin
  Picture := TPicture.Create();
  FOwner := aOwner;
  InitBtn(FInsertBtn, 'Images/AddRecord16x16.png');
  InitBtn(FEditBtn, 'Images/EditRecord16x16.png');
  InitBtn(FDeleteBtn, 'Images/DeleteRecord16x16.png');
end;

destructor TEditCellBtns.Destroy;
begin
  FreeAndNil(FInsertBtn);
  FreeAndNil(FEditBtn);
  FreeAndNil(FDeleteBtn);
end;

{$R *.lfm}

{ TScheduleForm }

procedure TScheduleForm.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  //Table := ListOfTables.GetTableByName('Schedule_items');
  Query := TSQLQueryCreator.Create(SchTable);
  SetLength(VisibleColumns, SchTable.ColCount - 1);

  Grid := TAnimatedGrid.Create(Self);
  with Grid do begin
    Parent := Self;
    Options := [goRowSizing, goHorzLine, goVertLine, goRelaxedRowSelect];
    Top := 10;
    Align := alClient;
    AnchorToNeighbour(akTop, 0, MenuPanel);
    DefaultColWidth := 300;
    DefaultRowHeight := ElementHeight;
    OnDrawCell := @GridDrawCell;
    OnDblClick := @GridDblClick;
    OnSelectCell := @GridSelectCell;
    OnClick := @GridClick;
    OnMouseDown := @GridMouseDown;
    OnMouseMove := @GridMouseMove;
    Animate := true;
  end;

  FilterPanel := TExpandPanel.Create(Self);
  with FilterPanel do begin
    Parent := Self;
    AnimationSpeed := 5;
    MaxWidth := 400;
    MinWidth := 0;
    Left := Self.Width;
    Height := 500;
    Anchors := [akRight];
    AnchorToNeighbour(akTop, 0, MenuPanel);
    OpeningSide := osLeft;
  end;

  VisibleColsPanel := TExpandPanel.Create(Self);
  with VisibleColsPanel do begin
    Parent := Self;
    AnimationSpeed := 5;
    MaxWidth := 200;
    MinWidth := 0;
    Left := Self.Width;
    Height := 250;
    Anchors := [akRight];
    AnchorToNeighbour(akTop, 0, MenuPanel);
    OpeningSide := osLeft;
  end;

  VisibleClmnsChkBox := TCheckGroup.Create(VisibleColsPanel);
  with VisibleClmnsChkBox do begin
    Parent := VisibleColsPanel;
    Left := 3;
    AutoSize := true;
    Align := alTop;
    Items.AddStrings(GetColCaptions(SchTable.GetCols(cstAll, [coVisible])));
    for i := 0 to Items.Count - 1 do begin
      Checked[i] := true;
      VisibleColumns[i] := true;
    end;
    OnItemClick := @CheckGroupClick;
  end;

  SetCurrElemHeight();

  FilterList := TFilterList.Create(FilterPanel, SchTable);
  with FilterList do begin
    Parent := FilterPanel;
    Top := 5;
    Align := alTop;
    OnFilterAdd := @Self.OnFilterAdd;
    OnFilterDel := @Self.OnFilterDel;
    OnFilterChange := @Self.OnFilterChange;
    OnHeightChange := @OnFilterHeightChange;
  end;

  with Query do begin
    Table := SchTable;
    FilterList := Self.FIlterList;
  end;

  EditCellBtns := TEditCellBtns.Create(Grid);
  with EditCellBtns do begin
    OnInsertClick := @OnInsertBtnClick;
    OnEditClick := @OnEditBtnClick;
    OnDeleteClick := @OnDeleteBtnClick;
  end;

  FColumns := SchTable.GetCols(cstAll, [coVisible]);

  XColumnBox.Items.AddStrings(GetColCaptions(FColumns));
  YColumnBox.Items.AddStrings(GetColCaptions(FColumns));
  XColumnBox.ItemIndex := COL_GROUP_ID;
  YColumnBox.ItemIndex := COL_DAY_ID;
  FillLookupResults();
  SetCaptions();
  FillItems();
end;

procedure TScheduleForm.RefreshButtonClick(Sender: TObject);
begin
  if not FilterList.CheckFields() then begin
    ShowMessage('Проверьте заполненность полей фильтра');
    FilterPanel.Open();
    Exit;
  end;
  FillItems();
  Grid.Invalidate();
  FilterPanel.Close();
  Conflicts.UpdateConflicts();
  SetDataIsActual();
end;

procedure TScheduleForm.AddFilterButtonClick(Sender: TObject);
begin
  FilterPanel.Open();
  VisibleColsPanel.Close();
  FilterList.Add();
end;

procedure TScheduleForm.ButtonExportClick(Sender: TObject);
begin
  SchExport.ExportToFile(Items, XTitles, YTitles,
     FColumns[XColumnBox.ItemIndex], FColumns[YColumnBox.ItemIndex],
     FilterList.GetFilterInfos(), FColumns);
end;

procedure TScheduleForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin

end;

procedure TScheduleForm.GridDblClick(Sender: TObject);
var
  TargetHeight: Integer;
  ListHeight: Integer;
begin
  with LastSelectedCell do begin
    ListHeight := GetListHeight(Row, Col);
    if Grid.RowHeights[Row] >= ListHeight then
      TargetHeight := ElementHeight
    else
      TargetHeight := ListHeight;
    Grid.SetRowHeight(Row, TargetHeight);
  end;
  LastOpenedCell := LastSelectedCell;
end;

procedure TScheduleForm.GridClick(Sender: TObject);
begin

end;

procedure TScheduleForm.GridDrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
const
  WarningImgSize = 16;
var
  ScheduleItem: TScheduleItem;
  RowBottom: Integer;
  i: Integer;
begin
  if (aRow > Length(YTitles)) or (aCol > Length(XTitles)) then Exit;
  if (aCol = 0) and (aRow > 0) then begin
    Grid.Canvas.TextRect(aRect, aRect.Left, aRect.Top + (aRect.Bottom - aRect.Top) div 2 - 8, YTitles[aRow-1].Value);
  end;

  if (aCol > 0) and (aRow > 0) then begin
    i := 0;
    RowBottom := aRect.Bottom;
    if InRect(MousePos, aRect) then
      EditCellBtns.Rect := Rect(aRect.Left, aRect.Top + ElementHeight*LastSelItem.Index, aRect.Right, aRect.Bottom);
    while (i <= High(Items[aRow-1][aCol-1])) and (aRect.Top < RowBottom) do begin
      with Items[aRow-1][aCol-1][i] do begin
        BorderRect := Rect(aRect.Left, aRect.Top + ElementHeight*i, aRect.Right, aRect.Top + ElementHeight*(i+1));
        Draw(Grid.Canvas);
      end;
      inc(i);
    end;
    if Grid.RowHeights[aRow] < GetListHeight(aRow, aCol) then
      ImageList.Draw(Grid.Canvas, aRect.Right - WarningImgSize, aRect.Bottom - WarningImgSize, 0, true);
  end;

  //ItemList.Draw(Self);
end;

procedure TScheduleForm.GridMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  col, row: Integer;
begin
  Grid.MouseToCell(X, Y, col, row);
  if (LastOpenedCell.Row = 0) or (LastOpenedCell.Col = 0) or
     (LastOpenedCell.Row = row) and (LastOpenedCell.Col = col)
  then Exit;
    Grid.CloseRow(LastOpenedCell.Row);
end;

procedure TScheduleForm.GridMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  col, row: Integer;
  CellRect: TRect;
  CurrSelItem: TItemIndex;
begin
  Grid.MouseToCell(X, Y, col, row);
  CellRect := Grid.CellRect(col, row);
  CurrSelItem.Index := (Y - CellRect.Top) div ElementHeight;
  CurrSelItem.Cell.Col := col;
  CurrSelItem.Cell.Row := row;
  if ItemByID(CurrSelItem) <> nil then begin
    if ((LastSelItem.Index <> CurrSelItem.Index) or
       (LastSelItem.Cell.Col <> CurrSelItem.Cell.Col) or
       (LastSelItem.Cell.Row <> CurrSelItem.Cell.Row)) and
       (ItemByID(LastSelItem) <> nil) then
         ItemByID(LastSelItem).DrawBtn := false;
    ItemByID(CurrSelItem).DrawBtn := true;
    LastSelItem := CurrSelItem;
  end;
  MousePos.X := X;
  MousePos.Y := Y;
end;

procedure TScheduleForm.GridSelectCell(Sender: TObject; aCol, aRow: Integer;
  var CanSelect: Boolean);
begin
  if (Items = nil) or (aRow < 0) or (aCol < 0) or
     (aRow-1 > High(Items)) or (aCol-1 > High(Items[aRow-1])) then
    Exit;

  with LastSelectedCell do begin
    Col := aCol;
    Row := aRow;
  end;

  EditCellBtns.Rect := Grid.CellRect(aCol, aRow);
end;

procedure TScheduleForm.CheckGroupClick(Sender: TObject; Index: Integer);
begin
  VisibleColumns[Index] := TCheckGroup(Sender).Checked[Index];
  SetCurrElemHeight();
  Grid.Invalidate();
end;

procedure TScheduleForm.OpenFilterPanelButtonClick(Sender: TObject);
begin
  //VisibleColsPanel.Open();
  if FilterList.Count = 0 then Exit;
  VisibleColsPanel.Close();
  if FilterPanel.Width > 0 then
    FilterPanel.Close()
  else
    FilterPanel.Open();
end;

procedure TScheduleForm.OnInsertBtnClick(Sender: TObject);
begin
  with TScheduleItem(Sender) do
    EditForms.ShowEditForm(Self, SchTable, 0, @RefreshButtonClick, [XColVal, YColVal]);
end;

procedure TScheduleForm.OnEditBtnClick(Sender: TObject);
begin
  with TScheduleItem(Sender) do
    EditForms.ShowEditForm(Self, SchTable, ItemByID(LastSelItem).ItemID, @RefreshButtonClick, [XColVal, YColVal]);
end;

procedure TScheduleForm.OnDeleteBtnClick(Sender: TObject);
begin
  with TScheduleItem(Sender) do begin


  end;
  RefreshButtonClick(Self);
end;

procedure TScheduleForm.OnFilterAdd(Sender: TObject);
begin
  OpenFilterPanelButton.Caption := Format('Фильтры(%d)', [FilterList.Count]);
  SetDataIsNotActual();
end;

procedure TScheduleForm.OnFilterDel(Sender: TObject);
begin
  If FilterList.Count = 0 then FilterPanel.Close();
  OpenFilterPanelButton.Caption := Format('Фильтры(%d)', [FilterList.Count]);
  SetDataIsNotActual();
end;

procedure TScheduleForm.OnFilterHeightChange(Sender: TObject);
begin
  FilterPanel.Height := FilterList.Height;
end;

procedure TScheduleForm.OnFilterChange(Sender: TObject);
begin
  SetDataIsNotActual();
end;

procedure TScheduleForm.OpenHideColumnsPanelClick(Sender: TObject);
begin
  FilterPanel.Close();
  if VisibleColsPanel.Width > 0 then
    VisibleColsPanel.Close()
  else
    VisibleColsPanel.Open();
end;

procedure TScheduleForm.XColumnBoxChange(Sender: TObject);
begin
  FillXTitles();
  SetColumnCaptions();
  FillItems();
  Grid.Refresh();
end;

procedure TScheduleForm.YColumnBoxChange(Sender: TObject);
begin
  FillYTitles();
  SetRowCaptions();
  FillItems();
  Grid.Refresh();
end;

procedure TScheduleForm.SetDataIsActual;
begin
  FSomethingChanged := false;
  RefreshButton.Enabled := false;
end;

procedure TScheduleForm.SetDataIsNotActual;
begin
  FSomethingChanged := true;
  RefreshButton.Enabled := true;
end;

procedure TScheduleForm.SetCaptions();
begin
  SetColumnCaptions();
  SetRowCaptions();
end;

function TScheduleForm.CreateLookupQuery(ColumnIndex: Integer): TSQLQueryCreator;
begin
  Result := TSQLQueryCreator.Create(TTableInfo(SchTable.Columns[ColumnIndex].RefTable));
  Result.SelectCols(cstOwn);
  Result.SetOrderBy([COL_ID]);
  Result.SendQuery();
end;

procedure TScheduleForm.SendQuery();
begin;
  Query.SelectCols(cstOwn);
  Query.SendQuery();
end;

procedure TScheduleForm.ClearItems;
var
  i, j, k, z: Integer;
begin
  for i := 0 to High(Items) do
    for j := 0 to High(Items[i]) do
      for k := 0 to High(Items[i][j]) do
         FreeAndNIl(Items[i][j][k]);
end;

procedure TScheduleForm.SetColumnCaptions();
var
  i: Integer;
begin
  Grid.Columns.Clear();
  for i := 0 to High(XTitles) do
    Grid.Columns.Add.Title.Caption := XTitles[i].Value;
  Grid.ColWidths[0] := 96;
end;

procedure TScheduleForm.SetRowCaptions();
begin
  Grid.RowCount := Length(YTitles) + 1;
  Grid.RowHeights[0] := ColCaptionsHeight;
  {for i := 0 to High(YTitles) do
    Grid.Rows[i+1].Text := YTitles[i].Value;}
end;

procedure TScheduleForm.SetCurrElemHeight;
var
  i: Integer;
begin
  ElementHeight := 30;
  for i := 0 to High(VisibleColumns) do
    if VisibleColumns[i] then
       ElementHeight += RowHeight;
  Grid.DefaultRowHeight := ElementHeight;
  Grid.RowHeights[0] := ColCaptionsHeight;
end;

function TScheduleForm.GetListHeight(aRow, aCol: Integer): Integer;
begin
  Result := Max(Length(Items[aRow-1][aCol-1])*ElementHeight, ElementHeight);
end;

function TScheduleForm.GetLookupResult(aColIndex: Integer): TLookupResult;
var
  LookupQuery: TSQLQueryCreator;
  i: Integer;
begin
  LookupQuery := CreateLookupQuery(aColIndex);
  SetLength(Result, LookupQuery.SQLQuery.RecordCount);
  i := 0;
  with LookupQuery.SQLQuery do
    while not LookupQuery.SQLQuery.EOF do begin
      if Length(Result) < RecordCount then
        SetLength(Result, RecordCount);
      Result[i].ID := Fields[0].Value;
      Result[i].Value := Fields[1].Value;
      Next();
      inc(i);
    end;
  FreeAndNil(LookupQuery);
end;

function TScheduleForm.GetXColInfo(aColIndex: Integer): TCOlumnInfo;
begin
  Result := SchTable.Columns[XColumnBox.ItemIndex];
end;

function TScheduleForm.GetYColInfo(aRowIndex: Integer): TCOlumnInfo;
begin
  Result := SchTable.Columns[YColumnBox.ItemIndex];
end;

function TScheduleForm.ItemByID(aItemID: TItemIndex): TScheduleItem;
begin
  with aItemId do begin
    if (Cell.Row <= 0) or (Cell.Row-1 > High(Items)) or
       (Cell.Col <= 0) or (Cell.Col-1 > High(Items[Cell.Row-1])) or
       (Index < 0) or (Index > High(Items[Cell.Row-1][Cell.Col-1]))
         then Exit(nil);
    Result := Items[Cell.Row-1][Cell.Col-1][Index];
  end;
end;

procedure TScheduleForm.FillXTitles();
begin
  XTitles := GetLookupResult(SchTable.GetOwnColByRef(FColumns[XColumnBox.ItemIndex]).ID);
end;

procedure TScheduleForm.FillYTitles();
begin
  YTitles := GetLookupResult(SchTable.GetOwnColByRef(FColumns[YColumnBox.ItemIndex]).ID);
end;

procedure TScheduleForm.FillLookupResults();
begin
  FillXTitles();
  FillYTitles();
end;

procedure TScheduleForm.FillItems();
var
  i, j, k: Integer;
  YFieldID, XFieldID, ColumnLen: Integer;
  XIDCol, YIDCol: TColumnInfo;
  XLen, YLen: Integer;
  ColVal: TColumnVal;
begin
  XLen := Length(XTitles);
  YLen := Length(YTitles);
  ColumnLen := Length(FColumns);
  XIDCol := SchTable.GetOwnColByRef(FColumns[XColumnBox.ItemIndex]);
  YIDCol := SchTable.GetOwnColByRef(FColumns[YColumnBox.ItemIndex]);
  Query.SelectCols(cstAll);
  Query.SetOrderBy([YIDCol, XIDCol, SchTable.GetColByName('time_id')]);
  Query.SendQuery();
  XFieldID := Query.SQLQuery.FieldByName(XIDCol.AliasName()).Index;
  YFieldID := Query.SQLQuery.FieldByName(YIDCol.AliasName()).Index;

  ClearItems();
  Items := nil;
  SetLength(Items, YLen);
  for i := 0 to YLen - 1 do
    SetLength(Items[i], XLen);
  i := 0;
  j := 0;
  with Query.SQLQuery do
    while not EOF do begin
      while Fields[YFieldID].AsInteger > YTitles[i].ID do begin inc(i); j := 0; end;
      while Fields[XFieldID].AsInteger > XTitles[j].ID do inc(j);
      SetLength(Items[i][j], Length(Items[i][j]) + 1);
      Items[i][j][High(Items[i][j])] := TScheduleItem.Create(Grid, FieldByName(SchTable.GetPrimaryCol.AliasName).AsInteger);
      SetLength(Items[i][j][High(Items[i][j])].Fields, ColumnLen);
      with Items[i][j][High(Items[i][j])] do begin
        OnInsertClick := @OnInsertBtnClick;
        OnEditClick := @OnEditBtnClick;
        OnDeleteClick := @OnDeleteBtnClick;
        Height := ElementHeight;

        ColVal.ColumnInfo := XIDCol;
        ColVal.Value := Query.SQLQuery.Fields[XFieldID].AsInteger;
        XColVal := ColVal;

        ColVal.ColumnInfo := YIDCol;
        ColVal.Value := Query.SQLQuery.Fields[YFieldID].AsInteger;
        YColVal := ColVal;

        VisibleColIDs := @VisibleColumns;

        for k := 0 to ColumnLen - 1 do begin
          if FColumns[k].FieldType = ftDate then
            Items[i][j][High(Items[i][j])].Fields[k] := FormatDateTime('hh:mm', FieldByName(FColumns[k].AliasName).AsDateTime)
          else
            Items[i][j][High(Items[i][j])].Fields[k] := FieldByName(FColumns[k].AliasName).AsString;
          //ShowMessage(FieldByName(FColumns[k].AliasName).AsString);
          Items[i][j][High(Items[i][j])].Columns := FColumns;
        end;
      end;
      Next();
    end;
end;

procedure TScheduleItem.EditClick(Sender: TObject);
begin
  if FOnEditClick <> nil then FOnEditClick(Self);
end;

procedure TScheduleItem.InsertClick(Sender: TObject);
begin
  if FOnInsertClick <> nil then FOnInsertClick(Self);
end;

procedure TScheduleItem.DeleteClick(Sender: TObject);
begin
  if FOnDeleteClick <> nil then FOnDeleteClick(Self);
end;

procedure TScheduleItem.SetRect(aRect: TRect);
begin
  FEditCellBtns.Rect := aRect;
  FBorderRect := aRect;
end;

procedure TScheduleItem.SetDrawBtn(aVal: boolean);
begin
  FEditCellBtns.Visible := aVal;
end;

function TScheduleItem.GetDrawBtn: boolean;
begin
  Result := FEditCellBtns.Visible;
end;

constructor TScheduleItem.Create(aOwner: TWinControl; aItemID: Integer);
begin
  FOwner := aOwner;
  FItemID := aItemID;
  FEditCellBtns := TEditCellBtns.Create(aOwner);
  with FEditCellBtns do begin
    OnEditClick := @EditClick;
    OnInsertClick := @InsertClick;
    OnDeleteClick := @DeleteClick;
  end;
end;

destructor TScheduleItem.Destroy;
begin
  FreeAndNil(FEditCellBtns);
end;

procedure TScheduleItem.Draw(aCanvas: TCanvas);
const
  LeftSpace = 5;
  TopSpace = 5;
  Space = 5;
var
  i, CurrHeight: Integer;
begin
  CurrHeight := TopSpace;
  for i := 0 to High(Fields) do
    if VisibleColIDs^[i] then begin
      aCanvas.TextOut(BorderRect.Left + LeftSpace, BorderRect.Top + CurrHeight, Columns[i].Caption + ': ' + Fields[i]);
      CurrHeight += RowHeight;
    end;
  aCanvas.Pen.Color := clGray;
  if Conflicts.IsConflict(ItemID) then
    ScheduleForm.ImageList.Draw(aCanvas, BorderRect.Right - 30, BorderRect.Bottom - 30, 1, true);
  aCanvas.Line(BorderRect.Left + LeftSpace, BorderRect.Top + CurrHeight, BorderRect.Right - LeftSpace, BorderRect.Top + CurrHeight);
end;

constructor TScheduleItemList.Create(aOwner: TWinControl);
begin
  FScrollBar := TScrollBar.Create(aOwner);
  with FScrollBar do begin
    Parent := aOwner;
    Visible := false;
  end;
end;

destructor TScheduleItemList.Destroy();
begin
  FreeAndNil(FScrollBar);
  inherited;
end;

procedure TScheduleItemList.InitItems();
var
  i: Integer;
begin
  SetLength(FItems, Length(FStringsArr));
  for i := 0 to High(FItems) do
    with FItems[i] do begin
      Canvas := FCanvas;
      Height := ElemHeight;
      Fields := FStringsArr[i];
      //Fill := true;
    end;
end;

procedure TScheduleItemList.ShowItems();
begin
  InitItems();
  if Length(FItems) * ElemHeight > FHeight then
    with FScrollBar do begin
      Height := FHeight;
      Visible := true;
    end;
end;

procedure TScheduleItemList.Draw(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to High(FItems) do
    with FItems[i] do begin
      BorderRect := Rect(FRect.Left, FRect.Top + i * ElemHeight, FRect.Right, FRect.Bottom + i * ElemHeight);
      Draw(Canvas);
    end;
end;

function TScheduleItemList.GetElemHeight(): Integer;
begin
  Result := FRect.Bottom - FRect.Top;
end;

procedure TScheduleItemList.SetElemHeight(aHeight: Integer);
begin
  FRect.Bottom := Frect.Top + aHeight;
  InitItems();
end;

function TScheduleItemList.GetElemWidth(): Integer;
begin
  Result := FRect.Right - FRect.Left;
end;

procedure TScheduleItemList.SetElemWidth(aWidth: Integer);
begin
  FRect.Right := Frect.Left + aWidth;
end;

procedure TScheduleItemList.SetStringsArr(aStringsArr: TStringsArr);
begin
  FStringsArr := aStringsArr;
  InitItems();
end;

initialization
SchTable := ListOfTables.GetTableByName('Schedule_Items');

end.

