unit UTables;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DBGrids, DbCtrls, sqldb, db, Menus, Types, Dialogs, variants;

type

  Strings = TStringDynArray;

  TFilterInfo = record
    FieldName: String;
    ParamName: String;
    Condition: String;
    FieldType: TFieldType;
    Value: String;
  end;

  TFilterInfos = array of TFilterInfo;

  TColumnInfo = record
    Name: String;
    Caption: String;
    IsReference: boolean;
    ReferenceTable: String;
    ReferenceName: String;
    FieldType: TFieldType;
    Size: Integer;
  end;

  TColumnVal = record
    ColumnInfo: TColumnInfo;
    Value: Variant;
  end;

  TSortOrder = record
    ColumnIndex: Integer;
    SortById: boolean;
  end;

  TSortOrders = array of TSortOrder;
  TColumnInfos = array of TColumnInfo;
  TColumnVals = array of TColumnVal;

  TTableInfo = class
  private
    FColumnInfos: TColumnInfos;
    FFilterInfos: TFilterInfos;
    FCaption: String;
    FName: String;
    FOrderBy: TSortOrders;
    FSortOrder: boolean;
    function AddColumn(aName, aCaption, aReferenceName: String; aFieldType: TFieldType; aSize: Integer): TTableInfo;
    function AddColumn(aName, aCaption: String; aFieldType: TFieldType; aSize: Integer): TTableInfo;
    procedure SetCaptions(DBGrid: TDBGrid);
  public
    constructor Create(aCaption, aName: String);
    function GetColumnCaptions(): Strings;
    function GetColIndexByName(aName: String): Integer;
    function GetGeneratorName: String;
    function ColCount(): Integer;
    function CreateQuery(): String;
    procedure SendQuery(SQLQuery: TSQLQuery);
    procedure SetFilters(aFilters: TFilterInfos);
    procedure SetOrderBy(const FieldIDs: array of Integer);
    procedure SetOrderBy(const SortOrders: TSortOrders);
    procedure ShowItems(SQLQuery: TSQLQuery; DBGrid: TDBGrid);
    property GetCaption: String read FCaption;
    property GetColumnInfos: TColumnInfos read FColumnInfos;
    property GetName: String read FName;
    property SortOrder: boolean read FSortOrder write FSortOrder;
  end;
  TTableInfoClass = class of TTableInfo;
  TTableInfos = array of TTableInfo;

  TListOfTables = class
  private
    FTableInfos: TTableInfos;
    function Add(aCaption, aName: String): TTableInfo;
  public
    function GetCaptions(): Strings;
    function GetTableByName(const aName: String): TTableInfo;
    procedure ShowTable(index: Integer; SQLQuery: TSQLQuery; DBGrid: TDBGrid);
    property GetTable: TTableInfos read FTableInfos;
    constructor Create();
  end;

  procedure InitListOfTables(aList: TListOfTables);
  function CreateListOfTables(): TListOfTables;
  function GetColName(aColumn: TColumnInfo): String;
  function GetColFullName(aColumn: TColumnInfo): String;
  function GetColSQLName(aColumn: TColumnInfo): String;

const
  SORT_BY_NAME = 0;
  SORT_BY_ID = 1;

  COL_SUBJECT_ID = 0;
  COL_SUBJECT_TYPE_ID = 1;
  COL_PROFESSOR_ID = 2;
  COL_TIME_ID = 3;
  COL_DAY_ID = 4;
  COL_GROUP_ID = 5;
  COL_ROOM_ID = 6;

{var
  ListOfTables: TListOfTables;}

implementation

function TTableInfo.CreateQuery(): String;
var
  i: Integer;

  function GetFullColName(i, j : Integer): String;
  begin
    if FColumnInfos[i].IsReference then
      Result := FColumnInfos[j].ReferenceTable + '.' +  FColumnInfos[j].ReferenceName
    else
      Result := FName + '.' + FColumnInfos[j].Name + ' ';
  end;

begin
  Result := Format('select %s.ID, ', [FName]);
  for i := 0 to High(FColumnInfos) do begin
    Result += Format('%s as %s', [GetFullColName(i, i), GetColSqlName(FColumnInfos[i])]);
    if FColumnInfos[i].IsReference then
      Result += Format(', %s.ID as %s', [FColumnInfos[i].ReferenceTable, FColumnInfos[i].Name]);
    if i < High(FColumnInfos) then Result += ', ';
  end;
  Result += ' from ' + FName;
  for i := 0 to High(FColumnInfos) do begin
    if not FColumnInfos[i].IsReference then continue;
    Result += Format(' inner join %s on %s.%s = %s.ID ',
              [FColumnInfos[i].ReferenceTable, FNAme, FColumnInfos[i].Name,
               FColumnInfos[i].ReferenceTable]);
  end;
  if Length(FFilterInfos) > 0 then
    Result += ' where ';
  for i := 0 to High(FFilterInfos) do begin
    Result += Format(' %s %s :%s ', [FFilterInfos[i].FieldName, FFilterInfos[i].Condition,
              FFilterInfos[i].ParamName]);
    if i < High(FFilterInfos) then Result += ' and ';
  end;
  if Length(FOrderBy) > 0 then begin
    Result += ' order by ';
    for i := 0 to High(ForderBy) do begin
      if FOrderBy[i].SortById then
        Result += FColumnInfos[FOrderBy[i].ColumnIndex].Name
      else
        Result += GetFullColName(i, FOrderBy[i].ColumnIndex);
      if i < High(ForderBy) then Result += ', ';
    end;
    if FSortOrder then Result += ' desc ';
  end;
  //ShowMessage(Result);
end;

procedure TTableInfo.SendQuery(SQLQuery: TSQLQuery);
var
  i: Integer;
begin
  SQLQuery.Close();
  SQLQuery.Params.Clear();
  for i := 0 to High(FFilterInfos) do
    with FFilterInfos[i] do
      SQLQuery.Params.CreateParam(FieldType, ParamName, ptInput).Text := Value;
  SQLQuery.SQL.Text := CreateQuery();
  SQLQuery.Open();
  //SQLQuery.Edit();
  //SQLQuery.Last();
  //SQLQuery.Insert();
end;

function TTableInfo.AddColumn(aName, aCaption, aReferenceName: String; aFieldType: TFieldType; aSize: Integer): TTableInfo;
begin
  SetLength(FColumnInfos, length(FColumnInfos) + 1);
  with FColumnInfos[High(FColumnInfos)] do begin
       Name := aName;
       Caption := aCaption;
       IsReference := aReferenceName <> '';
       if IsReference then begin
          ReferenceTable := LeftStr(aName, Length(aName) - 3) + 's';
          ReferenceName := aReferenceName;
       end else
          ReferenceTable := FName;
       FieldType := aFieldType;
       Size := aSize;
  end;
  Result := Self;
end;

function TTableInfo.AddColumn(aName, aCaption: String; aFieldType: TFieldType; aSize: Integer): TTableInfo;
begin
  Result := AddColumn(aName, aCaption, '', aFieldType, aSize);
end;

procedure TTableInfo.SetCaptions(DBGrid: TDBGrid);
var
  i, j: Integer;
begin
  DBGrid.Columns[0].Visible := false;
  j := 0; i := 0;
  while i < DBGrid.Columns.Count do begin
    if (j <= High(FColumnInfos)) and (LowerCase(GetColSqlName(FColumnInfos[j])) = LowerCase(DBGrid.Columns[i].Title.Caption)) then begin
      DBGrid.Columns[i].Title.Caption := FColumnInfos[j].Caption;
      DBGrid.Columns[i].Width := FColumnInfos[j].Size;
      inc(j); inc(i);
    end else begin
      DBGrid.Columns[i].Visible := false;
      inc(i);
    end;
  end;
end;

procedure TTableInfo.SetFilters(aFilters: TFilterInfos);
begin
  FFilterInfos := aFilters;
end;

procedure TTableInfo.SetOrderBy(const FieldIDs: array of Integer);
var
  i: Integer;
  OrderBy: TSortOrders;
begin
  if (Length(FieldIDs) and 1) > 0 then Exit;
  if (Length(FieldIDs) div 2) > Length(FColumnInfos) then Exit;
  i := 0;
  while i < High(FieldIDs) do begin
    if FieldIDs[i] > High(FColumnInfos) then Exit;
    i += 2;
  end;
  SetLength(OrderBy, Length(FieldIDs) div 2);
  i := 0;
  while i < High(FieldIDs) do begin
    OrderBy[i div 2].ColumnIndex := FieldIDs[i];
    OrderBy[i div 2].SortById := FieldIDs[i+1] > 0;
    i += 2;
  end;
  SetOrderBy(OrderBy);
end;

procedure TTableInfo.SetOrderBy(const SortOrders: TSortOrders);
var
  i: Integer;
begin
  for i := 0 to High(SortOrders) do begin
    if (SortOrders[i].ColumnIndex > High(FColumnInfos)) or
       (SortOrders[i].ColumnIndex < 0) then
         Exit;
  end;
  FOrderBy := SortOrders;
end;

function TTableInfo.GetColumnCaptions: Strings;
var
  i: Integer;
begin
  SetLength(Result, Length(FColumnInfos));
  for i := 0 to High(Result) do
    Result[i] := FColumnInfos[i].Caption;
end;

function TTableInfo.GetColIndexByName(aName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FColumnInfos) do
    if FColumnInfos[i].Name = aName then Exit(i);
end;

function TTableInfo.ColCount(): Integer;
begin
  Result := Length(FColumnInfos);
end;

constructor TTableInfo.Create(aCaption, aName: String);
begin
  FCaption := aCaption;
  FName := aName;
  FOrderBy := nil;
end;

procedure TTableInfo.ShowItems(SQLQuery: TSQLQuery; DBGrid: TDBGrid);
begin
  SendQuery(SQLQuery);
  SetCaptions(DBGrid);
end;

//ListOfTables

function TListOfTables.Add(aCaption, aName: String): TTableInfo;
begin
  Result := TTableInfo.Create(aCaption, aName);
  SetLength(FTableInfos, Length(FTableInfos) + 1);
  FTableInfos[High(FTableInfos)] := Result;
end;

function TListOfTables.GetCaptions(): Strings;
var
  i: Integer;
begin
  SetLength(Result, Length(FTableInfos));
  for i := 0 to high(Result) do begin
       Result[i] := FTableInfos[i].GetCaption;
  end;
end;

function TListOfTables.GetTableByName(const aName: String): TTableInfo;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to high(FTableInfos) do begin
     if LowerCase(FTableInfos[i].GetName) = LowerCase(aName) then
       Exit(FTableInfos[i]);
  end;
end;

procedure TListOfTables.ShowTable(index: Integer; SQLQuery: TSQLQuery; DBGrid: TDBGrid);
begin
  FTableInfos[index].ShowItems(SQLQuery, DBGrid);
end;

constructor TListOfTables.Create();
begin

end;

function TTableInfo.GetGeneratorName(): String;
begin
  Result := FName + '_GEN';
end;

function GetColName(aColumn: TColumnInfo): String;
begin
  if aColumn.IsReference then
    Result := aColumn.ReferenceName
  else
    Result := aColumn.Name;
end;

function GetColFullName(aColumn: TColumnInfo): String;
begin
  Result := aColumn.ReferenceTable + '.';
  if aColumn.IsReference then
    Result += aColumn.ReferenceName
  else
    Result += aColumn.Name;
end;

function GetColSQLName(aColumn: TColumnInfo): String;
begin
  Result := aColumn.ReferenceTable;
  if aColumn.IsReference then
    Result += aColumn.ReferenceName
  else
    Result += aColumn.Name;
end;

procedure InitListOfTables(aList: TListOfTables);
begin
  with aList do begin
  Add('Предметы', 'subjects').
    AddColumn('name', 'Название предмета', ftString, 400);
  Add('Группы', 'groups').
    AddColumn('name', 'Группа', ftString, 100).
    AddColumn('group_size', 'Размер группы', ftInteger, 100);
  Add('Преподаватели', 'professors').
    AddColumn('name', 'Преподаватель', ftString, 150);
  Add('Кабинеты', 'rooms').
    AddColumn('name', 'Номер кабинета', ftString, 100).
    AddColumn('room_size', 'Вместимость', ftInteger, 80);
  Add('Расписание', 'schedule_items').
    AddColumn('subject_id', 'Название предмета', 'name', ftString, 400).
    AddColumn('subject_type_id', 'Тип', 'name', ftString, 50).
    AddColumn('professor_id', 'Преподаватель','name', ftString, 150).
    AddColumn('time_id', 'Начало', 'begin_time', ftDate, 70).
    AddColumn('day_id', 'День недели', 'name', ftDate, 100).
    AddColumn('group_id', 'Группа', 'name', ftString, 100).
    AddColumn('room_id', 'Номер кабинета', 'name', ftString, 100);
  end;
end;

function CreateListOfTables(): TListOfTables;
begin
  Result := TListOfTables.Create();
  InitListOfTables(Result);
end;

initialization
//ListOfTables := TListOfTables.Create();
//InitListOfTables(ListOfTables);

end.

