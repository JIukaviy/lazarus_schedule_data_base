unit UMetaData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DBGrids, DbCtrls, sqldb, db, Menus, Types, Dialogs, variants, Math;

const
  SORT_BY_NAME = 0;
  SORT_BY_ID = 1;

  COL_DEF_SIZE = 0;

  COL_ID = 0;
  COL_SUBJECT_ID = 0;
  COL_SUBJECT_TYPE_ID = 1;
  COL_PROFESSOR_ID = 2;
  COL_TIME_BEGIN_ID = 3;
  COL_TIME_END_ID = 4;
  COL_DAY_ID = 5;
  COL_WEEK_ID = 6;
  COL_GROUP_ID = 7;
  COL_ROOM_ID = 8;

type

  Strings = TStringDynArray;

  { TColumnInfo }

  TColumnOption = (coVisible, coEditable);
  TColumnOptions = Set of TColumnOption;

  TColSelType = (cstAll, cstRef, cstOwn);

  TColumnInfo = object
    Name: String;
    Caption: String;
    RefTable: TObject;
    Owner: TObject;
    RefKeyColID: Integer;
    RefCols: array of TColumnInfo;
    LinkedCols: array of TColumnInfo;
    FieldType: TFieldType;
    Size: Integer;
    Options: TColumnOptions;
    ID: Integer;
    procedure AddLink(aCol: TColumnInfo);
    function RefKeyCol(): TColumnInfo;
    //function LinkedColByTblID(aTblID: Integer): TColumnInfo;
    function RefColCount(): Integer;
    function RefTableName(): String;
    function OwnName(): String;
    function AliasName(): String;
  end;

  TSortOrder = record
    ColumnIndex: Integer;
    SortById: boolean;
  end;

  TColumnVal = record
    ColumnInfo: TColumnInfo;
    Value: Variant;
  end;

  TColumnInfos = array of TColumnInfo;
  TColumnVals = array of TColumnVal;

  { TTableInfo }

  TTableInfo = class
  private
    FColumnInfos: TColumnInfos;
    FCaption: String;
    FName: String;
    FID: Integer;
    //FLinkedTbls: array of TTableInfo;
    function AddColumn(aName, aCaption: String; aFieldType: TFieldType; aSize: Integer; aOptions: TColumnOptions = [coVisible, coEditable]): TTableInfo;
    function AddColumn(aName: String; aRefTable: TObject; const aVisibleColIDs: array of Integer;
          aRefColID: Integer = COL_ID; aOptions: TColumnOptions = [coEditable]): TTableInfo;
    function AddColumn(aName, aCaption: String; aFieldType: TFieldType;
          aRefTable: TObject; const aVisibleColIDs: array of Integer; aRefColID: Integer;
          aSize: Integer; aOptions: TColumnOptions): TTableInfo;
    function AddIDColumn(): TTableInfo;
    function GetRefCols(): TColumnInfos;
  public
    constructor Create(aCaption, aName: String);
    function GetColIndexByName(aName: String): Integer;
    function GetCols(aColSelType: TColSelType): TColumnInfos;
    function GetCols(aColSelType: TColSelType; aOptions: TColumnOptions): TColumnInfos;
    function GetColByName(aName: String): TColumnInfo;
    function GetGeneratorName(): String;
    function GetPrimaryCol(): TColumnInfo;
    //function GetLinkedTables(): array of TTableInfo;
    function GetOwnColByRef(aCol: TColumnInfo): TColumnInfo;
    function ColCount(): Integer;
    property Caption: String read FCaption;
    property Columns: TColumnInfos read FColumnInfos;
    property Name: String read FName;
    property ID: Integer read FID write FID;
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
    property Tables: TTableInfos read FTableInfos;
    constructor Create();
  end;

  function GetColCaptions(aCols: TColumnInfos): Strings;
  function GetColsWithOpts(aCols: TColumnInfos; aOptions: TColumnOptions): TColumnInfos;

  procedure InitListOfTables(aList: TListOfTables);
  function CreateListOfTables(): TListOfTables;

var
  ListOfTables: TListOfTables;

implementation

function GetColCaptions(aCols: TColumnInfos): Strings;
var
  i: Integer;
begin
  SetLength(Result, Length(aCols));
  for i := 0 to High(aCols) do
    Result[i] := aCols[i].Caption;
end;

function GetColsWithOpts(aCols: TColumnInfos; aOptions: TColumnOptions): TColumnInfos;
var
  i, pos: Integer;
begin
  pos := 0;
  SetLength(Result, Length(aCols));
  for i := 0 to High(aCols) do
    if aOptions <= aCols[i].Options then begin
      Result[pos] := aCols[i];
      inc(pos)
    end;
  SetLength(Result, pos);
end;

{ TColumnInfo }

procedure TColumnInfo.AddLink(aCol: TColumnInfo);
var
  TblId: Integer;
begin
  TblId := TTableInfo(aCol.Owner).ID;
  SetLength(LinkedCols, Max(TblID + 1, Length(LinkedCols)));
  LinkedCols[TblID] := aCol;
end;

function TColumnInfo.RefKeyCol: TColumnInfo;
begin
  Result := TTableInfo(RefTable).Columns[RefKeyColID];
end;

{function TColumnInfo.LinkedColByTblID(aTblID: Integer): TColumnInfo;
begin
  if aTblID = TTableInfo(Owner).ID then
    Exit(Self);
  if aTblID > High(LinkedCols) then
    Exit(nil);
  Exit(LinkedCols[aTblID]);
end;}

function TColumnInfo.RefColCount: Integer;
begin
  Result := Length(RefCols);
end;

function TColumnInfo.RefTableName: String;
begin
  if FieldType = ftReference then
    Result := TTableInfo(RefTable).Name
  else
    Result := '';
end;

function TColumnInfo.OwnName: String;
begin
  Result := Format('%s.%s', [TTableInfo(Owner).Name, Name]);
end;

function TColumnInfo.AliasName: String;
begin
  Result := Format('%s%s', [TTableInfo(Owner).Name, Name])
end;

{ TTableInfo }

function TTableInfo.GetColIndexByName(aName: String): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FColumnInfos) do
    if FColumnInfos[i].Name = aName then Exit(i);
end;

function TTableInfo.GetCols(aColSelType: TColSelType): TColumnInfos;
var
  ResLen, i: Integer;
  RefCols: TColumnInfos;
begin
  if (aColSelType = cstAll) or (aColSelType = cstOwn) then
    Result := FColumnInfos;

  if (aColSelType = cstAll) or (aColSelType = cstRef) then begin
    RefCols := GetRefCols();
    ResLen := Length(Result);
    SetLength(Result, ResLen + Length(RefCols));
    //Move(RefCols, Result[ResLen], Length(RefCols));
    for i := 0 to High(RefCols) do
      Result[ResLen + i] := RefCols[i];
  end;
end;

function TTableInfo.GetCols(aColSelType: TColSelType;
  aOptions: TColumnOptions): TColumnInfos;
begin
  Result := GetColsWithOpts(GetCols(aColSelType), aOptions);
end;

function TTableInfo.GetColByName(aName: String): TColumnInfo;
var
  ColID: Integer;
begin
  ColID := GetColIndexByName(aName);
  if ColID < 0 then begin
    ShowMessage(Format('Колонка с именем: %s не найдена в таблице: %s', [aName, FName]));
    Exit;
  end;
  Exit(FColumnInfos[ColID]);
end;

function TTableInfo.GetRefCols: TColumnInfos;
var
  i, j, pos: Integer;
begin
  pos := 0;
  for i := 0 to High(FColumnInfos) do begin
    with FColumnInfos[i] do
      if FieldType = ftReference then begin
        SetLength(Result, Length(Result) + RefColCount());
        for j := 0 to RefColCount() - 1 do begin
          Result[pos] := RefCols[j];
          inc(pos);
        end;
      end;
  end;
end;

function TTableInfo.GetOwnColByRef(aCol: TColumnInfo): TColumnInfo;
begin
  Result := aCol.LinkedCols[ID];
end;

function TTableInfo.ColCount(): Integer;
begin
  Result := Length(FColumnInfos);
end;

function TTableInfo.AddColumn(aName, aCaption: String; aFieldType: TFieldType; aSize: Integer; aOptions: TColumnOptions): TTableInfo;
begin
  Result := AddColumn(aName, aCaption, aFieldType, nil, [], -1, aSize, aOptions);
end;

function TTableInfo.AddColumn(aName: String; aRefTable: TObject; const aVisibleColIDs: array of Integer;
          aRefColID: Integer; aOptions: TColumnOptions): TTableInfo;
begin
  Result := AddColumn(aName, '', ftReference, aRefTable, aVisibleColIDs, aRefColID, COL_DEF_SIZE, aOptions);
end;

function TTableInfo.AddColumn(aName, aCaption: String; aFieldType: TFieldType;
          aRefTable: TObject; const aVisibleColIDs: array of Integer; aRefColID: Integer;
          aSize: Integer; aOptions: TColumnOptions): TTableInfo;
var
  i: Integer;
begin
  if (aFieldType = ftReference) and (aRefTable = nil) then begin
    ShowMessage(Format('%s, %s. Не указана ссылка на таблицу.', [FName, aName]));
    Exit;
  end;

  SetLength(FColumnInfos, length(FColumnInfos) + 1);
  with FColumnInfos[High(FColumnInfos)] do begin
    Name := aName;
    Caption := aCaption;
    RefTable := aRefTable;
    RefKeyColID := aRefColID;
    FieldType := aFieldType;
    Options := aOptions;
    Owner := Self;
    Size := aSize;
    ID := High(FColumnInfos);

    SetLength(RefCols, Length(aVisibleColIDs));
    for i := 0 to High(aVisibleColIDs) do begin
      RefCols[i] := TTableInfo(aRefTable).Columns[aVisibleColIDs[i]];
      RefCols[i].AddLink(FColumnInfos[High(FColumnInfos)]);
    end;
    if (aFieldType = ftReference) then
       Caption := RefCols[0].Caption;
  end;

  Result := Self;
end;

function TTableInfo.GetGeneratorName(): String;
begin
  Result := FName + '_GEN';
end;

function TTableInfo.GetPrimaryCol: TColumnInfo;
begin
  Result := Columns[COL_ID];
end;

function TTableInfo.AddIDColumn: TTableInfo;
begin
  AddColumn('id', 'ID', ftInteger, 20, []);
end;

constructor TTableInfo.Create(aCaption, aName: String);
begin
  FCaption := aCaption;
  FName := aName;
  AddIDColumn();
end;

//ListOfTables

function TListOfTables.Add(aCaption, aName: String): TTableInfo;
begin
  Result := TTableInfo.Create(aCaption, aName);
  SetLength(FTableInfos, Length(FTableInfos) + 1);
  Result.ID := High(FTableInfos);
  FTableInfos[High(FTableInfos)] := Result;
end;

function TListOfTables.GetCaptions(): Strings;
var
  i: Integer;
begin
  SetLength(Result, Length(FTableInfos));
  for i := 0 to high(Result) do begin
       Result[i] := FTableInfos[i].Caption;
  end;
end;

function TListOfTables.GetTableByName(const aName: String): TTableInfo;
var
  i: Integer;
begin
  Result := nil;
  for i := 0 to high(FTableInfos) do begin
     if LowerCase(FTableInfos[i].Name) = LowerCase(aName) then
       Exit(FTableInfos[i]);
  end;
end;

constructor TListOfTables.Create();
begin

end;

procedure InitListOfTables(aList: TListOfTables);
begin
  with aList do begin
  Add('Предметы', 'subjects').
    AddColumn('name',       'Название предмета',  ftString,   400);

  Add('Типы предметов', 'subject_types').
    AddColumn('name',       'Тип предмета',       ftString,   30);

  Add('Преподаватели', 'professors').
    AddColumn('name',       'Преподаватель',      ftString,   150);

  Add('Время проведения занятий', 'times').
    AddColumn('begin_time', 'Начало',             ftDate,     70).
    AddColumn('end_time',   'Конец',              ftDate,     70);

  Add('Дни недели', 'days').
    AddColumn('name',       'День',               ftString,   100);

  Add('Группы', 'groups').
    AddColumn('name',       'Группа',             ftString,   100).
    AddColumn('group_size', 'Размер группы',      ftInteger,  50);

  Add('Кабинеты', 'rooms').
    AddColumn('name',       'Номер кабинета',     ftString,   100).
    AddColumn('room_size',  'Вместимость',        ftInteger,  80);

  Add('Недели', 'weeks').
    AddColumn('name',       'Неделя',             ftString,   100);

  Add('Профессоры и предметы', 'professors_subjects').
    AddColumn('professor_id',      GetTableByName('professors'),      [1]).
    AddColumn('subject_id',        GetTableByName('subjects'), [1]);

  Add('Предметы и группы', 'subjects_groups').
    AddColumn('subject_id',        GetTableByName('subjects'),      [1]).
    AddColumn('group_id',          GetTableByName('groups'), [1]);

  Add('Расписание', 'schedule_items').
    AddColumn('subject_id',        GetTableByName('subjects'),      [1]).
    AddColumn('subject_type_id',   GetTableByName('subject_types'), [1]).
    AddColumn('professor_id',      GetTableByName('professors'),    [1]).
    AddColumn('time_id',           GetTableByName('times'),         [1, 2]).
    AddColumn('day_id',            GetTableByName('days'),          [1]).
    AddColumn('week_id',           GetTableByName('weeks'),         [1]).
    AddColumn('group_id',          GetTableByName('groups'),        [1]).
    AddColumn('room_id',           GetTableByName('rooms'),         [1]);
  end;
end;

function CreateListOfTables(): TListOfTables;
begin
  Result := TListOfTables.Create();
  InitListOfTables(Result);
end;

initialization
ListOfTables := TListOfTables.Create();
InitListOfTables(ListOfTables);

end.

