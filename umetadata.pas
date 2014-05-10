unit UMetaData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, DBGrids, DbCtrls, sqldb, db, Menus, Types, Dialogs, variants;

type

  Strings = TStringDynArray;

  TColumnInfo = record
    Name: String;
    Caption: String;
    IsReference: boolean;
    ReferenceTable: String;
    ReferenceName: String;
    FieldType: TFieldType;
    Size: Integer;
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

  TTableInfo = class
  private
    FColumnInfos: TColumnInfos;
    FCaption: String;
    FName: String;
    function AddColumn(aName, aCaption, aReferenceName: String; aFieldType: TFieldType; aSize: Integer): TTableInfo;
    function AddColumn(aName, aCaption: String; aFieldType: TFieldType; aSize: Integer): TTableInfo;
  public
    constructor Create(aCaption, aName: String);
    function GetColumnCaptions(): Strings;
    function GetColIndexByName(aName: String): Integer;
    function GetGeneratorName: String;
    function ColCount(): Integer;
    property Caption: String read FCaption;
    property Columns: TColumnInfos read FColumnInfos;
    property Name: String read FName;
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

var
  ListOfTables: TListOfTables;

implementation

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
ListOfTables := TListOfTables.Create();
InitListOfTables(ListOfTables);

end.

