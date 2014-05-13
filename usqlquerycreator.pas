unit USQLQueryCreator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, umetadata, UFilters, sqldb, db, DBGrids, Dialogs, UDBData,
  types, Math, Clipbrd;

type

  { TSQLQueryCreator }

  TSortOrders = array of TSortOrder;

  TSQLQueryCreator = class
  private
    FTable: TTableInfo;
    FColsToShow: TColumnInfos;
    FFilters: TFilterInfos;
    FFilterList: TFilterList;
    FOrderByCols: TColumnInfos;
    FDesc: boolean;
    FSQLQuery: TSQLQuery;
    FDataSource: TDataSource;
    function CreateQuery(): String;
    procedure ApplyFilterParams(Filters: TFilterInfos);
    procedure SetUpdateSQLQuery();
    procedure SetInsertSQLQuery();
    procedure SetDeleteSQLQuery();
  public
    constructor Create(aTable: TTableInfo);
    destructor Destroy(); override;
    procedure SelectCols(aColSelType: TColSelType);
    procedure SendQuery();
    procedure FilterById(aID: Integer);
    procedure SetOrderBy(const aColumnIDs: array of Integer);
    procedure SetOrderBy(const aColumns: array of TColumnInfo);
    procedure SetOrderBy(const aColumns: TColumnInfos);
    procedure SetCaptions(DBGrid: TDBGrid);
    procedure ShowItems(DBGrid: TDBGrid);
    procedure AddFilter(aFilter: TFilterInfo);
    procedure ClearFilters();
    function GetNextID: Integer;
    function GetCurrID: Integer;
    property Table: TTableInfo read FTable write FTable;
    property FilterList: TFilterList read FFilterList write FFilterList;
    property OrderDesc: boolean read FDesc write FDesc;
    property DataSource: TDataSource read FDataSource;
    property SQLQuery: TSQLQuery read FSQLQuery;
  end;

implementation

{ TSQLQueryCreator }

constructor TSQLQueryCreator.Create(aTable: TTableInfo);
var
  i: Integer;
begin
  FTable := aTable;
  FFilterList := nil;
  FSQLQuery := TSQLQuery.Create(nil);
  FDataSource := TDataSource.Create(nil);
  SetUpdateSQLQuery();
  SetInsertSQLQuery();
  SetDeleteSQLQuery();
  InitConnection(FDataSource, FSQLQuery);
end;

destructor TSQLQueryCreator.Destroy;
begin
  inherited;
  FreeAndNil(FSQLQuery);
end;

function TSQLQueryCreator.CreateQuery(): String;
var
  i: Integer;
 { function GetFullColName(i, j : Integer): String;
  begin
    if FTable.Columns[i].IsReference then
      Result := FTable.Columns[j].ReferenceTable + '.' +  FTable.Columns[j].ReferenceName
    else
      Result := FTable.Name + '.' + FTable.Columns[j].Name + ' ';
  end;}

begin
  if Length(FColsToShow) = 0 then begin
    ShowMessage('Не указаны колонки для выборки');
    Exit();
  end;

  //Select

  Result := Format('select ', [Ftable.Name]);
  for i := 0 to High(FColsToShow) do begin
    Result += Format('%s as %s', [FColsToShow[i].OwnName, FColsToShow[i].AliasName]);
    if i < High(FColsToShow) then Result += ', ';
  end;

  //From

  Result += ' from ' + Ftable.Name;
  for i := 0 to High(FColsToShow) do begin
    if FColsToShow[i].FieldType <> ftReference then continue;
    Result += Format(' inner join %s on %s = %s ', [FColsToShow[i].RefTableName,
                 FColsToShow[i].OwnName, FColsToShow[i].RefKeyCol.OwnName]);
  end;

  //Set filters

  if Length(FFilters) > 0 then begin
    Result += ' where ';
    for i := 0 to High(FFilters) do begin
      Result += Format(' %s %s :%s%d ', [FFilters[i].Column.OwnName(), FFilters[i].Condition,
                         FFilters[i].Column.AliasName, i]);
      if i < High(FFilters) then Result += ' and ';
    end;
  end;

  //Set order by

  if Length(FOrderByCols) > 0 then begin
    Result += ' order by ';
    for i := 0 to High(FOrderByCols) do begin
      Result += FOrderByCols[i].AliasName;
      if i < High(FOrderByCols) then Result += ', ';
    end;
    if FDesc then Result += ' desc ';
  end;
  Clipboard.AsText := Result;
  //ShowMessage(Result);
end;

procedure TSQLQueryCreator.ApplyFilterParams(Filters: TFilterInfos);
var
  i: Integer;
begin
  SQLQuery.Params.Clear();
  for i := 0 to High(Filters) do
    with Filters[i] do
      SQLQuery.Params.CreateParam(Column.FieldType, Column.AliasName + IntToStr(i), ptInput).Text := Value;
end;

procedure TSQLQueryCreator.SetUpdateSQLQuery;
var
  i: Integer;
  EditableCols: TColumnInfos;
begin
  EditableCols := FTable.GetCols(cstOwn, [coEditable]);

  with FSQLQuery.UpdateSQL do begin
    Append(Format('update %s set', [FTable.Name]));
    for i := 0 to High(EditableCols) do begin
      Append(Format('%s = :%s', [EditableCols[i].Name, EditableCols[i].AliasName()]));
      if i < High(EditableCols) then Add(',');
    end;
    Append(Format('where %s = :OLD_%s', [FTable.GetPrimaryCol.Name, FTable.GetPrimaryCol.AliasName()]));
  end;
end;

procedure TSQLQueryCreator.SetInsertSQLQuery;
var
  i: Integer;
begin
  with FSQLQuery.InsertSQL do begin
    with FTable do begin
      Append(Format('insert into %s (', [Name]));
      for i := 0 to High(Columns) do begin
        Append(Format('%s', [Columns[i].Name]));
        if i < High(Columns) then Add(',');
      end;
      Append(') values (');
      for i := 0 to High(Columns) do begin
        if i = COL_ID then
          Append(Format('next value for %s', [GetGeneratorName()]))
        else
          Append(Format(':%s', [Columns[i].AliasName()]));
        if i < High(Columns) then Add(',')
      end;
    end;
    Append(')');
  end;
end;

procedure TSQLQueryCreator.SetDeleteSQLQuery;
begin
  with FSQLQuery.DeleteSQL, Table.GetPrimaryCol do
    Append(Format('delete from %s where %s = :%s', [Table.Name, Name, AliasName]));
end;

procedure TSQLQueryCreator.SendQuery();
begin
  FSQLQuery.Close();
  if FFilterList <> nil then
    FFilters := FFilterList.GetFilterInfos();
  ApplyFilterParams(FFilters);
  FSQLQuery.SQL.Text := CreateQuery();
  FSQLQuery.Open();
end;

procedure TSQLQueryCreator.SelectCols(aColSelType: TColSelType);
begin
  FColsToShow := FTable.GetCols(aColSelType);
end;

procedure TSQLQueryCreator.FilterById(aID: Integer);
begin
  ClearFilters();
  AddFilter(CreateIdFilter(FTable, aID));
end;

procedure TSQLQueryCreator.SetCaptions(DBGrid: TDBGrid);
var
  i: Integer;
begin
  for i := 0 to DBGrid.Columns.Count -1 do begin
    with FColsToShow[i] do
      if coVisible in Options then begin
        DBGrid.Columns[i].Title.Caption := Caption;
        DBGrid.Columns[i].Width := Size;
      end else
        DBGrid.Columns[i].Visible := false;
  end;
end;

procedure TSQLQueryCreator.SetOrderBy(const aColumnIDs: array of Integer);
var
  i: Integer;
begin
  if not InRange(High(aColumnIDs), 0, High(FColsToShow)) then Exit;

  for i := 0 to High(aColumnIDs) do
    if not InRange(aColumnIDs[i], 0, High(FColsToShow)) then Exit;

  SetLength(FOrderByCols, Length(aColumnIDs));
  for i := 0 to High(aColumnIDs) do
    FOrderByCols[i] := FColsToShow[aColumnIDs[i]];
end;

procedure TSQLQueryCreator.SetOrderBy(const aColumns: array of TColumnInfo);
var
  i: Integer;
  Cols: TColumnInfos;
begin
  SetLength(Cols, Length(aColumns));
  for i := 0 to High(aColumns) do
    Cols[i] := aColumns[i];
  SetOrderBy(cols);
end;

procedure TSQLQueryCreator.SetOrderBy(const aColumns: TColumnInfos);
var
  i: Integer;
begin
  if not InRange(High(aColumns), 0, High(FColsToShow)) then Exit;

  FOrderByCols := aColumns;
end;

procedure TSQLQueryCreator.ShowItems(DBGrid: TDBGrid);
begin
  SelectCols(cstAll);
  SendQuery();
  SetCaptions(DBGrid);
end;

procedure TSQLQueryCreator.AddFilter(aFilter: TFilterInfo);
begin
  SetLength(FFilters, Length(FFilters) + 1);
  FFilters[High(FFilters)] := aFilter;
end;

procedure TSQLQueryCreator.ClearFilters;
begin
  FFilters := nil;
end;

function TSQLQueryCreator.GetNextID: Integer;
begin
  FSQLQuery.Close();
  FSQLQuery.SQL.Text := Format('SELECT NEXT VALUE FOR %s FROM RDB$DATABASE', [FTable.GetGeneratorName()]);
  FSQLQuery.Open();
  Result := SQLQuery.FieldByName('GEN_ID').AsInteger;
  FSQLQuery.Close();
end;

function TSQLQueryCreator.GetCurrID: Integer;
begin
  Result := FSQLQuery.FieldByName(FTable.GetPrimaryCol.AliasName()).AsInteger
end;

end.

