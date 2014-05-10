unit USQLQueryCreator;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, umetadata, UFilters, sqldb, db, DBGrids, Dialogs;

type

  { TSQLQueryCreator }

  TSortOrder = record
    ColumnIndex: Integer;
    SortById: boolean;
  end;
  TSortOrders = array of TSortOrder;

  TSQLQueryCreator = object
  private
    FTable: TTableInfo;
    FFilterList: TFilterList;
    FSortOrders: TSortOrders;
    FDesc: boolean;
  public
    function CreateQuery(): String;
    procedure SendQuery(SQLQuery: TSQLQuery);
    procedure SetCaptions(DBGrid: TDBGrid);
    procedure SetOrderBy(const FieldIDs: array of Integer);
    procedure SetOrderBy(const SortOrders: TSortOrders);
    procedure ShowItems(SQLQuery: TSQLQuery; DBGrid: TDBGrid);
    procedure DeleteRecordByID(aID: Integer; SQLQuery: TSQLQuery);
    property Table: TTableInfo read FTable write FTable;
    property FilterList: TFilterList read FFilterList write FFilterList;
    property OrderDesc: boolean read FDesc write FDesc;
  end;

implementation

{ TSQLQueryCreator }

function TSQLQueryCreator.CreateQuery: String;
var
  i: Integer;
  FilterInfos: TFilterInfos;
  function GetFullColName(i, j : Integer): String;
  begin
    if FTable.Columns[i].IsReference then
      Result := FTable.Columns[j].ReferenceTable + '.' +  FTable.Columns[j].ReferenceName
    else
      Result := FTable.Name + '.' + FTable.Columns[j].Name + ' ';
  end;

begin
  with FTable do begin
  //
  Result := Format('select %s.ID, ', [Name]);
  for i := 0 to High(Columns) do begin
    Result += Format('%s as %s', [GetFullColName(i, i), GetColSqlName(Columns[i])]);
    if Columns[i].IsReference then
      Result += Format(', %s.ID as %s', [Columns[i].ReferenceTable, Columns[i].Name]);
    if i < High(Columns) then Result += ', ';
  end;
  Result += ' from ' + Name;
  for i := 0 to High(Columns) do begin
    if not Columns[i].IsReference then continue;
    Result += Format(' inner join %s on %s.%s = %s.ID ', [Columns[i].ReferenceTable,
                 NAme, Columns[i].Name, Columns[i].ReferenceTable]);
  end;
  //Set filters
  if FFilterList.Count > 0 then begin
    Result += ' where ';
    FilterInfos := FFilterList.GetFilterInfos();
    for i := 0 to High(FilterInfos) do begin
      Result += Format(' %s %s :%s ', [FilterInfos[i].FieldName, FilterInfos[i].Condition,
                         FilterInfos[i].ParamName]);
      if i < High(FilterInfos) then Result += ' and ';
    end;
  end;
  //Set order by
  if Length(FSortOrders) > 0 then begin
    Result += ' order by ';
    for i := 0 to High(FSortOrders) do begin
      if FSortOrders[i].SortById then
        Result += Columns[FSortOrders[i].ColumnIndex].Name
      else
        Result += GetFullColName(i, FSortOrders[i].ColumnIndex);
      if i < High(FSortOrders) then Result += ', ';
    end;
    if FDesc then Result += ' desc ';
  end;
  end;
  //ShowMessage(Result);
end;

procedure TSQLQueryCreator.SendQuery(SQLQuery: TSQLQuery);
var
  i: Integer;
  Filters: TFilterInfos;
begin
  Filters := FFilterList.GetFilterInfos();
  SQLQuery.Close();
  SQLQuery.Params.Clear();
  for i := 0 to High(Filters) do
    with Filters[i] do
      SQLQuery.Params.CreateParam(FieldType, ParamName, ptInput).Text := Value;
  SQLQuery.SQL.Text := CreateQuery();
  SQLQuery.Open();
end;

procedure TSQLQueryCreator.SetCaptions(DBGrid: TDBGrid);
var
  i, j: Integer;
begin
  DBGrid.Columns[0].Visible := false;
  j := 0; i := 0;
  while i < DBGrid.Columns.Count do begin
    if (j <= High(Table.Columns)) and (LowerCase(GetColSqlName(Table.Columns[j])) = LowerCase(DBGrid.Columns[i].Title.Caption)) then begin
      DBGrid.Columns[i].Title.Caption := Table.Columns[j].Caption;
      DBGrid.Columns[i].Width := Table.Columns[j].Size;
      inc(j); inc(i);
    end else begin
      DBGrid.Columns[i].Visible := false;
      inc(i);
    end;
  end;
end;

procedure TSQLQueryCreator.SetOrderBy(const FieldIDs: array of Integer);
var
  i: Integer;
  OrderBy: TSortOrders;
begin
  if (Length(FieldIDs) and 1) > 0 then Exit;
  if (Length(FieldIDs) div 2) > Length(FTable.Columns) then Exit;
  i := 0;
  while i < High(FieldIDs) do begin
    if FieldIDs[i] > High(FTable.Columns) then Exit;
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

procedure TSQLQueryCreator.SetOrderBy(const SortOrders: TSortOrders);
var
  i: Integer;
begin
  for i := 0 to High(SortOrders) do begin
    if (SortOrders[i].ColumnIndex > Table.ColCount - 1) or
       (SortOrders[i].ColumnIndex < 0) then
         Exit;
  end;
  FSortOrders := SortOrders;
end;

procedure TSQLQueryCreator.ShowItems(SQLQuery: TSQLQuery; DBGrid: TDBGrid);
begin
  SendQuery(SQLQuery);
  SetCaptions(DBGrid);
end;

procedure TSQLQueryCreator.DeleteRecordByID(aID: Integer; SQLQuery: TSQLQuery);
begin
  SQLQuery.Close();
  SQLQuery.SQL.Text := Format('delete from %s where ID = %d', [Table.Name, aID]);
  SQLQuery.ExecSQL();
end;

end.

