unit USchConflicts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, UMetaData, types, Dialogs, UDBData, db, sqldb;

type

  { TConflictInfo }

  TConflictItem = object
    ConfType_ID: Integer;
    Conf_ID: Integer;
    Item_ID: Integer;
    Group_ID: Integer;
    Name: String;
  end;

  TConflictItems = array of TConflictItem;
  TGroupedConflictItems = array of TConflictItems;

  { TConfGroup }

  { TConflictGroup }

  TConflictGroup = object
    Members: array of Integer;
    ID: Integer;
    procedure Add(var Conf: TConflictItem);
    procedure Add(aID: Integer);
  end;

  { TConfGroupsList }

  TConfGroupsList = object
    Groups: array of TConflictGroup;
    procedure Add();
    function LastGroup(): TConflictGroup;
  end;



  TParseQueryProc = function(aStrQuery: String; aSQLQuery: TSQLQuery): TConflictItems of object;

  { TConflictTypeInfo }

  TConflictTypeInfo = record
    ID: Integer;
    Name: String;
    Query: String;
    ParseProc: TParseQueryProc;
  end;

  TConflictTypeInfos = array of TConflictTypeInfo;

  TBaseConflict = class
  public
    function GetConflictInfos(): TConflictTypeInfos; virtual; abstract;
  end;

  { TConflictsType1 }

  TConfType1Info = record
    Name: String;
    Equal: array of Integer;
    UnEqual: array of Integer;
  end;

  TConflictsType1 = class(TBaseConflict)
  protected
    FConfs: array of TConfType1Info;
    function CreateQuery(aID: Integer): String;
    function OpenToDynArray(a: array of Integer): TIntegerDynArray;
    function ParseProc(aStrQuery: String; aSQLQuery: TSQLQuery): TConflictItems;
  private
    function Add(aName: String; aEqual, aUnequal: array of Integer): TConflictsType1;
  public
    function GetConflictInfos(): TConflictTypeInfos; override;
  end;

  { TConflictsTypeList }

  TConflictsTypeList = class
  protected
    //ConfInfos: TConflictTypeInfos;
    Conflicts: array of TBaseConflict;
  public
    procedure Add(aConf: TBaseConflict);
    function GetConfInfos(): TConflictTypeInfos;
  end;

  TConflicts = class
  protected
    FQuery: TSQLQuery;
    FDataSource: TDataSource;
    FConfByConfID: array of Integer;
    FConfByItemID: array of Integer;
    FGroups: array of TConfGroupsList;
    FConflicts: TConflictItems;
    FConfTypeInfos: TConflictTypeInfos;
  private
    procedure SetConflictTypeInfos(aConfs: TConflictTypeInfos);
  public
    constructor Create();
    destructor Destroy();
    procedure UpdateConflicts();
    function GetConfsByItemID(aID: Integer): TConflictItems;
    function GetConfsByConf(Conf: TConflictItem): TConflictItems;
    function GetConfsByTypeID(aID: Integer): TGroupedConflictItems;
  end;

  procedure QSort(var a: TConflictItems; l, r: Integer);

var
  Conflicts: TConflicts;

implementation

  function FindConfID(a: TConflictItems; ID: Integer): Integer;
  var
    l, r: Integer;
  begin
    Result := -1;
    l := 0;
    r := High(a);
    while (l < r) do begin
      if (ID > a[(l + r) div 2].Item_ID) then
        l := (l + r) div 2 + 1
      else if (ID < a[(l + r) div 2].Item_ID) then
        r := (l + r) div 2
      else
        Exit((l + r) div 2);
    end;
  end;

  function FindConfItems(a: TConflictItems; ID: Integer): TConflictItems;
  var
    Conf_ID, l, r, i: Integer;
  begin
    Result := nil;
    Conf_ID := FindConfID(a, ID);
    if ID < 0 then
      Exit(nil);

    l := ID;
    r := ID;

    while ((l-1 >= 0) and (a[l-1].Item_ID = a[Conf_ID].Item_ID)) do dec(l);
    while ((r+1 <= High(a)) and (a[r+1].Item_ID = a[Conf_ID].Item_ID)) do inc(r);

    SetLength(Result, r - l + 1);

    for i := 0 to r-l do
      Result[i] := a[l+i];
  end;

var
  SchOwnCols: TColumnInfos;
  ConflictsTypeList: TConflictsTypeList;

procedure QSort(var a: TConflictItems; l, r: Integer);
var
  x: TConflictItem;
  i, j: integer;

  function cmp(a, b: TConflictItem): boolean;
  begin
    if a.Item_ID = b.Item_ID then
      Exit(a.ConfType_ID > b.ConfType_ID)
    else
      Exit(a.Item_ID > b.ConfType_ID);
  end;

  procedure swap(var a, b: TConflictItem);
  var
    t: TConflictItem;
  begin
    t := a;
    a := b;
    b := t;
  end;

begin
  x := a[(l + r) div 2];
  i := l;
  j := r;
  while i < j do begin
    while cmp(a[i], x) do Inc(i);
    while cmp(x, a[j]) do Dec(j);
    if i <= j then begin
      swap(a[i], a[j]);
      Inc(i);
      Dec(j);
    end;
  end;
  if l < j then
    qsort(a, l, j);
  if i < r then
    qsort(a, i, r);
end;

{ TConfGroupsList }

procedure TConfGroupsList.Add();
begin
  SetLength(Groups, Length(Groups) + 1);
  Groups[High(Groups)].ID := High(Groups);
end;

function TConfGroupsList.LastGroup: TConflictGroup;
begin
  Result := Groups[High(Groups)];
end;

{ TConflictGroup }

procedure TConflictGroup.Add(var Conf: TConflictItem);
begin
  SetLength(Members, Length(Members) + 1);
  Members[High(Members)] := Conf.Conf_ID;
  Conf.Group_ID := ID;
end;

procedure TConflictGroup.Add(aID: Integer);
begin
  SetLength(Members, Length(Members) + 1);
  Members[High(Members)] := aID;
end;

procedure TConflicts.UpdateConflicts;
var
  i, j, ResLen: Integer;
  TempConfList: TConflictItems;
begin
  FConflicts := nil;
  FGroups := nil;
  ResLen := 0;
  for i := 0 to High(FConfTypeInfos) do begin
    with FConfTypeInfos[i] do
      TempConfList := ParseProc(Query, FQuery);

    ResLen := Length(FConflicts);
    SetLength(FConflicts, ResLen + Length(TempConfList));

    for j := 0 to High(TempConfList) do begin
      FConflicts[ResLen + j] := TempConfList[j];
      FConflicts[ResLen + j].ConfType_ID := i;
      FConflicts[ResLen + j].Name := FConfTypeInfos[j].Name;
    end;
  end;
  QSort(FConflicts, 0, High(FConflicts));

  SetLength(FGroups, Length(FConfTypeInfos));

  for i := 0 to High(FConflicts) do begin
    with FConflicts[i] do begin
      if High(FGroups[ConfType_ID].Groups) < Group_ID then
        SetLength(FGroups[ConfType_ID].Groups, Group_ID + 1);
      FGroups[ConfType_ID].Groups[Group_ID].Add(i);
      Conf_ID := i;
    end;
  end;

  {res := '';
  for i := 0 to High(FGroups) do begin
    res += FConfTypeInfos[i].Name + ': '#13#10;
    for j := 0 to High(FGroups[i].Groups) do begin
      res += 'Группа ' + IntToStr(j + 1) + ': ';
      for k := 0 to High(FGroups[i].Groups[j].Members) do begin
        res += IntToStr(FConflicts[FGroups[i].Groups[j].Members[k]].Item_ID) + ', ';
      end;
      res += #13#10;
    end;
  end;
  ShowMessage(res);}
end;

procedure TConflicts.SetConflictTypeInfos(aConfs: TConflictTypeInfos);
begin
  FConfTypeInfos := aConfs;
end;

constructor TConflicts.Create;
begin
  FDataSource := TDataSource.Create(nil);
  FQuery := TSQLQuery.Create(nil);
  InitConnection(FDataSource, FQuery);
end;

destructor TConflicts.Destroy;
begin
  FreeAndNil(FDataSource);
  FreeAndNil(FQuery);
end;

function TConflicts.GetConfsByItemID(aID: Integer): TConflictItems;
begin
  Result := FindConfItems(FConflicts, aID);
end;

function TConflicts.GetConfsByConf(Conf: TConflictItem): TConflictItems;
var
  i: Integer;
begin
  with FGroups[Conf.ConfType_ID] do begin
    SetLength(Result, Length(Groups[Conf.Group_ID].Members));

    for i := 0 to High(Groups[Conf.Group_ID].Members) do
      Result[i] := FConflicts[Groups[Conf.Group_ID].Members[i]];
  end;
end;

function TConflicts.GetConfsByTypeID(aID: Integer): TGroupedConflictItems;
var
  i, j: Integer;
begin
  SetLength(Result, Length(FGroups[aID].Groups));
  for i := 0 to High(FGroups[aID].Groups) do begin
    SetLength(Result[i], Length(FGroups[aID].Groups[i].Members));
    for j := 0 to High(FGroups[aID].Groups[i].Members) do
      Result[i][j] := FConflicts[FGroups[aID].Groups[i].Members[j]];
  end;
end;

{ TConflictsTypeList }

procedure TConflictsTypeList.Add(aConf: TBaseConflict);
begin
  SetLength(Conflicts, Length(Conflicts) + 1);
  Conflicts[High(Conflicts)] := aConf;
end;

function TConflictsTypeList.GetConfInfos: TConflictTypeInfos;
var
  i, j, CurrConfID: Integer;
  TempConfs: TConflictTypeInfos;
  ResLen: Integer;
begin
  CurrConfID := 0;
  for i := 0 to High(Conflicts) do begin
    TempConfs := Conflicts[i].GetConflictInfos();

    ResLen := Length(Result);
    SetLength(Result, ResLen + Length(TempConfs));

    for j := 0 to High(TempConfs) do begin
      TempConfs[j].ID := CurrConfID;
      CurrConfID += 1;
      Result[j + ResLen] := TempConfs[j];
    end;
  end;
end;

{ TConflictsType1 }

function TConflictsType1.OpenToDynArray(a: array of Integer): TIntegerDynArray;
var
  i: Integer;
begin
  SetLength(Result, Length(a));
  for i := 0 to High(a) do
    Result[i] := a[i];
end;

function TConflictsType1.ParseProc(aStrQuery: String; aSQLQuery: TSQLQuery): TConflictItems;
var
  i, CurrItemID, Conf_ID, CurrGroupID: Integer;
  Res: TConflictItems;
begin
  CurrGroupID := 0;
  with aSQLQuery do begin
    Close;
    SQL.Clear;
    SQL.Text := aStrQuery;
    Open;

    while not EOF do begin
      if (FindConfID(Res, Fields[0].AsInteger) >= 0) then
        continue;

      SetLength(Res, Length(Res) + 1);

      with Res[High(Res)] do begin
        Item_ID := Fields[0].AsInteger;
        ShowMessage(IntToStr(Item_ID));
        Group_ID := -1;
      end;

      Conf_ID := FindConfID(Res, Fields[1].AsInteger);

      if Conf_ID >= 0 then begin
         if (Res[Conf_ID].Group_ID < 0) then begin
            Res[Conf_ID].Group_ID := CurrGroupID;
            Res[High(Res)].Group_ID := CurrGroupID;
            CurrGroupID += 1;
         end else begin
            Res[High(Res)].Group_ID := Res[High(Res)].Group_ID;
         end;
      end;

      Next;
    end;
  end;
  Result := Res;
end;

function TConflictsType1.Add(aName: String; aEqual,
  aUnequal: array of Integer): TConflictsType1;
var
  i: Integer;
begin
  SetLength(FConfs, Length(FConfs) + 1);
  with FConfs[High(FConfs)] do begin
    Name := aName;
    Equal := OpenToDynArray(aEqual);
    UnEqual := OpenToDynArray(aUnequal);
  end;
  Result := Self;
end;

function TConflictsType1.CreateQuery(aID: Integer): String;
var
  i: Integer;
  pairs: String;
  CurrEq, CurrUnEq: array of Integer;
begin
  Result := 'SELECT A.ID, B.ID FROM Schedule_Items A INNER JOIN Schedule_Items B ON ';
  pairs := '';

  CurrEq := FConfs[aID].Equal;
  CurrUnEq := FConfs[aID].UnEqual;

  for i := 0 to High(CurrEq) do
    with SchOwnCols[CurrEq[i]] do
      pairs += Format('AND A.%s = B.%s ', [Name, Name]);

  for i := 0 to High(CurrUnEq) do
    with SchOwnCols[CurrUnEq[i]] do
      pairs += Format('AND A.%s <> B.%s ', [Name, Name]);

  Delete(pairs, 1, 4);
  pairs += 'order by a.ID, B.ID';
  Result += pairs;
end;

function TConflictsType1.GetConflictInfos: TConflictTypeInfos;
var
  i: Integer;
begin
  SetLength(Result, Length(FConfs));
  for i := 0 to High(FConfs) do begin
    with Result[i] do begin
      ID := i;
      Name := FConfs[i].Name;
      Query := CreateQuery(i);
      ParseProc := @Self.ParseProc;
    end;
  end;
end;

initialization
  SchOwnCols := ListOfTables.GetTableByName('schedule_items').GetCols(cstOwn);
  ConflictsTypeList := TConflictsTypeList.Create();
  with ConflictsTypeList do begin
    Add(TConflictsType1.Create().
       Add('Разные пары в одной аудитории',       [4, 5, 6, 8],             [3]).
       Add('Преподаватель в разных аудиториях',   [3, 4, 5, 6],             [8]).
       Add('Преподаватель на разных дисциплинах', [3, 4, 5, 6],             [1]).
       Add('Группа в разных аудиториях',          [4, 5, 6, 7],             [8]).
       Add('Группа на разных дициплинах',         [4, 5, 6, 7],             [1]).
       Add('Дублирующися пары',                   [1, 2, 3, 4, 5, 6, 7, 8], [0]));
    GetConfInfos();
  end;

  Conflicts := TConflicts.Create();
  Conflicts.SetConflictTypeInfos(ConflictsTypeList.GetConfInfos());
  Conflicts.UpdateConflicts();

end.

