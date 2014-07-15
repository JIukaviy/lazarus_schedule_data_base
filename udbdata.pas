unit UDBData;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, db, sqldb, IBConnection;

procedure InitConnection(aDataSource: TDataSource; aSQLQuery: TSQLQuery);
procedure InitSQLQuery(aSQLQuery: TSQLQuery);

const
  cDatabaseName = 'F:\Students4\SCHEDULE.FDB';
  cHostName = 'localhost';
  cPassword = 'masterkey';
  cUserName = 'sysdba';

var
  SQLTransaction: TSQLTransaction;
  DBConnection: TIBConnection;

implementation

procedure InitDataSource(aDataSource: TDataSource; aSQLQuery: TSQLQuery);
begin
  aDataSource.DataSet := aSQLQuery;
end;

procedure InitUnit();
begin
  SQLTransaction := TSQLTransaction.Create(nil);

  DBConnection := TIBConnection.Create(nil);
  with DBConnection do begin
    DatabaseName := cDatabaseName;
    HostName := cHostName;
    Password := cPassword;
    UserName := cUserName;
    Transaction := SQLTransaction;
  end;

  SQLTransaction.Active := true;
end;

procedure InitSQLQuery(aSQLQuery: TSQLQuery);
begin
  if DBConnection = nil then
    InitUnit();
  with aSQLQuery do begin
    DataBase := DBConnection;
    Transaction := SQLTransaction;
  end;
end;

procedure InitConnection(aDataSource: TDataSource; aSQLQuery: TSQLQuery);
begin
  InitSQLQuery(aSQLQuery);
  InitDataSource(aDataSource, aSQLQuery);
end;

finalization
SQLTransaction.Active:= False;
DBConnection.Connected:= False;
FreeAndNil(SQLTransaction);
FreeAndNil(DBConnection);

end.

