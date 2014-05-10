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
uses UMain;

procedure InitDataSource(aDataSource: TDataSource; aSQLQuery: TSQLQuery);
begin
  aDataSource.DataSet := aSQLQuery;
end;

procedure InitSQLQuery(aSQLQuery: TSQLQuery);
begin
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

initialization
  SQLTransaction := TSQLTransaction.Create(MainForm);

  DBConnection := TIBConnection.Create(MainForm);
  with DBConnection do begin
    DatabaseName := cDatabaseName;
    HostName := cHostName;
    Password := cPassword;
    UserName := cUserName;
    Transaction := SQLTransaction;
  end;

  with SQLTransaction do begin
    //Database := DBConnection;
    Active := true;
  end;

finalization
SQLTransaction.Active:= False;
DBConnection.Connected:= False;
FreeAndNil(SQLTransaction);
FreeAndNil(DBConnection);

end.

