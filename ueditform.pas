unit UEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DbCtrls,
  StdCtrls, ExtCtrls, umetadata, ufieldeditor, db, sqldb, UDBData;

type

  { TEditForm }

  TEditForm = class(TForm)
    ApplyButton: TButton;
    Datasource: TDatasource;
    OKButton: TButton;
    CancelButton: TButton;
    SQLQuery: TSQLQuery;
    procedure ApplyButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    Table: TTableInfo;
    FieldEditors: array of TFieldEditor;
    Columns: TColumnInfos;
    IsEdit: boolean;
    ID: Integer;
    FOnApply: TNotifyEvent;
    function GetNextId(aGenName: String): Integer;
    procedure InsertIntoTable();
    procedure SetOnApplyEvent(aOnApply: TNotifyEvent);
    procedure OnFieldValChange(Sender: TObject);
  public
    constructor Create(aOwner: TWinControl; aTableInfo: TTableInfo; aID: Integer);
    destructor Destroy(); override;
    function CheckFields(): boolean;
    property OnApply: TNotifyEvent read FOnApply write SetOnApplyEvent;
  end;

  { TEditFormList }

  TEditFormList = object
  private
    FEditForms: array of TEditForm;
    procedure DeleteFromList(aID: Integer);
    function FindForm(aID: Integer): Integer;
  public
    procedure ShowEditForm(aOwner: TWinControl; aTableinfo: TTableInfo;
                aID: Integer; aOnApply: TNotifyEvent);
  end;

var
  EditForms: TEditFormList;

implementation

{ TEditFormList }

procedure TEditFormList.DeleteFromList(aID: Integer);
var
  i, FormIndex: Integer;
begin
  FormIndex := FindForm(aID);
  if FormIndex < 0 then Exit;
  for i := FormIndex to High(FEditForms) - 1 do
    FEditForms[i] := FEditForms[i+1];
  SetLength(FEditForms, Length(FEditForms) - 1);
end;

function TEditFormList.FindForm(aID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FEditForms) do
    if FEditForms[i].ID = aID then
      Exit(i);
end;

procedure TEditFormList.ShowEditForm(aOwner: TWinControl;
  aTableinfo: TTableInfo; aID: Integer; aOnApply: TNotifyEvent);
var
  i, FormIndex: Integer;
begin
  FormIndex := FindForm(aID);
  if FormIndex >= 0 then
    FEditForms[FormIndex].Show()
  else begin
    SetLength(FEditForms, Length(FEditForms) + 1);
    FEditForms[High(FEditForms)] := TEditForm.Create(aOwner, aTableInfo, aID);
    with FEditForms[High(FEditForms)] do begin
      OnApply := aOnApply;
      Show();
    end;
  end;
end;

{$R *.lfm}

procedure TEditForm.OnFieldValChange(Sender: TObject);
begin
  ApplyButton.Enabled := true;
end;

procedure TEditForm.ApplyButtonClick(Sender: TObject);
begin
  SQLQuery.ApplyUpdates();
  //MainForm.SomethingChanged := true;
  if isEdit then
    ApplyButton.Enabled := false
  else
    Self.Close();
  if FOnApply <> nil then FOnApply(Sender);
end;

procedure TEditForm.CancelButtonClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TEditForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  i: Integer;
begin
  CloseAction := caFree;
  EditForms.DeleteFromList(ID);
end;

procedure TEditForm.FormCreate(Sender: TObject);
begin

end;

procedure TEditForm.OKButtonClick(Sender: TObject);
begin
  ApplyButtonClick(Self);
  Self.Close();
end;

constructor TEditForm.Create(aOwner: TWinControl; aTableInfo: TTableInfo; aID: Integer);
var
  i: Integer;
  //NullValues: array of const;
begin
  inherited Create(aOwner);
  isEdit := true;
  Table := aTableInfo;
  ID := aID;
  FOnApply := nil;
  Columns := Table.Columns;
  InitConnection(Datasource, SQLQuery);

  if ID = 0 then
    ID := GetNextId(Table.GetGeneratorName);

  SQLQuery.SQL.Text := Format('select * from %s where ID = %d', [Table.Name, aID]);
  SQLQUery.UsePrimaryKeyAsKey := true;
  SQLQuery.Open();

  if aID = 0 then begin
    OKButton.Visible := false;
    ApplyButton.Caption := 'Добавить';
    SQLQuery.Append();
    SQLQuery.FieldByName('ID').AsInteger := ID;
    isEdit := false;
  end;

  For i := 0 to High(Columns) do begin
    SetLength(FieldEditors, Length(FieldEditors) + 1);
    FieldEditors[High(FieldEditors)] := TFieldEditor.Create(Self, Columns[i], DataSource);
    with FieldEditors[High(FieldEditors)] do begin
      Parent := Self;
      Align := alTop;
      OnChange := @OnFieldValChange;
    end;
  end;
end;

procedure TEditForm.InsertIntoTable();
var
  i: Integer;
  ParamName: String;
begin
  SQLQuery.Close();
  SQLQuery.SQL.Text := Format('insert into %s values(next value for %s, ', [Table.Name, Table.GetGeneratorName]);
  for i := 0 to High(Columns) do begin
    ParamName := Format('param%d', [i]);
    SQLQuery.SQL.Add(Format(':%s, ', [ParamName]));
    SQLQuery.Params.CreateParam(Columns[i].FieldType, ParamName, ptInput).Text := FieldEditors[i].Value;
    if i < High(Columns) then SQLQuery.SQL.Add(', ');
  end;
  SQLQuery.SQL.Add(');');
  ShowMessage(SQLQuery.SQL.Text);
  SQLQuery.ExecSQL;
end;

function TEditForm.CheckFields(): boolean;
var
  i: Integer;
begin
  Result := true;
  for i := 0 to High(FieldEditors) do
    if not FieldEditors[i].CheckField() then Exit(false);
end;

procedure TEditForm.SetOnApplyEvent(aOnApply: TNotifyEvent);
begin
  FOnApply := aOnApply;
end;

function TEditForm.GetNextId(aGenName: String): Integer;
begin
  SQLQuery.Close();
  SQLQuery.SQL.Text := Format('SELECT NEXT VALUE FOR %s FROM RDB$DATABASE', [aGenName]);
  SQLQuery.Open();
  Result := SQLQuery.FieldByName('GEN_ID').AsInteger;
  SQLQuery.Close();
end;

destructor TEditForm.Destroy();
var
  i: Integer;
begin
  for i := 0 to High(FieldEditors) do
    FreeAndNil(FieldEditors[i]);
  inherited;
end;

end.

