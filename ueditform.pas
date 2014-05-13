unit UEditForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DbCtrls,
  StdCtrls, ExtCtrls, umetadata, ufieldeditor, db, sqldb, UDBData, USQLQueryCreator;

type

  { TEditForm }

  TEditForm = class(TForm)
    ApplyButton: TButton;
    OKButton: TButton;
    CancelButton: TButton;
    procedure ApplyButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure CancelButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure OKButtonClick(Sender: TObject);
  private
    Query: TSQLQueryCreator;
    Table: TTableInfo;
    FieldEditors: array of TFieldEditor;
    Columns: TColumnInfos;
    IsEdit: boolean;
    ID: Integer;
    FOnApply: TNotifyEvent;
    function GetNextID(): Integer;
    procedure InitAddForm();
    procedure InitEditForm(aID: Integer);
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
    procedure DeleteFromList(aID, aTableID: Integer);
    function FindForm(aID, aTableID: Integer): Integer;
  public
    procedure ShowEditForm(aOwner: TWinControl; aTableinfo: TTableInfo;
                aID: Integer; aOnApply: TNotifyEvent);
  end;

var
  EditForms: TEditFormList;

implementation

{ TEditFormList }

procedure TEditFormList.DeleteFromList(aID, aTableID: Integer);
var
  i, FormIndex: Integer;
begin
  FormIndex := FindForm(aID, aTableID);
  if FormIndex < 0 then Exit;
  for i := FormIndex to High(FEditForms) - 1 do
    FEditForms[i] := FEditForms[i+1];
  SetLength(FEditForms, Length(FEditForms) - 1);
end;

function TEditFormList.FindForm(aID, aTableID: Integer): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to High(FEditForms) do
    if (FEditForms[i].ID = aID) and (FEditForms[i].Table.ID = aTableID) then
      Exit(i);
end;

procedure TEditFormList.ShowEditForm(aOwner: TWinControl;
  aTableinfo: TTableInfo; aID: Integer; aOnApply: TNotifyEvent);
var
  FormIndex: Integer;
begin
  FormIndex := FindForm(aID, aTableInfo.ID);
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
  Query.SQLQuery.ApplyUpdates();
  ApplyButton.Enabled := false;
  if FOnApply <> nil then FOnApply(Sender);
end;

procedure TEditForm.AddButtonClick(Sender: TObject);
begin
  Query.SQLQuery.ApplyUpdates();
  if FOnApply <> nil then FOnApply(Sender);
  Self.Close();
end;

procedure TEditForm.CancelButtonClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TEditForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  EditForms.DeleteFromList(ID, Table.ID);
end;

procedure TEditForm.OKButtonClick(Sender: TObject);
begin
  ApplyButtonClick(Self);
  Self.Close();
end;

function TEditForm.GetNextID: Integer;
var
  LookUpQuery: TSQLQueryCreator;
begin
  LookUpQuery := TSQLQueryCreator.Create(Table);
  Result := LookUpQuery.GetNextID;
  FreeAndNil(LookUpQuery);
end;

procedure TEditForm.InitAddForm;
begin
  OKButton.Visible := false;
  ApplyButton.Caption := 'Добавить';
  ApplyButton.OnClick := @AddButtonClick;
  Query.SQLQuery.UsePrimaryKeyAsKey := true;
  Query.SelectCols(cstOwn);
  Query.SendQuery();
  Query.SQLQuery.Append();
  Query.SQLQuery.Fields[COL_ID].Required := false;
  isEdit := false;
end;

procedure TEditForm.InitEditForm(aID: Integer);
begin
  ApplyButton.OnClick := @ApplyButtonClick;
  Query.SelectCols(cstOwn);
  Query.FilterById(aID);
  Query.SendQuery();
end;

constructor TEditForm.Create(aOwner: TWinControl; aTableInfo: TTableInfo; aID: Integer);
var
  i: Integer;
begin
  inherited Create(aOwner);
  ID := aID;
  Table := aTableInfo;
  FOnApply := nil;
  Columns := Table.GetCols(cstOwn, [coEditable]);
  Query := TSQLQueryCreator.Create(aTableInfo);

  if aID = 0 then
    InitAddForm()
  else
    InitEditForm(aID);

  For i := 0 to High(Columns) do begin
    SetLength(FieldEditors, Length(FieldEditors) + 1);
    FieldEditors[High(FieldEditors)] := TFieldEditor.Create(Self, Columns[i], Query);
    with FieldEditors[High(FieldEditors)] do begin
      Parent := Self;
      Align := alTop;
      OnChange := @OnFieldValChange;
    end;
  end;
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

destructor TEditForm.Destroy();
var
  i: Integer;
begin
  for i := 0 to High(FieldEditors) do
    FreeAndNil(FieldEditors[i]);
  inherited;
end;

end.

