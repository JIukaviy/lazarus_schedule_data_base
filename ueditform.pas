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
    FQuery: TSQLQueryCreator;
    FTable: TTableInfo;
    FFieldEditors: array of TFieldEditor;
    FColumns: TColumnInfos;
    FID: Integer;
    FOnApply: TNotifyEvent;
    function GetNextID(): Integer;
    procedure InitAddForm();
    procedure InitEditForm(aID: Integer);
    procedure SetOnApplyEvent(aOnApply: TNotifyEvent);
    procedure OnFieldValChange(Sender: TObject);
  public
    constructor Create(aOwner: TWinControl; aTableInfo: TTableInfo;
      aID: Integer; const aColVals: array of TColumnVal);
    constructor Create(aOwner: TWinControl; aTableInfo: TTableInfo; aID: Integer;
      aColVals: TColumnVals = nil);
    destructor Destroy(); override;
    function CheckFields(): boolean;
    property OnApply: TNotifyEvent read FOnApply write SetOnApplyEvent;
    property RecordID: Integer read FID;
    property Table: TTableInfo read FTable;
  end;

  { TEditFormList }

  TEditFormList = object
  private
    FEditForms: array of TEditForm;
    procedure DeleteFromList(aID, aTableID: Integer);
    function FindForm(aID, aTableID: Integer): Integer;
  public
    procedure ShowEditForm(aOwner: TWinControl; aTableinfo: TTableInfo;
                aID: Integer; aOnApply: TNotifyEvent; aColVals: TColumnVals = nil);
    procedure ShowEditForm(aOwner: TWinControl; aTableinfo: TTableInfo;
                aID: Integer; aOnApply: TNotifyEvent; const aColVals: array of TColumnVal);
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
    if (FEditForms[i].FID = aID) and (FEditForms[i].FTable.ID = aTableID) then
      Exit(i);
end;

procedure TEditFormList.ShowEditForm(aOwner: TWinControl;
  aTableinfo: TTableInfo; aID: Integer; aOnApply: TNotifyEvent;
  aColVals: TColumnVals);
var
  FormIndex: Integer;
begin
  FormIndex := FindForm(aID, aTableInfo.ID);
  if FormIndex >= 0 then
    FEditForms[FormIndex].Show()
  else begin
    SetLength(FEditForms, Length(FEditForms) + 1);
    FEditForms[High(FEditForms)] := TEditForm.Create(aOwner, aTableInfo, aID, aColVals);
    with FEditForms[High(FEditForms)] do begin
      OnApply := aOnApply;
      Show();
    end;
  end;
end;

procedure TEditFormList.ShowEditForm(aOwner: TWinControl;
  aTableinfo: TTableInfo; aID: Integer; aOnApply: TNotifyEvent;
  const aColVals: array of TColumnVal);
var
  ColVals: TColumnVals;
  i: Integer;
begin
  SetLength(ColVals, Length(aColVals));
  for i := 0 to High(aColVals) do
    ColVals[i] := aColVals[i];
  ShowEditForm(aOwner, aTableInfo, aID, aOnApply, ColVals);
end;

{$R *.lfm}

procedure TEditForm.OnFieldValChange(Sender: TObject);
begin
  ApplyButton.Enabled := true;
end;

procedure TEditForm.ApplyButtonClick(Sender: TObject);
begin
  FQuery.SQLQuery.ApplyUpdates();
  ApplyButton.Enabled := false;
  if FOnApply <> nil then FOnApply(Self);
end;

procedure TEditForm.AddButtonClick(Sender: TObject);
begin
  FQuery.SQLQuery.ApplyUpdates();
  if FOnApply <> nil then FOnApply(Self);
  Self.Close();
end;

procedure TEditForm.CancelButtonClick(Sender: TObject);
begin
  Self.Close();
end;

procedure TEditForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  CloseAction := caFree;
  EditForms.DeleteFromList(FID, FTable.ID);
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
  LookUpQuery := TSQLQueryCreator.Create(FTable);
  Result := LookUpQuery.GetNextID;
  FreeAndNil(LookUpQuery);
end;

procedure TEditForm.InitAddForm;
begin
  OKButton.Visible := false;
  ApplyButton.Caption := 'Добавить';
  ApplyButton.OnClick := @AddButtonClick;
  FQuery.SQLQuery.UsePrimaryKeyAsKey := true;
  FQuery.SelectCols(cstOwn);
  FQuery.SendQuery();
  FQuery.SQLQuery.Append();
  FQuery.SQLQuery.Fields[COL_ID].Required := false;
  Caption := 'Добавить';
end;

procedure TEditForm.InitEditForm(aID: Integer);
begin
  ApplyButton.OnClick := @ApplyButtonClick;
  FQuery.SelectCols(cstOwn);
  FQuery.FilterById(aID);
  FQuery.SendQuery();
  FQuery.SQLQuery.Edit;
  Caption := 'Редактировать';
end;

constructor TEditForm.Create(aOwner: TWinControl; aTableInfo: TTableInfo;
  aID: Integer; const aColVals: array of TColumnVal);
var
  ColVals: TColumnVals;
  i: Integer;
begin
  SetLength(ColVals, Length(aColVals));
  for i := 0 to High(aColVals) do
    ColVals[i] := aColVals[i];
  Create(aOwner, aTableInfo, aID, aColVals);
end;

constructor TEditForm.Create(aOwner: TWinControl; aTableInfo: TTableInfo;
  aID: Integer; aColVals: TColumnVals);
var
  i: Integer;
begin
  inherited Create(aOwner);
  FID := aID;
  FTable := aTableInfo;
  FOnApply := nil;
  FColumns := FTable.GetCols(cstOwn, [coEditable]);
  FQuery := TSQLQueryCreator.Create(aTableInfo);

  if aID = 0 then
    InitAddForm()
  else
    InitEditForm(aID);

  SetColVals(FQuery.SQLQuery, aColVals);

  For i := 0 to High(FColumns) do begin
    SetLength(FFieldEditors, Length(FFieldEditors) + 1);
    FFieldEditors[High(FFieldEditors)] := TFieldEditor.Create(Self, FColumns[i], FQuery);
    with FFieldEditors[High(FFieldEditors)] do begin
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
  for i := 0 to High(FFieldEditors) do
    if not FFieldEditors[i].CheckField() then Exit(false);
end;

procedure TEditForm.SetOnApplyEvent(aOnApply: TNotifyEvent);
begin
  FOnApply := aOnApply;
end;

destructor TEditForm.Destroy();
var
  i: Integer;
begin
  for i := 0 to High(FFieldEditors) do
    FreeAndNil(FFieldEditors[i]);
  inherited;
end;

end.

