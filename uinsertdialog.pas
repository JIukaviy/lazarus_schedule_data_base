unit UInsertDialog;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DbCtrls,
  StdCtrls, ExtCtrls, UTables, ufieldeditor, db, sqldb;

type

  { TInsertDialogForm }

  TInsertDialogForm = class(TForm)
    ApplyButton: TButton;
    OKButton: TButton;
    CancelButton: TButton;
    procedure CancelButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label1Click(Sender: TObject);
    procedure Panel1Click(Sender: TObject);
  private
    Table: TTableInfo;
    FieldEditors: array of TFieldEditor;
  public
    constructor Create(aTheOwner: TWinControl;
                       aTableInfo: TTableInfo;
                       aColumns: TColumnInfos;
                       aSQLQuery: TSQLQuery;
                       aDataSource: TDataSource);
  end;

var
  InsertDialogForm: TInsertDialogForm;

implementation

{$R *.lfm}

procedure TInsertDialogForm.FormCreate(Sender: TObject);
begin

end;

procedure TInsertDialogForm.CancelButtonClick(Sender: TObject);
begin

end;

procedure TInsertDialogForm.Label1Click(Sender: TObject);
begin

end;

procedure TInsertDialogForm.Panel1Click(Sender: TObject);
begin

end;

constructor TInsertDialogForm.Create(aTheOwner: TWinControl;
                       aTableInfo: TTableInfo;
                       aColumns: TColumnInfos;
                       aSQLQuery: TSQLQuery;
                       aDataSource: TDataSource);
var
  i: Integer;
begin
  inherited Create(aTheOwner);
  Table := aTableInfo;

  For i := 0 to High(aColumns) do begin
    SetLength(FieldEditors, Length(FieldEditors) + 1);
    FieldEditors[High(FieldEditors)] := TFieldEditor.Create(Self, aColumns[i], aDataSource);
    with FieldEditors[High(FieldEditors)] do begin
      Parent := Self;
      Align := alTop;
    end;
  end;

end;

end.

