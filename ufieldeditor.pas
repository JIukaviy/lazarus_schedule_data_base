unit UFieldEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, umetadata, Controls, StdCtrls, Graphics, ExtCtrls, sqldb,
  db, DBCtrls, UDBData, DIalogs, USQLQueryCreator;

type

  { TFieldEditor }

  TFieldEditor = class
  private
    FPanel: TPanel;
    FParent: TWinControl;
    FColumn: TColumnInfo;
    FFieldNameLabel: TLabel;
    FEdit: TWinControl;
    FLookupQuery: TSQLQueryCreator;
    FQuery: TSQLQueryCreator;
    FAlign: TAlign;
    FOnChange: TNotifyEvent;
    procedure SetParent(aParent: TWinControl);
    procedure SetAlign(aAlign: TAlign);
    procedure SetOnChangeEvent(aOnChange: TNotifyEvent);
    function ReadVal(): Variant;
  public
    constructor Create(aOwner: TWinControl;
                       aColumn: TColumnInfo;
                       aQuery: TSQLQueryCreator);
    destructor Destroy(); override;
    function CheckField(): boolean;
    property Parent: TWinControl read FParent write SetParent;
    property Align: TAlign read FAlign write SetAlign;
    property OnChange: TNotifyEvent read FOnChange write SetOnChangeEvent;
    property Value: Variant read ReadVal;
  end;

implementation

constructor TFieldEditor.Create(aOwner: TWinControl;
                                aColumn: TColumnInfo;
                                aQuery: TSQLQueryCreator);
begin
  inherited Create();
  FColumn := aColumn;
  FQuery := aQuery;

  FPanel := TPanel.Create(aOwner);
  with FPanel do begin
    Height := 25;
    BorderSpacing.Around := 1;
  end;

  FFieldNameLabel := TLabel.Create(FPanel);
  with FFieldNameLabel do begin
    Parent := FPanel;
    Layout := tlCenter;
    Caption := aColumn.Caption;
    AutoSize := false;
    Align := alLeft;
    Width := 120;
    BorderSpacing.Left := 3;
  end;

  if aColumn.FieldType = ftReference then begin
    FEdit := TDBLookupComboBox.Create(aOwner);
    with TDBLookupComboBox(FEdit) do begin
      FLookupQuery := TSQLQueryCreator.Create(TTableInfo(aColumn.RefTable));

      FLookupQuery.SelectCols(cstAll);
      FLookupQuery.SendQuery();

      ListField := aColumn.RefCols[0].AliasName;
      KeyField := TTableInfo(aColumn.RefTable).GetPrimaryCol.AliasName;
      DataField := aColumn.AliasName;

      DataSource := FQuery.DataSource;
      ListSource := FLookupQuery.DataSource;

      OnChange := FOnChange;
      Style := csDropDownList;
    end;
  end else begin
    FEdit := TDBEdit.Create(aOwner);
    with TDBEdit(FEdit) do begin
      DataSource := FQuery.DataSource;
      DataField := aColumn.AliasName();
      OnChange := FOnChange;
    end;
  end;

  with FEdit do begin
    Parent := FPanel;
    Align := alRight;
    AnchorToNeighbour(akLeft, 5, FFieldNameLabel);
  end;
end;

function TFieldEditor.CheckField(): boolean;
begin
  if FEdit <> nil then
    if FColumn.FieldType = ftReference then
      Result := TDBLookupComboBox(FEdit).ListFieldIndex > 0
    else
      Result := Trim(TDBEdit(FEdit).Text) <> '';
  //ShowMessage(Format('%d, %d', [TDBLookupComboBox(FEdit).ListFieldIndex, TDBLookupComboBox(FEdit).ItemIndex]));
end;

procedure TFieldEditor.SetParent(aParent: TWinControl);
begin
  FParent := aParent;
  FPanel.Parent := aParent;
end;

procedure TFieldEditor.SetAlign(aAlign: TAlign);
begin
  FAlign := aAlign;
  FPanel.Align := aAlign;
end;

procedure TFieldEditor.SetOnChangeEvent(aOnChange: TNotifyEvent);
begin
  FOnChange := aOnChange;
  if FEdit <> nil then
    if FColumn.FieldType = ftReference then
      TDBLookupComboBox(FEdit).OnChange := FOnChange
    else
      TDBEdit(FEdit).OnChange := FOnChange;
end;

function TFieldEditor.ReadVal(): Variant;
begin
  if FEdit <> nil then
    if FColumn.FieldType = ftReference then
      Result := TDBLookupComboBox(FEdit).KeyValue
    else
      Result := Trim(TDBEdit(FEdit).Text);
  //ShowMessage(TDBEdit(FEdit).Text);
end;

destructor TFieldEditor.Destroy();
begin
  FreeAndNil(FFieldNameLabel);
  FreeAndNil(FEdit);
  FreeAndNil(FPanel);
  FreeAndNil(FLookupQuery);
  inherited Destroy();
end;

end.

