unit UFieldEditor;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, umetadata, Controls, StdCtrls, Graphics, ExtCtrls, sqldb, db, DBCtrls, UDBData, DIalogs;

type
  TFieldEditor = class
  private
    FPanel: TPanel;
    FParent: TWinControl;
    FColumn: TColumnInfo;
    FFieldNameLabel: TLabel;
    FEdit: TWinControl;
    FDataSource: TDataSource;
    FListSource: TDataSource;
    FLookupQuery: TSQLQuery;
    FAlign: TAlign;
    FOnChange: TNotifyEvent;
    procedure SetParent(aParent: TWinControl);
    procedure SetAlign(aAlign: TAlign);
    procedure SetOnChangeEvent(aOnChange: TNotifyEvent);
    function ReadVal(): Variant;
  public
    constructor Create(aOwner: TWinControl;
                       aColumn: TColumnInfo;
                       aDataSource: TDataSource);
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
                                aDataSource: TDataSource);
begin
  inherited Create();
  FColumn := aColumn;
  FDataSource := aDataSource;

  FPanel := TPanel.Create(aOwner);

  FFieldNameLabel := TLabel.Create(FPanel);
  with FFieldNameLabel do begin
    Parent := FPanel;
    Layout := tlCenter;
    Caption := FColumn.Caption;
    AutoSize := false;
    Align := alLeft;
    Width := 120;
    BorderSpacing.Left := 3;
  end;

  {case aColumn.ColumnInfo.FieldType of
    ftString: FEdit := TEdit.Create(FPanel);
  end;}

  if aColumn.IsReference then begin
    FEdit := TDBLookupComboBox.Create(aOwner);
    with TDBLookupComboBox(FEdit) do begin
      FLookupQuery := TSQLQuery.Create(aOwner);
      FListSource := TDataSource.Create(aOwner);
      InitConnection(FListSource, FLookupQuery);

      FLookupQuery.SQL.Text := Format('select ID, %s from %s',
                            [aColumn.ReferenceName, aColumn.ReferenceTable]);
      FLookupQuery.Open();

      Style := csDropDownList;

      ListField := aColumn.ReferenceName;
      KeyField := 'ID';
      DataField := aColumn.Name;
      DataSource := FDataSource;
      ListSource := FListSource;
      OnChange := FOnChange;
    end;
  end else begin
    FEdit := TDBEdit.Create(aOwner);
    with TDBEdit(FEdit) do begin
      DataSource := aDataSource;
      DataField := aColumn.Name;
      OnChange := FOnChange;
    end;
  end;

  with FEdit do begin
    Parent := FPanel;
    Align := alRight;
    AnchorToNeighbour(akLeft, 5, FFieldNameLabel);
  end;

   with FPanel do begin
    Height := 25;
    BorderSpacing.Around := 1;
  end;
end;

function TFieldEditor.CheckField(): boolean;
begin
  if FEdit <> nil then
    if FColumn.IsReference then
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
    if FColumn.IsReference then
      TDBLookupComboBox(FEdit).OnChange := FOnChange
    else
      TDBEdit(FEdit).OnChange := FOnChange;
end;

function TFieldEditor.ReadVal(): Variant;
begin
  if FEdit <> nil then
    if FColumn.IsReference then
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
  FreeAndNil(FListSource);
  inherited Destroy();
end;

end.

