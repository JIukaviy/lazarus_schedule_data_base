unit UDBGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  StdCtrls, Buttons, ExtCtrls, DbCtrls, db, sqldb, UTables, UFilters,
  UEditForm, UDBData;

type

  { TGridForm }

  TGridForm = class(TForm)
    AddFilterButton: TBitBtn;
    AddRecordButton: TBitBtn;
    DelRecordButton: TBitBtn;
    DBGrid: TDBGrid;
    ImageList: TImageList;
    MenuPanel: TPanel;
    DBGridPanel: TPanel;
    RefreshButton: TSpeedButton;
    SortBy: TComboBox;
    SortByLabel: TLabel;
    Datasource: TDatasource;
    SortOrderButton: TSpeedButton;
    SQLQuery: TSQLQuery;
    procedure AddFilterButtonClick(Sender: TObject);
    procedure AddRecordButtonClick(Sender: TObject);
    procedure DBEdit1Change(Sender: TObject);
    procedure DBGridCellClick(Column: TColumn);
    procedure DBGridDblClick(Sender: TObject);
    procedure DelRecordButtonClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure ListHeightChange(Sender: TObject);
    procedure SortByLabelClick(Sender: TObject);
    procedure SortOrderButtonClick(Sender: TObject);
    procedure RefreshButtonClick(Sender: TObject);
  private
    Table: TTableInfo;
    FIlterList: TFilterList;
  public
    constructor Create(TheOwner: TComponent; aTable: TTableInfo);
  end;

implementation

{$R *.lfm}

procedure TGridForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SQLQuery.Close;
  FilterList.Destroy();
end;

procedure TGridForm.FormCreate(Sender: TObject);
begin

end;

procedure TGridForm.FormResize(Sender: TObject);
begin
  FilterList.Width := Self.Width;
end;

procedure TGridForm.ListHeightChange(Sender: TObject);
begin
  Self.Height := DBGridPanel.Height + FilterList.Height + MenuPanel.Height;
end;

procedure TGridForm.SortByLabelClick(Sender: TObject);
begin

end;

procedure TGridForm.SortOrderButtonClick(Sender: TObject);
begin
  Table.SortOrder := not Table.SortOrder;
  if Table.SortOrder then
    ImageList.GetBitmap(1, SortOrderButton.Glyph)
  else
    ImageList.GetBitmap(0, SortOrderButton.Glyph);
  RefreshButtonClick(Self);
end;

procedure TGridForm.RefreshButtonClick(Sender: TObject);
begin
  {SQLQuery.Close;
  SQLQuery.Params.CreateParam(ftInteger, 'rsize', ptInput).AsInteger := 50;
  SQLQuery.SQL.Text := 'Select a.Name, a."Size" from Rooms a where a."Size" < :rsize';
  SQLQuery.Open;}
  if not FilterList.CheckFields() then begin
    ShowMessage('Проверьте заполненность полей фильтра');
    Exit;
  end;
  Table.SetOrderBy([SortBy.ItemIndex, SORT_BY_NAME]);
  Table.SetFilters(FilterList.GetFilterInfos());
  Table.ShowItems(SQLQuery, DBGrid);
end;

procedure TGridForm.AddFilterButtonClick(Sender: TObject);
begin
  FilterList.Add();
end;

procedure TGridForm.AddRecordButtonClick(Sender: TObject);
var
  EditForm: TEditForm;
begin
  EditForm := TEditForm.Create(Self, Table, 0);
  EditForm.OnApply := @RefreshButtonClick;
  EditForm.Show;
end;

procedure TGridForm.DBEdit1Change(Sender: TObject);
begin

end;

procedure TGridForm.DBGridCellClick(Column: TColumn);
begin

end;

procedure TGridForm.DBGridDblClick(Sender: TObject);
var
  EditForm: TEditForm;
begin
  EditForm := TEditForm.Create(Self, Table, SQLQuery.FieldByName('ID').AsInteger);
  EditForm.OnApply := @RefreshButtonClick;
  EditForm.Show;
end;

procedure TGridForm.DelRecordButtonClick(Sender: TObject);
var
  ID: Integer;
begin
  ID := SQLQuery.FieldByName('ID').AsInteger;
  SQLQuery.Close();
  SQLQuery.SQL.Text := Format('delete from %s where ID = %d', [Table.GetName, ID]);
  SQLQuery.ExecSQL();
  RefreshButtonClick(Sender);
end;

constructor TGridForm.Create(TheOwner: TComponent; aTable: TTableInfo);
begin
  inherited Create(TheOwner);
  InitConnection(Datasource, SQLQuery);

  Table := aTable;
  Table.ShowItems(SQLQuery, DBGrid);
  SortBy.Items.AddStrings(aTable.GetColumnCaptions());
  FilterList := TFilterList.Create(Self, aTable);
  with FilterList do begin
    Parent := Self;
    Top := 5;
    Align := alTop;
    OnHeightChange := @ListHeightChange;
  end;
end;

end.

