unit UTableViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  StdCtrls, Buttons, ExtCtrls, DbCtrls, db, sqldb, umetadata, UFilters,
  UEditForm, UDBData, USQLQueryCreator;

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
    QueryCreator: TSQLQueryCreator;
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
  QueryCreator.OrderDesc := not QueryCreator.OrderDesc;
  if QueryCreator.OrderDesc then
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
  QueryCreator.SetOrderBy([SortBy.ItemIndex, SORT_BY_NAME]);
  QueryCreator.ShowItems(SQLQuery, DBGrid);
end;

procedure TGridForm.AddFilterButtonClick(Sender: TObject);
begin
  FilterList.Add();
end;

procedure TGridForm.AddRecordButtonClick(Sender: TObject);
begin
  EditForms.ShowEditForm(Self, Table, 0, @RefreshButtonClick);
end;

procedure TGridForm.DBEdit1Change(Sender: TObject);
begin

end;

procedure TGridForm.DBGridCellClick(Column: TColumn);
begin

end;

procedure TGridForm.DBGridDblClick(Sender: TObject);
begin
  EditForms.ShowEditForm(Self, Table, SQLQuery.FieldByName('ID').AsInteger, @RefreshButtonClick);
end;

procedure TGridForm.DelRecordButtonClick(Sender: TObject);
begin
  QueryCreator.DeleteRecordByID(SQLQuery.FieldByName('ID').AsInteger, SQLQuery);
  RefreshButtonClick(Sender);
end;

constructor TGridForm.Create(TheOwner: TComponent; aTable: TTableInfo);
begin
  inherited Create(TheOwner);
  InitConnection(Datasource, SQLQuery);

  Table := aTable;
  SortBy.Items.AddStrings(aTable.GetColumnCaptions());
  FilterList := TFilterList.Create(Self, aTable);
  with FilterList do begin
    Parent := Self;
    Top := 5;
    Align := alTop;
    OnHeightChange := @ListHeightChange;
  end;

  with QueryCreator do begin
    Table := aTable;
    FilterList := Self.FIlterList;
  end;

  QueryCreator.ShowItems(SQLQuery, DBGrid);
end;

end.

