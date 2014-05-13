unit UTableViewer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, DBGrids,
  StdCtrls, Buttons, ExtCtrls, DbCtrls, db, sqldb, umetadata, UFilters,
  UEditForm, UDBData, USQLQueryCreator, types;

type

  { TGridForm }

  TGridForm = class(TForm)
    AddFilterButton: TBitBtn;
    AddRecordButton: TBitBtn;
    DBGrid: TDBGrid;
    DelRecordButton: TBitBtn;
    ImageList: TImageList;
    MenuPanel: TPanel;
    DBGridPanel: TPanel;
    RefreshButton: TSpeedButton;
    SortBy: TComboBox;
    SortByLabel: TLabel;
    SortOrderButton: TSpeedButton;
    procedure AddFilterButtonClick(Sender: TObject);
    procedure AddRecordButtonClick(Sender: TObject);
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
    FColsToShow: TColumnInfos;
    FIlterList: TFilterList;
    Query: TSQLQueryCreator;
  public
    constructor Create(TheOwner: TComponent; aTable: TTableInfo);
  end;

implementation

{$R *.lfm}

procedure TGridForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FreeAndNil(Query);
  FreeAndNil(FilterList);
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
  Query.OrderDesc := not Query.OrderDesc;
  if Query.OrderDesc then
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
  Query.SetOrderBy(FColsToShow[SortBy.ItemIndex]);
  Query.ShowItems(DBGrid);
end;

procedure TGridForm.AddFilterButtonClick(Sender: TObject);
begin
  FilterList.Add();
end;

procedure TGridForm.AddRecordButtonClick(Sender: TObject);
begin
  EditForms.ShowEditForm(Self, Table, 0, @RefreshButtonClick);
end;

procedure TGridForm.DBGridDblClick(Sender: TObject);
begin
  EditForms.ShowEditForm(Self, Table, Query.GetCurrID, @RefreshButtonClick);
end;

procedure TGridForm.DelRecordButtonClick(Sender: TObject);
var
  UserChoice: Integer;
begin
  UserChoice := MessageDlg('Удаление', 'Вы действительно хотите удалить эту запись?', mtConfirmation, mbOKCancel, 0);
  if UserChoice = mrCancel then Exit;

  Query.SQLQuery.Delete();
  Query.SQLQuery.ApplyUpdates();
  RefreshButtonClick(Sender);
end;

constructor TGridForm.Create(TheOwner: TComponent; aTable: TTableInfo);
begin
  inherited Create(TheOwner);

  Table := aTable;
  FColsToShow := aTable.GetCols(cstAll, [coVisible]);
  SortBy.Items.AddStrings(GetColCaptions(FColsToShow));
  SortBy.ItemIndex := 0;
  FilterList := TFilterList.Create(Self, aTable);
  Query := TSQLQueryCreator.Create(aTable);
  DBGrid.DataSource := Query.DataSource;
  with FilterList do begin
    Parent := Self;
    Top := 5;
    Align := alTop;
    OnHeightChange := @ListHeightChange;
  end;

  Query.FilterList := FilterList;
  Query.ShowItems(DBGrid);
end;

end.

