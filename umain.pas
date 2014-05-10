unit UMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, IBConnection, sqldb, FileUtil, Forms, Controls,
  Graphics, Dialogs, Grids, DbCtrls, Menus, StdCtrls, umetadata, UTableViewer, UDBData,
  UScheduleForm;

type

  { TMainForm }

  TMainForm = class(TForm)
    Button1: TButton;
    MainMenu: TMainMenu;
    MenuFile: TMenuItem;
    MenuHelp: TMenuItem;
    MenuExit: TMenuItem;
    MenuAbout: TMenuItem;
    MenuTables: TMenuItem;
    procedure Button1Click(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure InitMenu();
    procedure MenuAboutClick(Sender: TObject);
    procedure MenuExitClick(Sender: TObject);
    procedure ShowTable(Sender: TObject);
  private
    { private declarations }
  public
    SomethingChanged: boolean;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.lfm}

{ TMainForm }


procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  UserChoice: Integer;
begin
  UserChoice := MessageDlg('Сохранение изменений', 'Подтвердить транзацию?', mtConfirmation, mbYesNoCancel, 0);
  case UserChoice of
    mrYes: SQLTransaction.Commit;
    mrNo: Exit;
    mrCancel: CloseAction := caNone;
  end;
end;

procedure TMainForm.Button1Click(Sender: TObject);
begin
  ScheduleForm.Show;
end;

procedure TMainForm.InitMenu();
begin

end;

procedure TMainForm.MenuAboutClick(Sender: TObject);
begin
  ShowMessage('Добрынин Н.В. (c)')
end;

procedure TMainForm.MenuExitClick(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.FormCreate(Sender: TObject);
var
  i: Integer;
  Names: strings;
  MenuItem: TMenuItem;
begin
  SomethingChanged := false;
  Names := ListOfTables.GetCaptions();
  for i := 0 to High(Names) do begin
    MenuItem := TMenuItem.Create(MainMenu);
    MenuItem.Caption := Names[i];
    MenuItem.Tag := i;
    MenuItem.OnClick := @ShowTable;
    MainMenu.Items.Items[1].Add(MenuItem);
  end;
end;

procedure TMainForm.ShowTable(Sender: TObject);
var
  NewForm: TGridForm;
begin
  NewForm := TGridForm.Create(MainForm, ListOfTables.GetTable[TMenuItem(Sender).Tag]);
  NewForm.Caption := TMenuItem(Sender).Caption;
  NewForm.Show;
end;

end.

