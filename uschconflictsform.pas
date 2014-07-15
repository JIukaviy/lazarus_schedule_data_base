unit USchConflictsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  StdCtrls, USchConflicts;

type

  { TScheduleConflictsForm }

  TScheduleConflictsForm = class(TForm)
    RefreshButton: TButton;
    ConfsTreeView: TTreeView;
    procedure RefreshButtonClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ScheduleConflictsForm: TScheduleConflictsForm;

implementation

{$R *.lfm}

{ TScheduleConflictsForm }

procedure TScheduleConflictsForm.RefreshButtonClick(Sender: TObject);
var
  i, j, k: Integer;
  ConfTypeInfos: TConflictTypeInfos;
  GroupedConfs: TGroupedConflictItems;
  CurrConfType: TTreeNode;
  CurrGroup: TTreeNode;
begin
  ConfsTreeView.Items.Clear();
  ConfsTreeView.Items.Add(nil, 'Конфликты');
  Conflicts.UpdateConflicts();
  ConfTypeInfos := Conflicts.GetConfTypeInfos();
  for i := 0 to High(ConfTypeInfos) do begin
    GroupedConfs := Conflicts.GetConfsByTypeID(ConfTypeInfos[i].ID);

    if Length(GroupedConfs) = 0 then
      Continue;

    with ConfsTreeView do begin
      CurrConfType := Items.Add(Items.Item[0], ConfTypeInfos[i].Name);

      for j := 0 to High(GroupedConfs) do begin
        CurrGroup := Items.AddChild(CurrConfType, IntToStr(j));

        for k := 0 to High(GroupedConfs[j]) do
          Items.AddChild(CurrGroup, IntToStr(GroupedConfs[j][k].Item_ID));
      end;
    end;
  end;
end;

end.

