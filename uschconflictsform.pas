unit USchConflictsForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls;

type

  { TScheduleConflictsForm }

  TScheduleConflictsForm = class(TForm)
    ConflictsTreeView: TTreeView;
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  ScheduleConflictsForm: TScheduleConflictsForm;

implementation

{$R *.lfm}

end.

