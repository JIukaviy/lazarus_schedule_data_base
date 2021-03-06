program Data_base;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, UMain, umetadata, UTableViewer, UFilters, ufieldeditor, UDBData,
  UScheduleForm, USQLQueryCreator, USchConflictsForm, USchConflicts, UExport;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TScheduleForm, ScheduleForm);
  Application.CreateForm(TScheduleConflictsForm, ScheduleConflictsForm);
  Application.Run;
end.

