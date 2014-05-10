unit UScheduleGrid;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Grids, ExtCtrls, LCLIntf, Math;

type

  { TScheduleGrid }

  TTargetHeight = record
    Row: Integer;
    Height: Integer;
  end;

  TScheduleGrid = class(TDrawGrid)
  private
    FTimer: TTimer;
    FAnimationSpeed: Integer;
    FTargetHeights: array of TTargetHeight;
  protected
    FLastTime: Integer;
    procedure DelAnimations();
    procedure Animation(Sender: TObject);
    procedure AddAnimation(aRow, aTargetHeight: Integer);
  public
    constructor Create(aOwner: TComponent); override;
    procedure CloseRow(aRow: Integer);
    procedure SetRowHeight(aRow, aHeight: Integer);
    property AnimationsSpeed: Integer read FAnimationSpeed write FAnimationSpeed;
  end;

implementation

{ TScheduleGrid }

procedure TScheduleGrid.DelAnimations;
var
  i, j: Integer;
begin
  j := 0;
  for i := 0 to High(FTargetHeights) do
    if FTargetHeights[i].Row >= 0 then begin
      FTargetHeights[j] := FTargetHeights[i];
      j += 1;
    end;
  SetLength(FTargetHeights, j);
end;

procedure TScheduleGrid.Animation(Sender: TObject);
var
  i: Integer;
  dheight: Integer;
  del: boolean;
  time: Integer;
  dtime: double;
begin
  if Length(FTargetHeights) = 0 then begin
    FTimer.Enabled := false;
    FLastTime := 0;
    Exit;
  end;
  del := false;
  time := GetTickCount();
  if FLastTime = 0 then
    FLastTime := time - 15;
  dtime := (time - FLastTime) / 1000;
  for i := 0 to High(FTargetHeights) do begin
    with FTargetHeights[i] do begin
      dheight := Height - RowHeights[Row];
      if dheight <> 0 then
        RowHeights[Row] := RowHeights[Row] + Round(dheight*dtime*FAnimationSpeed) + sign(dheight)
      else begin
        Row := -1;
        del := true;
      end;
    end;
  end;
  FLastTime := time;
  if del then
    DelAnimations();
end;

procedure TScheduleGrid.AddAnimation(aRow, aTargetHeight: Integer);
var
  i: Integer;
begin
  for i := 0 to High(FTargetHeights) do
    if FTargetHeights[i].Row = aRow then begin
      FTargetHeights[i].Height := aTargetHeight;
      Exit;
    end;
  SetLength(FTargetHeights, Length(FTargetHeights) + 1);
  with FTargetHeights[High(FTargetHeights)] do begin
    Row := aRow;
    Height := aTargetHeight;
  end;
  FTimer.Enabled := true;
end;

constructor TScheduleGrid.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  FTimer := TTimer.Create(Self);
  with FTimer do begin
    Interval := 20;
    Enabled := false;
    OnTimer := @Animation;
  end;
  FAnimationSpeed := 5;
end;

procedure TScheduleGrid.CloseRow(aRow: Integer);
begin
  AddAnimation(aRow, DefaultRowHeight);
end;

procedure TScheduleGrid.SetRowHeight(aRow, aHeight: Integer);
begin
  AddAnimation(aRow, aHeight);
end;

end.

