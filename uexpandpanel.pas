unit UExpandPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LCLIntf,
  Math, StdCtrls, ExtCtrls;

type

  TOpeningSide = (osLeft, osRight);

  { TExpandPanel }

  TExpandPanel = class(TPanel)
  private
    FTimer: TTimer;
    FMinWidth: Integer;
    FMaxWidth: Integer;
    FButton: TButton;
    FTargetWidth: Integer;
    FAnimationSpeed: Integer;
    FDoAnimation: boolean;
    FOpeningSide: TOpeningSide;
    procedure SetMinWidth(Value: Integer);
    procedure SetTargetWidth(Value: Integer);
    function GetIsMoving(): boolean;
  protected
    FLastTime: Integer;
    procedure Animation(Sender: TObject);
    procedure SetAnchors();
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open();
    procedure Close();
    property IsMoving: boolean read GetIsMoving;
  published
    property MinWidth: Integer read FMinWidth write SetMinWidth;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth;
    property TargetWidth: Integer read FTargetWidth write SetTargetWidth;
    property AnimationSpeed: Integer read FAnimationSpeed write FAnimationSpeed;
    property DoAnimation: boolean read FDoAnimation write FDoAnimation;
    property OpeningSide: TOpeningSide read FOpeningSide write FOpeningSide;
  end;

implementation

procedure TExpandPanel.Animation(Sender: TObject);
var
  dwidth: Integer;
  currdwidth: Integer;
  dtime: double;
  time: Integer;
begin
  dwidth := Width - FTargetWidth;
  if dwidth = 0 then begin
    FTimer.Enabled := false;
    FLastTime := 0;
    Exit;
  end;
  time := GetTickCount();
  if FLastTime = 0 then
    FLastTime := time - 15;
  dtime := (time - FLastTime) / 1000;
  currdwidth := Round(dwidth*dtime*FAnimationSpeed) + sign(dwidth);
  Width := Width - currdwidth;
  if FOpeningSide = osLeft then begin
    Left := Left + currdwidth;
  end;
  FLastTime := time;
end;

procedure TExpandPanel.SetAnchors;
var
  i: Integer;
begin
  for i := 1 to ComponentCount - 1 do
    with TWinControl(Components[i]) do
      if FOpeningSide = osLeft then
        Anchors := [akLeft]
      else
        Anchors := [akRight];
end;

procedure TExpandPanel.SetMinWidth(Value: Integer);
begin
  FMinWidth := Max(0, Value);
end;

procedure TExpandPanel.SetTargetWidth(Value: Integer);
begin
  FTargetWidth := EnsureRange(Value, FMinWidth, FMaxWidth);
  if not IsMoving then
    SetAnchors();
  if FDoAnimation then
    FTimer.Enabled := true
  else
    Width := FTargetWidth;
end;

function TExpandPanel.GetIsMoving: boolean;
begin
  Result := FTimer.Enabled;
end;

constructor TExpandPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FTimer := TTimer.Create(Self);
  with FTimer do begin
    Interval := 20;
    Enabled := false;
    OnTimer := @Animation;
  end;
  FMaxWidth := Width;
  FMinWidth := 0;
  FAnimationSpeed := 5;
  FDoAnimation := true;
  FOpeningSide := osLeft;
  Width := 0;
end;

destructor TExpandPanel.Destroy;
begin
  FreeAndNil(FTimer);
  inherited Destroy;
end;

procedure TExpandPanel.Open;
begin
  SetTargetWidth(FMaxWidth);
end;

procedure TExpandPanel.Close;
begin
  SetTargetWidth(FMinWidth);
end;

end.
