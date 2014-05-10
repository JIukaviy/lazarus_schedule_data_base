unit UExpandPanel;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type
  TExpandPanel = class(TPanel)
  private
    FTimer: TTimer;
    FMinWidth: Integer;
    FMaxWidth: Integer;
    FButton: TButton;
    FTargetWidth: Integer;
    FAnimationSpeed: Integer;
    FDoAnimation: boolean;
    procedure SetMinWidth(Value: Integer);
    procedure SetTargetWidth(Value: Integer);
  protected
    FLastTime: Integer;
    procedure Animation(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure Open();
    procedure Close();
  published
    property MinWidth: Integer read FMinWidth write SetMinWidth;
    property MaxWidth: Integer read FMaxWidth write FMaxWidth;
    property TargetWidth: Integer read FTargetWidth write SetTargetWidth;
    property AnimationSpeed: Integer read FAnimationSpeed write FAnimationSpeed;
    property DoAnimation: boolean read FDoAnimation write FDoAnimation;
  end;

procedure Register;

implementation

procedure Register;
begin
  {$I uexpandpanel_icon.lrs}
  RegisterComponents('Misc',[TExpandPanel]);
end;

procedure TExpandPanel.Animation(Sender: TObject);
var
  dheight: Integer;
  dtime: double;
begin
  dheight := TargetHeight - FTargetWidth;
  if dheight = 0 then begin
    Timer.Enabled := false;
    FLastTime := 0;
    Exit;
  end;
  time := GetTickCount();
  if FLastTime = 0 then
    FLastTime := time - 15;
  dtime := (time - FLastTime) / 1000;
  Width := Width + dheight*dtime*FAnimationSpeed;
end;

procedure TExpandPanel.SetMinWidth(Value: Integer);
begin
  FMinWidth := Max(0, Value);
end;

procedure TExpandPanel.SetTargetWidth(Value: Integer);
begin
  FTargetWidth := EnsureRange(Value, FMinWidth, FMaxWidth);
  if FDoAnimation then
    FTimer.Enabled := true;
  else
    Width := FTargetWidth;
end;

constructor TExpandPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FTimer := TTimer.Create();
  with FTimer do begin
    Interval := 20;
    Enabled := false;
    OnTimer := @Animation;
  end;
  FDoAnimation := true;
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

end.
