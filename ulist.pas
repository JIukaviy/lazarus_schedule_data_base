unit UListBox;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

PEvent = procedure(Sender: TObject) of object;

type
  TBaseListBox = class
    FElements: TObject;
    FLeft: Integer;
    FTop: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FSpace: Integer;
    FOnHeightChange: PEvent;
    procedure SetLeft(aLeft: integer);
    procedure SetTop(aTop: integer);
    procedure SetWidth(aWidth: integer);
    procedure UpdatePos();
  public
    constructor Create(aParent: TWinControl; aTable: TTableInfo);
    destructor Destroy(); override;
    procedure Add();
    procedure Del(Index: Integer);
    property Left: integer read FLeft write SetLeft;
    property Top: integer read FTop write SetTop;
    property Width: integer read FWidth write SetWidth;
    property Height: Integer read FHeight;
    property OnHeightChange: PEvent read FOnHeightChange write FOnHeightChange;
  end;

implementation

procedure TBaseListBox.Add();
var
  ArrLen: Integer;
  ArrHigh: Integer;
begin
  ArrLen := Length(FElements);
  SetLength(FElements, ArrLen + 1);
  ArrHigh := High(FElements);
  FElements[ArrHigh] := TFilter.Create(FParent, FTable);

  with Filters[ArrHigh] do begin
    Parent := Self.FParent;
    //SetButton(CreateButton('Images\AddFilter.png', @OnAddClick, ArrHigh));
    SetButton(CreateButton('Images\DelFilter.png', @OnDelClick, ArrHigh));
    Left := 5;
    Width := Self.FWidth;
  end;

  if ArrLen > 0 then begin
    //Filters[ArrHigh - 1].SetButton(CreateButton('Images\DelFilter.png', @OnDelClick, ArrHigh - 1));
  end;

  UpdatePos();
end;

end.

