{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit expandpanelpcg;

interface

uses
  UExpandPanel, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('UExpandPanel', @UExpandPanel.Register);
end;

initialization
  RegisterPackage('expandpanelpcg', @Register);
end.
