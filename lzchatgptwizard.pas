{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit lzchatgptwizard;

{$warn 5023 off : no warning about unused units}
interface

uses
  Umain, USetting, UConsts, UThread, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('Umain', @Umain.Register);
end;

initialization
  RegisterPackage('lzchatgptwizard', @Register);
end.
