// efg, July 1999
// www.efg2.com/lab
//
// Copyright 1999, All Rights Reserved.
// May be used freely for non-commercial purposes.

program HSV;

uses
  Forms,
  ScreenHSV in 'ScreenHSV.pas' {FormHSV},
  HSVLibrary in 'HSVLibrary.pas',
  IEEE754 in 'IEEE754.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFormHSV, FormHSV);
  Application.Run;
end.
