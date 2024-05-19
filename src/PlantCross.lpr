program PlantCross;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, memdslaz, w_plant_cross
  { you can add units after this };

{$R *.res}


var
  f_PlantCrossing: Tf_PlantCrossing;
begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(Tf_PlantCrossing, f_PlantCrossing);
  Application.Run;
end.

