program PlantCross;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, memdslaz, w_plant_cross, w_project_select
  { you can add units after this };

{$R *.res}


var
  f_PlantCrossing: Tf_PlantCrossing;
  fn: String;
begin
  RequireDerivedFormResource:=True;
  Application.Scaled:=True;
  Application.Initialize;

  if Tf_ProjectSelect.Execute(fn) then begin     
    Application.CreateForm(Tf_PlantCrossing, f_PlantCrossing);
    f_PlantCrossing.SetFilename(fn);     
    Application.Run;
  end;

end.

