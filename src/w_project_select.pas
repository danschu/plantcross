unit w_project_select;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Buttons;

type

  { Tf_ProjectSelect }

  Tf_ProjectSelect = class(TForm)
    b_ProjectNew: TBitBtn;
    b_ProjectOpen: TBitBtn;
    TheOpenDialog: TOpenDialog;
    TheSaveDialog: TSaveDialog;
    procedure b_ProjectNewClick(Sender: TObject);
    procedure b_ProjectOpenClick(Sender: TObject);
  private
    FFilename: String;
  public
    property Filename: String read FFilename write FFilename;
    class  function Execute(out _Filename: String): Boolean;
  end;

implementation

{$R *.lfm}

{ Tf_ProjectSelect }

procedure Tf_ProjectSelect.b_ProjectNewClick(Sender: TObject);
begin
  if TheSaveDialog.Execute then begin
    Filename := TheSaveDialog.FileName;
    ModalResult := mrOK;
  end;
end;

procedure Tf_ProjectSelect.b_ProjectOpenClick(Sender: TObject);
begin
  if TheOpenDialog.Execute then begin
    Filename := TheOpenDialog.FileName;
    ModalResult := mrOK;
  end;
end;

class function Tf_ProjectSelect.Execute(out _Filename: String): Boolean;
var
  frm: Tf_ProjectSelect;
begin
  frm := Tf_ProjectSelect.Create(nil);
  try
    Result := frm.ShowModal = mrOK;
    if Result then
      _Filename := frm.Filename;
  finally
    FreeAndNil(frm);
  end;
end;

end.

