unit w_plant_edit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, DateUtils, Variants, u_plant, RegExpr, crc, math;

type

  { Tf_AddPlant }

  Tf_AddPlant = class(TForm)
    b_Edit: TButton;
    chk_success: TCheckBox;
    chk_deleted: TCheckBox;
    cmb_Receiver: TComboBox;
    cmb_Donor: TComboBox;
    de_DateOfCrossing: TDateEdit;
    ed_Generation: TEdit;
    ed_Accession: TEdit;
    ed_Number: TEdit;
    ed_GenerationDonor: TEdit;
    ed_GenerationReceiver: TEdit;
    ed_UniqueID: TEdit;
    ed_Name: TEdit;
    l_Comment: TLabel;
    l_DateOfCrossing: TLabel;
    l_UnqiueID: TLabel;
    l_Generation: TLabel;
    l_Index: TLabel;
    l_GenerationDonor: TLabel;
    l_GenerationReceiver: TLabel;
    l_Receiver: TLabel;
    l_Accession: TLabel;
    l_Name: TLabel;
    l_Donor: TLabel;
    m_Comment: TMemo;
    procedure b_EditClick(Sender: TObject);
    procedure cmb_DonorChange(Sender: TObject);
    procedure cmb_ReceiverChange(Sender: TObject);
    procedure ed_AccessionChange(Sender: TObject);
    procedure ed_NameChange(Sender: TObject);
    procedure ed_UniqueIDChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FPlant: TPlant;
    FPlantList: TPlantListDB;
                     
    function CrcString(const _Text: string): String;
    function Get_F_BC(const _txt: String; out _f: integer; out _bc: integer): Boolean;
    procedure SetParentItem(_Cmb: TCombobox; _Edit: TEdit; _Item: TPlant);
    procedure UpdateUID(_Receiver: TPlant; _Donor: TPlant);
    procedure UpdateGeneration(_Receiver: TPlant; _Donor: TPlant);
    procedure CheckEnabled;
  public
    procedure SetData(_Plant: TPlant; _PlantList: TPlantListDB);
    class function Execute(_Owner: TComponent; _Plant: TPlant; _PlantList: TPlantListDB): Boolean;
  end;


implementation

{$R *.lfm}

{ Tf_AddPlant }


function Tf_AddPlant.CrcString(const _Text: string): String;
var
  crcvalue: longword;
begin
  crcvalue := crc32(0,nil,0);
  Result := IntToHex(crc32(crcvalue, @_Text[1], length(_Text)), 8);
end;

procedure Tf_AddPlant.SetData(_Plant: TPlant; _PlantList: TPlantListDB);
var
  cmb_text: String;
  i: integer;
begin
  FPlant := _Plant;
  FPlantList := _PlantList;

  cmb_Receiver.AddItem('None', nil);
  cmb_Donor.AddItem('None', nil);

  cmb_Receiver.ItemIndex := 0;
  cmb_Donor.ItemIndex := 0;

  for i := 0 to FPlantList.Count-1 do begin
    if FPlantList[i] <> FPlant then begin
      cmb_text := FPlantList[i].AsText();
      cmb_Receiver.AddItem(cmb_text, FPlantList[i]);
      cmb_Donor.AddItem(cmb_text, FPlantList[i]);
    end;
  end;

  if Assigned(_Plant) then begin
    chk_success.Checked := _Plant.Success;
    ed_Generation.Text := _Plant.Generation;
    ed_Number.Text := IntToStr(_Plant.Number);
    ed_Accession.Text := _Plant.Accession;
    de_DateOfCrossing.Date := _Plant.DateOfCrossing;
    ed_UniqueID.Text := _Plant.UID;
    m_Comment.Lines.Text := _Plant.Comment;
    chk_deleted.Checked := _Plant.Deleted;

    setParentItem(cmb_Receiver, ed_GenerationReceiver, FPlant.Receiver);
    setParentItem(cmb_Donor, ed_GenerationDonor, FPlant.Donor);

    ed_UniqueID.ReadOnly := True;
    ed_UniqueID.Color := clSilver;

    b_Edit.Caption := 'Update';
    Caption := 'Update Plant';
  end else begin
    b_Edit.Caption := 'Add';
    Caption := 'Add Plant';
  end;
  CheckEnabled;
end;

function Tf_AddPlant.Get_F_BC(const _txt: String; out _f: integer; out
  _bc: integer): Boolean;
var
  ep: TRegExpr;
  txt: String;
begin                
  _bc := 0;
  _f := 0;
  Result := False;
  txt := _txt;
  if txt = '' then
    txt := 'F0';
  ep := TRegExpr.Create('(BC[0-9]+)?(F[0-9]+)');
  try
     if ep.Exec(txt) then begin
       if ep.Match[1] = '' then begin
         _f := strtoint(Copy(ep.Match[2], 2));
         _bc := 0;
       end else begin
         _f := strtoint(Copy(ep.Match[2], 2));
         _bc := strtoint(Copy(ep.Match[1], 3));
       end;
       Result := True;
     end;   
  finally
    FreeAndNil(ep);
  end;
end;



procedure Tf_AddPlant.SetParentItem(_Cmb: TCombobox; _Edit: TEdit; _Item: TPlant
  );
var
  i: integer;
begin
  for i := 0 to _cmb.Items.Count-1 do begin
    if _Cmb.Items.Objects[i] = _Item then begin
      _Cmb.ItemIndex := i;  
      if Assigned(_Item) then
         _Edit.Text := _Item.Generation;
    end;
  end;
end;

procedure Tf_AddPlant.UpdateUID(_Receiver: TPlant; _Donor: TPlant);
var
  r1, r2: String;
  idx: integer;
  newuid: String;
  searchuid: String;
begin
  if ed_UniqueID.ReadOnly then
     Exit; // -->

  r1 := 'UNKNOWN';
  r2 := 'UNKNOWN';
  if Assigned(_Receiver) then
    r1 := _Receiver.AsUID();
  if Assigned(_Donor) then
    r2 := _Donor.AsUID();

  newuid := TPlant.AsUID(ed_Name.Text);
  if newuid = '' then begin
    newuid := TPlant.AsUID(ed_Accession.Text);
    if newuid = '' then
      newuid := Format('(%s)*(%s)', [r1, r2]);
  end;

  searchuid := newuid;
  idx := 1;
  while Assigned(FPlantList.FindByUID(searchuid)) do begin
    inc(idx);
    searchuid := Format('%s#%d', [newuid, idx]);
  end;

  ed_UniqueID.Text := searchuid;
  ed_Number.Text := IntToStr(idx);
end;

procedure Tf_AddPlant.UpdateGeneration(_Receiver: TPlant; _Donor: TPlant);
var
  FR: Integer;
  BCR: Integer;
  FD: Integer;
  BCD: Integer;
begin
  if Assigned(_Receiver) then begin
    ed_GenerationReceiver.Text := _Receiver.Generation;
  end else begin
    ed_GenerationReceiver.Text := '';
  end;

  if Assigned(_Donor) then begin
    ed_GenerationDonor.Text := _Donor.Generation;
  end else begin
    ed_GenerationDonor.Text := '';
  end;

  if Assigned(_Receiver) and Assigned(_Donor) then begin
    if Get_F_BC(_Receiver.Generation, FR, BCR) and Get_F_BC(_Donor.Generation, FD, BCD) then begin
      if (FR = FD) and (BCR = 0) and (BCD = 0) then begin
        ed_Generation.Text := 'F'+IntToStr(FR+1);
      end else if (FR <> FD) and (BCR = 0) and (BCD = 0) then begin
        ed_Generation.Text := 'BC1F1';
      end else if (FR = FD) and (BCR = BCD) then begin
        ed_Generation.Text := Format('BC%dF%d', [BCR, FR+1]);
      end else begin
        ed_Generation.Text := Format('BC%dF1', [Max(BCR, BCD)+1]);
      end;
    end;
  end;
end;

procedure Tf_AddPlant.CheckEnabled;
var
  uid_unique: Boolean;
begin
  uid_unique := Assigned(FPlantList) and (not Assigned(FPlantList.FindByUID(ed_UniqueID.Text))) and (Length(ed_UniqueID.Text) >= 3);
  if uid_unique or ed_UniqueID.ReadOnly then begin
    b_Edit.Enabled := True;
    if ed_UniqueID.ReadOnly then
      ed_UniqueID.Color := clSilver
    else
      ed_UniqueID.Color := clDefault;
  end else begin 
    ed_UniqueID.Color := clYellow;
    b_Edit.Enabled := False;
  end;
end;

procedure Tf_AddPlant.b_EditClick(Sender: TObject);
var
  plant: TPlant;
  idx: integer;
begin
  plant := TPlant.Create(FPlantList, '');
  try
    if Assigned(FPlant) then
      plant.GUID := FPlant.GUID;
    plant.Comment := m_Comment.Lines.Text;
    plant.DateOfCrossing := de_DateOfCrossing.Date;
    plant.Accession := ed_Accession.Text;
    plant.Generation := ed_Generation.Text;
    plant.Number := StrToIntDef(ed_Number.Text, 0);
    plant.Name := ed_Name.Text;
    idx := cmb_Donor.ItemIndex;
    plant.Donor := TPlant(cmb_Donor.Items.Objects[idx]);

    idx := cmb_Receiver.ItemIndex;
    plant.Receiver := TPlant(cmb_Receiver.Items.Objects[idx]);
    plant.Success := chk_success.Checked;
    plant.UID := ed_UniqueID.Text;
    plant.Deleted := chk_deleted.Checked;
    FPlantList.Save(plant); // wenn kein DB Fehler, dann interne Struktur anpassen
    if not Assigned(FPlant) then begin
      FPlantList.Add(plant);
      FPlant := plant;
      plant := nil; // do not free
    end else
      FPlant.UpdateFrom(plant);
  finally
    FreeAndNil(plant);
  end;
end;

procedure Tf_AddPlant.cmb_DonorChange(Sender: TObject);
var
  plant_receiver: TPlant;
  plant_donor: TPlant;
begin
  plant_receiver := TPlant(cmb_Receiver.Items.Objects[cmb_Receiver.ItemIndex]);
  plant_donor := TPlant(cmb_Donor.Items.Objects[cmb_Donor.ItemIndex]);
  UpdateUID(plant_receiver, plant_donor);    
  UpdateGeneration(plant_receiver, plant_donor);
end;

procedure Tf_AddPlant.cmb_ReceiverChange(Sender: TObject);
var
  plant_receiver: TPlant;   
  plant_donor: TPlant;
begin
  plant_receiver := TPlant(cmb_Receiver.Items.Objects[cmb_Receiver.ItemIndex]);
  plant_donor := TPlant(cmb_Donor.Items.Objects[cmb_Donor.ItemIndex]);
  UpdateUID(plant_receiver, plant_donor);
  UpdateGeneration(plant_receiver, plant_donor);
end;

procedure Tf_AddPlant.ed_AccessionChange(Sender: TObject);
var
  plant_receiver: TPlant;
  plant_donor: TPlant;
begin
  plant_receiver := TPlant(cmb_Receiver.Items.Objects[cmb_Receiver.ItemIndex]);
  plant_donor := TPlant(cmb_Donor.Items.Objects[cmb_Donor.ItemIndex]);
  UpdateUID(plant_receiver, plant_donor);
end;

procedure Tf_AddPlant.ed_NameChange(Sender: TObject);
var
  plant_receiver: TPlant;
  plant_donor: TPlant;
begin
  plant_receiver := TPlant(cmb_Receiver.Items.Objects[cmb_Receiver.ItemIndex]);
  plant_donor := TPlant(cmb_Donor.Items.Objects[cmb_Donor.ItemIndex]);
  UpdateUID(plant_receiver, plant_donor);
end;

procedure Tf_AddPlant.ed_UniqueIDChange(Sender: TObject);
begin
  CheckEnabled;
end;

procedure Tf_AddPlant.FormCreate(Sender: TObject);
var
  id: String;
  gd: TGuid;
begin                
  CreateGUID(gd);
  id := GUIDToString(gd);
  de_DateOfCrossing.Date := now;
  ed_Generation.Text := 'F0';
  ed_UniqueID.Text := CrcString(id);
  ed_Number.Text := '1';
end;

procedure Tf_AddPlant.FormDestroy(Sender: TObject);
begin
end;

class function Tf_AddPlant.Execute(_Owner: TComponent; _Plant: TPlant;
  _PlantList: TPlantListDB): Boolean;
var
  f_AddPlant: Tf_AddPlant;
begin
  f_AddPlant := Tf_AddPlant.Create(_Owner);
  try
     f_AddPlant.SetData(_Plant,_PlantList);
     result := f_AddPlant.ShowModal = mrOK;
  finally
    FreeAndNil(f_AddPlant)
  end;
end;

end.

