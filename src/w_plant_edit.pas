unit w_plant_edit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, EditBtn,
  ComCtrls, ExtCtrls, Spin, VirtualTrees, ExtendedNotebook, DateUtils, Variants,
  u_plant, RegExpr, crc, math;

type

  { Tf_AddPlant }

  Tf_AddPlant = class(TForm)
    b_SpikeUpdate: TButton;
    b_SpikeDelete: TButton;
    b_SpikeAdd: TButton;
    b_Edit: TButton;
    chk_Autogenerate: TCheckBox;
    chk_deleted: TCheckBox;
    cmb_Receiver: TComboBox;
    cmb_Donor: TComboBox;
    cmb_Status: TComboBox;
    cmb_NameFilter: TComboBox;
    cmb_ReceiverSpike: TComboBox;
    de_DateOfCrossing: TDateEdit;
    ed_SpikeName: TEdit;
    ed_Species: TEdit;
    ed_Generation: TEdit;
    ed_Accession: TEdit;
    ed_Number: TEdit;
    ed_GenerationDonor: TEdit;
    ed_GenerationReceiver: TEdit;
    ed_UniqueID: TEdit;
    en_Bottom: TExtendedNotebook;
    l_FlowerCount: TLabel;
    l_ReceiverSpike: TLabel;
    l_SeedCount: TLabel;
    l_SpikeName: TLabel;
    l_Comment: TLabel;
    l_NameFilter: TLabel;
    l_Species: TLabel;
    l_Status: TLabel;
    l_DateOfCrossing: TLabel;
    l_Generation: TLabel;
    l_Index: TLabel;
    l_GenerationDonor: TLabel;
    l_GenerationReceiver: TLabel;
    l_Receiver: TLabel;
    l_Accession: TLabel;
    l_Name: TLabel;
    l_Donor: TLabel;
    m_Comment: TMemo;
    p_SpikeRight: TPanel;
    se_SeedCount: TSpinEdit;
    se_FlowerCount: TSpinEdit;
    TheVirtualStringTree: TVirtualStringTree;
    ts_Spikes: TTabSheet;
    ts_Comment: TTabSheet;
    procedure b_SpikeAddClick(Sender: TObject);
    procedure b_EditClick(Sender: TObject);
    procedure b_SpikeDeleteClick(Sender: TObject);
    procedure b_SpikeUpdateClick(Sender: TObject);
    procedure chk_AutogenerateChange(Sender: TObject);
    procedure cmb_DonorChange(Sender: TObject);
    procedure cmb_NameFilterChange(Sender: TObject);
    procedure cmb_ReceiverChange(Sender: TObject);
    procedure cmb_StatusChange(Sender: TObject);
    procedure ed_AccessionChange(Sender: TObject);
    procedure ed_SpeciesChange(Sender: TObject);
    procedure ed_SpikeNameChange(Sender: TObject);
    procedure ed_UniqueIDChange(Sender: TObject);
    procedure en_BottomChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TheVirtualStringTreeClick(Sender: TObject);
    procedure TheVirtualStringTreeDblClick(Sender: TObject);
    procedure TheVirtualStringTreeDrawText(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure TheVirtualStringTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
  private
    FPlant: TPlant;
    FPlantList: TPlantListDB;
    FTempSpikes: TSpikeList;

    function GetCrossCount(): integer;
    procedure UpdateUUID;
    procedure SpikesUpdated;
    procedure UpdateSpikeName;
    procedure SetComboboxParents(_FilterIndex: Integer);
    procedure InitData;
    function CrcString(const _Text: string): String;
    function Get_F_BC(const _txt: String; out _f: integer; out _bc: integer): Boolean;
    procedure SetParentItem(_Cmb: TCombobox; _Edit: TEdit; _Item: TPlant);
    procedure UpdateGeneration(_Receiver: TPlant; _Donor: TPlant);
    procedure UpdateReceiverSpike(_Receiver: TPlant);
    procedure CheckEnabled;
    procedure CheckSpikeButtons;
    procedure CheckSpikeAddEnabled;
  public
    procedure SetData(_Plant: TPlant; _PlantList: TPlantListDB; _FilterIndex: Integer);
    class function Execute(_Owner: TComponent; _Plant: TPlant; _PlantList: TPlantListDB; _FilterIndex: Integer): Boolean;
  end;


implementation

{$R *.lfm}

{ Tf_AddPlant }


function Tf_AddPlant.CrcString(const _Text: string): String;
var
  crcvalue: longword;
  txt: String;
begin
  txt := _Text;
  if txt = '' then
    txt := 'unknown';
  crcvalue := crc32(0,nil,0);
  Result := IntToHex(crc32(crcvalue, @txt[1], length(txt)), 8);
end;

procedure Tf_AddPlant.SetData(_Plant: TPlant; _PlantList: TPlantListDB;
  _FilterIndex: Integer);
var
  plantStatus: TPlantStatus;
  Spike: TSpike;
begin
  FPlant := _Plant;
  FPlantList := _PlantList;
  if Assigned(_Plant) then begin
    chk_Autogenerate.Checked := false;
  end;

  InitData;
  SetComboboxParents(_FilterIndex);

  if Assigned(_Plant) then begin
    for Spike in _Plant.Spikes do
      FTempSpikes.Add(Spike.Copy(nil));

    cmb_Status.ItemIndex := 1;
    for plantStatus in TPlantStatus do begin
      if plantStatus = _Plant.Status then
        cmb_Status.ItemIndex := cmb_Status.Items.Count;
    end;

    cmb_Status.ItemIndex := Integer(_Plant.Status);
    ed_Generation.Text := _Plant.Generation;
    ed_Number.Text := IntToStr(_Plant.Number);
    ed_Accession.Text := _Plant.Accession;
    ed_Species.Text := _Plant.Species;
    de_DateOfCrossing.Date := _Plant.DateOfCrossing;
    ed_UniqueID.Text := _Plant.UID;
    m_Comment.Lines.Text := _Plant.Comment;
    chk_deleted.Checked := _Plant.Deleted;

    b_Edit.Caption := 'Update Plant/Seed';
    Caption := 'Update Plant/Seed';
  end else begin
    b_Edit.Caption := 'Add Plant/Seed';
    Caption := 'Add Plant/Seed';
  end;

  UpdateSpikeName;
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
  ed_Number.Text := IntToStr(GetCrossCount());
end;

procedure Tf_AddPlant.UpdateReceiverSpike(_Receiver: TPlant);
var
  Spike: TSpike;
begin
  cmb_ReceiverSpike.Clear;
  cmb_ReceiverSpike.AddItem('unknown', nil);
  cmb_ReceiverSpike.ItemIndex := 0;
  if not Assigned(_Receiver) then
     Exit; // ->
  for Spike in _Receiver.Spikes do
    cmb_ReceiverSpike.AddItem(Spike.Name, Spike);
end;

procedure Tf_AddPlant.CheckEnabled;
var
  uid_unique: Boolean;
begin
  uid_unique := Assigned(FPlantList) and (not Assigned(FPlantList.FindByUID(ed_UniqueID.Text))) and (Length(ed_UniqueID.Text) >= 3);
  if uid_unique or Assigned(FPlant) then begin
    b_Edit.Enabled := True;
    ed_UniqueID.Color := clDefault;
  end else begin 
    ed_UniqueID.Color := clYellow;
    b_Edit.Enabled := False;
  end;
end;

procedure Tf_AddPlant.CheckSpikeButtons;
var
  Node: PVirtualNode;
  idx: Integer;
begin                        
  CheckSpikeAddEnabled;
  b_SpikeDelete.Enabled := False;
  b_SpikeUpdate.Enabled := False;
  if TheVirtualStringTree.SelectedCount = 0 then
    Exit; // -->
  Node := TheVirtualStringTree.GetFirstSelected;
  if not Assigned(Node) then
    Exit; // -->
  idx := Node^.Index;
  if idx >= FTempSpikes.Count then
    Exit; // -->
  b_SpikeDelete.Enabled := True;
  b_SpikeUpdate.Enabled := True;
  ed_SpikeName.Text := FTempSpikes[idx].Name;
  se_SeedCount.Value := FTempSpikes[idx].SeedCount;
  se_FlowerCount.Value := FTempSpikes[idx].FlowerCount;
end;

procedure Tf_AddPlant.CheckSpikeAddEnabled;
begin       
  b_SpikeAdd.Enabled:= (Length(ed_SpikeName.Text) > 0)
    and (not Assigned(FTempSpikes.FindByName(ed_SpikeName.Text)));
end;

procedure Tf_AddPlant.b_EditClick(Sender: TObject);
var
  plant: TPlant;
  Spike: TSpike;
  idx: integer;
begin
  plant := TPlant.Create(FPlantList, '');
  try
    if Assigned(FPlant) then
      plant.GUID := FPlant.GUID;
    plant.Comment := m_Comment.Lines.Text;
    plant.DateOfCrossing := de_DateOfCrossing.Date;
    plant.Species := ed_Species.Text;
    plant.Accession := ed_Accession.Text;
    plant.Generation := ed_Generation.Text;
    plant.Number := StrToIntDef(ed_Number.Text, 0);
    plant.UID := ed_UniqueID.Text;
    idx := cmb_Donor.ItemIndex;
    plant.Donor := TPlant(cmb_Donor.Items.Objects[idx]);

    idx := cmb_Receiver.ItemIndex;
    plant.Receiver := TPlant(cmb_Receiver.Items.Objects[idx]);         
    idx := cmb_ReceiverSpike.ItemIndex;
    plant.ReceiverSpike := TSpike(cmb_ReceiverSpike.Items.Objects[idx]);

    plant.Status := psUnknown;
    if cmb_Status.ItemIndex >= 0 then
      plant.Status := TPlantStatus(Int64(cmb_Status.Items.Objects[cmb_Status.ItemIndex]));

    plant.Deleted := chk_deleted.Checked;

    for Spike in FTempSpikes do
       plant.Spikes.Add(Spike.Copy(plant));

    FPlantList.Save(plant, True); // wenn kein DB Fehler, dann interne Struktur anpassen
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

procedure Tf_AddPlant.b_SpikeDeleteClick(Sender: TObject);
var
  Node: PVirtualNode;
  idx: Integer;
begin
  if TheVirtualStringTree.SelectedCount = 0 then
    Exit; // -->
  Node := TheVirtualStringTree.GetFirstSelected;
  if not Assigned(Node) then
    Exit; // -->
  idx := Node^.Index;
  if idx >= FTempSpikes.Count then
    Exit; // -->
  FTempSpikes.Extract(FTempSpikes.Items[idx]).Free;   
  SpikesUpdated; 
  CheckSpikeButtons;
end;

procedure Tf_AddPlant.b_SpikeUpdateClick(Sender: TObject);
var
  Node: PVirtualNode;
  idx: Integer;
begin
  if TheVirtualStringTree.SelectedCount = 0 then
    Exit; // -->
  Node := TheVirtualStringTree.GetFirstSelected;
  if not Assigned(Node) then
    Exit; // -->
  idx := Node^.Index;
  if idx >= FTempSpikes.Count then
    Exit; // -->

  FTempSpikes[idx].Name := ed_SpikeName.Text;
  FTempSpikes[idx].SeedCount := se_SeedCount.Value;
  FTempSpikes[idx].FlowerCount := se_FlowerCount.Value;
  SpikesUpdated; 
  CheckSpikeButtons;
end;

procedure Tf_AddPlant.chk_AutogenerateChange(Sender: TObject);
begin
  if chk_Autogenerate.Checked then begin
    ed_UniqueID.Enabled := False;
    ed_UniqueID.Color := clSilver;
  end else begin
    ed_UniqueID.Enabled := True;
    ed_UniqueID.Color := clWindow;
  end;
  UpdateUUID;
end;

procedure Tf_AddPlant.b_SpikeAddClick(Sender: TObject);
var
  Spike: TSpike;
begin
  Spike := FTempSpikes.FindByName(ed_SpikeName.Text);
  if not Assigned(Spike) then begin
    Spike := TSpike.Create(FPlant, '', ed_SpikeName.Text, se_SeedCount.Value, se_FlowerCount.Value);
    FTempSpikes.Add(Spike);
  end else begin
    Spike.SeedCount := se_SeedCount.Value;
    Spike.FlowerCount := se_FlowerCount.Value;
  end;
  UpdateSpikeName;
  SpikesUpdated;   
  CheckSpikeButtons;
end;

procedure Tf_AddPlant.cmb_DonorChange(Sender: TObject);
var
  plant_receiver: TPlant;
  plant_donor: TPlant;
begin
  plant_receiver := TPlant(cmb_Receiver.Items.Objects[cmb_Receiver.ItemIndex]);
  plant_donor := TPlant(cmb_Donor.Items.Objects[cmb_Donor.ItemIndex]);
  UpdateGeneration(plant_receiver, plant_donor);

  UpdateUUID;
end;

procedure Tf_AddPlant.cmb_NameFilterChange(Sender: TObject);
begin
  SetComboboxParents(cmb_NameFilter.ItemIndex);
end;

procedure Tf_AddPlant.cmb_ReceiverChange(Sender: TObject);
var
  plant_receiver: TPlant;   
  plant_donor: TPlant;
begin
  plant_receiver := TPlant(cmb_Receiver.Items.Objects[cmb_Receiver.ItemIndex]);
  plant_donor := TPlant(cmb_Donor.Items.Objects[cmb_Donor.ItemIndex]);
  UpdateGeneration(plant_receiver, plant_donor);
  UpdateReceiverSpike(plant_receiver);

  UpdateUUID;
end;

procedure Tf_AddPlant.cmb_StatusChange(Sender: TObject);
begin

end;

procedure Tf_AddPlant.ed_AccessionChange(Sender: TObject);
begin
  UpdateUUID;
end;

procedure Tf_AddPlant.ed_SpeciesChange(Sender: TObject);
begin
  UpdateUUID;
end;

procedure Tf_AddPlant.ed_SpikeNameChange(Sender: TObject);
begin
  CheckSpikeAddEnabled;
end;

procedure Tf_AddPlant.ed_UniqueIDChange(Sender: TObject);
begin
  CheckEnabled;
end;

procedure Tf_AddPlant.en_BottomChange(Sender: TObject);
begin

end;



procedure Tf_AddPlant.FormCreate(Sender: TObject);
var
  id: String;
  gd: TGuid;
begin
  Constraints.MinHeight := Height;
  Constraints.MinWidth := Width;
  FTempSpikes := TSpikeList.Create(True);
  InitData;

  CreateGUID(gd);
  id := GUIDToString(gd);
  de_DateOfCrossing.Date := now;
  ed_Generation.Text := 'F0';
  ed_UniqueID.Text := CrcString(id);
  ed_Number.Text := '1';
end;

procedure Tf_AddPlant.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FTempSpikes);
end;

procedure Tf_AddPlant.TheVirtualStringTreeClick(Sender: TObject);
begin
  CheckSpikeButtons;
end;

procedure Tf_AddPlant.TheVirtualStringTreeDblClick(Sender: TObject);

begin

end;

procedure Tf_AddPlant.TheVirtualStringTreeDrawText(Sender: TBaseVirtualTree;
  TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
  const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
begin
end;

procedure Tf_AddPlant.TheVirtualStringTreeGetText(Sender: TBaseVirtualTree;
  Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
  var CellText: String);
const
  COLUMN_SPIKE_NAME = 0;
  COLUMN_SPIKE_SEED_COUNT = 1;
  COLUMN_SPIKE_FLOWER_COUNT = 2;
var
  idx: integer;
  spike: TSpike;
begin
  idx := Node^.Index;
  if idx >= FTempSpikes.Count then
    Exit; // -->

  spike := FTempSpikes[idx];

  case Column of
     COLUMN_SPIKE_NAME: CellText := spike.Name;
     COLUMN_SPIKE_SEED_COUNT: CellText := IntToStr(spike.SeedCount);
     COLUMN_SPIKE_FLOWER_COUNT: CellText := IntToStr(spike.FlowerCount);
  end;
end;

function Tf_AddPlant.GetCrossCount: integer;
var
  plant_receiver: TPlant;
  plant_donor: TPlant;
  pl: TPlant;
  spec: String;
  c: integer;
  plant: TPlant;
begin
  Result := 1;
  plant_receiver := TPlant(cmb_Receiver.Items.Objects[cmb_Receiver.ItemIndex]);
  plant_donor := TPlant(cmb_Donor.Items.Objects[cmb_Donor.ItemIndex]);
  if Assigned(plant_receiver) and Assigned(plant_donor) then begin
    pl := TPlant.Create(FPlantList);
    try
      pl.Donor := plant_donor;
      pl.Receiver := plant_receiver;
      spec := pl.AsSpeciesText();
    finally
      FreeAndNil(pl);
    end;
    c := 1;
    for plant in FPlantList do begin
      if plant.AsSpeciesText() = spec then begin
        c := c+1;
      end;
    end;
    Result := c;
  end;
end;

procedure Tf_AddPlant.UpdateUUID;
var
  gen: String;
  na: string;
  c: integer;
begin
  if not chk_Autogenerate.Checked then
    Exit; // -->

  gen := ed_Generation.Text;

  c := GetCrossCount;

  na := Copy(uppercase(ed_Species.Text), 1, 4);
  na := na.Replace(' ', '_');
  na := na.Replace('-', '_');
  na := na.Replace('#', '_');
  if na = '' then
    na := 'NEW';

  ed_UniqueID.Text := Format('%s-%s%d-C%d', [gen, na, FPlantList.Count, c]);
end;

procedure Tf_AddPlant.SpikesUpdated;
var
  cnt: integer;
begin
  cnt := FTempSpikes.Count;
  TheVirtualStringTree.RootNodeCount := Max(1, cnt);
  TheVirtualStringTree.Invalidate;
end;

procedure Tf_AddPlant.UpdateSpikeName;
begin
  ed_SpikeName.Text := Format('Spike%d',[FTempSpikes.Count+1]);
  SpikesUpdated
end;

procedure Tf_AddPlant.SetComboboxParents(_FilterIndex: Integer);
var
  i: integer;
  cmb_text: string;
begin
  cmb_NameFilter.ItemIndex := _FilterIndex;

  if not Assigned(FPlantList) then
    Exit; // -->


  cmb_Receiver.Clear;      
  cmb_Receiver.AddItem('Unknown', nil);
  cmb_Donor.Clear;
  cmb_Donor.AddItem('Unknown', nil);

  for i := 0 to FPlantList.Count-1 do begin
    if (FPlantList[i] <> FPlant) and (FPlantList[i].Status = psPlant) and (FPlantList[i].Spikes.Count > 0) then begin
      case _FilterIndex of
        1: cmb_text := FPlantList[i].AsSpeciesText();
        2: cmb_text := FPlantList[i].AsAccessionText();
      else
        cmb_text := FPlantList[i].AsUIDText();
      end;

      cmb_Receiver.AddItem(cmb_text, FPlantList[i]);
      cmb_Donor.AddItem(cmb_text, FPlantList[i]);
    end;
  end;

  cmb_Receiver.ItemIndex := 0;
  cmb_Donor.ItemIndex := 0;
  cmb_ReceiverSpike.ItemIndex := 0;
  if Assigned(FPlant) then begin
    if Assigned(FPlant.Receiver) then
      setParentItem(cmb_Receiver, ed_GenerationReceiver, FPlant.Receiver);
    if Assigned(FPlant.Donor) then
      setParentItem(cmb_Donor, ed_GenerationDonor, FPlant.Donor);
  end;


          
  if Assigned(FPlant) then begin     
    UpdateReceiverSpike(FPlant.Receiver);
    if Assigned(FPlant.ReceiverSpike) then begin
      for i := 0 to cmb_ReceiverSpike.Items.Count-1 do begin
        if cmb_ReceiverSpike.Items.Objects[i] = FPlant.ReceiverSpike then
          cmb_ReceiverSpike.ItemIndex := i;
      end;
    end;
  end;
end;

procedure Tf_AddPlant.InitData;
var
  plantStatus: TPlantStatus;
begin
  cmb_Receiver.Clear;
  cmb_Donor.Clear;    
  cmb_ReceiverSpike.Clear;

  cmb_Receiver.AddItem('Unknown', nil);
  cmb_ReceiverSpike.AddItem('Unknown', nil);
  cmb_Donor.AddItem('Unknown', nil);

  cmb_Receiver.ItemIndex := 0;
  cmb_Donor.ItemIndex := 0;
  cmb_ReceiverSpike.ItemIndex := 0;

  cmb_Status.Clear;
  for plantStatus in TPlantStatus do
    cmb_Status.AddItem(TPlant.PlantStatusToStr(plantStatus), TObject(Int64(plantStatus)));

  if cmb_Status.ItemIndex < 0 then
    cmb_Status.ItemIndex := 1;
end;

class function Tf_AddPlant.Execute(_Owner: TComponent; _Plant: TPlant;
  _PlantList: TPlantListDB; _FilterIndex: Integer): Boolean;
var
  f_AddPlant: Tf_AddPlant;
begin
  f_AddPlant := Tf_AddPlant.Create(_Owner);
  try
     f_AddPlant.SetData(_Plant, _PlantList, _FilterIndex);
     result := f_AddPlant.ShowModal = mrOK;
  finally
    FreeAndNil(f_AddPlant)
  end;
end;

end.

