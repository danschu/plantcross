unit u_plant;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, SQLDB, DB, Variants, types, TypInfo;


const
  SQL_PLANT_ADD = 'REPLACE INTO plants '+
                '(id, uid, species, accession, id_receiver, id_receiver_spike, id_donor, generation, number, dateofcrossing, id_status, comment, deleted) VALUES '+
                '(:id, :uid, :species, :accession, :id_receiver, :id_receiver_spike, :id_donor, :generation, :number, :dateofcrossing, :id_status, :comment, :deleted)';


  SQL_PLANT_STATUS_CREATE_TABLE = 'CREATE TABLE IF NOT EXISTS plant_status(id text primary key, name text);';
  SQL_PLANT_STATUS_ADD = 'REPLACE INTO plant_status(id, name) VALUES (:id, :name);';

  SQL_SPIKES_CREATE_TABLE = 'CREATE TABLE IF NOT EXISTS spikes ('+
                'id text primary key, '+
                'id_plant text references plant, '+
                'name text, '+
                'seed_count integer, '+
                'flower_count integer, '+
                'UNIQUE(id_plant, name)'+
                ')';

  SQL_SPIKE_ADD = 'REPLACE INTO spikes ' +
    '(id , id_plant, name, seed_count, flower_count) VALUES '+
    '(:id, :id_plant, :name, :seed_count, :flower_count)';

  SQL_SPIKES_GET = 'SELECT * FROM spikes WHERE id_plant = :id_plant ORDER BY name';

  SQL_PLANTS_CREATE_TABLE =
      'CREATE TABLE IF NOT EXISTS plants ('+
      'id text primary key, '+
      'uid text unique, ' + 
      'species text, '+
      'accession text, '+
      'id_receiver text REFERENCES plants, '+
      'id_receiver_spike text REFERENCES spikes, '+
      'id_donor text REFERENCES plants, '+
      'generation text, '+
      'number integer, '+
      'dateofcrossing datetime, '+
      'id_status text REFERENCES plant_status, '+
      'comment text, '+
      'deleted bool'+
      ')';
             
  SQL_PLANTS_GET = 'SELECT * FROM plants ORDER BY deleted, dateofcrossing';

type          
  TPlant = class;

  { TSpike }

  TSpike = class(TObject)
  private
    FGUID: String;
    FName: String;
    FSeedCount: Integer;
    FFlowerCount: Integer;
    FParent: TPlant;
    procedure SetGUID(const _GUID: String);
  public
    property GUID: String read FGUID write SetGUID;
    property Name: String read FName write FName;
    property SeedCount: integer read FSeedCount write FSeedCount;
    property FlowerCount: integer read FFlowerCount write FFlowerCount;
    constructor Create(_Parent: TPlant; const _GUID: String = ''; const _Name: String = ''; _SeedCount: Integer = 0; _FlowerCount: Integer = 0);
    function Copy(_Parent: TPlant): TSpike;
  end;      
            
  TSpikeList_ = specialize TFPGObjectList<TSpike>;

  { TSpikeList }

  TSpikeList = class(TSpikeList_)
  public
    function FindByName(const _Name: String): TSpike;
  end;


  TPlantStatus = (psUnknown, psSeed, psPlant);

  TPlantList = class;
  TPlantListDB = class;
  TPlantNotify = procedure (_Plant: TPlant) of object;

  { TPlant }

  TPlant = class(TObject)
  private
    FTopleft: TPoint;
    FGUID: String;   
    FUid: String;
    FGeneration: String;
    FSpecies: String;
    FAccession: String;
    FReceiverId: String;
    FReceiverSpikeId: String;
    FDonorId: String;
    FNumber: Integer;
    FDateOfCrossing: TDate;
    FComment: String;
    FDeleted: Boolean;
    FParent: TPlantList;
    FReceiver: TPlant;
    FDonor: TPlant;
    FGraphNode: TObject;
    FOnBeforeDestroy: TPlantNotify;
    FStatus: TPlantStatus;
    FReceiverSpike: TSpike;
    FSpikeList: TSpikeList;
    procedure SetDonor(_Donor: TPlant);
    procedure SetGUID(const _GUID: String);
    procedure SetReceiver(_Receiver: TPlant);
    procedure SetReceiverSpike(_Spike: TSpike);
  public
    class function PlantStatusToStr(_PlantStatus: TPlantStatus): string;
    class function StrToPlantStatus(const _PlantStatus: string): TPlantStatus;

    property TopLeft: TPoint read FTopleft write FTopLeft;

    function AsAccessionText(): String;
    function AsSpeciesText(_Parents: Boolean = False): String;
    function AsUIDText(): String;

    property Parent: TPlantList read FParent;
    constructor Create(_Parent: TPlantList; const _GUID: String = ''; const _UID: String = ''; const _Species: String = ''; const _Accession: String = '';
            const _ReceiverId: String = ''; const _ReceiverSpikeId: String = ''; const _DonorId: String = '';
            const _Generation: String = ''; _Number: Integer = 1; _DateOfCrossing: TDate = 0; _Status: TPlantStatus = psUnknown;
            const _Comment: String = ''; _Deleted: Boolean = False);

    destructor Destroy; override;
    property Spikes: TSpikeList read FSpikeList;
    property GUID: String read FGUID write SetGUID;
    property UID: String read FUID write FUID;               
    property Species: String read FSpecies write FSpecies;
    property Accession: String read FAccession write FAccession;
    property ReceiverId: String read FReceiverId;
    property DonorId: String read FDonorId;
    property ReceiverSpikeId: String read FReceiverSpikeId;

    property Receiver: TPlant read FReceiver write SetReceiver;
    property ReceiverSpike: TSpike read FReceiverSpike write SetReceiverSpike;
    property Donor: TPlant read FDonor write SetDonor;

    property Generation: String read FGeneration write FGeneration;
    property Number: Integer read FNumber write FNumber;
    property DateOfCrossing: TDate read FDateOfCrossing write FDateOfCrossing;
    property Status: TPlantStatus read FStatus write FStatus default psUnknown;
    property Comment: String read FComment write FComment;
    property Deleted: Boolean read FDeleted write FDeleted;
    property GraphNode: TObject read FGraphNode write FGraphNode;

    procedure UpdateFrom(_Plant: TPlant);
    property OnBeforeDestroy: TPlantNotify read FOnBeforeDestroy write FOnBeforeDestroy;
  end;

  TPlantList_ = specialize TFPGObjectList<TPlant>;

  { TPlantListDB }
                           
  TPlantList = class(TPlantList_)

  end;

  TPlantListDB = class(TPlantList)
  private
    FSQLConnection: TSQLConnection;
    FSQLTransaction: TSQLTransaction;
    //FDatasource: TDatasource;
    procedure ReadFromDatabase;
    procedure TryCreateNew;         
    function GetQuery(const _SQL: String): TSQLQuery;
    procedure ExecQuery(const _SQL: String);
    function NewID: String;
  public
    procedure Save(_Plant: TPlant; _commit: boolean); overload;
    procedure Save(_Spike: TSpike; _commit: boolean; const _ParentGUID: String); overload;
    function FindByGUID(const _GUID: String): TPlant;
    function FindByUID(const _UID: String): TPlant;
    constructor Create(_SQLConnection: TSQLConnection);
    destructor Destroy; override;
  end;

implementation

{ TSpikeList }

function TSpikeList.FindByName(const _Name: String): TSpike;
var
  Spike: TSpike;
begin
  Result := nil;
  for Spike in self do begin
    if SameText(Spike.Name, _Name) then begin
      Result := Spike;
      Exit; // -->
    end;
  end;
end;

{ TSpike }

procedure TSpike.SetGUID(const _GUID: String);
begin
  if (FGUID <> '') and (FGUID <> _GUID) then
    raise Exception.Create('TSpike.SetGUID');
  FGUID:= _GUID;
end;

constructor TSpike.Create(_Parent: TPlant; const _GUID: String;
  const _Name: String; _SeedCount: Integer; _FlowerCount: Integer = 0);
begin
  inherited Create;
  FParent := _Parent;
  FGUID := _GUID;
  FName := _Name;
  FSeedCount := _SeedCount;
  FFlowerCount := _FlowerCount;
end;

function TSpike.Copy(_Parent: TPlant): TSpike;
begin
  Result := TSpike.Create(
     _Parent, GUID, Name, SeedCount, FlowerCount
  );
end;

{ TPlantListDB }

function TPlantListDB.GetQuery(const _SQL: String): TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  //Result.DataSource := FDatasource;
  Result.DataBase := FSQLConnection;
  Result.SQL.Text := _SQL;
end;

procedure TPlantListDB.ExecQuery(const _SQL: String);
var
  qry: TSQLQuery;
begin
  qry := GetQuery(_SQL);
  try
    qry.ExecSQL;
  finally
    FreeAndNil(qry);
  end;
end;

procedure TPlantListDB.Save(_Plant: TPlant; _commit: boolean);
var
  qry: TSQLQuery;
  GUID: String;
  Spike: TSpike;
begin
 if _Plant.GUID = '' then
    GUID := NewID()
 else
    GUID := _Plant.GUID;

 qry := GetQuery(SQL_PLANT_ADD);
 try
    qry.ParamByName('id').Value := GUID;
    qry.ParamByName('uid').Value := _Plant.UID;        
    qry.ParamByName('species').Value := _Plant.Species;
    qry.ParamByName('accession').Value := _Plant.Accession;
    qry.ParamByName('id_receiver').Value := _Plant.ReceiverId;
    qry.ParamByName('id_receiver_spike').Value := _Plant.ReceiverSpikeId;
    qry.ParamByName('id_donor').Value := _Plant.DonorId;
    qry.ParamByName('generation').Value := _Plant.Generation;
    qry.ParamByName('number').Value := _Plant.Number;
    qry.ParamByName('dateofcrossing').Value := _Plant.DateOfCrossing;
    qry.ParamByName('id_status').Value := TPlant.PlantStatusToStr(_Plant.Status);
    qry.ParamByName('comment').Value := _Plant.Comment;
    qry.ParamByName('deleted').Value := _Plant.Deleted;
    qry.ExecSQL;   
    for Spike in _Plant.Spikes do begin
      Save(Spike, False, GUID);
    end;

    if _commit then
      FSQLConnection.CloseTransactions;

    _Plant.GUID := GUID;

  finally
    FreeAndNil(qry);
  end;
end;

procedure TPlantListDB.Save(_Spike: TSpike; _commit: boolean; const _ParentGUID: String);
var
  qry: TSQLQuery;
  GUID: String;
begin
 if _Spike.GUID = '' then
    GUID := NewID()
 else
    GUID := _Spike.GUID;

 qry := GetQuery(SQL_SPIKE_ADD);
 try
    qry.ParamByName('id').Value := GUID;   
    qry.ParamByName('id_plant').Value := _ParentGUID;
    qry.ParamByName('name').Value := _Spike.Name;
    qry.ParamByName('seed_count').Value := _Spike.SeedCount;
    qry.ParamByName('flower_count').Value := _Spike.FlowerCount;
    qry.ExecSQL;
    if _commit then
      FSQLConnection.CloseTransactions;       
    _Spike.GUID := GUID;
  finally
    FreeAndNil(qry);
  end;
end;

function TPlantListDB.NewID: String;
var
  gd: TGuid;
begin
  CreateGUID(gd);
  Result := GUIDToString(gd);
end;

function TPlantListDB.FindByGUID(const _GUID: String): TPlant;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count-1 do begin
    if Items[i].GUID = _GUID then begin
       Result := Items[i];
       Exit; // -->
    end;
  end;
end;

function TPlantListDB.FindByUID(const _UID: String): TPlant;
var
  i: integer;
begin
  Result := nil;
  for i := 0 to Count-1 do begin
    if Items[i].UID = _UID then begin
       Result := Items[i];
       Exit; // -->
    end;
  end;
end;

procedure TPlantListDB.ReadFromDatabase;
var
  qry: TSQLQuery;
  plant: TPlant;
  plant_p: TPlant;
  i: integer;
  GUID:  String;
  qry_spikes: TSQLQuery;
  Spike: TSpike;
begin
  qry := GetQuery(SQL_PLANTS_GET);
  try
    qry.Open;
    while not qry.EOF do begin
      plant := TPlant.Create(self,
        qry['id'],
        qry['uid'],   
        qry['species'],
        qry['accession'],
        qry['id_receiver'],
        qry['id_receiver_spike'],
        qry['id_donor'],
        qry['generation'],
        qry['number'],
        VarToDateTime(qry['dateofcrossing']),
        TPlant.StrToPlantStatus(qry['id_status']),
        qry['comment'],
        qry['deleted']
      );

      qry_spikes := GetQuery(SQL_SPIKES_GET);
      try
        qry_spikes.ParamByName('id_plant').Value := qry['id'];
        qry_spikes.Open;    
        while not qry_spikes.EOF do begin
          Spike := TSpike.Create(plant,
            qry_spikes['id'],
            qry_spikes['name'],
            qry_spikes['seed_count'],
            qry_spikes['flower_count']
            );
          plant.Spikes.Add(Spike);
          qry_spikes.Next;
        end;
      finally
        FreeAndNil(qry_spikes);
      end;

      Add(Plant);
      qry.Next;
    end;

    for i := 0 to Count-1 do begin
      plant := items[i];
      GUID := plant.DonorId;
      if GUID <> '' then begin
        plant_p := FindByGUID(GUID);
        if not Assigned(plant_p) then
           raise Exception.Create('TPlantList.ReadFromDatabase: DonorId');
        if Assigned(plant_p) then begin
           plant.Donor := plant_p;
           if plant.DonorId <> GUID then
             raise Exception.Create('TPlantList.ReadFromDatabase: DonorId');
        end;
      end;
      GUID := plant.ReceiverId;
      if GUID <> '' then begin
        plant_p := FindByGUID(GUID);
        if not Assigned(plant_p) then
           raise Exception.Create('TPlantList.ReadFromDatabase: ReceiverId');
        if Assigned(plant_p) then begin
           plant.Receiver := plant_p;
           if plant.ReceiverId <> GUID then
             raise Exception.Create('TPlantList.ReadFromDatabase: ReceiverId');
        end;
      end;
    end;
  finally
    FreeAndNil(qry);
  end;
end;

procedure TPlantListDB.TryCreateNew;
var
  plantStatus: TPlantStatus;
  qry: TSQLQuery;
begin
  FSQLTransaction.StartTransaction;
  try
    ExecQuery(SQL_PLANT_STATUS_CREATE_TABLE);
    for plantStatus in TPlantStatus do begin
      qry := GetQuery(SQL_PLANT_STATUS_ADD);
      try
        qry.ParamByName('id').Value := TPlant.PlantStatusToStr(plantStatus);
        qry.ParamByName('name').Value := TPlant.PlantStatusToStr(plantStatus);
        qry.ExecSQL;
      finally
        FreeAndNil(qry);
      end;
    end;
    ExecQuery(SQL_SPIKES_CREATE_TABLE);
    ExecQuery(SQL_PLANTS_CREATE_TABLE);
  finally
    FSQLTransaction.Commit;
  end;
end;

constructor TPlantListDB.Create(_SQLConnection: TSQLConnection);
begin
  inherited Create;
  FSQLConnection := _SQLConnection;
  FSQLTransaction := TSQLTransaction.Create(nil);
  FSQLTransaction.Action := caCommit;
  FSQLConnection.Transaction := FSQLTransaction;
  TryCreateNew;
  ReadFromDatabase;
end;

destructor TPlantListDB.Destroy;
begin
  FreeAndNil(FSQLTransaction);
  inherited Destroy;
end;

{ TPlant }

procedure TPlant.SetDonor(_Donor: TPlant);
begin
  FDonor := _Donor;
  if Assigned(_Donor) then
    FDonorId := _Donor.GUID
  else
    FDonorId := '';
end;

procedure TPlant.SetGUID(const _GUID: String);
begin
  if (FGUID <> '') and (FGUID <> _GUID) then
    raise Exception.Create('TPlant.SetGUID');
  FGUID:= _GUID;
end;

procedure TPlant.SetReceiver(_Receiver: TPlant);
begin
  FReceiver := _Receiver;
  if Assigned(_Receiver) then
    FReceiverId := _Receiver.GUID
  else
    FReceiverId := '';
end;

procedure TPlant.SetReceiverSpike(_Spike: TSpike);
begin
  FReceiverSpike := _Spike;
  if Assigned(_Spike) then
    FReceiverSpikeId := _Spike.GUID
  else
    FReceiverSpikeId := '';
end;

class function TPlant.PlantStatusToStr(_PlantStatus: TPlantStatus): string;
begin
  Result := Copy(GetEnumName(typeInfo(TPlantStatus), Ord(_PlantStatus)), 3);
end;

class function TPlant.StrToPlantStatus(const _PlantStatus: string): TPlantStatus;
begin
  Result := TPlantStatus(GetEnumValue(Typeinfo(TPlantStatus), 'ps'+_PlantStatus));
end;

function TPlant.AsSpeciesText(_Parents: Boolean): String;
var
  AccR: String;
  DonorR: String;
begin
  Result := Species;
  if (Result <> '') and (not _Parents) then
    Exit; // -->

  if Assigned(FReceiver) then begin
    AccR := FReceiver.AsSpeciesText;
    if AccR = '' then
      AccR := '???';
  end else
    AccR := '???';

  if Assigned(FDonor) then begin
    DonorR := FDonor.AsSpeciesText;
    if DonorR = '' then
      DonorR := '???';
  end else
    DonorR := '???';

  Result := Format('(%s*%s)', [Accr, DonorR]);
end;

function TPlant.AsUIDText: String;
var
  AccR: String;
  DonorR: String;
begin
  Result := UID;
  if Result <> '' then
    Exit; // -->

  if Assigned(FReceiver) then begin
    AccR := FReceiver.AsUIDText;
    if AccR = '' then
      AccR := '???';
  end else
    AccR := '???';

  if Assigned(FDonor) then begin
    DonorR := FDonor.AsUIDText;
    if DonorR = '' then
      DonorR := '???';
  end else
    DonorR := '???';

  Result := Format('(%s*%s)', [Accr, DonorR]);
end;

function TPlant.AsAccessionText: String;
var
  AccR: String;
  DonorR: String;
begin
  Result := Accession;
  if Result <> '' then
    Exit; // -->

  if Assigned(FReceiver) then begin
    AccR := FReceiver.AsAccessionText;
    if AccR = '' then
      AccR := '???';
  end else
    AccR := '???';
                        
  if Assigned(FDonor) then begin
    DonorR := FDonor.AsAccessionText;
    if DonorR = '' then
      DonorR := '???';
  end else
    DonorR := '???';

  Result := Format('(%s*%s)', [Accr, DonorR]);
end;


constructor TPlant.Create(_Parent: TPlantList; const _GUID: String;
  const _UID: String; const _Species: String;  const _Accession: String; const _ReceiverId: String;
  const _ReceiverSpikeId: String; const _DonorId: String; const _Generation: String;
  _Number: Integer; _DateOfCrossing: TDate; _Status: TPlantStatus;
  const _Comment: String; _Deleted: Boolean);
begin
  inherited Create();
  FSpikeList := TSpikeList.Create();
  FParent := _Parent;
  FGUID := _GUID;
  FUID := _UID;
  FSpecies := _Species;
  FAccession := _Accession;
  FReceiverId := _ReceiverId;
  FReceiverSpikeId := _ReceiverSpikeId;
  FDonorId := _DonorId;
  FGeneration := _Generation;
  FNumber := _Number;
  FDateOfCrossing := _DateOfCrossing;
  FStatus := _Status;
  FComment := _Comment;
  FDeleted := _Deleted;
end;

destructor TPlant.Destroy;
begin
  if Assigned(FOnBeforeDestroy) then
    FOnBeforeDestroy(self);
  FreeAndNil(FSpikeList);
  inherited Destroy;
end;

procedure TPlant.UpdateFrom(_Plant: TPlant);
var
  Spike: TSpike;
begin
  FParent := _Plant.Parent;
  FGUID := _Plant.GUID;
  FUID := _Plant.UID;        
  FSpecies := _Plant.Species;
  FAccession := _Plant.Accession;
  Receiver := _Plant.Receiver;
  ReceiverSpike := _Plant.ReceiverSpike;
  Donor := _Plant.Donor;
  FGeneration := _Plant.Generation;
  FNumber := _Plant.Number;
  FDateOfCrossing := _Plant.DateOfCrossing;
  FComment := _Plant.Comment;
  FStatus := _Plant.Status;
  FDeleted := _Plant.Deleted;
  FTopLeft := _Plant.TopLeft;

  FreeAndNil(FSpikeList);
  FSpikeList := TSpikeList.Create;

  for Spike in _Plant.Spikes do
     FSpikeList.Add(Spike.Copy(self));
end;

end.

