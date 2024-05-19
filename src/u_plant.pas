unit u_plant;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, fgl, SQLDB, DB, Variants;


const
  SQL_PLANT_ADD = 'INSERT INTO plants '+
                '(id, name, accession, id_receiver, id_donor, generation, number, dateofcrossing, success, uid, comment, deleted) VALUES '+
                '(:id, :name, :accession, :id_receiver, :id_donor, :generation, :number, :dateofcrossing, :success, :uid, :comment, :deleted)';

  SQL_PLANT_UPDATE = 'UPDATE plants SET '+
                'name = :name, '+
                'accession = :accession, '+
                'id_receiver = :id_receiver, '+
                'id_donor = :id_donor, '+
                'generation = :generation, '+
                'number = :number, '+
                'dateofcrossing = :dateofcrossing, '+
                'success = :success, '+
                'uid = :uid, '+
                'comment = :comment,'+
                'deleted = :deleted WHERE id = :id';

  SQL_PLANTS_CREATE_TABLE =
      'CREATE TABLE IF NOT EXISTS plants ('+
      'id text primary key, '+
      'name text, ' +
      'accession text, '+
      'id_receiver text, '+
      'id_donor text, '+
      'generation text, '+
      'number integer, '+
      'dateofcrossing datetime, '+
      'success bool, '+
      'uid text unique,'+
      'comment text, '+
      'deleted bool'+
      ')';
             
  SQL_PLANTS_GET = 'SELECT * FROM plants ORDER BY deleted';

type
  TPlantList = class;
  TPlantListDB = class;
  TPlant = class;
  TPlantNotify = procedure (_Plant: TPlant) of object;

  { TPlant }

  TPlant = class(TObject)
  private
    FTopleft: TPoint;
    FGUID: String;
    FName: String;
    FGeneration: String;
    FAccession: String;
    FReceiverId: String;
    FDonorId: String;
    FNumber: Integer;
    FDateOfCrossing: TDate;
    FSuccess: Boolean;
    FUid: String;
    FComment: String;
    FDeleted: Boolean;
    FParent: TPlantList;
    FReceiver: TPlant;
    FDonor: TPlant;
    FGraphNode: TObject;
    FOnBeforeDestroy: TPlantNotify;
    procedure SetDonor(_Donor: TPlant);
    procedure SetGUID(const _GUID: String);
    procedure SetReceiver(_Receiver: TPlant);
  public
    property TopLeft: TPoint read FTopleft write FTopLeft;

    function AsText(): String;
    function AsUID(): String; overload;
    class function AsUID(const _Text: String): String; overload;

    property Parent: TPlantList read FParent;
    constructor Create(_Parent: TPlantList; const _GUID: String = ''; const _UID: String = ''; const _Name: String = ''; const _Accession: String = '';
            const _ReceiverId: String = ''; const _DonorId: String = '';
            const _Generation: String = ''; _Number: Integer = 1; _DateOfCrossing: TDate = 0; _Success: Boolean = False; const _Comment: String = ''; _Deleted: Boolean = False);

    destructor Destroy; override;
    property GUID: String read FGUID write SetGUID;
    property Name: String read FName write FName;
    property Accession: String read FAccession write FAccession;
    property ReceiverId: String read FReceiverId;
    property DonorId: String read FDonorId;

    property Receiver: TPlant read FReceiver write SetReceiver;
    property Donor: TPlant read FDonor write SetDonor;

    property Generation: String read FGeneration write FGeneration;
    property Number: Integer read FNumber write FNumber;
    property DateOfCrossing: TDate read FDateOfCrossing write FDateOfCrossing;
    property Success: boolean read FSuccess write FSuccess;
    property UID: String read FUID write FUID;
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
    function NewID: String;
  public                        
    procedure Save(_Plant: TPlant);
    function FindByGUID(const _GUID: String): TPlant;
    function FindByUID(const _UID: String): TPlant;
    constructor Create(_SQLConnection: TSQLConnection);
    destructor Destroy; override;
  end;

implementation

{ TPlantListDB }

function TPlantListDB.GetQuery(const _SQL: String): TSQLQuery;
begin
  Result := TSQLQuery.Create(nil);
  //Result.DataSource := FDatasource;
  Result.DataBase := FSQLConnection;
  Result.SQL.Text := _SQL;
end;

procedure TPlantListDB.Save(_Plant: TPlant);
var
  qry: TSQLQuery;
  GUID: String;
begin
 if _Plant.GUID = '' then begin
    GUID := NewID();
    qry := GetQuery(SQL_PLANT_ADD);
 end else begin
    GUID := _Plant.GUID;
    qry := GetQuery(SQL_PLANT_UPDATE);
 end;
 try
    qry.ParamByName('id').Value := GUID;
    qry.ParamByName('uid').Value := _Plant.UID;
    qry.ParamByName('name').Value := _Plant.Name;
    qry.ParamByName('accession').Value := _Plant.Accession;
    qry.ParamByName('id_receiver').Value := _Plant.ReceiverId;
    qry.ParamByName('id_donor').Value := _Plant.DonorId;
    qry.ParamByName('generation').Value := _Plant.Generation;
    qry.ParamByName('number').Value := _Plant.Number;
    qry.ParamByName('dateofcrossing').Value := _Plant.DateOfCrossing;
    qry.ParamByName('success').Value := _Plant.Success;
    qry.ParamByName('comment').Value := _Plant.Comment;
    qry.ParamByName('deleted').Value := _Plant.Deleted;
    qry.ExecSQL;
    _Plant.GUID := GUID;
    FSQLConnection.CloseTransactions;
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
begin
  qry := GetQuery(SQL_PLANTS_GET);
  try
    qry.Open;
    while not qry.EOF do begin
      plant := TPlant.Create(self,
        qry['id'],
        qry['uid'],
        qry['name'],
        qry['accession'],
        qry['id_receiver'],
        qry['id_donor'],
        qry['generation'],
        qry['number'],
        VarToDateTime(qry['dateofcrossing']),
        qry['success'],
        qry['comment'],
        qry['deleted']
      );
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
  qry: TSQLQuery;
begin
  qry := GetQuery(SQL_PLANTS_CREATE_TABLE);
  try
    qry.ExecSQL;
  finally
    FreeAndNil(qry);
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

function TPlant.AsText: String;
begin
  Result := '';
  if FUid <> '' then begin
    Result := FUid;
    Exit; // -->
  end;

  if FName <> '' then
    Result := FName;
  if FAccession <> '' then begin
    if Result = '' then
      Result := FAccession
    else
      Result := Result + Format('(%s)', [FAccession]);
  end;


  if (FGUID <> '') and (Result = '') then
    Result := FGUID;
end;

function TPlant.AsUID: String;
begin
  Result := AsUID(AsText);
end;

class function TPlant.AsUID(const _Text: String): String;
begin
  Result :=
    StringReplace(
      StringReplace(
        UpperCase(_Text)
    ,' ', '_', [rfReplaceAll])
    ,'.', '', [rfReplaceAll]);
end;

constructor TPlant.Create(_Parent: TPlantList; const _GUID: String;
  const _UID: String; const _Name: String; const _Accession: String;
  const _ReceiverId: String; const _DonorId: String; const _Generation: String;
  _Number: Integer; _DateOfCrossing: TDate; _Success: Boolean;
  const _Comment: String; _Deleted: Boolean);
begin
  inherited Create();
  FParent := _Parent;
  FGUID := _GUID;
  FUID := _UID;
  FName := _Name;
  FAccession := _Accession;
  FReceiverId := _ReceiverId;
  FDonorId := _DonorId;
  FGeneration := _Generation;
  FNumber := _Number;
  FDateOfCrossing := _DateOfCrossing;
  FComment := _Comment;
  FSuccess := _Success;
  FDeleted := _Deleted;
end;

destructor TPlant.Destroy;
begin
  if Assigned(FOnBeforeDestroy) then
    FOnBeforeDestroy(self);
  inherited Destroy;
end;

procedure TPlant.UpdateFrom(_Plant: TPlant);
begin
  FParent := _Plant.Parent;
  FGUID := _Plant.GUID;
  FUID := _Plant.UID;
  FName := _Plant.Name;
  FAccession := _Plant.Accession;
  Receiver := _Plant.Receiver;
  Donor := _Plant.Donor;
  FGeneration := _Plant.Generation;
  FNumber := _Plant.Number;
  FDateOfCrossing := _Plant.DateOfCrossing;
  FComment := _Plant.Comment;
  FSuccess := _Plant.Success;
  FDeleted := _Plant.Deleted;
  FTopLeft := _Plant.TopLeft;
end;

end.

