unit w_plant_cross;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SQLite3Conn, SQLDB, DB, BufDataset, Forms, Controls,
  Graphics, Dialogs, Menus, DBGrids, ComCtrls, ExtCtrls, StdCtrls, ActnList,
  VirtualTrees, w_plant_edit, u_plant, math, uSimpleGraph;

type

  { Tf_PlantCrossing }

  Tf_PlantCrossing = class(TForm)
    act_PlantAdd: TAction;
    act_Close: TAction;
    mi_PlantAdd: TMenuItem;
    mi_Plant: TMenuItem;
    mi_Close: TMenuItem;
    mi_File: TMenuItem;
    p_Graph: TPanel;
    p_Client: TPanel;
    TheSplitterGraph: TSplitter;
    TheActionList: TActionList;
    b_AddNewPlant: TButton;
    p_Left: TPanel;
    TheStatusbar: TStatusBar;
    TheDatabase: TSQLite3Connection;
    TheMenu: TMainMenu;
    TheVirtualStringTree: TVirtualStringTree;
    procedure act_PlantAddExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure TheVirtualStringTreeDblClick(Sender: TObject);
    procedure TheVirtualStringTreeDrawText(Sender: TBaseVirtualTree;
      TargetCanvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex;
      const CellText: String; const CellRect: TRect; var DefaultDraw: Boolean);
    procedure TheVirtualStringTreeGetText(Sender: TBaseVirtualTree;
      Node: PVirtualNode; Column: TColumnIndex; TextType: TVSTTextType;
      var CellText: String);
  private
    FPlantList: TPlantListDB;
    FGraph: TEvsSimpleGraph;
    procedure PlantsUpdated(_Autolayout: Boolean = False);
    procedure OnPlantDestroy(_Plant: TPlant);
  public

  end;


implementation

{$R *.lfm}

{ Tf_PlantCrossing }

type
  TPlantNode = class(TEvsRoundRectangularNode)
  private
    FPlant: TPlant;
    procedure SetPlant(_Plant: TPlant);
  public
    property Plant: TPlant read FPlant write SetPlant;
  end;



const
  COLUMN_NAME = 0;
  COLUMN_ID = 1;
  COLUMN_ACCESSION = 2;
  COLUMN_DATE_OF_CROSSING = 3;
  COLUMN_RECEIVER = 4;
  COLUMN_DONOR = 5;
  COLUMN_GENERATION = 6;
  COLUMN_SUCCESS = 7;
  COLUMN_NUMBER = 8;

procedure TPlantNode.SetPlant(_Plant: TPlant);
begin
  FPlant := _Plant;
  Text := FPlant.AsText;
end;

procedure Tf_PlantCrossing.FormCreate(Sender: TObject);
begin
  TheDatabase.Connected := True;

  FGraph := TEvsSimpleGraph.Create(p_Graph);
  FGraph.Parent := p_Graph;
  FGraph.Align := alClient;

  FPlantList := TPlantListDB.Create(TheDatabase);
  PlantsUpdated;
end;

procedure Tf_PlantCrossing.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FPlantList);
end;

procedure Tf_PlantCrossing.act_PlantAddExecute(Sender: TObject);
begin
  if Tf_AddPlant.Execute(self, nil, FPlantList) then
     PlantsUpdated;
end;

procedure Tf_PlantCrossing.TheVirtualStringTreeDblClick(Sender: TObject);
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
  if idx >= FPlantList.Count then
    Exit; // -->
  
  if Tf_AddPlant.Execute(self, FPlantList[idx], FPlantList) then
    PlantsUpdated;
end;

procedure Tf_PlantCrossing.TheVirtualStringTreeDrawText(
  Sender: TBaseVirtualTree; TargetCanvas: TCanvas; Node: PVirtualNode;
  Column: TColumnIndex; const CellText: String; const CellRect: TRect;
  var DefaultDraw: Boolean);
var
  idx: integer;
  plant: TPlant;
begin
  idx := Node^.Index;
  if idx >= FPlantList.Count then
    Exit; // -->
         
  plant := FPlantList[idx];
  if plant.Deleted then begin
    TargetCanvas.Font.Color := clSilver;
    TargetCanvas.Font.Style := [fsItalic];
  end else begin
    TargetCanvas.Font.Color := clBlack;
    TargetCanvas.Font.Style := [];
  end;
end;

procedure Tf_PlantCrossing.TheVirtualStringTreeGetText(
  Sender: TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex;
  TextType: TVSTTextType; var CellText: String);
var
  idx: integer;
  plant: TPlant;
begin
  idx := Node^.Index;
  if idx >= FPlantList.Count then
    Exit; // -->

  plant := FPlantList[idx];

  case Column of
     COLUMN_NAME: CellText := plant.Name;
     COLUMN_ID: CellText := plant.UID;
     COLUMN_ACCESSION: CellText := plant.Accession;
     COLUMN_DATE_OF_CROSSING: CellText := DateToStr(plant.DateOfCrossing);
     COLUMN_RECEIVER: if Assigned(plant.Receiver) then
         CellText := plant.Receiver.AsText();
     COLUMN_DONOR: if Assigned(plant.Donor) then
         CellText := plant.Donor.AsText();
     COLUMN_GENERATION: CellText := plant.Generation;
     COLUMN_SUCCESS: if plant.Success then
         CellText := 'X'
       else       
         CellText := '';
     COLUMN_NUMBER: CellText := IntToStr(plant.Number);
  end;
end;

procedure Tf_PlantCrossing.PlantsUpdated(_Autolayout: Boolean);
var
  Plant: TPlant;
  PlantNode, PlantNodeTo: TPlantNode;
  Link: TEvsGraphLink;

  TmpPlantList: TPlantList;
  TmpPlantListCurr: TPlantList;
  TmpPlantListNext: TPlantList;
  idx: integer;
  i: integer;
begin
  TheVirtualStringTree.RootNodeCount := Max(1, FPlantList.Count);
  TheVirtualStringTree.Invalidate;
                
  for Plant in FPlantList do begin
    if Assigned(Plant.GraphNode) then begin
      PlantNode := Plant.GraphNode as TPlantNode;
      Plant.TopLeft := Point(PlantNode.Left, PlantNode.Top)
    end;
    Plant.GraphNode := nil;
  end;
  FGraph.Clear;

  for Plant in FPlantList do begin
    if not Assigned(Plant.GraphNode) then begin
      PlantNode := FGraph.InsertNode(Rect(0, 0, 100, 100), TPlantNode) as TPlantNode;
      Plant.OnBeforeDestroy := @OnPlantDestroy;
      PlantNode.Plant := Plant;
      Plant.GraphNode := PlantNode;
    end;
  end;   
  for Plant in FPlantList do begin
    if Assigned(Plant.Donor) then begin
      Link := FGraph.InsertLink(Plant.Donor.GraphNode as TPlantNode, Plant.GraphNode as TPlantNode, TEVSBezierLink);
      Link.Text := 'Donor';
      Link.Pen.Color := clSilver;
      Link.Pen.Width := 2;
      Link.Pen.Style := psSolid;         
      Link.EndStyle := lsArrow;
    end;
    if Assigned(Plant.Receiver) then begin
      Link := FGraph.InsertLink(Plant.Receiver.GraphNode as TPlantNode, Plant.GraphNode as TPlantNode, TEVSBezierLink);
      FGraph.Canvas.Pen.Color := clBlack;
      FGraph.Canvas.Pen.Style := psSolid;
      Link.Text := 'Receiver';  
      Link.Pen.Color := clSilver;
      Link.Pen.Width := 2;     
      Link.Pen.Style := psSolid;
      Link.EndStyle := lsArrow;
    end;
  end;

  TmpPlantList := nil;
  TmpPlantListCurr := nil;
  TmpPlantListNext := nil;
  idx := 1;
  try
    TmpPlantList := TPlantList.Create(False);
    TmpPlantListCurr := TPlantList.Create(False);
    TmpPlantListNext := TPlantList.Create(False);

    for Plant in FPlantList do begin
      if not Assigned(Plant.Donor) and not Assigned(Plant.Receiver) then begin
        TmpPlantListNext.Add(Plant);
        TmpPlantList.Add(Plant);
        PlantNode := Plant.GraphNode as TPlantNode;
        if _Autolayout or Plant.TopLeft.IsZero then begin
          PlantNode.Left := (PlantNode.Width+10)*TmpPlantListNext.Count;
          PlantNode.Top := 10*idx;
        end else begin
          PlantNode.Top := PlantNode.Plant.TopLeft.Y;
          PlantNode.Left := PlantNode.Plant.TopLeft.X;
        end;
      end;
    end;

    while TmpPlantListNext.Count > 0 do begin  
      idx := idx+1;
      while TmpPlantListNext.Count > 0 do
        TmpPlantListCurr.Add(TmpPlantListNext.Extract(TmpPlantListNext.Items[0]));

      while TmpPlantListCurr.Count > 0 do begin
        Plant := TmpPlantListCurr.Extract(TmpPlantListCurr.Items[0]);
        PlantNode := (Plant.GraphNode as TPlantNode);
        for i := 0 to PlantNode.LinkOutputCount-1 do begin
          PlantNodeTo := (PlantNode.LinkOutputs[i].Target as TPlantNode);
          if TmpPlantList.IndexOf(PlantNodeTo.Plant) = -1 then begin
            TmpPlantList.Add(PlantNodeTo.Plant);
            TmpPlantListNext.Add(PlantNodeTo.Plant);  
            if _Autolayout or Plant.TopLeft.IsZero then begin
              PlantNodeTo.Top := (PlantNodeTo.Height+10)*idx;
              PlantNodeTo.Left := TmpPlantListNext.Count*(PlantNodeTo.Width+10);
            end else begin
              PlantNodeTo.Top := PlantNodeTo.Plant.TopLeft.Y;
              PlantNodeTo.Left := PlantNodeTo.Plant.TopLeft.X;
            end;
          end;

       end;
     end;
   end;

  finally
    FreeAndNil(TmpPlantList);
    FreeAndNil(TmpPlantListCurr);
    FreeAndNil(TmpPlantListNext);
  end;
end;

procedure Tf_PlantCrossing.OnPlantDestroy(_Plant: TPlant);
begin
  if Assigned(_Plant.GraphNode) then begin
    _Plant.GraphNode.Free;
  end;
end;


end.

