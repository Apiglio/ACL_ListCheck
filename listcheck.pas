unit ListCheck;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  TListCheckError = class(Exception)

  end;

  TListCheckNode = class
  private
    FName:string;
    FDispName:string;
    FUnfold:boolean;
    FChecked:boolean;
    FItems:TList;
    FData:TObject;
    FLevel:integer;
    FOrder:integer;
  protected
    function GetItem(index:integer):TListCheckNode;
    function GetItemByName(index:string):TListCheckNode;
    function GetItemIndexByName(index:string):integer;
    function GetItemCount:integer;
    function GetDispName:string;
  public
    property Items[index:integer]:TListCheckNode read GetItem;
    property ItemsByName[index:string]:TListCheckNode read GetItemByName;
    property ItemCount:integer read GetItemCount;
    property Checked:boolean read FChecked write FChecked;
    property Unfold:boolean read FUnfold write FUnfold;
    property DisplayName:string read GetDispName write FDispName;
    property Data:TObject read FData write FData;
  public
    function AddItem(AName:string;AData:TObject=nil):TListCheckNode;
    function DeleteItem(AName:string):boolean;
    function RenameItem(OldName,NewName:string):boolean;
    procedure Clear;
  public
    procedure CheckAllSubordinates;
    procedure UnCheckAllSubordinates;
    procedure FoldAllSubordinates;
    procedure UnFoldAllSubordinates;
    function UpdateOrder(initial_order:integer):integer;//从initial_order开始给自己和子项编号，保存在FOrder，最后返回所有各级子项中最后一个编号+1
    procedure SaveToList(var AList:TList;ignore_fold_items:boolean=true);
  public
    constructor Create;
    destructor Destroy; override;
  end;

  TListCheckRegion = record
    Item:TListCheckNode;
    IconRect:TRect;
    CapsRect:TRect;
  end;
  TListCheckIconState = (lconUnfold, lconFold, lconChecked, lconUnChecked);
  TListCheckItemCheckedEvent = procedure(Sender:TObject;Item:TListCheckNode) of object;

  TCustomListCheck = class(TCustomPanel)
  private
    FRoot:TListCheckNode;
    FRegions:array of TListCheckRegion;
    FFirstOrder:Integer;//第一个显示的项目的Order
  private
    FOnItemChecked:TListCheckItemCheckedEvent;
  public
    FItemHeight:Integer;
    FItemGap:Integer;
    FItemFont:TFont;
    FItemColor:TColor;
    property ItemFont:TFont read FItemFont write FItemFont;
    property ItemHeight:Integer read FItemHeight write FItemHeight;
    property ItemGap:Integer read FItemGap write FItemGap;
    property ItemColor:TColor read FItemColor write FItemColor;
  public
    property OnItemChecked:TListCheckItemCheckedEvent read FOnItemChecked write FOnItemChecked;
  private
    procedure UpdateRegion;
    procedure Paint; override;
    procedure MouseUp(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
    procedure MouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  protected

  public
    property Root:TListCheckNode read FRoot;

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TListCheck = class(TCustomListCheck)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BorderSpacing;
    property BevelColor;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BidiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property ChildSizing;
    property ClientHeight;
    property ClientWidth;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property ItemColor;
    property ItemFont;
    property ItemGap;
    property ItemHeight;
    property ParentBidiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property UseDockManager default True;
    property Visible;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnGetDockCaption;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnPaint;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

var ItemCaptionStyle:TTextStyle;

procedure Register;

implementation

procedure Register;
begin
  {$I listcheck_icon.lrs}
  RegisterComponents('Apiglio Component',[TListCheck]);
end;

{ TListCheckNode }

function TListCheckNode.GetItem(index:integer):TListCheckNode;
begin
  if index>=FItems.Count then raise TListCheckError.Create('FItems下标超界。');
  if index<-FItems.Count then raise TListCheckError.Create('FItems下标超界。');
  if index>=0 then
    result:=TListCheckNode(FItems[index])
  else
    result:=TListCheckNode(FItems[FItems.Count+index]);
end;

function TListCheckNode.GetItemByName(index:string):TListCheckNode;
var pi:integer;
    tmp:TListCheckNode;
begin
  pi:=0;
  result:=nil;
  while pi<FItems.Count do begin
    tmp:=TListCheckNode(FItems[pi]);
    if tmp.FName=index then begin
      result:=tmp;
      exit;
    end;
    inc(pi);
  end;
end;

function TListCheckNode.GetItemIndexByName(index:string):integer;
var pi:integer;
    tmp:TListCheckNode;
begin
  pi:=0;
  result:=-1;
  while pi<FItems.Count do begin
    tmp:=TListCheckNode(FItems[pi]);
    if tmp.FName=index then begin
      result:=pi;
      exit;
    end;
    inc(pi);
  end;
end;

function TListCheckNode.GetItemCount:integer;
begin
  result:=FItems.Count;
end;

function TListCheckNode.GetDispName:string;
begin
  if FDispName<>'' then result:=FDispName
  else result:=FName;
end;

function TListCheckNode.AddItem(AName:string;AData:TObject=nil):TListCheckNode;
var tmp:TListCheckNode;
begin
  tmp:=GetItemByName(AName);
  if tmp=nil then begin
    tmp:=TListCheckNode.Create;
    tmp.FLevel:=FLevel+1;
    tmp.FName:=AName;
    tmp.FData:=AData;
    FItems.Add(tmp);
  end;
  result:=tmp;
end;

function TListCheckNode.DeleteItem(AName:string):boolean;
var pi:integer;
    tmp:TListCheckNode;
begin
  pi:=GetItemIndexByName(AName);
  tmp:=TListCheckNode(FItems[pi]);
  result:=false;
  if tmp=nil then exit;
  FItems.Delete(pi);
  tmp.Free;
  result:=true;
end;

function TListCheckNode.RenameItem(OldName,NewName:string):boolean;
var tmp1,tmp2:TListCheckNode;
begin
  result:=false;
  tmp1:=GetItemByName(OldName);
  tmp2:=GetItemByName(NewName);
  if (tmp1=nil) or (tmp2<>nil) then exit;
  tmp1.FName:=NewName;
  result:=true;
end;

procedure TListCheckNode.Clear;
var tmp:TListCheckNode;
begin
  while FItems.Count>0 do begin
    tmp:=TListCheckNode(FItems[0]);
    tmp.Clear;
    tmp.Free;
    FItems.Delete(0);
  end;
end;

procedure TListCheckNode.CheckAllSubordinates;
var pi:integer;
begin
  FChecked:=true;
  pi:=0;
  while pi<FItems.Count do begin
    TListCheckNode(FItems[pi]).CheckAllSubordinates;
    inc(pi);
  end;
end;

procedure TListCheckNode.UnCheckAllSubordinates;
var pi:integer;
begin
  FChecked:=false;
  pi:=0;
  while pi<FItems.Count do begin
    TListCheckNode(FItems[pi]).UnCheckAllSubordinates;
    inc(pi);
  end;
end;

procedure TListCheckNode.FoldAllSubordinates;
var pi:integer;
begin
  FUnfold:=false;
  pi:=0;
  while pi<FItems.Count do begin
    TListCheckNode(FItems[pi]).FoldAllSubordinates;
    inc(pi);
  end;
end;

procedure TListCheckNode.UnFoldAllSubordinates;
var pi:integer;
begin
  FUnfold:=true;
  pi:=0;
  while pi<FItems.Count do begin
    TListCheckNode(FItems[pi]).UnFoldAllSubordinates;
    inc(pi);
  end;
end;

function TListCheckNode.UpdateOrder(initial_order:integer):integer;
var pi:integer;
    tmp:TListCheckNode;
begin
  FOrder:=initial_order;
  result:=initial_order+1;
  pi:=0;
  if not FUnfold then exit;
  while pi<FItems.Count do begin
    tmp:=TListCheckNode(FItems[pi]);
    result:=tmp.UpdateOrder(result);
    inc(pi);
  end;
end;

procedure TListCheckNode.SaveToList(var AList:TList;ignore_fold_items:boolean=true);
var pi:integer;
begin
  Alist.Add(Self);
  if (not ignore_fold_items) or FUnfold then begin
    pi:=0;
    while pi<FItems.Count do begin
      TListCheckNode(FItems[pi]).SaveToList(AList,ignore_fold_items);
      inc(pi);
    end;
  end;
end;

constructor TListCheckNode.Create;
begin
  inherited Create;
  FItems:=TList.Create;
  FLevel:=0;
end;

destructor TListCheckNode.Destroy;
begin
  Clear;
  FItems.Free;
  inherited Destroy;
end;



{ TCustomListCheck }

procedure draw_icon(canvas:TCanvas;arect:TRect;icon_state:TListCheckIconState);
var new_rect:TRect;
    ymid,xmid,gap:integer;
begin
  gap:=arect.Height div 5;
  canvas.Brush.Style:=bsClear;
  canvas.Brush.Color:=clWhite;
  canvas.Pen.Color:=clBlack;
  canvas.Pen.Width:=1;
  new_rect:=Rect(arect.Left+gap,arect.Top+gap,arect.Right-gap,arect.Bottom-gap);
  xmid:=arect.Left + (arect.Right-arect.Left) div 2;
  ymid:=arect.Top + (arect.Bottom-arect.Top) div 2;
  case icon_state of
    lconUnfold:begin
      canvas.Rectangle(new_rect);
      canvas.Line(arect.Left+gap,ymid,arect.Right-gap,ymid);
    end;
    lconFold:begin
      canvas.Rectangle(new_rect);
      canvas.Line(arect.Left+gap,ymid,arect.Right-gap,ymid);
      canvas.Line(xmid,arect.Top+gap,xmid,arect.Bottom-gap);
    end;
    lconChecked:begin
      canvas.Ellipse(new_rect);
      canvas.Brush.Style:=bsSolid;
      canvas.Brush.Color:=clBlack;
      new_rect:=Rect(arect.Left+2*gap,arect.Top+2*gap,arect.Right-2*gap,arect.Bottom-2*gap);
      canvas.Ellipse(new_rect);
    end;
    lconUnChecked:begin
      canvas.Ellipse(new_rect);
    end;
  end;

end;

procedure TCustomListCheck.UpdateRegion;
var tmpList:TList;
    node:TListCheckNode;
    pi,len:integer;
    x,y:integer;
begin
  FRoot.UpdateOrder(0);
  try
    tmpList:=TList.Create;
    FRoot.SaveToList(tmpList);
    len:=tmpList.Count;
    SetLength(FRegions,0);
    SetLength(FRegions,len-1);
    for pi:=1 to len-1 do begin
      node:=TListCheckNode(tmpList[pi]);
      FRegions[pi-1].Item:=node;
      x:=(node.FLevel-1)*ItemHeight;
      y:=(node.FOrder-FFirstOrder-1)*ItemHeight;
      FRegions[pi-1].IconRect:=Rect(x,y,x+ItemHeight,y+ItemHeight);
      FRegions[pi-1].CapsRect:=Rect(x+ItemHeight,y,Width-ItemGap,y+ItemHeight);
    end;
  finally
    tmpList.Free;
  end;
end;

procedure TCustomListCheck.Paint;
var len,pi:integer;
    tmp:TListCheckNode;
    rec,rec_ofs:TRect;
begin
  Canvas.Brush.Color:=Color;
  Canvas.Brush.Style:=bsSolid;
  Canvas.Pen.Color:=clBlack;
  Canvas.Font.Assign(ItemFont);
  Canvas.Font.Height:=ItemHeight-2*ItemGap;
  Canvas.Clear;
  UpdateRegion;
  len:=Length(FRegions);
  for pi:=0 to len-1 do begin
    tmp:=FRegions[pi].Item;
    if tmp.ItemCount=0 then begin
      if tmp.Checked then begin
        draw_icon(Canvas,FRegions[pi].IconRect,lconChecked)
      end else begin
        draw_icon(Canvas,FRegions[pi].IconRect,lconUnChecked)
      end;
    end else begin
      if tmp.Unfold then begin
        draw_icon(Canvas,FRegions[pi].IconRect,lconUnfold);
      end else begin
        draw_icon(Canvas,FRegions[pi].IconRect,lconFold);
      end;
    end;
    rec:=FRegions[pi].CapsRect;
    rec_ofs:=Rect(rec.Left+ItemGap,rec.Top+ItemGap,rec.Right-ItemGap,rec.Bottom-ItemGap);
    Canvas.Brush.Color:=ItemColor;
    Canvas.Pen.Color:=ItemColor;
    Canvas.Rectangle(rec_ofs);
    Canvas.TextRect(rec_ofs,rec_ofs.Left+2,rec_ofs.Top,tmp.DisplayName,ItemCaptionStyle);
  end;
end;

procedure TCustomListCheck.MouseUp(Sender:TObject;Button:TMouseButton;Shift:TShiftState;X,Y:Integer);
var pi,len:integer;
    icon_rect:TRect;
    item:TListCheckNode;
begin
  len:=Length(FRegions);
  for pi:=0 to len-1 do begin
    icon_rect:=FRegions[pi].IconRect;
    if Y<=icon_rect.Top then continue;
    if Y>=icon_rect.Bottom then continue;
    if X<=icon_rect.Left then continue;
    if X>=icon_rect.Right then continue;
    item:=FRegions[pi].Item;
    if item.ItemCount=0 then item.Checked:=not item.Checked
    else item.Unfold:=not item.Unfold;
    if assigned(FOnItemChecked) then FOnItemChecked(Sender,item);
    break;
  end;
  Paint;
end;

procedure TCustomListCheck.MouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
var tmpFO:integer;
begin
  tmpFO:=FFirstOrder;
  if WheelDelta>0 then dec(tmpFO) else inc(tmpFO);
  if (tmpFO>=0) and (tmpFO<1+Length(FRegions)-Height div ItemHeight) then begin
    FFirstOrder:=tmpFO;
    Paint;
  end;
end;

procedure set_dsgn_time_data(ARoot:TListCheckNode);
begin
  with ARoot.AddItem('Item 1') do begin
    FUnfold:=false;
    AddItem('SubItem 1.1').Checked:=true;
    AddItem('SubItem 1.2').DisplayName:='SubItem 1.2 (Disp.)';
    AddItem('SubItem 1.3').Checked:=true;
  end;
  with ARoot.AddItem('Item 2') do begin
    FUnfold:=false;
    AddItem('SubItem 2.1').Checked:=true;
    AddItem('SubItem 2.2');
    AddItem('SubItem 2.3').DisplayName:='SubItem 2.3 (Disp.)';
  end;
  with ARoot.AddItem('Item 3') do begin
    FUnfold:=true;
    AddItem('SubItem 3.1').Checked:=true;
    with AddItem('SubItem 3.2') do begin
      AddItem('SubItem 3.2.a').Checked:=true;
      AddItem('SubItem 3.2.b').Checked:=true;
      AddItem('SubItem 3.2.c');
    end;
    AddItem('SubItem 3.3').DisplayName:='SubItem 2.3 (Disp.)';
  end;
end;

constructor TCustomListCheck.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FRoot:=TListCheckNode.Create;
  FRoot.Unfold:=true;
  ItemFont:=TFont.Create;
  ItemColor:=clWhite;
  FFirstOrder:=0;
  ItemGap:=2;
  ItemHeight:=25;
  ItemFont.Name:='default';
  ItemFont.Height:=21;
  OnMouseUp:=@MouseUp;
  OnMouseWheel:=@MouseWheel;
  //设计模式默认显示
  if csDesigning in ComponentState then set_dsgn_time_data(FRoot);
end;

destructor TCustomListCheck.Destroy;
begin
  FRoot.Free;
  ItemFont.Free;
  inherited Destroy;
end;

initialization
  with ItemCaptionStyle do begin
    Alignment:=taLeftJustify;
    Clipping:=true;
    Wordbreak:=true;
  end;
end.
