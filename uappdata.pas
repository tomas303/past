unit uappdata;

{$mode ObjFPC}{$H+}

interface

uses
  rea_udesigncomponentdata, SysUtils,
  rea_idesigncomponent, trl_ipersist,
  trl_udifactory, rea_iflux;

type

  { TStorageData }

  TStorageData = class
  private
    fFileNameData: TEditData;
    fPasswordData: TEditData;
    fOpened: Boolean;
    fStore: IPersistStore;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    property Opened: Boolean read fOpened write fOpened;
    property Store: IPersistStore read fStore write fStore;
  published
    property FileNameData: TEditData read fFileNameData write fFileNameData;
    property PasswordData: TEditData read fPasswordData write fPasswordData;
  end;

  { TMyGridDataProvider }

  TMyGridDataProvider = class(TInterfacedObject, IGridDataProvider)
  private type
    { IBookmark }

    IBookmark = interface
    ['{2D420B25-6405-4517-BDB6-1971EF8E6AC4}']
      function GetData: Integer;
      procedure SetData(AValue: Integer);
      property Data: Integer read GetData write SetData;
    end;

    { TBookmark }

    TBookmark = class(TInterfacedObject, IBookmark)
    private
      fData: Integer;
      function GetData: Integer;
      procedure SetData(AValue: Integer);
      property Data: Integer read GetData write SetData;
    public
      constructor Create(const AData: Integer);
    end;

  private
    fCurrent: Integer;
    fDeltaMove: Integer;
    fSilent: Boolean;
    procedure NotifyMove;
  private
    function Prev: Boolean;
    function Next: Boolean;
    function IsEmpty: Boolean;
    function GetValue(Ind: integer): string;
    procedure SetValue(Ind: integer; AValue: string);
    function NewBookmark: IInterface;
    procedure GotoBookmark(ABookmark: IInterface);
    function GetSilent: Boolean;
    procedure SetSilent(AValue: Boolean);
    function GetMoveActionID: Integer;
    property Value[Ind: integer]: string read GetValue write SetValue; default;
    property Silent: Boolean read GetSilent write SetSilent;
    property MoveActionID: Integer read GetMoveActionID;
  protected
    fRecords: IPersistRefList;
    fFactory2: TDIFactory2;
    fMoveNotifier: IFluxNotifier;
    fRenderNotifier: IFluxNotifier;
  published
    property Records: IPersistRefList read fRecords write fRecords;
    property Factory2: TDIFactory2 read fFactory2 write fFactory2;
    property MoveNotifier: IFluxNotifier read fMoveNotifier write fMoveNotifier;
    property RenderNotifier: IFluxNotifier read fRenderNotifier write fRenderNotifier;
  end;

implementation

{ TMyGridDataProvider.TBookmark }

function TMyGridDataProvider.TBookmark.GetData: Integer;
begin
  Result := fData;
end;

procedure TMyGridDataProvider.TBookmark.SetData(AValue: Integer);
begin
  fData := AValue;
end;

constructor TMyGridDataProvider.TBookmark.Create(const AData: Integer);
begin
  inherited Create;
  fData := AData;
end;

{ TMyGridDataProvider }

procedure TMyGridDataProvider.NotifyMove;
begin
  if MoveNotifier <> nil then begin
    MoveNotifier.Notify;
    if RenderNotifier <> nil then
      RenderNotifier.Notify;
  end;
end;

function TMyGridDataProvider.Prev: Boolean;
begin
  Result := fCurrent > 0;
  if Result then begin
    Dec(fCurrent);
    if not Silent and (MoveNotifier <> nil) then begin
      fDeltaMove := -1;
      NotifyMove;
    end;
  end;
end;

function TMyGridDataProvider.Next: Boolean;
begin
  Result := fCurrent < High(fRecords.Count - 1);
  if Result then begin
    Inc(fCurrent);
    if not Silent and (MoveNotifier <> nil) then begin
      fDeltaMove := 1;
      NotifyMove;
    end;
  end;
end;

function TMyGridDataProvider.IsEmpty: Boolean;
begin
  Result := fRecords.Count = 0;
end;

function TMyGridDataProvider.GetValue(Ind: integer): string;
begin
  Result := fRecords.Data[fCurrent].Items[Ind].AsString;
end;

procedure TMyGridDataProvider.SetValue(Ind: integer; AValue: string);
begin
  fRecords.Data[fCurrent].Items[Ind].AsString := AValue;
end;

function TMyGridDataProvider.NewBookmark: IInterface;
begin
  Result := TBookmark.Create(fCurrent);
end;

procedure TMyGridDataProvider.GotoBookmark(ABookmark: IInterface);
begin
  fCurrent := (ABookmark as IBookmark).Data;
end;

function TMyGridDataProvider.GetSilent: Boolean;
begin
  Result := fSilent;
end;

procedure TMyGridDataProvider.SetSilent(AValue: Boolean);
begin
  fSilent := AValue;
end;

function TMyGridDataProvider.GetMoveActionID: Integer;
begin
  Result := fMoveNotifier.ActionID;
end;

{ TStorageData }

procedure TStorageData.AfterConstruction;
begin
  inherited AfterConstruction;
  fFileNameData := TEditData.Create;
  fPasswordData := TEditData.Create;
end;

procedure TStorageData.BeforeDestruction;
begin
  FreeAndNil(fFileNameData);
  FreeAndNil(fPasswordData);
  inherited BeforeDestruction;
end;

end.

