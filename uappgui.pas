unit uappgui;

{$mode delphi}{$H+}

interface

uses
  rea_idesigncomponent, rea_udesigncomponent, rea_udesigncomponentfactory,
  trl_iprops, rea_udesigncomponentdata, rea_ilayout, trl_itree, trl_imetaelement,
  rea_idataconnector, trl_isequence, Graphics, uappfunc, rea_iflux,
  uappdata, tal_uwinfunc, rea_irenderer, rea_udesigncomponentfunc,
  trl_ipersist;

type

  { IDCOpenStorageFactory }

  IDCOpenStorageFactory = interface(IDesignComponentFactory)
  ['{30C52156-D6BD-47FD-A460-E40DA8CE29C5}']
  end;

  { TDCOpenStorageFactory }

  TDCOpenStorageFactory = class(TDesignComponentFactory, IDCOpenStorageFactory)
  private
    function NewPasswordEdit(const AProps: IProps; AEdit: TEditData): IDesignComponent;
    function NewFileEdit(const AProps: IProps; AEdit: TEditData): IDesignComponent;
    function NewOpenButton(const AProps: IProps; AData: TStorageData): IDesignComponent;
  protected
    function DoNew(const AProps: IProps): IDesignComponent; override;
  end;

  { IDCPasswordListFactory }

  IDCPasswordListFactory = interface(IDesignComponentFactory)
  ['{267A8685-8B1B-4EB5-AF3D-62E8980F15C1}']
  end;

  { TDCPasswordListFactory }

  TDCPasswordListFactory = class(TDesignComponentFactory, IDCPasswordListFactory)
  private
    function NewGrid(const AProps: IProps): IDesignComponent;
  protected
    function DoNew(const AProps: IProps): IDesignComponent; override;
  end;


  { TGUI }

  TGUI = class(TDesignComponent, IDesignComponentApp)
  private
    fMainFormData: TFormData;
    fPasswordGridData : TGridData;
    fMainForm: IDesignComponent;
    fDCOpenStorage: IDesignComponent;
    fDCPasswordGrid: IDesignComponent;
    fStorageData: TStorageData;
    fProvider: IGridDataProvider;
    fMoveNotifier: IFluxNotifier;
    fRenderNotifier: IFluxNotifier;
    procedure NewData;
    procedure ConnectData;
    procedure RegisterClose;
    function NewMainForm: IDesignComponent;
    function NewDCOpenStorage: IDesignComponent;
    function NewPasswordGrid: IDesignComponent;
  protected
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
    procedure InitValues; override;
  protected
    fDataConnector: IDataConnector;
    fSequence: ISequence;
    fFluxDispatcher: IFluxDispatcher;
  published
    property DataConnector: IDataConnector read fDataConnector write fDataConnector;
    property Sequence: ISequence read fSequence write fSequence;
    property FluxDispatcher: IFluxDispatcher read fFluxDispatcher write fFluxDispatcher;
  end;

implementation

{ TDCPasswordListFactory }

function TDCPasswordListFactory.NewGrid(const AProps: IProps): IDesignComponent;
var
  mF: IDesignComponentGridFactory;
begin
  mF := Factory2.Locate<IDesignComponentGridFactory>;
  Result := mF.New(NewProps
    .SetObject(cProps.Data, AProps.AsObject(cProps.Data))
    .SetInt('MMHeight', 1000)
    .SetInt('MMWidth', 1000)
    .SetInt(cProps.RowMMHeight, 25)
    .SetInt(cProps.ColMMWidth, 25)
    //.SetInt(cProps.ColOddColor, clLime)
    //.SetInt(cProps.ColEvenColor, clAqua)
    .SetInt(cProps.RowOddColor, clBlack)
    .SetInt(cProps.RowEvenColor, clBlack)
    .SetInt(cProps.Color, clBlack)
    .SetInt(cProps.TextColor, clWhite)
    .SetInt('LaticeColColor', clLime)
    .SetInt('LaticeColSize', 1)
    .SetInt('LaticeRowColor', clLime)
    .SetInt('LaticeRowSize', 1)
//    .SetIntf('DataToGUI', fMoveNotifier)
  );
end;

function TDCPasswordListFactory.DoNew(const AProps: IProps): IDesignComponent;
var
  mF: IDesignComponentStripFactory;
  //mData: TStorageData;
begin
  //mData := AProps.AsObject(cProps.Data) as TPasswordsData;
  mF := Factory2.Locate<IDesignComponentStripFactory>;
  AProps.SetInt(cProps.Layout, cLayout.Vertical).SetInt(cProps.Place, cPlace.Elastic);
  Result := mF.New(AProps);
  (Result as INode).AddChild(NewGrid(AProps) as INode);
end;

{ TGUI }

procedure TGUI.NewData;
begin
  fMainFormData := TFormData.Create;
  fMainFormData.Left := 0;
  fMainFormData.Top := 0;
  fMainFormData.Width := 400;
  fMainFormData.Height := 200;
  fStorageData := TStorageData.Create;
  fPasswordGridData := TGridData.Create;
  fPasswordGridData.RowCount := 10;
  fPasswordGridData.ColCount := 4;
end;

procedure TGUI.ConnectData;
begin
  DataConnector.Connect(fProvider, fPasswordGridData, [0,1,2,3]);
end;

procedure TGUI.RegisterClose;
var
  mFunc: IFluxFunc;
begin
  mFunc := Factory2.Locate<IFluxFunc>(TCloseStorageFunc,
    NewProps
    .SetInt(cAction.ID, cNotifyCloseGUI)
    .SetObject(cProps.Data, fStorageData)
  );
  FluxDispatcher.RegisterFunc(mFunc);
  mFunc := TStopExecutorFunc.Create(cNotifyCloseGUI);
  FluxDispatcher.RegisterFunc(mFunc);
end;

function TGUI.NewMainForm: IDesignComponent;
var
  mF: IDesignComponentFormFactory;
begin
  mF := Factory2.Locate<IDesignComponentFormFactory>;
  //Result := mF.New(NewProps.SetObject('Data', fMainFormData));
  Result := mF.New(NewProps
    .SetObject(cForm.Data, fMainFormData)
  );
end;

function TGUI.NewDCOpenStorage: IDesignComponent;
var
  mF: IDCOpenStorageFactory;
begin
  mF := Factory2.Locate<IDCOpenStorageFactory>;
  //Result := mF.New(NewProps.SetObject('Data', fMainFormData));
  Result := mF.New(NewProps
    //.SetInt(cProps.Place, cPlace.Elastic)
    //.SetInt(cProps.Layout, cLayout.Vertical)
    .SetObject(cProps.Data, fStorageData)
    .SetInt(cProps.MMWidth, 200)
    .SetInt(cProps.MMHeight, 25)
    .SetInt(cProps.Color, clGreen)
    );
end;

function TGUI.NewPasswordGrid: IDesignComponent;
var
  mF: IDCPasswordListFactory;
begin
  mF := Factory2.Locate<IDCPasswordListFactory>;
  Result := mF.New(NewProps
    //.SetInt(cProps.Place, cPlace.Elastic)
    //.SetInt(cProps.Layout, cLayout.Vertical)
    .SetObject(cProps.Data, fPasswordGridData)
    .SetInt(cProps.MMWidth, 200)
    .SetInt(cProps.MMHeight, 25)
    .SetInt(cProps.Color, clGreen)
    );
end;

function TGUI.DoCompose(const AProps: IProps; const AChildren: TMetaElementArray
  ): IMetaElement;
var
  mDCOpenStorage: IMetaElement;
  mPasswords: IPersistRefList;
begin
  if fStorageData.Opened then begin
    mPasswords := (fStorageData.Store as IPersistQuery).SelectClass('TPassword');
    fProvider := Factory2.Locate<IGridDataProvider>(NewProps
      .SetIntf('Records', mPasswords)
      .SetIntf('MoveNotifier', fMoveNotifier)
      .SetIntf('RenderNotifier', fRenderNotifier)
      );
    ConnectData;
    fMoveNotifier.Notify;
    Result := fMainForm.Compose(AProps, [fDCPasswordGrid.Compose(AProps, nil)]);
  end else begin
    mDCOpenStorage := fDCOpenStorage.Compose(AProps, nil);
    Result := fMainForm.Compose(AProps, [mDCOpenStorage]);
  end;
end;

procedure TGUI.InitValues;
begin
  inherited InitValues;
  fMoveNotifier := NewNotifier(Sequence.Next);
  fRenderNotifier := NewNotifier(cNotifyRender);
  NewData;
  RegisterClose;
  fMainForm := NewMainForm;
  fDCOpenStorage := NewDCOpenStorage;
  fDCPasswordGrid := NewPasswordGrid;
end;

{ TDCOpenStorageFactory }

function TDCOpenStorageFactory.NewPasswordEdit(const AProps: IProps; AEdit: TEditData): IDesignComponent;
var
  mF: IDesignComponentLabelEditFactory;
begin
  mF := Factory2.Locate<IDesignComponentLabelEditFactory>;
  Result := mF.New(mF.NewDCProps
    .SetData(AEdit)
    .SetCaption('Password')
    .AsIProps
  );
end;

function TDCOpenStorageFactory.NewFileEdit(const AProps: IProps; AEdit: TEditData): IDesignComponent;
var
  mF: IDesignComponentLabelEditFactory;
begin
  mF := Factory2.Locate<IDesignComponentLabelEditFactory>;
  Result := mF.New(mF.NewDCProps
    .SetData(AEdit)
    .SetCaption('File')
    .AsIProps
  );
end;

function TDCOpenStorageFactory.NewOpenButton(const AProps: IProps; AData: TStorageData): IDesignComponent;
var
  mF: IDesignComponentButtonFactory;
  mOpenStorageNotifier: IFluxNotifier;
  mOpenStorageFunc: IFluxFunc;
begin
  mOpenStorageNotifier := NewNotifier(Sequence.Next);
  mOpenStorageFunc := Factory2.Locate<IFluxFunc>(TOpenStorageFunc,
    NewProps
    .SetInt(cAction.ID, mOpenStorageNotifier.ActionID)
    .SetObject(cProps.Data, AData)
    .SetIntf('RenderNotifier', NewNotifier(cNotifyRender))
  );
  FluxDispatcher.RegisterFunc(mOpenStorageFunc);
  mF := Factory2.Locate<IDesignComponentButtonFactory>;
  Result := mF.New(mF.NewDCProps
    .SetText('Open storage')
    .SetClickNotifier(mOpenStorageNotifier)
    .AsIProps
  );
end;

function TDCOpenStorageFactory.DoNew(const AProps: IProps): IDesignComponent;
var
  mF: IDesignComponentStripFactory;
  mData: TStorageData;
begin
  mData := AProps.AsObject(cProps.Data) as TStorageData;
  mF := Factory2.Locate<IDesignComponentStripFactory>;
  AProps.SetInt(cProps.Layout, cLayout.Vertical).SetInt(cProps.Place, cPlace.Elastic);
  Result := mF.New(AProps);
  (Result as INode).AddChild(NewFileEdit(AProps, mData.FileNameData) as INode);
  (Result as INode).AddChild(NewPasswordEdit(AProps, mData.PasswordData) as INode);
  (Result as INode).AddChild(NewOpenButton(AProps, mData) as INode);
end;

end.

