unit uappgui;

{$mode delphi}{$H+}

interface

uses
  rea_idesigncomponent, rea_udesigncomponent, rea_udesigncomponentfactory,
  trl_iprops, rea_udesigncomponentdata, rea_ilayout, trl_itree, trl_imetaelement,
  rea_idataconnector, trl_isequence, Graphics, uappfunc, rea_iflux,
  uappdata, tal_uwinfunc, rea_irenderer, rea_udesigncomponentfunc;

type
  IDCOpenStorageFactory = interface(IDesignComponentFactory)
  ['{30C52156-D6BD-47FD-A460-E40DA8CE29C5}']
  end;

  //TDesignComponentLabelEditFactory = class(TDesignComponentFactory, IDesignComponentLabelEditFactory)

  { TDCOpenStorageFactory }

  TDCOpenStorageFactory = class(TDesignComponentFactory, IDCOpenStorageFactory)
  private
    function NewPasswordEdit(const AProps: IProps; AEdit: TEditData): IDesignComponent;
    function NewFileEdit(const AProps: IProps; AEdit: TEditData): IDesignComponent;
    function NewOpenButton(const AProps: IProps; AData: TStorageData): IDesignComponent;
  protected
    function DoNew(const AProps: IProps): IDesignComponent; override;
  end;

  { TGUI }

  TGUI = class(TDesignComponent, IDesignComponentApp)
  private
    fMainFormData: TFormData;
    fMainForm: IDesignComponent;
    fDCOpenStorage: IDesignComponent;
    fStorageData: TStorageData;
    procedure NewData;
    procedure RegisterClose;
    function NewMainForm: IDesignComponent;
    function NewDCOpenStorage: IDesignComponent;
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

{ TGUI }

procedure TGUI.NewData;
begin
  fMainFormData := TFormData.Create;
  fMainFormData.Left := 0;
  fMainFormData.Top := 0;
  fMainFormData.Width := 400;
  fMainFormData.Height := 200;
  fStorageData := TStorageData.Create;
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

function TGUI.DoCompose(const AProps: IProps; const AChildren: TMetaElementArray
  ): IMetaElement;
var
  mDCOpenStorage: IMetaElement;
begin
  mDCOpenStorage := fDCOpenStorage.Compose(AProps, nil);
  Result := fMainForm.Compose(AProps, [mDCOpenStorage]);
end;

procedure TGUI.InitValues;
begin
  inherited InitValues;
  NewData;
  RegisterClose;
  fMainForm := NewMainForm;
  fDCOpenStorage := NewDCOpenStorage;
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
  mCloseStorageFunc: IFluxFunc;
begin
  mOpenStorageNotifier := NewNotifier(Sequence.Next);
  mOpenStorageFunc := Factory2.Locate<IFluxFunc>(TOpenStorageFunc,
    NewProps
    .SetInt(cAction.ID, mOpenStorageNotifier.ActionID)
    .SetObject(cProps.Data, AData)
  );
  FluxDispatcher.RegisterFunc(mOpenStorageFunc);
  mF := Factory2.Locate<IDesignComponentButtonFactory>;
  Result := mF.New(mF.NewDCProps
    .SetText('Open storage')
    .SetClickNotifier(mOpenStorageNotifier)
    .AsIProps
  );
  //mCloseStorageFunc := Factory2.Locate<IFluxFunc>(TCloseStorageFunc,
  //  NewProps
  //  .SetInt(cAction.ID, cNotifyCloseGUI)
  //  .SetObject(cProps.Data, AData)
  //);
  //FluxDispatcher.RegisterFunc(mCloseStorageFunc);
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

