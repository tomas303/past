unit uappboot;

{$mode objfpc}{$H+}

interface

uses
  rea_udesigncomponent, rea_idesigncomponent, trl_iprops, trl_imetaelement,
  flu_iflux, trl_igenericaccess, forms, typinfo, graphics,
  rea_ilayout, rea_ibits, rea_irenderer,
  ipast;

type

  { TOpenDataFunc }

  TOpenDataFunc = class(TDesignComponentFunc)
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  protected
    fStoreManager: IStoreManager;
    fRenderNotifier: IFluxNotifier;
  published
    property StoreManager: IStoreManager read fStoreManager write fStoreManager;
    property RenderNotifier: IFluxNotifier read fRenderNotifier write fRenderNotifier;
  end;

  { TDesignComponentApp }

  TDesignComponentApp = class(TDesignComponent, IDesignComponentApp)
  public const
    cCloseQueryNotifier = 'CloseQueryNotifier';
    cOpenDataNotifier = 'OpenDataNotifier';
    cDataFile = 'file';
    cDataKey = 'key';
  private
    function NewOpenDataFunc: IFluxFunc;
    function NewCloseQueryFunc: IFluxFunc;
  private
    function CloseQueryNotifier: IFluxNotifier;
    function OpenDataNotifier: IFluxNotifier;
    function ComposeOpenButton: IMetaElement;
    function ComposeOpenData: IMetaElement;
    function ComposeMain: IMetaElement;
  protected
    procedure DoInitState(const AState: IGenericAccess); override;
    procedure DoStartingValues; override;
    function DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement; override;
  end;

  TC = class(TObject)
  public
    class function Path: string;
    class function Name: string;
  end;

  MainForm = class(TC)
  public type
    Left = class(TC);
    Top = class(TC);
    Width = class(TC);
    Height = class(TC);
  end;

  cActions = class
  public const
    InitFunc = 0;
    ResizeFunc = 1;
    HelloFunc = 2;
    ClickOne = 3;
    ClickTwo = 4;
    ClickThree = 5;
  end;

implementation

{ TOpenDataFunc }

procedure TOpenDataFunc.DoExecute(const AAction: IFluxAction
  );
begin
  StoreManager.Open(
    AAction.Props.AsStr(TDesignComponentApp.cDataFile),
    AAction.Props.AsStr(TDesignComponentApp.cDataKey)
  );
  RenderNotifier.Notify;
end;

{ TC }

class function TC.Path: string;
begin
  Result := ClassName;
end;

class function TC.Name: string;
var
  mInfo: PTypeInfo;
begin
  mInfo := ClassInfo;
  Result := mInfo^.Name;
end;

{ TDesignComponentApp }

function TDesignComponentApp.NewOpenDataFunc: IFluxFunc;
begin
  Result := IFluxFunc(Factory.Locate(IFluxFunc, 'TOpenDataFunc',
    NewProps
    .SetInt('ID', FuncSequence.Next)
    .SetIntf('State', State)
    .SetIntf('RenderNotifier', NewNotifier(cFuncRender))
  ));
end;

function TDesignComponentApp.NewCloseQueryFunc: IFluxFunc;
begin
  Result := IFluxFunc(Factory.Locate(IFluxFunc, 'TCloseQueryFunc',
    NewProps
    .SetInt('ID', FuncSequence.Next)
  ));
end;

function TDesignComponentApp.CloseQueryNotifier: IFluxNotifier;
begin
  Result := State.AsIntf(cCloseQueryNotifier) as IFluxNotifier;
end;

function TDesignComponentApp.OpenDataNotifier: IFluxNotifier;
begin
  Result := State.AsIntf(cOpenDataNotifier) as IFluxNotifier;
end;

function TDesignComponentApp.ComposeOpenButton: IMetaElement;
begin
  Result := ElementFactory.CreateElement(IDesignComponentButton,
    NewProps.SetStr(cProps.Text, 'Open')
    .SetInt(cProps.Place, cPlace.FixFront)
    .SetInt(cProps.MMHeight, 40)
    .SetIntf(cProps.ClickNotifier, OpenDataNotifier)
  );
end;

function TDesignComponentApp.ComposeOpenData: IMetaElement;
begin
  Result :=
    ElementFactory.CreateElement(IStripBit,
      NewProps.SetInt(cProps.Layout, cLayout.Horizontal),
      [
        ElementFactory.CreateElement(IStripBit,
        NewProps.SetInt(cProps.Layout, cLayout.Vertical).SetInt(cProps.Place, cPlace.FixFront).SetInt(cProps.MMWidth, 30),
        [
          ElementFactory.CreateElement(ITextBit, NewProps.SetStr(cProps.Text, 'File').SetInt(cProps.Place, cPlace.FixFront).SetInt(cProps.MMHeight, 20)),
          ElementFactory.CreateElement(ITextBit, NewProps.SetStr(cProps.Text, 'Key').SetInt(cProps.Place, cPlace.FixFront).SetInt(cProps.MMHeight, 20))
        ]),
        ElementFactory.CreateElement(IStripBit,
        NewProps.SetInt(cProps.Layout, cLayout.Vertical).SetInt(cProps.Place, cPlace.Elastic),
        [
          ElementFactory.CreateElement(IDesignComponentEdit,
            NewProps
            .SetStr(cProps.ID, cDataFile)
            .SetStr(cProps.DataPath, cDataFile)
            .SetInt(cProps.Place, cPlace.FixFront)
            .SetInt(cProps.MMHeight, 20)
            .SetIntf(cProps.AskNotifier, OpenDataNotifier)),
          ElementFactory.CreateElement(IDesignComponentEdit,
            NewProps
            .SetStr(cProps.ID, cDataKey)
            .SetStr(cProps.DataPath, cDataKey)
            .SetInt(cProps.Place, cPlace.FixFront)
            .SetInt(cProps.MMHeight, 20)
            .SetIntf(cProps.AskNotifier, OpenDataNotifier))
        ]),
        ElementFactory.CreateElement(IStripBit,
        NewProps.SetInt(cProps.Layout, cLayout.Vertical).SetInt(cProps.Place, cPlace.FixBack).SetInt(cProps.MMWidth, 50),
        [
          ComposeOpenButton
        ])
      ]
    );
end;

function TDesignComponentApp.ComposeMain: IMetaElement;
begin

end;

procedure TDesignComponentApp.DoInitState(const AState: IGenericAccess);
begin
  inherited DoInitState(AState);
  AddFuncNotifier(AState, NewCloseQueryFunc, cCloseQueryNotifier);
  AddFuncNotifier(AState, NewOpenDataFunc, cOpenDataNotifier);
end;

procedure TDesignComponentApp.DoStartingValues;
begin
  inherited DoStartingValues;
  DataPath := 'app';
end;

function TDesignComponentApp.DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
begin
  Result := ElementFactory.CreateElement(
      IDesignComponentForm,
        NewProps
          .SetStr('DataPath', 'main')
          .SetStr(cProps.Title, 'PAST')
          .SetIntf(cProps.CloseQueryNotifier, CloseQueryNotifier),
        [ComposeOpenData]);
end;

end.

