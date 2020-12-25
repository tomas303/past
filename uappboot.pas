unit uappboot;

{$mode objfpc}{$H+}

interface

uses
  rea_udesigncomponent, rea_idesigncomponent, trl_iprops, trl_imetaelement,
  flu_iflux, trl_igenericaccess, forms, typinfo, graphics,
  rea_ilayout, rea_ibits, rea_irenderer,
  ipast;

type

  { TCloseQueryFunc }

  TCloseQueryFunc = class(TInterfacedObject, IFluxFunc)
  private
    fID: integer;
  protected
    procedure Execute(const AAction: IFluxAction);
    function RunAsync: Boolean;
    function GetID: integer;
  public
    constructor Create(AID: integer);
  end;

  { TOpenDataFunc }

  TOpenDataFunc = class(TDesignComponentFunc)
  private
    fRenderNotifier: IFluxNotifier;
    //IStoreManager
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  public
    constructor Create(AID: integer; const AState: IGenericAccess;
      const ARenderNotifier: IFluxNotifier);
  protected
    fStoreManager: IStoreManager;
  published
    property StoreManager: IStoreManager read fStoreManager write fStoreManager;
  end;

  { TDesignComponentApp }

  TDesignComponentApp = class(TDesignComponent, IDesignComponentApp)
  private const
    cOpenDataNotifier = 'OpenDataNotifier';
  private
    function NewOpenDataFunc: IFluxFunc;
  private
    function OpenDataNotifier: IFluxNotifier;
    function ComposeOpenButton: IMetaElement;
    function ComposeOpenData: IMetaElement;
    function ComposeMain: IMetaElement;
  protected
    procedure DoInitState(const AState: IGenericAccess); override;
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
  fRenderNotifier.Notify;
end;

constructor TOpenDataFunc.Create(AID: integer;
  const AState: IGenericAccess; const ARenderNotifier: IFluxNotifier);
begin
  inherited Create(AID, AState);
  fRenderNotifier := ARenderNotifier;
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

{ TCloseQueryFunc }

procedure TCloseQueryFunc.Execute(const AAction: IFluxAction);
begin
  Application.Terminate;
end;

function TCloseQueryFunc.RunAsync: Boolean;
begin
  Result := False;
end;

function TCloseQueryFunc.GetID: integer;
begin
  Result := fID;
end;

constructor TCloseQueryFunc.Create(AID: integer);
begin
  inherited Create;
  fID := AID;
end;

{ TDesignComponentApp }

function TDesignComponentApp.NewOpenDataFunc: IFluxFunc;
begin
  Result := IFluxFunc(Factory.Locate(IFluxFunc, 'TOpenDataFunc',
    NewProps
    .SetInt('ID', FuncSequence.Next)
    .SetIntf('State', State)
  ));
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
          ElementFactory.CreateElement(IDesignComponentEdit, NewProps.SetInt(cProps.Place, cPlace.FixFront).SetInt(cProps.MMHeight, 20)),
          ElementFactory.CreateElement(IDesignComponentEdit, NewProps.SetInt(cProps.Place, cPlace.FixFront).SetInt(cProps.MMHeight, 20))
        ]),
        ElementFactory.CreateElement(IStripBit,
        NewProps.SetInt(cProps.Layout, cLayout.Vertical).SetInt(cProps.Place, cPlace.FixBack).SetInt(cProps.MMWidth, 50),
        [
          ElementFactory.CreateElement(IDesignComponentButton, NewProps.SetStr(cProps.Text, 'Open').SetInt(cProps.Place, cPlace.FixFront).SetInt(cProps.MMHeight, 40))
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
  AddFuncNotifier(AState, NewOpenDataFunc, cOpenDataNotifier);
end;

function TDesignComponentApp.DoCompose(const AProps: IProps; const AChildren: TMetaElementArray): IMetaElement;
var
  mCQ: IFluxNotifier;
begin
  mCQ := NewNotifier(-303);
  FluxFuncReg.RegisterFunc(TCloseQueryFunc.Create(-303));
  Result := ElementFactory.CreateElement(
      IDesignComponentForm,
        NewProps
          .SetStr('DataPath', 'main')
          .SetStr(cProps.Title, 'PAST')
          .SetIntf(cProps.CloseQueryNotifier, mCQ),
        [ComposeOpenData]);
end;

end.

