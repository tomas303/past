unit uapp;

{$mode delphi}{$H+}

interface

uses
  graphics, Classes, tal_uapp,
  rea_idesigncomponent, rea_udesigncomponent, rea_ilayout,
  trl_imetaelement, trl_iprops, trl_dicontainer, trl_itree,
  trl_pubsub, rea_ibits, trl_ilauncher,
  trl_urttibroker, trl_irttibroker,
  trl_upersiststore, trl_ipersist,
  trl_upersistxml,
  rea_idata, sysutils,
  trl_icryptic, uCryptic;

type

  { TApp }

  TApp = class(TALApp)
  protected
    procedure RegisterTools;
    procedure RegisterGUI;
    procedure RegisterPersist;
    procedure RegisterAppServices; override;
  end;

  { TAppSettings }

  TAppSettings = class
  private
    fTop: Integer;
    fLeft: Integer;
    fWidth: Integer;
    fHeight: Integer;
    fLastOpenedFile: String;
  published
    property Top: Integer read fTop write fTop;
    property Left: Integer read fLeft write fLeft;
    property Width: Integer read fWidth write fWidth;
    property Height: Integer read fHeight write fHeight;
    property LastOpenedFile: String read fLastOpenedFile write fLastOpenedFile;
  end;

  { TPassword }

  TPassword = class
  private
    fLogin: string;
    fPassword: string;
    fLink: string;
    fRemark: string;
  published
    property Login: string read fLogin write fLogin;
    property Password: string read fPassword write fPassword;
    property Link: string read fLink write fLink;
    property Remark: string read fRemark write fRemark;
  end;

  { TCrypto }

  TCrypto = class
  private
    fData: TStream;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property Data: TStream read fData write fData;
  end;


  { IGUIStore }

  IGUIStore = interface(IDesignComponent)
  ['{7B5E661F-B0BD-4A09-B665-D126B19F4088}']
    function GetFileEdit: IDesignComponentEdit;
    property FileEdit: IDesignComponentEdit read GetFileEdit;
  end;

  { TGUIStore }

  TGUIStore = class(TDesignComponent, IGUIStore)
  private
    fFileEdit: IDesignComponentEdit;
    fPasswordEdit: IDesignComponentEdit;
    fOpen: IDesignComponentButton;
    function OpenEncryptedStore(const AFile: String): TStream;
    procedure CloseEncryptedStore(const AData: TStream);
    procedure OpenDecryptedStore(const ACryptedData: TStream; const APassword: string);
    function CloseDecryptedStore: TStream;
    procedure PSClickOpenObserver;
  protected
    procedure InitValues; override;
    function DoCompose: IMetaElement; override;
  private
    function GetFileEdit: IDesignComponentEdit;
  protected
    fCryptic: ICryptic;
    fEncryptedStore: IPersistStore;
    fDecryptedStore: IPersistStore;
    fPSGUIChannel: IPSGUIChannel;
  published
    property Cryptic: ICryptic read fCryptic write fCryptic;
    property EncryptedStore: IPersistStore read fEncryptedStore write fEncryptedStore;
    property DecryptedStore: IPersistStore read fDecryptedStore write fDecryptedStore;
    property PSGUIChannel: IPSGUIChannel read fPSGUIChannel write fPSGUIChannel;
  end;


  TGUI = class(TDesignComponent, IDesignComponentApp)
  private
    fAppSettings: IRBData;
    function GetAppSettings: IRBData;
    procedure PublishAppSettings;
  private
    fForm: IDesignComponentForm;
    fOpenStore: IGUIStore;
    function NewForm(const ADCs: TArray<IDesignComponent>): IDesignComponentForm;
    function NewStore: IGUIStore;
    procedure CreateComponents;
  private
    procedure PSSizeObserver(const AValue: TSizeData);
    procedure PSPositionObserver(const AValue: TPositionData);
    procedure PSCloseProgramObserver;
  protected
    procedure InitValues; override;
    function DoCompose: IMetaElement; override;
  protected
    fStore: IPersistStore;
    fSettingsStore: IPersistStore;
    fPersistFactory: IPersistFactory;
    fPSGUIChannel: IPSGUIChannel;
  published
    property Store: IPersistStore read fStore write fStore;
    property SettingsStore: IPersistStore read fSettingsStore write fSettingsStore;
    property PersistFactory: IPersistFactory read fPersistFactory write fPersistFactory;
    property PSGUIChannel: IPSGUIChannel read fPSGUIChannel write fPSGUIChannel;
  end;

implementation

{ TGUI }

function TGUI.GetAppSettings: IRBData;
var
  mList: IPersistRefList;
begin
  mList := (SettingsStore as IPersistQuery).SelectClass(TAppSettings.ClassName);
  if mList.Count = 0 then
  begin
    Result := PersistFactory.Create(IRBData, TAppSettings.ClassName) as IRBData;
    Result.ItemByName['Width'].AsInteger := 600;
    Result.ItemByName['Height'].AsInteger := 200;
    Result.ItemByName['Left'].AsInteger := 300;
    Result.ItemByName['Top'].AsInteger := 400;
  end
  else
  begin
    Result := mList.Data[0];
  end;
end;

procedure TGUI.PublishAppSettings;
begin
  fForm.PSSizeChannel.Publish(TSizeData.Create(Self, fAppSettings.ItemByName['Width'].AsInteger, fAppSettings.ItemByName['Height'].AsInteger));
  fForm.PSPositionChannel.Publish(TPositionData.Create(Self, fAppSettings.ItemByName['Left'].AsInteger, fAppSettings.ItemByName['Top'].AsInteger));
  fOpenStore.FileEdit.PSTextChannel.Publish(fAppSettings.ItemByName['LastOpenedFile'].AsString);
end;

function TGUI.NewForm(const ADCs: TArray<IDesignComponent>): IDesignComponentForm;
var
  mDC: IDesignComponent;
begin
  Result := Factory2.Locate<IDesignComponentForm>(NewProps
    .SetStr(cProps.ID, 'mainform')
    .SetIntf('PSGUIChannel', fPSGUIChannel)
    .SetStr(cProps.Caption, 'past ... password storage')
    );
  Result.PSSizeChannel.Subscribe(PSSizeObserver);
  Result.PSPositionChannel.Subscribe(PSPositionObserver);
  Result.PSCloseChannel.Subscribe(PSCloseProgramObserver);
  for mDC in ADCs do begin
    (Result as INode).AddChild(mDC as INode);
  end;
end;

function TGUI.NewStore: IGUIStore;
begin
  Result := Factory2.Locate<IGUIStore>(NewProps.SetIntf('PSGUIChannel', PSGUIChannel));
end;

procedure TGUI.CreateComponents;
begin
  fOpenStore := NewStore;
  fForm := NewForm([]);
end;

procedure TGUI.PSSizeObserver(const AValue: TSizeData);
begin
  fAppSettings.ItemByName['Width'].AsInteger := AValue.Width;
  fAppSettings.ItemByName['Height'].AsInteger := AValue.Height;
  PSGUIChannel.Debounce(TGUIData.Create(gaRender));
end;

procedure TGUI.PSPositionObserver(const AValue: TPositionData);
begin
  fAppSettings.ItemByName['Left'].AsInteger := AValue.Left;
  fAppSettings.ItemByName['Top'].AsInteger := AValue.Top;
end;

procedure TGUI.PSCloseProgramObserver;
begin
  SettingsStore.Save(fAppSettings);
  SettingsStore.Close;
  raise ELaunchStop.Create('');
end;

procedure TGUI.InitValues;
begin
  inherited InitValues;
  SettingsStore.Open('~/settings.xml');
  fAppSettings := GetAppSettings;
  CreateComponents;
  //CreateDataConnectors;
  PublishAppSettings;
end;

function TGUI.DoCompose: IMetaElement;
begin
  Result := fForm.Compose;
  if Store.IsOpened then begin
    fAppSettings.ItemByName['LastOpenedFile'].AsString := fOpenStore.FileEdit.Text;
    //Result := fForm.Compose
  end else begin
    (Result as INode).AddChild(fOpenStore.Compose as INode);
  end;
end;

{ TGUIStore }

function TGUIStore.OpenEncryptedStore(const AFile: String): TStream;
var
  mList: IPersistRefList;
begin
  Result := TMemoryStream.Create;
  EncryptedStore.Open(AFile);
  mList := (EncryptedStore as IPersistQuery).SelectClass(TCrypto.ClassName);
  if mList.Count > 0 then
  begin
    Result.CopyFrom(mList.Data[0].ItemByName['Data'].AsObject as TStream, 0);
  end;
end;

procedure TGUIStore.CloseEncryptedStore(const AData: TStream);
var
  mList: IPersistRefList;
  mData: IRBData;
begin
  mList := (EncryptedStore as IPersistQuery).SelectClass(TCrypto.ClassName);
  if mList.Count = 0 then
  begin
    mData := Factory2.Locate<IRBData>(TCrypto.ClassName);
  end
  else
  begin
    mData := mList.Data[0];
  end;
  AData.Position := 0;
  (mData.ItemByName['Data'].AsObject as TStream).Position := 0;
  (mData.ItemByName['Data'].AsObject as TStream).CopyFrom(AData, 0);
  EncryptedStore.Save(mData);
  EncryptedStore.Close;
end;

procedure TGUIStore.OpenDecryptedStore(const ACryptedData: TStream; const APassword: string);
var
  mData: TMemoryStream;
begin
  mData := TMemoryStream.Create;
  try
    Cryptic.Key := APassword;
    if ACryptedData.Size > 0 then begin
      try
        ACryptedData.Position := 0;
        Cryptic.Decode(ACryptedData, mData);
        mData.Position := 0;
        DecryptedStore.Open(mData);
      except
        on E: Exception do begin
          DecryptedStore.Close;
          raise Exception.Create('Cannot open crypted data - probably bad password');
        end;
      end;
    end else begin
      DecryptedStore.Open;
    end;
  finally
    mData.Free;
  end;
end;

function TGUIStore.CloseDecryptedStore: TStream;
var
  mData: TMemoryStream;
begin
  mData := TMemoryStream.Create;
  try
    DecryptedStore.Close(mData);
    Result := TMemoryStream.Create;
    Cryptic.Encode(mData, Result);
  finally
    mData.Free;
  end;
end;

procedure TGUIStore.PSClickOpenObserver;
var
  mEncrypted: TStream;
begin
  mEncrypted := OpenEncryptedStore(fFileEdit.Text);
  try
    OpenDecryptedStore(mEncrypted, fPasswordEdit.Text);
    PSGUIChannel.Debounce(TGUIData.Create(gaRender));
  finally
    mEncrypted.Free;
  end;
end;

procedure TGUIStore.InitValues;
begin
  inherited InitValues;
  fFileEdit := Factory2.Locate<IDesignComponentEdit>(NewComposeProps
    .SetStr(cProps.ID, 'guiopenstore_file')
    );
  fPasswordEdit := Factory2.Locate<IDesignComponentEdit>(NewComposeProps
    .SetStr(cProps.ID, 'guiopenstore_password')
    );
  fOpen := Factory2.Locate<IDesignComponentButton>(NewComposeProps
    .SetStr(cProps.ID, 'guiopenstore_open')
    .SetStr(cProps.Text, 'Open')
    );
  fOpen.PSClickChannel.Subscribe(PSClickOpenObserver);
end;

function TGUIStore.DoCompose: IMetaElement;
var
  mBox: IDesignComponent;
begin
  mBox := Factory2.Locate<IDesignComponentVBox>;
  (mBox as INode).AddChild(Morph.WrapUp(fFileEdit, 30, 'File:', 100) as INode);
  (mBox as INode).AddChild(Morph.WrapUp(fPasswordEdit, 30, 'Password:', 100) as INode);
  (mBox as INode).AddChild(Morph.WrapUp(fOpen, 30) as INode);
  Result := mBox.Compose;
end;

function TGUIStore.GetFileEdit: IDesignComponentEdit;
begin
  Result := fFileEdit;
end;

{ TCrypto }

procedure TCrypto.AfterConstruction;
begin
  inherited AfterConstruction;
  fData := TMemoryStream.Create;
end;

procedure TCrypto.BeforeDestruction;
begin
  FreeAndNil(fData);
  inherited BeforeDestruction;
end;

{ TApp }

procedure TApp.RegisterTools;
var
  mReg: TDIReg;
begin
  mReg := DIC.Add(TCryptic, ICryptic, '', ckSingle);
end;

procedure TApp.RegisterGUI;
var
  mReg: TDIReg;
begin
  RegReact.RegisterCommon;
  RegReact.RegisterPubSubLauncher;
  RegApps.RegisterWindowLog;
  mReg := RegReact.RegisterDesignComponent(TGUI, IDesignComponentApp);
  mReg.InjectProp('Store', IPersistStore);
  mReg.InjectProp('SettingsStore', IPersistStore, 'settings');
  mReg.InjectProp('PersistFactory', IPersistFactory);

  mReg := RegReact.RegisterDesignComponent(TGUIStore, IGUIStore);
  mReg.InjectProp('Cryptic', ICryptic);
  mReg.InjectProp('EncryptedStore', IPersistStore, 'encrypted');
  mReg.InjectProp('DecryptedStore', IPersistStore);

end;

procedure TApp.RegisterPersist;
var
  mReg: TDIReg;
begin
  mReg := DIC.Add(TRBData, IRBData);
  //
  mReg := DIC.Add(TSIDList, ISIDList);
  //
  mReg := DIC.Add(TPersistRef, IPersistRef);
  mReg.InjectProp('Store', IPersistStore);
  //
  mReg := DIC.Add(TPersistManyRefs, IPersistManyRefs);
  mReg.InjectProp('Store', IPersistStore);
  //
  mReg := DIC.Add(TPersistRefList, IPersistRefList);
  // persist data
  RegisterDataClass(DIC, TAppSettings);
  RegisterDataClass(DIC, TPassword);
  RegisterDataClass(DIC, TCrypto);
  mReg := DIC.Add(TPersistRef<TAppSettings>, IPersistRef, TAppSettings.ClassName);
  mReg.InjectProp('Store', IPersistStore);
  mReg := DIC.Add(TPersistRef<TPassword>, IPersistRef, TPassword.ClassName);
  mReg.InjectProp('Store', IPersistStore);
  mReg := DIC.Add(TPersistRef<TCrypto>, IPersistRef, TCrypto.ClassName);
  mReg.InjectProp('Store', IPersistStore);
  //
  mReg := DIC.Add(TStoreCache);
  //
  mReg := DIC.Add(TPersistStore, IPersistStore, 'settings', ckSingle);
  mReg.InjectProp('Factory', IPersistFactory);
  mReg.InjectProp('Device', IPersistStoreDevice, 'xml');
  mReg.InjectProp('Cache', TStoreCache);
  //
  mReg := DIC.Add(TPersistStore, IPersistStore, 'encrypted', ckSingle);
  mReg.InjectProp('Factory', IPersistFactory);
  mReg.InjectProp('Device', IPersistStoreDevice, 'xml');
  mReg.InjectProp('Cache', TStoreCache);
  //
  mReg := DIC.Add(TPersistStore, IPersistStore, '', ckSingle);
  mReg.InjectProp('Factory', IPersistFactory);
  mReg.InjectProp('Device', IPersistStoreDevice, 'xml');
  mReg.InjectProp('Cache', TStoreCache);
  //
  mReg := DIC.Add(TXmlStore, IPersistStoreDevice, 'xml');
  mReg.InjectProp('Factory', IPersistFactory);
  //
  mReg := DIC.Add(TPersistFactory, IPersistFactory);
  mReg.InjectProp('Container', TDIContainer);
end;

procedure TApp.RegisterAppServices;
begin
  inherited;
  RegisterTools;
  RegisterPersist;
  RegisterGUI;
end;

end.

