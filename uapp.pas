unit uapp;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, tvl_uapplaunch,
  forms, fMain, fGroup,
  trl_ipersist,  trl_upersist, trl_upersiststore,
  trl_dicontainer,
  trl_irttibroker, trl_urttibroker,
  trl_upersistxml,
  tvl_udatabinder, tvl_udatabinders, tvl_utallybinders,
  tvl_ibindings, tvl_iedit, tvl_ubehavebinder,
  uPasswords, uSettings,
  uCryptic, trl_icryptic, fOpen;

type

  { TKicker }

  TKicker = class(TInterfacedObject, IGUIKicker)
  private
    fMainForm: IMainForm;
  protected
    procedure StartUp;
    procedure ShutDown;
    function GetMainForm: IMainForm;
  published
    property MainForm: IMainForm read fMainForm write fMainForm;
  end;

  { TApp }

  TApp = class
  public const
    cPersistRID = 'PERSIST';
    cCryptoPersistRID = 'CRYPTOPERSIST';
    cSettingsStore = 'SETTINGSSTORE';
  private
    fDIC: TDIContainer;
    fDataFile: string;
    fSettingsFile: string;
  protected
    procedure InjectPersistRef(const AItem: IRBDataItem);
    procedure Setup;
    procedure RegisterDataClass(ADIC: TDIContainer; AClass: TClass);
    procedure RegisterGUI;
    procedure RegisterPersist;
    procedure RegisterCryptoPersist;
    procedure RegisterServices;
    procedure Launch;
  public
    constructor Create;
    destructor Destroy; override;
    class procedure Go;
  end;

implementation

{ TKicker }

procedure TKicker.StartUp;
begin
  MainForm.StartUp;
end;

procedure TKicker.ShutDown;
begin
  MainForm.ShutDown;
end;

function TKicker.GetMainForm: IMainForm;
begin
  Result := fMainForm;
end;

{ TApp }

procedure TApp.InjectPersistRef(const AItem: IRBDataItem);
var
  mPersistDIC: TDIContainer;
begin
  if AItem.IsInterface and Supports(AItem.AsInterface, IPersistRef) then
  begin
    // IPersistRef need resolve data via Store
    mPersistDIC := fDIC.Locate(TDIContainer, cPersistRID);
    (AItem.AsInterface as IPersistRef).Store := mPersistDIC.Locate(IPersistStore);
  end
  else
  if AItem.IsInterface and Supports(AItem.AsInterface, IPersistManyRefs) then
  begin
    // need to create IPersistRef members
    mPersistDIC := fDIC.Locate(TDIContainer, cPersistRID);
    (AItem.AsInterface as IPersistManyRefs).Factory := mPersistDIC.Locate(IPersistFactory, cPersistRID);
  end;
end;

procedure TApp.Setup;
var
  mAppDir, mSubdir, mExt: string;
begin
  if Paramcount > 0 then
    mAppDir := ParamStr(1)
  else
  begin
    mSubdir := '.' + ExtractFileName(ParamStr(0));
    mExt := ExtractFileExt(ParamStr(0));
    mSubDir := copy(mSubDir, 1, Length(mSubdir) - Length(mExt));
    {$IFDEF UNIX}
    mAppDir := GetEnvironmentVariable('HOME') + PathDelim + mSubdir + PathDelim;
    {$ENDIF UNIX}
    {$IFDEF WINDOWS}
    mAppDir := GetEnvironmentVariable('APPDATA') + PathDelim + mSubdir + PathDelim;
    {$ENDIF WINDOWS}
  end;
  if not DirectoryExists(mAppDir) then
  begin
    if not ForceDirectories(mAppDir) then
      raise Exception.Create('Cannot create directory ' + mAppDir);
  end;
  fDataFile := mAppDir + 'data.xml';
  fSettingsFile := mAppDir + 'settings.xml';
end;

procedure TApp.RegisterDataClass(ADIC: TDIContainer; AClass: TClass);
var
  mReg: TDIReg;
begin
  // persist class
  mReg := ADIC.Add(AClass);
  mReg.InjectProp('', InjectPersistRef);
  // data envelop for persist class
  mReg := ADIC.Add(TRBData, IRBData, AClass.ClassName);
  mReg.InjectProp('UnderObject', AClass);
end;

procedure TApp.RegisterGUI;
var
  mReg: TDIReg;
  mPersistDIC: TDIContainer;
  mCryptoPersistDIC: TDIContainer;
begin
  mPersistDIC := fDIC.Locate(TDIContainer, cPersistRID);
  //
  mReg := fDIC.Add(TDIOwner, '', ckSingle);
  //
  mReg := fDIC.Add(TGUILauncher, '', ckSingle);
  mReg.InjectProp('Kicker', IGUIKicker);
  //
  mReg := fDIC.Add(TRBBehavioralBinder, IRBBehavioralBinder);
  //
  mReg := fDIC.Add(TKicker, IGUIKicker);
  mReg.InjectProp('MainForm', IMainForm);
  //
  mReg := fDIC.Add(TMainForm, fDIC.Locate(TDIOwner), IListData, 'MainForm');
  mReg.InjectProp('Store', IPersistStore, '', mPersistDIC);
  mReg.InjectProp('Factory', IPersistFactory, cPersistRID, mPersistDIC);
  mReg.InjectProp('Binder', IRBTallyBinder, 'listbox', mPersistDIC);
  mReg.InjectProp('Edit', IEditData, 'GroupForm');
  //
  mReg := fDIC.Add(TGroupForm, fDIC.Locate(TDIOwner), IEditData, 'GroupForm');
  mReg.InjectProp('Binder', IRBDataBinder, '', mPersistDIC);
  mReg.InjectProp('BehaveBinder', IRBBehavioralBinder);
  //
  mReg := fDIC.Add(TCryptic, ICryptic, '', ckSingle);
  //
  mCryptoPersistDIC := fDIC.Locate(TDIContainer, cCryptoPersistRID);
  //
  mReg := fDIC.Add(TOpenForm, fDIC.Locate(TDIOwner), IMainForm);
  mReg.InjectProp('Factory', IPersistFactory, '', mCryptoPersistDIC);
  mReg.InjectProp('EncrytedStore', IPersistStore, '', mCryptoPersistDIC);
  mReg.InjectProp('DecrytedStore', IPersistStore, '', mPersistDIC);
  mReg.InjectProp('CryptedFile', fDataFile);
  mReg.InjectProp('Cryptic', ICryptic);
  mReg.InjectProp('MainForm', IListData, 'MainForm');
  mReg.InjectProp('Settings', IPersistStore, cSettingsStore, mPersistDIC);
  mReg.InjectProp('SettingsFile', fSettingsFile);
end;

procedure TApp.RegisterPersist;
var
  mReg: TDIReg;
  mDIC: TDIContainer;
begin
  // single persist conatiner
  mReg := fDIC.Add(TDIContainer, cPersistRID, ckSingle);
  mDIC := fDIC.Locate(TDIContainer, cPersistRID);
  //
  mReg := mDIC.Add(TRBData, IRBData);
  //
  mReg := mDIC.Add(TSIDList, ISIDList);
  //
  mReg := mDIC.Add(TPersistRef, IPersistRef);
  mReg.InjectProp('Store', IPersistStore);
  //
  mReg := mDIC.Add(TPersistManyRefs, IPersistManyRefs);
  mReg.InjectProp('Store', IPersistStore);
  //
  mReg := mDIC.Add(TPersistRefList, IPersistRefList);
  // persist data
  RegisterDataClass(mDIC, TPassword);
  RegisterDataClass(mDIC, TGroup);
  RegisterDataClass(mDIC, TAppSettingDataFile);
  RegisterDataClass(mDIC, TAppSetting);
  //
  mReg := mDIC.Add(TStoreCache);
  //
  mReg := mDIC.Add(TPersistStore, IPersistStore, '', ckSingle);
  mReg.InjectProp('Factory', IPersistFactory, cPersistRID);
  mReg.InjectProp('Device', IPersistStoreDevice, 'xml');
  mReg.InjectProp('Cache', TStoreCache);
  //
  mReg := mDIC.Add(TXmlStore, IPersistStoreDevice, 'xml');
  mReg.InjectProp('Factory', IPersistFactory, cPersistRID);
  // factory for persist data(will work on top of cPersistRID container(which is registered in fDIC))
  mReg := mDIC.Add(TPersistFactory, IPersistFactory, cPersistRID);
  mReg.InjectProp('Container', TDIContainer, cPersistRID, fDIC);
  // binders(conection between data and GUI)
  mReg := mDIC.Add(TListBoxBinder, IRBTallyBinder, 'listbox');
  mReg.InjectProp('Store', IPersistStore);
  mReg.InjectProp('Factory', IPersistFactory, cPersistRID);
  //
  mReg := mDIC.Add(TDrawGridBinder, IRBTallyBinder, 'drawgrid');
  mReg.InjectProp('Store', IPersistStore);
  mReg.InjectProp('Factory', IPersistFactory, cPersistRID);
  //
  mReg := mDIC.Add(TRBDataBinder, IRBDataBinder);
  //
  mReg := mDIC.Add(TPersistStore, IPersistStore, cSettingsStore);
  mReg.InjectProp('Factory', IPersistFactory, cPersistRID);
  mReg.InjectProp('Device', IPersistStoreDevice, 'xml');
  mReg.InjectProp('Cache', TStoreCache);
end;

procedure TApp.RegisterCryptoPersist;
var
  mReg: TDIReg;
  mDIC: TDIContainer;
begin
  // single persist conatiner for crypted data
  mReg := fDIC.Add(TDIContainer, cCryptoPersistRID, ckSingle);
  mDIC := fDIC.Locate(TDIContainer, cCryptoPersistRID);
  //
  RegisterDataClass(mDIC, TCrypto);
  //
  mReg := mDIC.Add(TStoreCache);
  //
  mReg := mDIC.Add(TPersistRef, IPersistRef);
  mReg.InjectProp('Store', IPersistStore);
  //
  mReg := mDIC.Add(TPersistRefList, IPersistRefList);
  //
  mReg := mDIC.Add(TSIDList, ISIDList);
  //
  mReg := mDIC.Add(TPersistStore, IPersistStore, '', ckSingle);
  mReg.InjectProp('Factory', IPersistFactory);
  mReg.InjectProp('Device', IPersistStoreDevice, 'xml');
  mReg.InjectProp('Cache', TStoreCache);
  //
  mReg := mDIC.Add(TXmlStore, IPersistStoreDevice, 'xml');
  mReg.InjectProp('XMLFile', fDataFile);
  mReg.InjectProp('Factory', IPersistFactory);
  // factory for persist data(will work on top of cPersistRID container(which is registered in fDIC))
  mReg := mDIC.Add(TPersistFactory, IPersistFactory);
  mReg.InjectProp('Container', TDIContainer, cCryptoPersistRID, fDIC);
  //
  mReg := mDIC.Add(TRBDataBinder, IRBDataBinder);
end;

procedure TApp.RegisterServices;
begin
  RegisterPersist;
  RegisterCryptoPersist;
  RegisterGUI;
end;

procedure TApp.Launch;
var
  mGUILauncher: TGUILauncher;
begin
  mGUILauncher := fDIC.Locate(TGUILauncher);
  mGUILauncher.Launch;
end;

constructor TApp.Create;
begin
  fDIC := TDIContainer.Create;
end;

destructor TApp.Destroy;
begin
  FreeAndNil(fDIC);
  inherited Destroy;
end;

class procedure TApp.Go;
var
  mApp: TApp;
begin
  mApp := TApp.Create;
  try
    mApp.Setup;
    mApp.RegisterServices;
    mApp.Launch;
  finally
    mApp.Free;
  end;
end;

end.

