unit uapp;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  forms, fMain, fGroup,
  tal_uapp, tal_uguilauncher, tal_ilauncher,
  tal_uwindowlog,
  trl_ipersist,  trl_upersist, trl_upersiststore,
  trl_dicontainer,
  trl_irttibroker, trl_urttibroker,
  trl_upersistxml,
  trl_ilog, trl_ulazlog,
  tvl_udatabinder, tvl_udatabinders, tvl_utallybinders,
  tvl_ibindings, tal_iedit, tvl_ubehavebinder,
  uPasswords, uSettings,
  SettingsBroker, uSettingsBroker,
  uCryptic, trl_icryptic, fOpen;

type

  { TApp }

  TApp = class(TALApp)
  public const
    cPersistRID = 'PERSIST';
    cCryptoPersistRID = 'CRYPTOPERSIST';
    cSettingsStore = 'SETTINGSSTORE';
  protected
    fPersistDIC: TDIContainer;
    fCryptoPersistDIC: TDIContainer;
    function GetCryptoPersistDIC: TDIContainer;
    function GetPersistDIC: TDIContainer;
    property PersistDIC: TDIContainer read GetPersistDIC;
    property CryptoPersistDIC: TDIContainer read GetCryptoPersistDIC;
  protected
    function CreateMainFormInstance: TObject;
  protected
    procedure RegisterDICs;
    procedure RegisterTools;
    procedure RegisterGUI;
    procedure RegisterPersist;
    procedure RegisterCryptoPersist;
    procedure RegisterAppServices; override;
  end;

implementation

{ TApp }

function TApp.GetCryptoPersistDIC: TDIContainer;
begin
  if fCryptoPersistDIC = nil then begin
    fCryptoPersistDIC := DIC.Locate(TDIContainer, cCryptoPersistRID);
  end;
  Result := fCryptoPersistDIC;
end;

function TApp.GetPersistDIC: TDIContainer;
begin
  if fPersistDIC = nil then begin
    fPersistDIC := DIC.Locate(TDIContainer, cPersistRID);
  end;
  Result := fPersistDIC;
end;

function TApp.CreateMainFormInstance: TObject;
begin
  Application.CreateForm(TOpenForm, Result);
end;

procedure TApp.RegisterDICs;
begin
  DIC.Add(TDIContainer, cPersistRID, ckSingle);
  DIC.Add(TDIContainer, cCryptoPersistRID, ckSingle);
end;

procedure TApp.RegisterTools;
var
  mReg: TDIReg;
begin
  mReg := DIC.Add(TWindowLog, ILog, '', ckSingle);
  //
  mReg := DIC.Add(TCryptic, ICryptic, '', ckSingle);
  mReg.InjectProp('Log', ILog);
end;

procedure TApp.RegisterGUI;
var
  mReg: TDIReg;
begin
  mReg := DIC.Add(TDIOwner, '', ckSingle);
  //
  mReg := DIC.Add(TGUILauncher, ILauncher);
  mReg.InjectProp('MainForm', IMainForm);
  //
  mReg := DIC.Add(TRBBehavioralBinder, IRBBehavioralBinder);
  //
  mReg := DIC.Add(TMainForm, DIC.Locate(TDIOwner), IListData, 'MainForm');
  mReg.InjectProp('Store', IPersistStore, '', PersistDIC);
  mReg.InjectProp('Factory', IPersistFactory, '', PersistDIC);
  mReg.InjectProp('Binder', IRBTallyBinder, 'listbox', PersistDIC);
  mReg.InjectProp('Edit', IEditData, 'GroupForm');
  mReg.InjectProp('SettingsBroker', ISettingsBroker);
  //
  mReg := DIC.Add(TGroupForm, DIC.Locate(TDIOwner), IEditData, 'GroupForm');
  mReg.InjectProp('Binder', IRBDataBinder, '', PersistDIC);
  mReg.InjectProp('BehaveBinder', IRBBehavioralBinder);
  mReg.InjectProp('SettingsBroker', ISettingsBroker);
  mReg.InjectProp('Log', ILog);
  //
  mReg := DIC.Add(CreateMainFormInstance, IMainForm);
  mReg.InjectProp('Factory', IPersistFactory, '', CryptoPersistDIC);
  mReg.InjectProp('EncrytedStore', IPersistStore, '', CryptoPersistDIC);
  mReg.InjectProp('DecrytedStore', IPersistStore, '', PersistDIC);
  mReg.InjectProp('CryptedFile', DataFile);
  mReg.InjectProp('Cryptic', ICryptic);
  mReg.InjectProp('MainForm', IListData, 'MainForm');
  mReg.InjectProp('SettingsBroker', ISettingsBroker);
  //
  mReg := DIC.Add(TSettingsBroker, ISettingsBroker, '', ckSingle);
  mReg.InjectProp('Store', IPersistStore, cSettingsStore, PersistDIC);
  mReg.InjectProp('FileName', SettingsFile);
end;

procedure TApp.RegisterPersist;
var
  mReg: TDIReg;
begin
  mReg := PersistDIC.Add(TRBData, IRBData);
  //
  mReg := PersistDIC.Add(TSIDList, ISIDList);
  //
  mReg := PersistDIC.Add(TPersistRef, IPersistRef);
  mReg.InjectProp('Store', IPersistStore);
  //
  mReg := PersistDIC.Add(TPersistManyRefs, IPersistManyRefs);
  mReg.InjectProp('Store', IPersistStore);
  //
  mReg := PersistDIC.Add(TPersistRefList, IPersistRefList);
  // persist data
  RegisterDataClass(PersistDIC, TPassword);
  RegisterDataClass(PersistDIC, TGroup);
  RegisterDataClass(PersistDIC, TAppSetting);
  RegisterDataClass(PersistDIC, TAppSettingWindow);
  //
  mReg := PersistDIC.Add(TStoreCache);
  //
  mReg := PersistDIC.Add(TPersistStore, IPersistStore, '', ckSingle);
  mReg.InjectProp('Factory', IPersistFactory);
  mReg.InjectProp('Device', IPersistStoreDevice, 'xml');
  mReg.InjectProp('Cache', TStoreCache);
  //
  mReg := PersistDIC.Add(TXmlStore, IPersistStoreDevice, 'xml');
  mReg.InjectProp('Factory', IPersistFactory);
  // factory for persist data(will work on top of cPersistRID container(which is registered in DIC))
  mReg := PersistDIC.Add(TPersistFactory, IPersistFactory);
  mReg.InjectProp('Container', TDIContainer, cPersistRID, DIC);
  // binders(conection between data and GUI)
  mReg := PersistDIC.Add(TListBoxBinder, IRBTallyBinder, 'listbox');
  mReg.InjectProp('Store', IPersistStore);
  mReg.InjectProp('Factory', IPersistFactory);
  //
  mReg := PersistDIC.Add(TDrawGridBinder, IRBTallyBinder, 'drawgrid');
  mReg.InjectProp('Store', IPersistStore);
  mReg.InjectProp('Factory', IPersistFactory);
  //
  mReg := PersistDIC.Add(TRBDataBinder, IRBDataBinder);
  //
  mReg := PersistDIC.Add(TPersistStore, IPersistStore, cSettingsStore);
  mReg.InjectProp('Factory', IPersistFactory);
  mReg.InjectProp('Device', IPersistStoreDevice, 'xml');
  mReg.InjectProp('Cache', TStoreCache);
end;

procedure TApp.RegisterCryptoPersist;
var
  mReg: TDIReg;
begin
  RegisterDataClass(CryptoPersistDIC, TCrypto);
  //
  mReg := CryptoPersistDIC.Add(TStoreCache);
  //
  mReg := CryptoPersistDIC.Add(TPersistRef, IPersistRef);
  mReg.InjectProp('Store', IPersistStore);
  //
  mReg := CryptoPersistDIC.Add(TPersistRefList, IPersistRefList);
  //
  mReg := CryptoPersistDIC.Add(TSIDList, ISIDList);
  //
  mReg := CryptoPersistDIC.Add(TPersistStore, IPersistStore, '', ckSingle);
  mReg.InjectProp('Factory', IPersistFactory);
  mReg.InjectProp('Device', IPersistStoreDevice, 'xml');
  mReg.InjectProp('Cache', TStoreCache);
  //
  mReg := CryptoPersistDIC.Add(TXmlStore, IPersistStoreDevice, 'xml');
  mReg.InjectProp('XMLFile', DataFile);
  mReg.InjectProp('Factory', IPersistFactory);
  // factory for persist data(will work on top of cPersistRID container(which is registered in DIC))
  mReg := CryptoPersistDIC.Add(TPersistFactory, IPersistFactory);
  mReg.InjectProp('Container', TDIContainer, cCryptoPersistRID, DIC);
  //
  mReg := CryptoPersistDIC.Add(TRBDataBinder, IRBDataBinder);
end;

procedure TApp.RegisterAppServices;
begin
  inherited;
  RegisterDICs;
  RegisterTools;
  RegisterPersist;
  RegisterCryptoPersist;
  RegisterGUI;
end;

end.

