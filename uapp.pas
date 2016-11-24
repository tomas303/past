unit uapp;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  forms, fMain, fGroup,
  tal_uapp, tal_uguilauncher, tal_ilauncher,
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
    function CreateMainFormInstance: TObject;
  protected
    procedure RegisterTools;
    procedure RegisterGUI;
    procedure RegisterPersist;
    procedure RegisterCryptoPersist;
    procedure RegisterAppServices; override;
  end;

implementation

{ TApp }

function TApp.CreateMainFormInstance: TObject;
begin
  Application.CreateForm(TOpenForm, Result);
end;

procedure TApp.RegisterTools;
var
  mReg: TDIReg;
begin
  mReg := DIC.Add(TLazLog, ILog, '', ckSingle);
  mReg.InjectProp('LogName', LogFile);
  mReg.InjectProp('UseStdOut', False);
  mReg.InjectProp('CloseLogFileBetweenWrites', True);
  //
  mReg := DIC.Add(TCryptic, ICryptic, '', ckSingle);
  mReg.InjectProp('Log', ILog);
end;

procedure TApp.RegisterGUI;
var
  mReg: TDIReg;
  mPersistDIC: TDIContainer;
  mCryptoPersistDIC: TDIContainer;
begin
  mPersistDIC := DIC.Locate(TDIContainer, cPersistRID);
  //
  mReg := DIC.Add(TDIOwner, '', ckSingle);
  //
  mReg := DIC.Add(TGUILauncher, ILauncher);
  mReg.InjectProp('MainForm', IMainForm);
  //
  mReg := DIC.Add(TRBBehavioralBinder, IRBBehavioralBinder);
  //
  mReg := DIC.Add(TMainForm, DIC.Locate(TDIOwner), IListData, 'MainForm');
  mReg.InjectProp('Store', IPersistStore, '', mPersistDIC);
  mReg.InjectProp('Factory', IPersistFactory, '', mPersistDIC);
  mReg.InjectProp('Binder', IRBTallyBinder, 'listbox', mPersistDIC);
  mReg.InjectProp('Edit', IEditData, 'GroupForm');
  mReg.InjectProp('SettingsBroker', ISettingsBroker);
  //
  mReg := DIC.Add(TGroupForm, DIC.Locate(TDIOwner), IEditData, 'GroupForm');
  mReg.InjectProp('Binder', IRBDataBinder, '', mPersistDIC);
  mReg.InjectProp('BehaveBinder', IRBBehavioralBinder);
  mReg.InjectProp('SettingsBroker', ISettingsBroker);
  mReg.InjectProp('Log', ILog);
  //
  mCryptoPersistDIC := DIC.Locate(TDIContainer, cCryptoPersistRID);
  //
  mReg := DIC.Add(CreateMainFormInstance, IMainForm);
  mReg.InjectProp('Factory', IPersistFactory, '', mCryptoPersistDIC);
  mReg.InjectProp('EncrytedStore', IPersistStore, '', mCryptoPersistDIC);
  mReg.InjectProp('DecrytedStore', IPersistStore, '', mPersistDIC);
  mReg.InjectProp('CryptedFile', DataFile);
  mReg.InjectProp('Cryptic', ICryptic);
  mReg.InjectProp('MainForm', IListData, 'MainForm');
  mReg.InjectProp('SettingsBroker', ISettingsBroker);
  //
  mReg := DIC.Add(TSettingsBroker, ISettingsBroker, '', ckSingle);
  mReg.InjectProp('Store', IPersistStore, cSettingsStore, mPersistDIC);
  mReg.InjectProp('FileName', SettingsFile);
end;

procedure TApp.RegisterPersist;
var
  mReg: TDIReg;
  mDIC: TDIContainer;
begin
  // single persist conatiner
  mReg := DIC.Add(TDIContainer, cPersistRID, ckSingle);
  mDIC := DIC.Locate(TDIContainer, cPersistRID);
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
  RegisterDataClass(mDIC, TAppSetting);
  RegisterDataClass(mDIC, TAppSettingWindow);
  //
  mReg := mDIC.Add(TStoreCache);
  //
  mReg := mDIC.Add(TPersistStore, IPersistStore, '', ckSingle);
  mReg.InjectProp('Factory', IPersistFactory);
  mReg.InjectProp('Device', IPersistStoreDevice, 'xml');
  mReg.InjectProp('Cache', TStoreCache);
  //
  mReg := mDIC.Add(TXmlStore, IPersistStoreDevice, 'xml');
  mReg.InjectProp('Factory', IPersistFactory);
  // factory for persist data(will work on top of cPersistRID container(which is registered in DIC))
  mReg := mDIC.Add(TPersistFactory, IPersistFactory);
  mReg.InjectProp('Container', TDIContainer, cPersistRID, DIC);
  // binders(conection between data and GUI)
  mReg := mDIC.Add(TListBoxBinder, IRBTallyBinder, 'listbox');
  mReg.InjectProp('Store', IPersistStore);
  mReg.InjectProp('Factory', IPersistFactory);
  //
  mReg := mDIC.Add(TDrawGridBinder, IRBTallyBinder, 'drawgrid');
  mReg.InjectProp('Store', IPersistStore);
  mReg.InjectProp('Factory', IPersistFactory);
  //
  mReg := mDIC.Add(TRBDataBinder, IRBDataBinder);
  //
  mReg := mDIC.Add(TPersistStore, IPersistStore, cSettingsStore);
  mReg.InjectProp('Factory', IPersistFactory);
  mReg.InjectProp('Device', IPersistStoreDevice, 'xml');
  mReg.InjectProp('Cache', TStoreCache);
end;

procedure TApp.RegisterCryptoPersist;
var
  mReg: TDIReg;
  mDIC: TDIContainer;
begin
  // single persist conatiner for crypted data
  mReg := DIC.Add(TDIContainer, cCryptoPersistRID, ckSingle);
  mDIC := DIC.Locate(TDIContainer, cCryptoPersistRID);
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
  mReg.InjectProp('XMLFile', DataFile);
  mReg.InjectProp('Factory', IPersistFactory);
  // factory for persist data(will work on top of cPersistRID container(which is registered in DIC))
  mReg := mDIC.Add(TPersistFactory, IPersistFactory);
  mReg.InjectProp('Container', TDIContainer, cCryptoPersistRID, DIC);
  //
  mReg := mDIC.Add(TRBDataBinder, IRBDataBinder);
end;

procedure TApp.RegisterAppServices;
begin
  inherited;
  RegisterTools;
  RegisterPersist;
  RegisterCryptoPersist;
  RegisterGUI;
end;

end.

