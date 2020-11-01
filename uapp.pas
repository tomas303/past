unit uapp;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils,
  forms, fMain, fGroup,
  tal_uapp, tal_uguilauncher, trl_ilauncher,
  tal_uwindowlog,
  trl_ipersist,  trl_upersist, trl_upersiststore,
  trl_dicontainer,
  trl_irttibroker, trl_urttibroker,
  trl_upersistxml,
  trl_ilog, trl_ulazlog,
  tvl_udatabinder, tvl_udatabinders, tvl_utallybinders,
  tvl_ibindings, tal_iedit, tvl_ubehavebinder,
  uPasswords,
  uCryptic, trl_icryptic, fOpen,
  tal_ihistorysettings,
  tvl_imainform;

type

  { TApp }

  TApp = class(TALApp)
  public const
    cPersistRID = 'PERSIST';
    cCryptoPersistRID = 'CRYPTOPERSIST';
    cSettingsRID = 'SETTINGS';
  protected
    fPersistDIC: TDIContainer;
    fCryptoPersistDIC: TDIContainer;
    fSettingsDIC: TDIContainer;
    function GetCryptoPersistDIC: TDIContainer;
    function GetPersistDIC: TDIContainer;
    function GetSettingsDIC: TDIContainer;
    property PersistDIC: TDIContainer read GetPersistDIC;
    property CryptoPersistDIC: TDIContainer read GetCryptoPersistDIC;
    property SettingsDIC: TDIContainer read GetSettingsDIC;
  protected
    function CreateMainFormInstance(const AClass: TClass): TObject;
  protected
    procedure RegisterDICs;
    procedure RegisterTools;
    procedure RegisterSettings;
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

function TApp.GetSettingsDIC: TDIContainer;
begin
  if fSettingsDIC = nil then begin
    fSettingsDIC := DIC.Locate(TDIContainer, cSettingsRID);
  end;
  Result := fSettingsDIC;
end;

function TApp.CreateMainFormInstance(const AClass: TClass): TObject;
begin
  Application.CreateForm(TOpenForm, Result);
end;

procedure TApp.RegisterDICs;
begin
  DIC.Add(TDIContainer, cPersistRID, ckSingle);
  DIC.Add(TDIContainer, cCryptoPersistRID, ckSingle);
  DIC.Add(TDIContainer, cSettingsRID, ckSingle);
end;

procedure TApp.RegisterTools;
var
  mReg: TDIReg;
begin
  mReg := DIC.Add(TWindowLog, ILog, '', ckSingle);
  //
  mReg := DIC.Add(TCryptic, ICryptic, '', ckSingle);
end;

procedure TApp.RegisterSettings;
var
  mReg: TDIReg;
begin
  mReg := SettingsDIC.Add(TStoreCache);
  //
  mReg := SettingsDIC.Add(TRBData, IRBData);
  //
  mReg := SettingsDIC.Add(TSIDList, ISIDList);
  //
  mReg := SettingsDIC.Add(TPersistRef, IPersistRef);
  mReg.InjectProp('Store', IPersistStore);
  //
  mReg := SettingsDIC.Add(TPersistManyRefs, IPersistManyRefs);
  mReg.InjectProp('Store', IPersistStore);
  //
  mReg := SettingsDIC.Add(TPersistRefList, IPersistRefList);
  //
  mReg := SettingsDIC.Add(TPersistStore, IPersistStore, '', ckSingle);
  mReg.InjectProp('Factory', IPersistFactory);
  mReg.InjectProp('Device', IPersistStoreDevice);
  mReg.InjectProp('Cache', TStoreCache);
  //
  mReg := SettingsDIC.Add(TXmlStore, IPersistStoreDevice);
  mReg.InjectProp('XMLFile', SettingsFile);
  mReg.InjectProp('Factory', IPersistFactory);
  //
  mReg := SettingsDIC.Add(TPersistFactory, IPersistFactory);
  mReg.InjectProp('Container', TDIContainer, cSettingsRID, DIC);
  //
  RegisterHistorySettings(SettingsDIC);
end;

procedure TApp.RegisterGUI;
var
  mReg: TDIReg;
begin
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
  mReg.InjectProp('HistorySettings', IHistorySettings, '', SettingsDIC);
  //
  mReg := DIC.Add(TGroupForm, DIC.Locate(TDIOwner), IEditData, 'GroupForm');
  mReg.InjectProp('Binder', IRBDataBinder, '', PersistDIC);
  mReg.InjectProp('BehaveBinder', IRBBehavioralBinder);
  mReg.InjectProp('HistorySettings', IHistorySettings, '', SettingsDIC);
  //
  mReg := DIC.Add(CreateMainFormInstance, IMainForm);
  mReg.InjectProp('Factory', IPersistFactory, '', CryptoPersistDIC);
  mReg.InjectProp('EncrytedStore', IPersistStore, '', CryptoPersistDIC);
  mReg.InjectProp('DecrytedStore', IPersistStore, '', PersistDIC);
  mReg.InjectProp('CryptedFile', DataFile);
  mReg.InjectProp('Cryptic', ICryptic);
  mReg.InjectProp('MainForm', IListData, 'MainForm');
  mReg.InjectProp('HistorySettings', IHistorySettings, '', SettingsDIC);
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
  RegisterSettings;
  RegisterPersist;
  RegisterCryptoPersist;
  RegisterGUI;
end;

end.

