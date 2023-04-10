(******************************************************************************
* Copyright (C) 2023 Tomáš Horák
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)
unit uapp;

{$mode delphi}{$H+}{$M+}
{$ModeSwitch functionreferences}
{$ModeSwitch anonymousfunctions}

interface

uses
  graphics, Classes, tal_uapp,
  rea_idesigncomponent, rea_udesigncomponent, rea_ilayout,
  trl_imetaelement, trl_iprops, trl_dicontainer, trl_itree,
  trl_pubsub, rea_ibits, trl_ilauncher,
  trl_urttibroker, trl_irttibroker,
  trl_ipersist, trl_upersist,
  trl_upersistxml,
  rea_idata, sysutils,
  trl_icryptic, uCryptic,
  trl_usystem,
  trl_udifactory;

type

  { TApp }

  TApp = class(TALApp)
  protected
    procedure RegisterTools;
    procedure RegisterGUI;
    procedure RegisterPersist;
    procedure RegisterAppServices; override;
  end;


  { TAppSettingsForm }

  TAppSettingsForm = class(TPlainObject)
  private
    fTop: Integer;
    fLeft: Integer;
    fWidth: Integer;
    fHeight: Integer;
  published
    property Top: Integer read fTop write fTop;
    property Left: Integer read fLeft write fLeft;
    property Width: Integer read fWidth write fWidth;
    property Height: Integer read fHeight write fHeight;
  end;

  { TAppSettings }

  TAppSettings = class(TPlainObject)
  private
    fID: String;
    fOpenForm: TAppSettingsForm;
    fEditForm: TAppSettingsForm;
    fLastOpenedFile: String;
  published
    [PersistIDAttribute, PersistAUTOAttribute]
    property ID: String read fID write fID;
    property OpenForm: TAppSettingsForm read fOpenForm write fOpenForm;
    property EditForm: TAppSettingsForm read fEditForm write fEditForm;
    property LastOpenedFile: String read fLastOpenedFile write fLastOpenedFile;
  end;


  { IDataListAppSettings }

  IDataListAppSettings = interface(IMiniDataList<TAppSettings>)
  ['{C3947554-8939-40B2-8EDB-720A4E089170}']
  end;

  { TDataListAppSettings }

  TDataListAppSettings = class(TMiniDataList<TAppSettings>, IDataListAppSettings)
  end;

  { TTag }

  TTag = class
  private
    fVal: String;
  published
    property Val: String read fVal write fVal;
  end;

  { IListTags }

  IListTags = interface(IMiniList<TTag>)
  ['{9645123C-1BD7-4113-85EB-E3664B1610E9}']
  end;

  { TListTags }

  TListTags = class(TMiniList<TTag>, IListTags)
  end;

  { TPassword }

  TPassword = class
  private
    fID: String;
    fLogin: string;
    fPassword: string;
    fLink: string;
    fRemark: string;
    fTags: IListTags;
  published
    [PersistIDAttribute, PersistAUTOAttribute]
    property ID: String read fID write fID;
    property Login: string read fLogin write fLogin;
    property Password: string read fPassword write fPassword;
    property Link: string read fLink write fLink;
    property Remark: string read fRemark write fRemark;
    property Tags: IListTags read fTags write fTags;
  end;

  { IDataListPassword }

  IDataListPassword = interface(IMiniDataList<TPassword>)
  ['{35164555-97D5-4447-A762-8565C39C076E}']
  end;

  { TDataListPassword }

  TDataListPassword = class(TMiniDataList<TPassword>, IDataListPassword)
  end;

  { TCrypto }

  TCrypto = class
  private
    fID: String;
    fData: TMemoryStream;
  public
    procedure BeforeDestruction; override;
  published
    [PersistIDAttribute, PersistAUTOAttribute]
    property ID: String read fID write fID;
    property Data: TMemoryStream read fData write fData;
  end;

  { IDataListCrypto }

  IDataListCrypto = interface(IMiniDataList<TCrypto>)
  ['{A8C74445-4478-4C28-95DC-13B48A638A00}']
  end;

  { TDataListCrypto }

  TDataListCrypto = class(TMiniDataList<TCrypto>, IDataListCrypto)
  end;

  { IGUIStore }

  IGUIStore = interface(IDesignComponent)
  ['{7B5E661F-B0BD-4A09-B665-D126B19F4088}']
    procedure Close;
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
    procedure Close;
  protected
    fUsedKey: String;
    fCryptic: ICryptic;
    fEncryptedStore: IPersistStoreDevice;
    fDecryptedStore: IPersistStoreDevice;
    fPSGUIChannel: IPSGUIChannel;
  published
    property Cryptic: ICryptic read fCryptic write fCryptic;
    property EncryptedStore: IPersistStoreDevice read fEncryptedStore write fEncryptedStore;
    property DecryptedStore: IPersistStoreDevice read fDecryptedStore write fDecryptedStore;
    property PSGUIChannel: IPSGUIChannel read fPSGUIChannel write fPSGUIChannel;
  end;

  { IGUIPasswords }

  IGUIPasswords = interface(IDesignComponent)
  ['{4C326C46-CFB0-469D-BCAF-7819BA5BF2C2}']
    function GetFilter: IDesignComponentFilter;
    function GetLoginEdit: IDesignComponentEdit;
    function GetPasswordEdit: IDesignComponentEdit;
    function GetLinkEdit: IDesignComponentEdit;
    function GetRemarkEdit: IDesignComponentMemo;
    function GetGrid: IDesignComponentGrid;
    function GetTags: IDesignComponentGrid;
    property Filter: IDesignComponentFilter read GetFilter;
    property LoginEdit: IDesignComponentEdit read GetLoginEdit;
    property PasswordEdit: IDesignComponentEdit read GetPasswordEdit;
    property LinkEdit: IDesignComponentEdit read GetLinkEdit;
    property RemarkEdit: IDesignComponentMemo read GetRemarkEdit;
    property Grid: IDesignComponentGrid read GetGrid;
    property Tags: IDesignComponentGrid read GetTags;
  end;

  { TGUIPasswords }

  TGUIPasswords = class(TDesignComponent, IGUIPasswords)
  private
    fFilter: IDesignComponentFilter;
    fLoginEdit: IDesignComponentEdit;
    fPasswordEdit: IDesignComponentEdit;
    fLinkEdit: IDesignComponentEdit;
    fRemarkEdit: IDesignComponentMemo;
    fGrid: IDesignComponentGrid;
    fTags: IDesignComponentGrid;
  protected
    procedure InitValues; override;
    function DoCompose: IMetaElement; override;
    function GetFilter: IDesignComponentFilter;
    function GetLoginEdit: IDesignComponentEdit;
    function GetPasswordEdit: IDesignComponentEdit;
    function GetLinkEdit: IDesignComponentEdit;
    function GetRemarkEdit: IDesignComponentMemo;
    function GetGrid: IDesignComponentGrid;
    function GetTags: IDesignComponentGrid;
  protected
    fPSGUIChannel: IPSGUIChannel;
  published
    property PSGUIChannel: IPSGUIChannel read fPSGUIChannel write fPSGUIChannel;
  end;


  TGUI = class(TDesignComponent, IDesignComponentApp)
  private
    fAppSettings: IRBData;
    function GetAppSettings: IRBData;
    procedure PublishAppSettings;
  private
    fDataConnector: IDataConnector;
    fTagsConnector: IDataConnector;
    fForm: IDesignComponentForm;
    fOpenStore: IGUIStore;
    fPasswords: IGUIPasswords;
    fIsOpened: Boolean;
    function NewForm(const ADCs: TArray<IDesignComponent>): IDesignComponentForm;
    function NewLogButton: IDesignComponentButton;
    function NewStore: IGUIStore;
    function NewPasswords: IGUIPasswords;
    procedure CreateComponents;
  private
    fPasswordsData: IDataListPassword;
    function GetPasswordsData: IDataListPassword;
    procedure CreateDataConnectors;
  private
    procedure PSSizeObserver(const AValue: TSizeData);
    procedure PSPositionObserver(const AValue: TPositionData);
    procedure PSCloseProgramObserver;
    procedure PSShowLogObserver;
    procedure PSTextFilterChannelObserver(const AValue: String);
  protected
    procedure InitValues; override;
    function DoCompose: IMetaElement; override;
  protected
    fStore: IPersistStoreDevice;
    fSettingsStore: IPersistStoreDevice;
    fPersistFactory: IPersistFactory;
    fPSGUIChannel: IPSGUIChannel;
  published
    property Store: IPersistStoreDevice read fStore write fStore;
    property SettingsStore: IPersistStoreDevice read fSettingsStore write fSettingsStore;
    property PersistFactory: IPersistFactory read fPersistFactory write fPersistFactory;
    property PSGUIChannel: IPSGUIChannel read fPSGUIChannel write fPSGUIChannel;
  end;

implementation

{ TAppSettings }

{ TGUIPasswords }

procedure TGUIPasswords.InitValues;
begin
  inherited InitValues;
  fFilter := Factory2.Locate<IDesignComponentFilter>(NewComposeProps
    .SetInt('Interval', 300)
    );
  fLoginEdit := Factory2.Locate<IDesignComponentEdit>(NewComposeProps
    .SetStr(cProps.ID, 'passwords_login')
    );
  fPasswordEdit := Factory2.Locate<IDesignComponentEdit>(NewComposeProps
    .SetStr(cProps.ID, 'passwords_password')
    );
  fLinkEdit := Factory2.Locate<IDesignComponentEdit>(NewComposeProps
    .SetStr(cProps.ID, 'passwords_link')
    );
  fRemarkEdit := Factory2.Locate<IDesignComponentMemo>(NewComposeProps
    .SetStr(cProps.ID, 'passwords_remark')
    );
  fGrid := Factory2.Locate<IDesignComponentGrid>(NewComposeProps
    .SetStr(cProps.ID, 'passwords_grid')
    .SetIntf('PSGUIChannel', PSGUIChannel)
    .SetInt(cGrid.RowCount, 30)
    .SetInt(cGrid.ColCount, 2)
    .SetInt(cGrid.RowMMHeight, 25)
    .SetInt(cGrid.LaticeColColor, clBlack)
    .SetInt(cGrid.LaticeRowColor, clBlack)
    .SetInt(cGrid.LaticeColSize, 2)
    .SetInt(cGrid.LaticeRowSize, 2)
    );
  fTags := Factory2.Locate<IDesignComponentGrid>(NewComposeProps
      .SetStr(cProps.ID, 'tags_grid')
      .SetIntf('PSGUIChannel', PSGUIChannel)
      .SetBool(cProps.Transparent, SelfProps.AsBool(cProps.Transparent))
      .SetInt(cProps.Color, SelfProps.AsInt(cProps.Color))
      .SetInt(cGrid.RowCount, 5)
      .SetInt(cGrid.ColCount, 1)
      .SetInt(cGrid.RowMMHeight, 25)
      .SetInt(cGrid.LaticeColColor, clBlack)
      .SetInt(cGrid.LaticeRowColor, clBlack)
      .SetInt(cGrid.LaticeColSize, 1)
      .SetInt(cGrid.LaticeRowSize, 1)
      );
end;

function TGUIPasswords.DoCompose: IMetaElement;
var
  mBoxEdit: IDesignComponent;
  mBox: IDesignComponent;
  mAll: IDesignComponent;
begin
  mBoxEdit := Factory2.Locate<IDesignComponentVBox>;
  (mBoxEdit as INode).AddChild(Morph.WrapUp(fLinkEdit, 30, 'Link:', 100) as INode);
  (mBoxEdit as INode).AddChild(Morph.WrapUp(fRemarkEdit, 200, 'Remark:', 100) as INode);
  (mBoxEdit as INode).AddChild(Morph.WrapUpElastic(fTags, 'Tags', 100) as INode);
  mBox := Factory2.Locate<IDesignComponentHBox>;
  (mBox as INode).AddChild(fGrid as INode);
  (mBox as INode).AddChild(mBoxEdit as INode);
  //Result := mBox.Compose;
  mAll := Factory2.Locate<IDesignComponentVBox>;
  (mAll as INode).AddChild(Morph.WrapUp(fFilter, 30, 'Filter:', 50) as INode);
  (mAll as INode).AddChild(mBox as INode);
  Result := mAll.Compose;
end;

function TGUIPasswords.GetFilter: IDesignComponentFilter;
begin
  Result := fFilter;
end;

function TGUIPasswords.GetLoginEdit: IDesignComponentEdit;
begin
  Result := fLoginEdit;
end;

function TGUIPasswords.GetPasswordEdit: IDesignComponentEdit;
begin
  Result := fPasswordEdit;
end;

function TGUIPasswords.GetLinkEdit: IDesignComponentEdit;
begin
  Result := fLinkEdit;
end;

function TGUIPasswords.GetRemarkEdit: IDesignComponentMemo;
begin
  Result := fRemarkEdit;
end;

function TGUIPasswords.GetGrid: IDesignComponentGrid;
begin
  Result := fGrid;
end;

function TGUIPasswords.GetTags: IDesignComponentGrid;
begin
  Result := fTags;
end;

{ TGUI }

function TGUI.GetAppSettings: IRBData;
var
  mEnum: IRBDataEnumerator;
  mF: TAppSettingsForm;
begin
  mEnum := SettingsStore.Select2(TAppSettings.ClassName).GetEnumerator;
  if mEnum.MoveNext then
    Result := mEnum.Current
  else begin
    Result := PersistFactory.Create(IRBData, TAppSettings.ClassName) as IRBData;
    mF := (Result.UnderObject as TAppSettings).OpenForm;
    mF.Width := 600;
    mF.Height := 200;
    mF.Left := 300;
    mF.Top := 400;
    mF := (Result.UnderObject as TAppSettings).EditForm;
    mF.Width := 600;
    mF.Height := 600;
    mF.Left := 300;
    mF.Top := 400;
  end;
end;


procedure TGUI.PublishAppSettings;
var
  mF: TAppSettingsForm;
begin
  if fStore.IsOpened then begin
    mF := (fAppSettings.UnderObject as TAppSettings).EditForm;
    fForm.PSSizeChannel.Debounce(TSizeData.Create(Self, mF.Width, mF.Height));
    fForm.PSPositionChannel.Debounce(TPositionData.Create(Self, mF.Left, mF.Top));
  end else begin
    mF := (fAppSettings.UnderObject as TAppSettings).OpenForm;
    fForm.PSSizeChannel.Debounce(TSizeData.Create(Self, mF.Width, mF.Height));
    fForm.PSPositionChannel.Debounce(TPositionData.Create(Self, mF.Left, mF.Top));
    fOpenStore.FileEdit.PSTextChannel.Debounce((fAppSettings.UnderObject as TAppSettings).LastOpenedFile);
  end;
end;

function TGUI.NewForm(const ADCs: TArray<IDesignComponent>): IDesignComponentForm;
var
  mDC: IDesignComponent;
begin
  Result := Factory2.Locate<IDesignComponentForm>(NewProps
    .SetStr(cProps.ID, 'mainform')
    .SetIntf('PSGUIChannel', fPSGUIChannel)
    .SetStr(cProps.Caption, 'past ... password storage')
    .SetInt(cProps.Layout, cLayout.Vertical)
    );
  Result.PSSizeChannel.Subscribe(PSSizeObserver);
  Result.PSPositionChannel.Subscribe(PSPositionObserver);
  Result.PSCloseChannel.Subscribe(PSCloseProgramObserver);
  for mDC in ADCs do begin
    (Result as INode).AddChild(mDC as INode);
  end;
end;

function TGUI.NewLogButton: IDesignComponentButton;
begin
  Result := Factory2.Locate<IDesignComponentButton>(NewProps
    .SetInt(cProps.Place, cPlace.FixFront)
    .SetInt(cProps.PlaceSize, 50)
    .SetStr(cProps.Text, 'SHOW LOG')
  );
  Result.PSClickChannel.Subscribe(PSShowLogObserver);
end;

function TGUI.NewStore: IGUIStore;
begin
  Result := Factory2.Locate<IGUIStore>(NewProps.SetIntf('PSGUIChannel', PSGUIChannel));
end;

function TGUI.NewPasswords: IGUIPasswords;
begin
  Result := Factory2.Locate<IGUIPasswords>(NewProps.SetIntf('PSGUIChannel', PSGUIChannel));
  Result.Filter.PSTextFilterChannel.Subscribe(PSTextFilterChannelObserver);
end;

procedure TGUI.CreateComponents;
begin
  fOpenStore := NewStore;
  fPasswords := NewPasswords;
  fForm := NewForm([]);
end;

function TGUI.GetPasswordsData: IDataListPassword;
begin
  Result := Factory2.Locate<IDataListPassword>;
  Result.Load;
end;

procedure TGUI.CreateDataConnectors;
begin
  fDataConnector := Factory2.Locate<IDataConnector>('TStoreConnector');
  fDataConnector.RegisterEdit('Login', fPasswords.LoginEdit);
  fDataConnector.RegisterEdit('Password', fPasswords.PasswordEdit);
  fDataConnector.RegisterEdit('Link', fPasswords.LinkEdit);
  fDataConnector.RegisterMemo('Remark', fPasswords.RemarkEdit);
  fDataConnector.RegisterGrid(TArray<String>.Create('Login', 'Password'), fPasswords.Grid, TPassword);

  fTagsConnector := Factory2.Locate<IDataConnector>('TStoreConnector');
  fTagsConnector.RegisterGrid(TArray<String>.Create('Val'), fPasswords.Tags, TTag);
  fDataConnector.RegisterConnector('Tags', fTagsConnector);

  fDataConnector.PSListChangeChannel.Publish(TListChange.New(fPasswordsData.NewList));
end;

procedure TGUI.PSSizeObserver(const AValue: TSizeData);
begin
  if fStore.IsOpened then begin
    (fAppSettings.UnderObject as TAppSettings).EditForm.Width := AValue.Width;
    (fAppSettings.UnderObject as TAppSettings).EditForm.Height := AValue.Height;
  end else begin
    (fAppSettings.UnderObject as TAppSettings).OpenForm.Width := AValue.Width;
    (fAppSettings.UnderObject as TAppSettings).OpenForm.Height := AValue.Height;
  end;
end;

procedure TGUI.PSPositionObserver(const AValue: TPositionData);
begin
  if fStore.IsOpened then begin
    (fAppSettings.UnderObject as TAppSettings).EditForm.Left := AValue.Left;
    (fAppSettings.UnderObject as TAppSettings).EditForm.Top := AValue.Top;
  end else begin
    (fAppSettings.UnderObject as TAppSettings).OpenForm.Left := AValue.Left;
    (fAppSettings.UnderObject as TAppSettings).OpenForm.Top := AValue.Top;
  end;
end;

procedure TGUI.PSCloseProgramObserver;
begin
  if Store.IsOpened then begin
    fPasswordsData.Save;
    fOpenStore.Close;
  end;
  SettingsStore.Save2(fAppSettings);
  SettingsStore.Close;
  raise ELaunchStop.Create('');
end;

procedure TGUI.PSShowLogObserver;
begin
  Log.Visible := not Log.Visible;
end;

procedure TGUI.PSTextFilterChannelObserver(const AValue: String);
begin
  if AValue = '' then
    fDataConnector.PSListChangeChannel.Publish(TListChange.New(fPasswordsData.NewList))
  else
    fDataConnector.PSListChangeChannel.Publish(TListChange.New(
      fPasswordsData.NewList(
        function(const x: IRBData): Boolean
        begin
          Result := trl_ipersist.FilterInsensitiveContains(AValue.ToLower, x);
        end
      )
    ));
  PSGUIChannel.Debounce(TGUIData.Create(gaRender));
end;

procedure TGUI.InitValues;
begin
  inherited InitValues;
  SettingsStore.Open('~/settings.xml');
  fAppSettings := GetAppSettings;
  CreateComponents;
  PublishAppSettings;
end;

function TGUI.DoCompose: IMetaElement;
begin
  Result := fForm.Compose;
  if Store.IsOpened then begin
    if not fIsOpened then begin
      fPasswordsData := GetPasswordsData;
      CreateDataConnectors;
      fAppSettings.ItemByName['LastOpenedFile'].AsString := fOpenStore.FileEdit.Text;
      fIsOpened := True;
      PublishAppSettings;
    end;
    (Result as INode).AddChild(fPasswords.Compose as INode);
  end else begin
    (Result as INode).AddChild(fOpenStore.Compose as INode);
  end;
  //(Result as INode).AddChild(NewLogButton.Compose as INode);
end;

{ TGUIStore }

function TGUIStore.OpenEncryptedStore(const AFile: String): TStream;
var
  mEnum: IRBDataEnumerator;
begin
  Result := TMemoryStream.Create;
  EncryptedStore.Open(AFile);
  mEnum := EncryptedStore.Select2(TCrypto.ClassName).GetEnumerator;
  if mEnum.MoveNext then begin
    Result.CopyFrom(mEnum.Current.ItemByName['Data'].AsObject as TStream, 0);
  end;
end;

procedure TGUIStore.CloseEncryptedStore(const AData: TStream);
var
  mData: IRBData;
  mEnum: IRBDataEnumerator;
begin
  mEnum := EncryptedStore.Select2(TCrypto.ClassName).GetEnumerator;
  if mEnum.MoveNext then begin
    mData := mEnum.Current;
  end else begin
    mData := Factory2.Locate<IRBData>(TCrypto.ClassName);
  end;
  AData.Position := 0;
  (mData.ItemByName['Data'].AsObject as TStream).Size := 0;
  (mData.ItemByName['Data'].AsObject as TStream).CopyFrom(AData, 0);
  EncryptedStore.Save2(mData);
  EncryptedStore.Close;
end;

procedure TGUIStore.OpenDecryptedStore(const ACryptedData: TStream; const APassword: string);
var
  mData: TMemoryStream;
begin
  mData := TMemoryStream.Create;
  try
    fUsedKey := APassword;
    UniqueString(fUsedKey);
    Cryptic.Key := APassword;
    if fUsedKey <> Cryptic.Key then
      raise Exception.Create('criptic key mismatch');
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
    if fUsedKey <> Cryptic.Key then
      raise Exception.Create('criptic key mismatch');
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

procedure TGUIStore.Close;
var
  mEncrypted: TStream;
begin
  mEncrypted := CloseDecryptedStore;
  try
    CloseEncryptedStore(mEncrypted);
  finally
    mEncrypted.Free;
  end;
end;

{ TCrypto }

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
  mReg.InjectProp('Store', IPersistStoreDevice);
  mReg.InjectProp('SettingsStore', IPersistStoreDevice, 'settings');
  mReg.InjectProp('PersistFactory', IPersistFactory);

  mReg := RegReact.RegisterDesignComponent(TGUIStore, IGUIStore);
  mReg.InjectProp('Cryptic', ICryptic);
  mReg.InjectProp('EncryptedStore', IPersistStoreDevice, 'encrypted');
  mReg.InjectProp('DecryptedStore', IPersistStoreDevice);

  mReg := RegReact.RegisterDesignComponent(TGUIPasswords, IGUIPasswords);

end;

procedure TApp.RegisterPersist;
var
  mReg: TDIReg;
begin
  // persist data
  RegisterDataClass(DIC, TAppSettingsForm);
  RegisterDataClass(DIC, TAppSettings);
  RegisterDataClass(DIC, TPassword);
  RegisterDataClass(DIC, TTag);

  mReg := DIC.Add(TDataListAppSettings, IDataListAppSettings);
  mReg.InjectProp('PubSub', IPubSub);
  mReg.InjectProp('Device', IPersistStoreDevice, 'settings');
  mReg.InjectProp('Factory2', TDIFactory2);

  mReg := DIC.Add(TDataListPassword, IDataListPassword);
  mReg.InjectProp('PubSub', IPubSub);
  mReg.InjectProp('Device', IPersistStoreDevice, '');
  mReg.InjectProp('Factory2', TDIFactory2);

  mReg := DIC.Add(TDataListCrypto, IDataListCrypto);
  mReg.InjectProp('PubSub', IPubSub);
  mReg.InjectProp('Device', IPersistStoreDevice, 'encrypted');
  mReg.InjectProp('Factory2', TDIFactory2);

  mReg := DIC.Add(TListTags, IListTags);
  mReg.InjectProp('Factory2', TDIFactory2);




  mReg := DIC.Add(TPersistFactory, IPersistFactory);
  mReg.InjectProp('Container', TDIContainer);

  mReg := DIC.Add(TXmlStore, IPersistStoreDevice, 'settings', ckSingle);
  mReg.InjectProp('Factory', IPersistFactory);

  mReg := DIC.Add(TXmlStore, IPersistStoreDevice, 'encrypted', ckSingle);
  mReg.InjectProp('Factory', IPersistFactory);

  mReg := DIC.Add(TXmlStore, IPersistStoreDevice, '', ckSingle);
  mReg.InjectProp('Factory', IPersistFactory);



  mReg := DIC.Add(TMemoryStream);
  RegisterDataClass(DIC, TCrypto);



  end;

procedure TApp.RegisterAppServices;
begin
  inherited;
  RegisterTools;
  RegisterPersist;
  RegisterGUI;
end;

end.

