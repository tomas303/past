unit uappfunc;

{$mode delphi}{$H+}

interface

uses
  rea_udesigncomponentfunc, rea_udesigncomponentdata,
  rea_iflux, rea_idesigncomponent, dialogs,
  trl_ipersist, trl_icryptic, trl_irttibroker,
  trl_udifactory,
  Classes, sysutils,
  uappdata;

type

  { TStorageFunc }

  TStorageFunc = class(TDesignComponentFunc)
  protected
    fData: TStorageData;
    fFactory2: TDIFactory2;
    fEncryptedStore: IPersistStore;
    fDecryptedStore: IPersistStore;
    fCryptic: ICryptic;
    function StoreData: IRBData;
  published
    property Data: TStorageData read fData write fData;
    property Factory2: TDIFactory2 read fFactory2 write fFactory2;
    property EncryptedStore: IPersistStore read fEncryptedStore write fEncryptedStore;
    property DecryptedStore: IPersistStore read fDecryptedStore write fDecryptedStore;
    property Cryptic: ICryptic read fCryptic write fCryptic;
  end;

  { TOpenStorageFunc }

  TOpenStorageFunc = class(TStorageFunc)
  private
    procedure OpenEncryptedStore(const AFile: string);
    procedure OpenDecryptedStore(const AKey: string);
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  protected
    fRenderNotifier: IFluxNotifier;
  published
    property RenderNotifier: IFluxNotifier read fRenderNotifier write fRenderNotifier;
  end;

  { TCloseStorageFunc }

  TCloseStorageFunc = class(TStorageFunc)
  private
    procedure CloseEncryptedStore;
    procedure CloseDecryptedStore(const AKey: string);
  protected
    procedure DoExecute(const AAction: IFluxAction); override;
  end;

  EOpenFormException = class(Exception);

implementation

{ TStorageFunc }

function TStorageFunc.StoreData: IRBData;
var
  mList: IPersistRefList;
begin
  mList := (EncryptedStore as IPersistQuery).SelectClass('TCrypto');
  if mList.Count = 0 then
  begin
    Result := Factory2.Locate<IRBData>('TCrypto');
  end
  else
  begin
    Result := mList.Data[0];
  end;
end;

{ TCloseStorageFunc }

procedure TCloseStorageFunc.CloseEncryptedStore;
begin
  EncryptedStore.Close;
end;

procedure TCloseStorageFunc.CloseDecryptedStore(const AKey: string);
var
  mDecData: TMemoryStream;
  mEncData: TStream;
  mData: IRBData;
begin
  mDecData := TMemoryStream.Create;
  try
    DecryptedStore.Close(mDecData);
    mData := StoreData;
    mEncData := mData.ItemByName['Data'].AsObject as TStream;
    Cryptic.Key := AKey;
    Cryptic.Encode(mDecData, mEncData);
    EncryptedStore.Save(mData);
  finally
    mDecData.Free;
  end;
end;

procedure TCloseStorageFunc.DoExecute(const AAction: IFluxAction);
begin
  fData.Store := nil;
  fData.Opened := False;
  CloseDecryptedStore(Data.PasswordData.Text);
  CloseEncryptedStore;
end;

{ TOpenStorageFunc }

procedure TOpenStorageFunc.OpenEncryptedStore(const AFile: string);
begin
  EncryptedStore.Open(AFile);
end;

procedure TOpenStorageFunc.OpenDecryptedStore(const AKey: string);
var
  mDecData: TMemoryStream;
  mEncData: TStream;
begin
  mDecData := TMemoryStream.Create;
  try
    Cryptic.Key := AKey;
    mEncData := StoreData.ItemByName['Data'].AsObject as TStream;
    if mEncData.Size <> 0 then begin
      try
        Cryptic.Decode(mEncData, mDecData);
        mDecData.Position := 0;
        DecryptedStore.Open(mDecData);
      except
        on E: Exception do begin
          DecryptedStore.Close;
          raise EOpenFormException.Create('Cannot open crypted data - probably bad password');
        end;
      end;
    end else
      DecryptedStore.Open;
  finally
    mDecData.Free;
  end;
end;

procedure TOpenStorageFunc.DoExecute(const AAction: IFluxAction);
begin
  OpenEncryptedStore(Data.FileNameData.Text);
  OpenDecryptedStore(Data.PasswordData.Text);
  fData.Opened := True;
  fData.Store := DecryptedStore;
  fRenderNotifier.Notify;
end;

end.

