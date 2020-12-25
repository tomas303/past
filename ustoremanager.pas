unit ustoremanager;

{$mode objfpc}{$H+}

interface

uses
  Classes, sysutils,
  ipast,
  trl_ipersist, trl_icryptic, trl_irttibroker;

type

  { TStoreManager }

  TStoreManager = class(TInterfacedObject, IStoreManager)
  private
    fData: IRBData;
    procedure OpenEncryptedStore(const AFile: string);
    procedure CloseEncryptedStore;
    procedure OpenDecryptedStore(const AKey: string);
    procedure CloseDecryptedStore;
  protected
    //IStoreManager
    procedure Open(const AFile, AKey: string);
    procedure Close;
    function Store: IPersistStore;
  protected
    fFactory: IPersistFactory;
    fEncryptedStore: IPersistStore;
    fDecrytedStore: IPersistStore;
    fCriptic: ICryptic;
  published
    property Factory: IPersistFactory read fFactory write fFactory;
    property EncrytedStore: IPersistStore read fEncryptedStore write fEncryptedStore;
    property DecrytedStore: IPersistStore read fDecrytedStore write fDecrytedStore;
    property Cryptic: ICryptic read fCriptic write fCriptic;
  end;

  EOpenFormException = class(Exception);

implementation

{ TStoreManager }

procedure TStoreManager.OpenEncryptedStore(const AFile: string);
var
  mList: IPersistRefList;
begin
  EncrytedStore.Open(AFile);
  mList := (EncrytedStore as IPersistQuery).SelectClass('TCrypto');
  if mList.Count = 0 then
  begin
    fData := Factory.Create(IRBData, 'TCrypto') as IRBData;
  end
  else
  begin
    fData := mList.Data[0];
  end;
end;

procedure TStoreManager.CloseEncryptedStore;
begin
  EncrytedStore.Close;
end;

procedure TStoreManager.OpenDecryptedStore(const AKey: string);
var
  mDecData: TMemoryStream;
  mEncData: TStream;
begin
  mDecData := TMemoryStream.Create;
  try
    Cryptic.Key := AKey;
    mEncData := fData.ItemByName['Data'].AsObject as TStream;
    if mEncData.Size <> 0 then begin
      try
        Cryptic.Decode(mEncData, mDecData);
        mDecData.Position := 0;
        DecrytedStore.Open(mDecData);
      except
        on E: Exception do begin
          DecrytedStore.Close;
          raise EOpenFormException.Create('Cannot open crypted data - probably bad password');
        end;
      end;
    end else
      DecrytedStore.Open;
  finally
    mDecData.Free;
  end;
end;

procedure TStoreManager.CloseDecryptedStore;
var
  mDecData: TMemoryStream;
  mEncData: TStream;
begin
  mDecData := TMemoryStream.Create;
  try
    DecrytedStore.Close(mDecData);
    mEncData := fData.ItemByName['Data'].AsObject as TStream;
    Cryptic.Encode(mDecData, mEncData);
    EncrytedStore.Save(fData);
  finally
    mDecData.Free;
  end;
end;

procedure TStoreManager.Open(const AFile, AKey: string);
begin
  OpenEncryptedStore(AFile);
  OpenDecryptedStore(AKey);
end;

procedure TStoreManager.Close;
begin
  CloseDecryptedStore;
  CloseEncryptedStore;
end;

function TStoreManager.Store: IPersistStore;
begin
  Result := fDecrytedStore;
end;

end.

