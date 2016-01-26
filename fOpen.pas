unit fOpen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, Menus, trl_ipersist, trl_irttibroker, trl_icryptic, tvl_iedit;

type

  { TOpenForm }

  TOpenForm = class(TForm)
    btnOpen: TButton;
    edFile: TFileNameEdit;
    edKey: TEdit;
    lblKey: TLabel;
    lblFile: TLabel;
    procedure btnOpenClick(Sender: TObject);
  private
    fFactory: IPersistFactory;
    fEncryptedStore, fDecrytedStore: IPersistStore;
    fCriptic: ICryptic;
    fMainForm: IListData;
    fData: IRBData;
    function GetCryptedFile: string;
    procedure SetCryptedFile(AValue: string);
  protected
    procedure OLDOpenCryptedStore;
    procedure OpenEncryptedStore;
    procedure CloseEncryptedStore;
    procedure OpenDecryptedStore(const AKey: string);
    procedure CloseDecryptedStore;
    procedure EditData(const AKey: string);
  published
    property Factory: IPersistFactory read fFactory write fFactory;
    property EncrytedStore: IPersistStore read fEncryptedStore write fEncryptedStore;
    property DecrytedStore: IPersistStore read fDecrytedStore write fDecrytedStore;
    property CryptedFile: string read GetCryptedFile write SetCryptedFile;
    property Cryptic: ICryptic read fCriptic write fCriptic;
    property MainForm: IListData read fMainForm write fMainForm;
  end;

  EOpenFormException = class(Exception);

var
  OpenForm: TOpenForm;

implementation

{$R *.lfm}

{ TOpenForm }

procedure TOpenForm.SetCryptedFile(AValue: string);
begin
  edFile.Text := AValue;
end;

procedure TOpenForm.OLDOpenCryptedStore;
var
  mStream: TMemoryStream;
  mData: string;
  mList: IPersistRefList;
begin
  EncrytedStore.Open(CryptedFile);
  mList := (EncrytedStore as IPersistQuery).SelectClass('TCrypto');
  if mList.Count = 0 then
  begin
    fData := Factory.Create(IRBData, 'TCrypto') as IRBData;
  end
  else
  begin
    fData := mList.Data[0];
  end;
  mStream := TMemoryStream.Create;
  try
    mData := fData.ItemByName['Data'].AsString;
    Cryptic.Key := edKey.Text;
    if mData <> '' then begin
      try
        mData := Cryptic.Decode(mData);
        mStream.Write(mData[1], Length(mData));
        mStream.Position := 0;
        DecrytedStore.Open(mStream);
      except
        DecrytedStore.Close;
        EncrytedStore.Close;
        ShowMessage('cannot open coded data, probably bad password');
        Exit;
      end;
    end else
      DecrytedStore.Open;
    Hide;
    fMainForm.List;
    mStream.Size := 0;
    DecrytedStore.Close(mStream);
    SetLength(mData, mStream.Size);
    mStream.Position := 0;
    mStream.Read(mData[1], Length(mData));
    mData := Cryptic.Encode(mData);
    fData.ItemByName['Data'].AsString := mData;
    EncrytedStore.Save(fData);
    Show;
  finally
    mStream.Free;
  end;
  EncrytedStore.Close;
end;

procedure TOpenForm.OpenEncryptedStore;
var
  mList: IPersistRefList;
begin
  EncrytedStore.Open(CryptedFile);
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

procedure TOpenForm.CloseEncryptedStore;
begin
  EncrytedStore.Close;
end;

procedure TOpenForm.OpenDecryptedStore(const AKey: string);
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

procedure TOpenForm.CloseDecryptedStore;
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

procedure TOpenForm.EditData(const AKey: string);
begin
  OpenEncryptedStore;
  try
    OpenDecryptedStore(AKey);
    try
      Hide;
      fMainForm.List;
      Show;
    finally
      CloseDecryptedStore;
    end;
  finally
    CloseEncryptedStore;
  end;
end;

procedure TOpenForm.btnOpenClick(Sender: TObject);
begin
  EditData(edKey.Text);
end;

function TOpenForm.GetCryptedFile: string;
begin
  Result := edFile.Text;
end;

end.

