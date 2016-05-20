unit fOpen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn, Menus, trl_ipersist, trl_irttibroker, trl_icryptic, tvl_iedit;

type

  { TOpenForm }

  TOpenForm = class(TForm, IMainForm)
    btnOpen: TButton;
    btnOpenFile: TButton;
    edFile: TComboBox;
    edKey: TEdit;
    lblKey: TLabel;
    lblFile: TLabel;
    dlgOpen: TOpenDialog;
    procedure btnOpenClick(Sender: TObject);
    procedure btnOpenFileClick(Sender: TObject);
  private
    fFactory: IPersistFactory;
    fEncryptedStore, fDecrytedStore: IPersistStore;
    fCriptic: ICryptic;
    fMainForm: IListData;
    fData: IRBData;
    fSettings: IPersistStore;
    fSettingsFile: string;
    fAppSetting: IRBData;
    function GetCryptedFile: string;
    procedure SetCryptedFile(AValue: string);
  protected
    //IMainForm
    procedure StartUp;
    procedure ShutDown;
    procedure ConnectCloseHandler(OnCloseHandler: TCloseEvent);
  protected
    procedure OpenEncryptedStore;
    procedure CloseEncryptedStore;
    procedure OpenDecryptedStore(const AKey: string);
    procedure CloseDecryptedStore;
    procedure EditData(const AKey: string);
    procedure LoadSettings;
    procedure SaveSettings;
  published
    property Factory: IPersistFactory read fFactory write fFactory;
    property EncrytedStore: IPersistStore read fEncryptedStore write fEncryptedStore;
    property DecrytedStore: IPersistStore read fDecrytedStore write fDecrytedStore;
    property CryptedFile: string read GetCryptedFile write SetCryptedFile;
    property Cryptic: ICryptic read fCriptic write fCriptic;
    property MainForm: IListData read fMainForm write fMainForm;
    property Settings: IPersistStore read fSettings write fSettings;
    property SettingsFile: string read fSettingsFile write fSettingsFile;
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

procedure TOpenForm.StartUp;
begin
  LoadSettings;
  Show;
end;

procedure TOpenForm.ShutDown;
begin
  SaveSettings;
end;

procedure TOpenForm.ConnectCloseHandler(OnCloseHandler: TCloseEvent);
begin
  AddHandlerClose(OnCloseHandler);
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

procedure TOpenForm.LoadSettings;
var
  m: IPersistRefList;
  mDataFiles: IPersistMany;
  i: integer;
begin
  fSettings.Open(SettingsFile);
  m := (fSettings as IPersistQuery).SelectClass('TAppSetting');
  if m.Count = 0 then begin
    fAppSetting := fSettings.New('TAppSetting');
  end else begin
    fAppSetting := m.Data[0];
  end;
  edFile.Items.Clear;
  mDataFiles := fAppSetting.ItemByName['DataFiles'].AsInterface as IPersistMany;
  for i := 0 to mDataFiles.Count - 1 do begin
    edFile.Items.Insert(0, mDataFiles.AsPersistData[i].ItemByName['DataFile'].AsString);
  end;
  if edFile.Items.Count > 0 then
    edFile.ItemIndex :=0 ;
end;

procedure TOpenForm.SaveSettings;
var
  mDataFiles: IPersistMany;
  mInd: integer;
begin
  mDataFiles := fAppSetting.ItemByName['DataFiles'].AsInterface as IPersistMany;
  mDataFiles.Count := mDataFiles.Count + 1;
  mDataFiles.AsPersistData[mDataFiles.Count - 1].ItemByName['DataFile'].AsString := edFile.Text;
  mInd := 0;
  while mInd < mDataFiles.Count - 1 do
   if SameText(edFile.Text, mDataFiles.AsPersistData[mInd].ItemByName['DataFile'].AsString) then
     mDataFiles.Delete(mInd)
   else
     inc(mInd);
  while mDataFiles.Count > 10 do
    mDataFiles.Delete(0);
  fSettings.Save(fAppSetting);
end;

procedure TOpenForm.btnOpenClick(Sender: TObject);
begin
  EditData(edKey.Text);
end;

procedure TOpenForm.btnOpenFileClick(Sender: TObject);
begin
  if dlgOpen.Execute then
    edFile.Text := dlgOpen.FileName;
end;

function TOpenForm.GetCryptedFile: string;
begin
  Result := edFile.Text;
end;

end.

