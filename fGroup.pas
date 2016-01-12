unit fGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Buttons, Menus, tvl_iedit, trl_irttibroker, tvl_ibindings,
  trl_icryptic, trl_ipersist, uPasswords;

type

  { TGroupForm }

  TGroupForm = class(TForm, IEditData)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnEncode: TButton;
    Key: TEdit;
    Caption_bind: TEdit;
    lblKey: TLabel;
    lblCaption: TLabel;
    Passwords_bind: TStringGrid;
    procedure btnEncodeClick(Sender: TObject);
  private
    fBinder: IRBDataBinder;
    fBehaveBinder: IRBBehavioralBinder;
    fCriptic: ICryptic;
    fEncoded: Boolean;
    fFactory: IPersistFactory;
  protected
    function Encode(const AKey: string; const AData: IRBData): IRBData;
    function Decode(const AKey: string; const AData: IRBData): IRBData;
    procedure ResetGUI;
    function Edit(const AData: IRBData): Boolean;
  published
    property Binder: IRBDataBinder read fBinder write fBinder;
    property BehaveBinder: IRBBehavioralBinder read fBehaveBinder write fBehaveBinder;
    property Cryptic: ICryptic read fCriptic write fCriptic;
    property Factory: IPersistFactory read fFactory write fFactory;
  end;

var
  GroupForm: TGroupForm;

implementation

{$R *.lfm}

{ TGroupForm }

procedure TGroupForm.btnEncodeClick(Sender: TObject);
var
  mData: IRBData;
begin
  if fEncoded then
    mData := Decode(Key.Text, Binder.Data)
  else
    mData := Encode(Key.Text, Binder.Data);
  Binder.Data := mData;
  ResetGUI;
end;

function TGroupForm.Encode(const AKey: string; const AData: IRBData): IRBData;
var
  mPasswords: IPersistManyTPassword;
  i: integer;
  mData: IRBData;
begin
  if fEncoded then begin
    Result := AData;
    Exit;
  end;
  mData := Factory.Create(IRBData, 'TGroup') as IRBData;
  mData.Assign(AData);
  Cryptic.Key := Akey;
  mData.ItemByName['Key'].AsString := Cryptic.Encode(AKey);
  mPasswords := mData.ItemByName['Passwords'].AsInterface as IPersistManyTPassword;
  for i := 0 to mPasswords.Count - 1 do begin
    mPasswords[i].Login := Cryptic.Encode(mPasswords[i].Login);
    mPasswords[i].Password := Cryptic.Encode(mPasswords[i].Password);
  end;
  Result := mData;
  fEncoded := True;
end;

function TGroupForm.Decode(const AKey: string; const AData: IRBData): IRBData;
var
  mPasswords: IPersistManyTPassword;
  i: integer;
  mData: IRBData;
begin
  if not fEncoded then begin
    Result := AData;
    Exit;
  end;
  mData := Factory.Create(IRBData, 'TGroup') as IRBData;
  mData.Assign(AData);
  Cryptic.Key := AKey;
  if Cryptic.Decode(mData.ItemByName['Key'].AsString) <> Key.Text then
  begin
    Cryptic.Key := '';
    raise Exception.Create('invalid key');
  end;
  mPasswords := mData.ItemByName['Passwords'].AsInterface as IPersistManyTPassword;
  for i := 0 to mPasswords.Count - 1 do begin
    mPasswords[i].Login := Cryptic.Decode(mPasswords[i].Login);
    mPasswords[i].Password := Cryptic.Decode(mPasswords[i].Password);
  end;
  Result := mData;
  fEncoded := False;
end;

procedure TGroupForm.ResetGUI;
begin
  if fEncoded then
  begin
    Passwords_bind.Enabled := False;
    btnEncode.Caption := '&Decode';
  end
  else
  begin
    Passwords_bind.Enabled := True;
    btnEncode.Caption := 'Enco&de';
  end;
end;

function TGroupForm.Edit(const AData: IRBData): Boolean;
var
  mData: IRBData;
begin
  BehaveBinder.Bind(Self);
  try
    try
      try
        try
          mData := Factory.Create(IRBData, 'TGroup') as IRBData;
          mData.Assign(AData);
          Binder.Bind(Self, mData);
          fEncoded := mData.ItemByName['Key'].AsString <> '';
          ResetGUI;
          Result := ShowModal = mrOK;
        finally
          mData := Encode(Key.Text, mData);
          AData.Assign(mData);
          Key.Text := '';
        end;
      finally
        Cryptic.Key := '';
      end;
    finally
      Binder.Unbind;
    end;
  finally
    BehaveBinder.Unbind;
  end;
end;

end.

