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
  protected
    procedure Encode(const AKey: string; const AData: IRBData);
    procedure Decode(const AKey: string; const AData: IRBData);
    procedure ResetGUI;
    function Edit(const AData: IRBData): Boolean;
  published
    property Binder: IRBDataBinder read fBinder write fBinder;
    property BehaveBinder: IRBBehavioralBinder read fBehaveBinder write fBehaveBinder;
    property Cryptic: ICryptic read fCriptic write fCriptic;
  end;

var
  GroupForm: TGroupForm;

implementation

{$R *.lfm}

{ TGroupForm }

procedure TGroupForm.btnEncodeClick(Sender: TObject);
begin
  if fEncoded then
    Decode(Key.Text, Binder.Data)
  else
    Encode(Key.Text, Binder.Data);
  ResetGUI;
end;

procedure TGroupForm.Encode(const AKey: string; const AData: IRBData);
var
  mPasswords: IPersistManyTPassword;
  i: integer;
begin
  if fEncoded then
    Exit;
  Cryptic.Key := Akey;
  AData.ItemByName['Key'].AsString := Cryptic.Encode(AKey);
  mPasswords := AData.ItemByName['Passwords'].AsInterface as IPersistManyTPassword;
  for i := 0 to mPasswords.Count - 1 do begin
    mPasswords[i].Login := Cryptic.Encode(mPasswords[i].Login);
    mPasswords[i].Password := Cryptic.Encode(mPasswords[i].Password);
  end;
  fEncoded := True;
end;

procedure TGroupForm.Decode(const AKey: string; const AData: IRBData);
var
  mPasswords: IPersistManyTPassword;
  i: integer;
begin
  if not fEncoded then
    Exit;
  Cryptic.Key := AKey;
  if Cryptic.Decode(AData.ItemByName['Key'].AsString) <> Key.Text then
  begin
    Cryptic.Key := '';
    raise Exception.Create('invalid key');
  end;
  mPasswords := AData.ItemByName['Passwords'].AsInterface as IPersistManyTPassword;
  for i := 0 to mPasswords.Count - 1 do begin
    mPasswords[i].Login := Cryptic.Decode(mPasswords[i].Login);
    mPasswords[i].Password := Cryptic.Decode(mPasswords[i].Password);
  end;
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
  Binder.DataChange;
end;

function TGroupForm.Edit(const AData: IRBData): Boolean;
begin
  BehaveBinder.Bind(Self);
  try
    try
      try
        try
          Binder.Bind(Self, AData);
          fEncoded := AData.ItemByName['Key'].AsString <> '';
          ResetGUI;
          Result := ShowModal = mrOK;
        finally
          Encode(Key.Text, AData);
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

