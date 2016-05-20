unit uPasswords;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_ipersist, trl_upersist;

type

  { TPassword }

  TPassword = class
  private
    fLogin: string;
    fPassword: string;
    fRemark: string;
    fLink: string;
  published
    property Login: string read fLogin write fLogin;
    property Password: string read fPassword write fPassword;
    property Remark: string read fRemark write fRemark;
    property Link: string read fLink write fLink;
  end;

  IPersistManyTPassword = interface(IPersistMany<TPassword>)
  ['{9B271144-86D0-49FD-8EBC-40FEF2A287E2}']
  end;

  TPersistManyTPassword = class(TPersistManyObjects<TPassword>, IPersistManyTPassword)
  end;

  { TGroup }

  TGroup = class
  private
    fCaption: string;
    fKey: string;
    fPasswords: IPersistManyTPassword;
  public
    procedure AfterConstruction; override;
  published
    property Caption: string read fCaption write fCaption;
    property Key: string read fKey write fKey;
    property Passwords: IPersistManyTPassword read fPasswords;
  end;

  { TCrypto }

  TCrypto = class
  private
    fData: TStream;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property Data: TStream read fData write fData;
  end;

implementation

{ TCrypto }

procedure TCrypto.AfterConstruction;
begin
  inherited AfterConstruction;
  fData := TMemoryStream.Create;
end;

procedure TCrypto.BeforeDestruction;
begin
  FreeAndNil(fData);
  inherited BeforeDestruction;
end;

{ TGroup }

procedure TGroup.AfterConstruction;
begin
  inherited;
  fPasswords := TPersistManyTPassword.Create;
end;

end.

