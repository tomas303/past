unit uPasswords;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_ipersist, trl_upersist;

type

  { TPassword }

  TPassword = class
  private
    //fCaption: string;
    fLogin: string;
    fPassword: string;
  published
    //property Caption: string read fCaption write fCaption;
    property Login: string read fLogin write fLogin;
    property Password: string read fPassword write fPassword;
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

implementation

{ TGroup }

procedure TGroup.AfterConstruction;
begin
  inherited;
  fPasswords := TPersistManyTPassword.Create;
end;

end.

