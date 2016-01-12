unit uCryptic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, trl_icryptic, DCPtwofish, DCPsha256;


type

  ECryptic = class(Exception);

  { TCryptic }

  TCryptic = class(TInterfacedObject, ICryptic)
  private
    fKey: string;
  protected
    procedure CheckKey;
  protected
    function Decode(const AData: String): String;
    function Encode(const AData: String): String;
    function GetKey: string;
    procedure SetKey(AValue: string);
    property Key: string read GetKey write SetKey;
  end;

implementation

{ TCryptic }

procedure TCryptic.CheckKey;
begin
  if fKey = '' then
    raise ECryptic.Create('Invalid Key');
end;

function TCryptic.Decode(const AData: String): String;
var
  mCipher: TDCP_twofish;
begin
  CheckKey;
  if AData = '' then
  begin
    Result := '';
    Exit;
  end;
  mCipher:= TDCP_twofish.Create(nil);
  try
    mCipher.InitStr(fKey, TDCP_sha256);
    Result := mCipher.DecryptString(AData);
  finally
    mCipher.Free;
  end;
end;

function TCryptic.Encode(const AData: String): String;
var
  mCipher: TDCP_twofish;
begin
  CheckKey;
  mCipher := TDCP_twofish.Create(nil);
  try
    mCipher.InitStr(fKey, TDCP_sha256);
    Result := mCipher.EncryptString(AData);
  finally
    mCipher.Free;
  end;
end;

function TCryptic.GetKey: string;
begin
  Result := fKey;
end;

procedure TCryptic.SetKey(AValue: string);
begin
  fKey := AValue;
end;

end.

