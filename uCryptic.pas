unit uCryptic;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, trl_icryptic, DCPtwofish, DCPsha256, trl_ilog;


type

  ECryptic = class(Exception);

  { TCryptic }

  TCryptic = class(TInterfacedObject, ICryptic)
  private
    fKey: string;
  protected
    fLog: ILog;
    procedure CheckKey;
  protected
    function Decode(const AData: String): String;
    function Encode(const AData: String): String;
    procedure Decode(AInStream, AOutStream: TStream);
    procedure Encode(AInStream, AOutStream: TStream);
    function GetKey: string;
    procedure SetKey(AValue: string);
    property Key: string read GetKey write SetKey;
  published
    property Log: ILog read fLog write fLog;
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

procedure TCryptic.Decode(AInStream, AOutStream: TStream);
var
  mCipher: TDCP_twofish;
begin
  CheckKey;
  mCipher:= TDCP_twofish.Create(nil);
  try
    mCipher.InitStr(fKey, TDCP_sha256);
    AInStream.Position := 0;
    AOutStream.Size := 0;
    mCipher.DecryptStream(AInStream, AOutStream, AInStream.Size);
    Log.DebugLn('decoded by %s', [Key]);
  finally
    mCipher.Free;
  end;
end;

procedure TCryptic.Encode(AInStream, AOutStream: TStream);
var
  mCipher: TDCP_twofish;
begin
  CheckKey;
  mCipher := TDCP_twofish.Create(nil);
  try
    mCipher.InitStr(fKey, TDCP_sha256);
    AInStream.Position := 0;
    AOutStream.Size := 0;
    mCipher.EncryptStream(AInStream, AOutStream, AInStream.Size);
    Log.DebugLn('encoded by %s', [Key]);
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
  if fKey <> AValue then begin
    Log.DebugLn('key changed from %s to %s', [fKey, AValue]);
    fKey := AValue;
  end;
end;

end.

