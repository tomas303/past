(******************************************************************************
* Copyright (C) 2023 Tomáš Horák
*
* Permission is hereby granted, free of charge, to any person obtaining
* a copy of this software and associated documentation files (the
* "Software"), to deal in the Software without restriction, including
* without limitation the rights to use, copy, modify, merge, publish,
* distribute, sublicense, and/or sell copies of the Software, and to
* permit persons to whom the Software is furnished to do so, subject to
* the following conditions:
*
* The above copyright notice and this permission notice shall be
* included in all copies or substantial portions of the Software.
*
* THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
* EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
* MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
* IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
* CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
* TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
* SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
******************************************************************************)
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
    function Decode(const AData: String): String;
    function Encode(const AData: String): String;
    procedure Decode(AInStream, AOutStream: TStream);
    procedure Encode(AInStream, AOutStream: TStream);
    function GetKey: string;
    procedure SetKey(AValue: string);
    property Key: string read GetKey write SetKey;
  end;

implementation

{ TCryptic }

function TCryptic.Decode(const AData: String): String;
var
  mCipher: TDCP_twofish;
begin
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
  mCipher:= TDCP_twofish.Create(nil);
  try
    mCipher.InitStr(fKey, TDCP_sha256);
    AInStream.Position := 0;
    AOutStream.Size := 0;
    mCipher.DecryptStream(AInStream, AOutStream, AInStream.Size);
  finally
    mCipher.Free;
  end;
end;

procedure TCryptic.Encode(AInStream, AOutStream: TStream);
var
  mCipher: TDCP_twofish;
begin
  mCipher := TDCP_twofish.Create(nil);
  try
    mCipher.InitStr(fKey, TDCP_sha256);
    AInStream.Position := 0;
    AOutStream.Size := 0;
    mCipher.EncryptStream(AInStream, AOutStream, AInStream.Size);
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
    fKey := AValue;
  end;
end;

end.

