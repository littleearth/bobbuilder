unit Bob.Encryption;

interface

uses
  Lazy.Types,
  System.SysUtils, System.Classes;

type
  TEncryption = class(TLZObject)
  public
    class function DecryptPassword(APassword, ASalt: string): string;
    class function EncryptPassword(APassword, ASalt: string): string;
  end;

implementation

uses
  uTPLb_Codec, uTPLb_CryptographicLibrary, uTPLb_Constants;

{ TEncryption }

class function TEncryption.DecryptPassword(APassword, ASalt: string): string;
var
  LLibrary: TCryptographicLibrary;
  LCodec: TCodec;
begin
  Result := '';
  LLibrary := TCryptographicLibrary.Create(nil);
  LCodec := TCodec.Create(nil);
  try
    LCodec.CryptoLibrary := LLibrary;
    LCodec.StreamCipherId := BlockCipher_ProgId;
    LCodec.BlockCipherId := 'native.AES-256';
    LCodec.ChainModeId := 'native.ECB';
    LCodec.password := ASalt;
    LCodec.DecryptString(Result, APassword, TEncoding.ANSI);
  finally
    LCodec.Free;
    LLibrary.Free;
  end;
end;

class function TEncryption.EncryptPassword(APassword, ASalt: string): string;
var
  LLibrary: TCryptographicLibrary;
  LCodec: TCodec;
begin
  Result := '';
  LLibrary := TCryptographicLibrary.Create(nil);
  LCodec := TCodec.Create(nil);
  try
    LCodec.CryptoLibrary := LLibrary;
    LCodec.StreamCipherId := BlockCipher_ProgId;
    LCodec.BlockCipherId := 'native.AES-256';
    LCodec.ChainModeId := 'native.ECB';
    LCodec.password := ASalt;
    LCodec.EncryptString(APassword, Result, TEncoding.ANSI);
  finally
    LCodec.Free;
    LLibrary.Free;
  end;
end;

end.
