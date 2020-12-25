unit ipast;

{$mode objfpc}{$H+}

interface

uses
  trl_ipersist;

type

  { IStoreManager }

  IStoreManager = interface
  ['{C62A6D66-B619-449C-B43D-CA810DB228DF}']
    procedure Open(const AFile, AKey: string);
    procedure Close;
    function Store: IPersistStore;
  end;

implementation

end.

