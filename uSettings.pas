unit uSettings;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_ipersist, trl_upersist;

type
  TAppSettingDataFile = class
  private
    fDataFile: string;
  published
    property DataFile: string read fDataFile write fDataFile;
  end;

  IPersistManyTAppSettingDataFile = interface(IPersistMany<TAppSettingDataFile>)
  ['{6C7B708D-5267-42CB-9B2A-CB34FB11DE85}']
  end;

  TPersistManyTAppSettingDataFile = class(TPersistManyObjects<TAppSettingDataFile>, IPersistManyTAppSettingDataFile)
  end;

  { TAppSetting }

  TAppSetting = class
  private
    fDataFiles: IPersistManyTAppSettingDataFile;
  public
    procedure AfterConstruction; override;
  published
    property DataFiles: IPersistManyTAppSettingDataFile read fDataFiles;
  end;

implementation

{ TAppSetting }

procedure TAppSetting.AfterConstruction;
begin
  inherited AfterConstruction;
  fDataFiles := TPersistManyTAppSettingDataFile.Create;
end;

end.

