unit uSettings;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, trl_ipersist, trl_upersist;

type
  TAppSettingWindow = class
  private
    fID: string;
    fTop: integer;
    fLeft: integer;
    fWidth: integer;
    fHeight: integer;
  published
    property ID: string read fID write fID;
    property Top: integer read fTop write fTop;
    property Left: integer read fLeft write fLeft;
    property Width: integer read fWidth write fWidth;
    property Height: integer read fHeight write fHeight;
  end;

  IPersistManyTAppSettingWindow = interface(IPersistManyItems<TAppSettingWindow>)
  ['{9E1FC351-E39B-46C6-B44E-E9617142684E}']
  end;

  TPersistManyTAppSettingWindow = class(TPersistManyObjects<TAppSettingWindow>, IPersistManyTAppSettingWindow)
  end;

  { TAppSetting }

  TAppSetting = class
  private
    fDataFiles: IPersistManyStrings;
    fWindows: IPersistManyTAppSettingWindow;
  public
    procedure AfterConstruction; override;
  published
    property DataFiles: IPersistManyStrings read fDataFiles;
    property Windows: IPersistManyTAppSettingWindow read fWindows;
  end;

implementation

{ TAppSetting }

procedure TAppSetting.AfterConstruction;
begin
  inherited AfterConstruction;
  fDataFiles := TPersistManyStrings.Create;
  fWindows := TPersistManyTAppSettingWindow.Create;
end;

end.

