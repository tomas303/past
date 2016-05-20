unit SettingsBroker;

{$mode delphi}{$H+}

interface

uses
  Classes, SysUtils, Forms;

type
  ISettingsBroker = interface
  ['{E63F7324-AEA9-43DD-86FC-556211C742ED}']
    procedure StartUp;
    procedure ShutDown;
    procedure LoadStrings(const AName: string; AStrings: TStrings);
    procedure SaveStrings(const AName: string; AStrings: TStrings;
      const ANewText: string; AMaxCount: integer = 10);
    procedure LoadWindow(const AName: string; AForm: TCustomForm);
    procedure SaveWindow(const AName: string; AForm: TCustomForm);
  end;

implementation

end.

