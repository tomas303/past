unit fGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Buttons, Menus, tal_iedit, trl_irttibroker, tvl_ibindings,
  trl_icryptic, trl_ipersist, uPasswords, SettingsBroker, LCLIntf, trl_ilog;

type

  { TGroupForm }

  TGroupForm = class(TForm, IEditData)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    btnLink: TBitBtn;
    Caption_bind: TEdit;
    lblCaption: TLabel;
    Passwords_bind: TStringGrid;
    procedure btnLinkClick(Sender: TObject);
  private
    fBinder: IRBDataBinder;
    fBehaveBinder: IRBBehavioralBinder;
    fSettingsBroker: ISettingsBroker;
    fLog: ILog;
  protected
    function Edit(const AData: IRBData): Boolean;
  published
    property Binder: IRBDataBinder read fBinder write fBinder;
    property BehaveBinder: IRBBehavioralBinder read fBehaveBinder write fBehaveBinder;
    property SettingsBroker: ISettingsBroker read fSettingsBroker write fSettingsBroker;
    property Log: ILog read fLog write fLog;
  end;

implementation

{$R *.lfm}

{ TGroupForm }

procedure TGroupForm.btnLinkClick(Sender: TObject);
var
  mPasswords: IPersistMany;
  mIndex: integer;
  mUrl: string;
begin
 mPasswords := Binder.Data.ItemByName['Passwords'].AsInterface as IPersistMany;
 mIndex := Passwords_bind.Row - 1;
 if (mIndex >= 0) and (mIndex < mPasswords.Count) then begin
   mUrl := mPasswords.AsPersistData[mIndex].ItemByName['Link'].AsString;
   Log.DebugLn('opening url: %s', [mUrl]);
   OpenURL(mUrl);
 end;
end;

function TGroupForm.Edit(const AData: IRBData): Boolean;
begin
  BehaveBinder.Bind(Self);
  try
    Binder.BindArea(Self, AData);
    try
      SettingsBroker.LoadWindow('Windows', Self);
      Result := ShowModal = mrOK;
      SettingsBroker.SaveWindow('Windows', Self);
    finally
      Binder.Unbind;
    end;
  finally
    BehaveBinder.Unbind;
  end;
end;

end.

