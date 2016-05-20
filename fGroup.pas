unit fGroup;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Grids, Buttons, Menus, tvl_iedit, trl_irttibroker, tvl_ibindings,
  trl_icryptic, trl_ipersist, uPasswords, SettingsBroker;

type

  { TGroupForm }

  TGroupForm = class(TForm, IEditData)
    btnCancel: TBitBtn;
    btnOK: TBitBtn;
    Caption_bind: TEdit;
    lblCaption: TLabel;
    Passwords_bind: TStringGrid;
  private
    fBinder: IRBDataBinder;
    fBehaveBinder: IRBBehavioralBinder;
    fSettingsBroker: ISettingsBroker;
  protected
    function Edit(const AData: IRBData): Boolean;
  published
    property Binder: IRBDataBinder read fBinder write fBinder;
    property BehaveBinder: IRBBehavioralBinder read fBehaveBinder write fBehaveBinder;
    property SettingsBroker: ISettingsBroker read fSettingsBroker write fSettingsBroker;
  end;

var
  GroupForm: TGroupForm;

implementation

{$R *.lfm}

{ TGroupForm }

function TGroupForm.Edit(const AData: IRBData): Boolean;
var
  mData: IRBData;
begin
  BehaveBinder.Bind(Self);
  try
    Binder.Bind(Self, AData);
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

