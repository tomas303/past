unit fMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  trl_irttibroker, trl_ifactory, trl_ipersist,
  tal_iedit, tvl_ibindings, trl_icryptic, tal_ihistorysettings;

type

  { TMainForm }

  TMainForm = class(TForm, IListData)
    btnAdd: TButton;
    btnEdit: TButton;
    btnDelete: TButton;
    lbPasswords: TListBox;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
  private
    fFactory: IPersistFactory;
    fStore: IPersistStore;
    fBinder: IRBTallyBinder;
    fEdit: IEditData;
    fHistorySettings: IHistorySettings;
  protected
    procedure List;
  public const
    cDataClass = 'TGroup';
  published
    property Factory: IPersistFactory read fFactory write fFactory;
    property Store: IPersistStore read fStore write fStore;
    property Binder: IRBTallyBinder read fBinder write fBinder;
    property Edit: IEditData read fEdit write fEdit;
    property HistorySettings: IHistorySettings read fHistorySettings write fHistorySettings;
  end;

implementation

{$R *.lfm}

{ TMainForm }

procedure TMainForm.btnAddClick(Sender: TObject);
var
  mData: IRBData;
begin
  mData := Factory.CreateObject(cDataClass);
  if Edit.Edit(mData) then
  begin
    Store.Save(mData);
    Store.Flush;
    Binder.Reload;
  end;
end;

procedure TMainForm.btnDeleteClick(Sender: TObject);
begin
  Store.Delete(Binder.CurrentData);
  Store.Flush;
  Binder.Reload;
end;

procedure TMainForm.btnEditClick(Sender: TObject);
var
  mData, mNewData: IRBData;
begin
  mData := Binder.CurrentData;
  if mData = nil then
    Exit;
  mNewData := Factory.CreateObject(mData.ClassName);
  mNewData.Assign(mData);
  if Edit.Edit(mNewData) then
  begin
    mData.Assign(mNewData);
    Store.Save(mData);
    Store.Flush;
    Binder.Reload;
  end;
end;

procedure TMainForm.List;
begin
  HistorySettings.Load(Self);
  Binder.Bind(lbPasswords, cDataClass);
  ShowModal;
  HistorySettings.Save(Self);
end;

end.

