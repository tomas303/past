unit uSettingsBroker;

{$mode delphi}{$H+}

interface

uses
  classes, sysutils, forms, trl_irttibroker, trl_ipersist, SettingsBroker;

type

  { TSettingsBroker }

  TSettingsBroker = class(TInterfacedObject, ISettingsBroker)
  private
    fStore: IPersistStore;
    fFileName: string;
    fAppSetting: IRBData;
  protected
    function GetFormID(AForm: TCustomForm): string;
    function FindAppSettingWindow(const AName, AID: string): IRBData;
    function NewAppSettingWindow(const AName, AID: string): IRBData;
    procedure Form2Data(AForm: TCustomForm; const AData: IRBData);
    procedure Data2Form(const AData: IRBData; AForm: TCustomForm);
  public
    procedure StartUp;
    procedure ShutDown;
    procedure LoadStrings(const AName: string; AStrings: TStrings);
    procedure SaveStrings(const AName: string; AStrings: TStrings;
      const ANewText: string; AMaxCount: integer = 10);
    procedure LoadWindow(const AName: string; AForm: TCustomForm);
    procedure SaveWindow(const AName: string; AForm: TCustomForm);
  published
    property Store: IPersistStore read fStore write fStore;
    property FileName: string read fFileName write fFileName;
  end;

implementation

{ TSettingsBroker }

function TSettingsBroker.GetFormID(AForm: TCustomForm): string;
begin
  Result := AForm.ClassName + '.' + AForm.Name;
end;

function TSettingsBroker.FindAppSettingWindow(const AName, AID: string
  ): IRBData;
var
  mWindows: IPersistMany;
  i: integer;
begin
  Result := nil;
  mWindows := fAppSetting.ItemByName[AName].AsInterface as IPersistMany;
  for i := 0 to mWindows.Count - 1 do
    if mWindows.AsPersistData[i].ItemByName['ID'].AsString = AID then begin
      Result := mWindows.AsPersistData[i];
      Break;
    end;
end;

function TSettingsBroker.NewAppSettingWindow(const AName, AID: string): IRBData;
var
  mWindows: IPersistMany;
begin
  mWindows := fAppSetting.ItemByName[AName].AsInterface as IPersistMany;
  mWindows.Count := mWindows.Count + 1;
  Result := mWindows.AsPersistData[mWindows.Count - 1];
  Result.ItemByName['ID'].AsString := AID;
end;

procedure TSettingsBroker.Form2Data(AForm: TCustomForm; const AData: IRBData);
begin
  AData.ItemByName['Left'].AsInteger := AForm.Left;
  AData.ItemByName['Top'].AsInteger := AForm.Top;
  AData.ItemByName['Width'].AsInteger := AForm.Width;
  AData.ItemByName['Height'].AsInteger := AForm.Height;
end;

procedure TSettingsBroker.Data2Form(const AData: IRBData; AForm: TCustomForm);
begin
  AForm.Left := AData.ItemByName['Left'].AsInteger;
  AForm.Top := AData.ItemByName['Top'].AsInteger;
  AForm.Width := AData.ItemByName['Width'].AsInteger;
  AForm.Height := AData.ItemByName['Height'].AsInteger;
end;

procedure TSettingsBroker.StartUp;
var
  m: IPersistRefList;
begin
  fStore.Open(FileName);
  m := (fStore as IPersistQuery).SelectClass('TAppSetting');
  if m.Count = 0 then begin
    fAppSetting := fStore.New('TAppSetting');
  end else begin
    fAppSetting := m.Data[0];
  end;
end;

procedure TSettingsBroker.ShutDown;
begin
  fStore.Save(fAppSetting);
end;

procedure TSettingsBroker.LoadStrings(const AName: string; AStrings: TStrings
  );
var
  m: IPersistRefList;
  mStoreStrings: IPersistManyStrings;
  i: integer;
begin
  AStrings.Clear;
  mStoreStrings := fAppSetting.ItemByName[AName].AsInterface as IPersistManyStrings;
  for i := 0 to mStoreStrings.Count - 1 do begin
    AStrings.Insert(0, mStoreStrings[i]);
  end;
end;

procedure TSettingsBroker.SaveStrings(const AName: string; AStrings: TStrings;
  const ANewText: string; AMaxCount: integer = 10);
var
  mStoreStrings: IPersistManyStrings;
  mInd: integer;
begin
  mStoreStrings := fAppSetting.ItemByName[AName].AsInterface as IPersistManyStrings;
  mInd := 0;
  while mInd <= mStoreStrings.Count - 1 do
   if SameText(ANewText, mStoreStrings[mInd]) then
     mStoreStrings.Delete(mInd)
   else
     inc(mInd);
  mStoreStrings.Count := mStoreStrings.Count + 1;
  mStoreStrings[mStoreStrings.Count - 1] := ANewText;
  while mStoreStrings.Count > AMaxCount do
    mStoreStrings.Delete(0);
end;

procedure TSettingsBroker.LoadWindow(const AName: string; AForm: TCustomForm
  );
var
  mWinSettings :IRBData;
begin
  mWinSettings := FindAppSettingWindow(AName, GetFormID(AForm));
  if mWinSettings <> nil then begin
    Data2Form(mWinSettings, AForm);
  end;
end;

procedure TSettingsBroker.SaveWindow(const AName: string; AForm: TCustomForm
  );
var
  mWinSettings :IRBData;
begin
  mWinSettings := FindAppSettingWindow(AName, GetFormID(AForm));
  if mWinSettings = nil then
    mWinSettings := NewAppSettingWindow(AName, GetFormID(AForm));
  Form2Data(AForm, mWinSettings);
end;

end.

