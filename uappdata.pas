unit uappdata;

{$mode ObjFPC}{$H+}

interface

uses
  rea_udesigncomponentdata, SysUtils;

type

  { TStorageData }

  TStorageData = class
  private
    fFileNameData: TEditData;
    fPasswordData: TEditData;
  public
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
  published
    property FileNameData: TEditData read fFileNameData write fFileNameData;
    property PasswordData: TEditData read fPasswordData write fPasswordData;
  end;

implementation

{ TStorageData }

procedure TStorageData.AfterConstruction;
begin
  inherited AfterConstruction;
  fFileNameData := TEditData.Create;
  fPasswordData := TEditData.Create;
end;

procedure TStorageData.BeforeDestruction;
begin
  FreeAndNil(fFileNameData);
  FreeAndNil(fPasswordData);
  inherited BeforeDestruction;
end;

end.

