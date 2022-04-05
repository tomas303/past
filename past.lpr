program past;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  uApp, ustoremanager, ipast, uappgui, uappdata, uappfunc;

{$R *.res}

begin
  TApp.Go;
end.

