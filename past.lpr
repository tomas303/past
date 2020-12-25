program past;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  uApp, uappboot, ustoremanager, ipast;

{$R *.res}

begin
  TApp.Go;
end.

