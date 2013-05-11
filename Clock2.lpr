program Clock2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
main, FindThread, alarm, ClockMain, ClockSettings, MetOffice,
  Reminders, ReminderList, DatePicker, black, FindPicsThread, udpserver, sync,
  udpclient, music, udpcommandserver, MusicPlayer, WaitForMedia;

{$IFDEF WINDOWS}{$R picshow.rc}{$ENDIF}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmReminders, frmReminders);
  Application.CreateForm(TfrmReminderList, frmReminderList);
  Application.CreateForm(TfrmDatePicker, frmDatePicker);
  Application.Run;
end.

