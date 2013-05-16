program Clock2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms,
  Pictures, FindThread, alarm, ClockMain, Settings, MetOffice,
  Reminders, ReminderList, DatePicker, black, FindPicsThread, udpserver, sync,
  udpclient, music, udpcommandserver, MusicPlayer, WaitForMedia;

{$IFDEF WINDOWS}{$R picshow.rc}{$ENDIF}

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TfrmClockMain, frmClockMain);
  Application.CreateForm(TfrmSettings, frmSettings);
  Application.CreateForm(TfrmPictures, frmPictures);
  Application.CreateForm(TfrmReminders, frmReminders);
  Application.CreateForm(TfrmReminderList, frmReminderList);
  Application.CreateForm(TfrmDatePicker, frmDatePicker);
  Application.Run;
end.

