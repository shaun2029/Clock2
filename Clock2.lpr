program Clock2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, Pictures, FindThread, alarm, ClockMain, Settings, Reminders,
  ReminderList, FindPicsThread, udpserver, sync, udpclient, music,
  commandserver, MusicPlayer, WaitForMedia, PlaylistCreator, TouchList,
  UniqueInstanceRaw, DateTime, SourcePicker, ConnectionHealth, Email, lnetbase;

{$IFDEF WINDOWS}{$R picshow.rc}{$ENDIF}

{$R *.res}

begin
  if not InstanceRunning('Clock2App') then
  begin
    Application.Initialize;
    Application.CreateForm(TfrmClockMain, frmClockMain);
    Application.CreateForm(TfrmSettings, frmSettings);
    Application.CreateForm(TfrmPictures, frmPictures);
    Application.CreateForm(TfrmReminders, frmReminders);
    Application.CreateForm(TfrmReminderList, frmReminderList);
    Application.CreateForm(TfrmSourcePicker, frmSourcePicker);
    Application.Run;
  end;
end.

