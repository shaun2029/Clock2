program Clock2;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Interfaces, SysUtils, Classes, Forms, Pictures, FindThread, alarm, ClockMain,
  Settings, Reminders, ReminderList, FindPicsThread, udpserver, sync, udpclient,
  music, commandserver, MusicPlayer, WaitForMedia, PlaylistCreator, TouchList,
  DateTime, SourcePicker, ConnectionHealth, Email, SignalHandler, lnetbase,
  DiscoverServer, mplayereq, mpd, RadioStations, equaliser, ExceptionHandler;

{$IFDEF WINDOWS}{$R picshow.rc}{$ENDIF}

{$R *.res}

var
  InstanceFile: TFileStream;
begin
  try
    InstanceFile := TFileStream.Create('/tmp/clock2.pid', fmCreate);

    Application.Initialize;
    Application.CreateForm(TfrmClockMain, frmClockMain);
    Application.OnException := @frmClockMain.CustomExceptionHandler;

    Application.CreateForm(TfrmSettings, frmSettings);
    Application.CreateForm(TfrmPictures, frmPictures);
    Application.CreateForm(TfrmReminders, frmReminders);
    Application.CreateForm(TfrmReminderList, frmReminderList);
    Application.CreateForm(TfrmSourcePicker, frmSourcePicker);

    InitSignalHandlers;
    OnSignal := @frmClockMain.SignalCallback;
    Application.Run;

    // Close the file
    InstanceFile.Free;
    DeleteFile('/tmp/clock2.pid');
  except
    Exit;
  end;
end.

