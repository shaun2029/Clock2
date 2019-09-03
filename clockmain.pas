//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit ClockMain;

{$mode Delphi}

{$DEFINE PICSHOW}
//{$DEFINE DEBUG}

// Zipit does not support media keys
{$IFNDEF CPUARM}
  {$DEFINE GRABXKEYS}
{$ENDIF}

interface

uses
  gtk2, gdk2, Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, Alarm, Settings, Reminders, ReminderList, LCLProc,
  Buttons, Music, Sync, Process, MusicPlayer, PlaylistCreator, commandserver, X,
  Xlib, CTypes, WaitForMedia, Pictures, DateTime, SourcePicker,
  ConnectionHealth, Unix, Email, IniFiles, SignalHandler, Equaliser, MplayerEQ,
  DiscoverServer, RadioStations, ExceptionHandler;

const
  VERSION = '3.5.4';

type
  TMusicState = (msPlaying, msPaused);
  TMusicSource = (msrcMusic, msrcSleep, msrcMeditation, msrcRadio);
  TMediaKey = (mkNone, mkAudioPlay, mkAudioNext);

  { TfrmClockMain }

  TfrmClockMain = class(TForm)
    btnStopAlarm: TBitBtn;
    imgEqualiser: TImage;
    imgExit: TImage;
    imgPlayAlbums: TImage;
    imgPrevious: TImage;
    imgOn: TImage;
    imgOff: TImage;
    imgDisplay: TImage;
    imgPlay: TImage;
    imgRadio: TImage;
    imgReminders: TImage;
    imgVolUp: TImage;
    imgVolDown: TImage;
    imgNext: TImage;
    imgMusic: TImage;
    ImgSleep: TImage;
    imgPictures: TImage;
    imgUpdateMusic: TImage;
    imgSettings: TImage;
    labSongPrev2: TLabel;
    labSongPrev1: TLabel;
    lbEqualiser: TLabel;
    lbPlayAlbums: TLabel;
    lbRadio: TLabel;
    lbReminders: TLabel;
    lbReminderSummary: TLabel;
    lbExit: TLabel;
    lbMusic: TLabel;
    lbSettings1: TLabel;
    ListBox1: TListBox;
    Radio: TLabel;
    lbPrevious: TLabel;
    lbPlay: TLabel;
    lbMusic2: TLabel;
    lbVolUp: TLabel;
    lbVolDown: TLabel;
    lbNext: TLabel;
    lbPictures: TLabel;
    lbDisplay: TLabel;
    labSong: TLabel;
    lblTime: TLabel;
    tmrCommand: TTimer;
    UpdateMusic: TLabel;
    lbSettings: TLabel;
    tmrMinute: TTimer;
    tmrTime: TTimer;
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure imgExitClick(Sender: TObject);
    procedure imgRadioClick(Sender: TObject);
    procedure imgMusicClick(Sender: TObject);
    procedure imgRemindersClick(Sender: TObject);
    procedure imgVolDownClick(Sender: TObject);
    procedure imgVolUpClick(Sender: TObject);
    procedure imgUpdateMusicClick(Sender: TObject);
    procedure labSongClick(Sender: TObject);
    procedure lbDisplayClick(Sender: TObject);
    procedure lbEqualiserClick(Sender: TObject);
    procedure lblTimeClick(Sender: TObject);
    procedure lbNextClick(Sender: TObject);
    procedure lbPlayAlbumsClick(Sender: TObject);
    procedure lbPicturesClick(Sender: TObject);
    procedure lbPlayClick(Sender: TObject);
    procedure lbPreviousClick(Sender: TObject);
    procedure lbSettingsClick(Sender: TObject);
    procedure lbMusic2Click(Sender: TObject);
    procedure tmrCommandTimer(Sender: TObject);
    procedure tmrTimeTimer(Sender: TObject);
    procedure tmrMinuteTimer(Sender: TObject);
  private
    { private declarations }
    FMplayerEQ: TMplayerEQ;
    FMPGPlayer: TMusicPlayer;
    FAlarm, FReminderAlarm: TAlarm;
    FCurrentReminders: TReminders;
    FTimer: TAlarm;
    FSyncServer: TSyncServer;
    FSyncClient: TSyncClient;
    FServerAddress, FServerPort: String;
    FDiscoverServer: TDiscoverServer;

    FAfterAlarmResumeMusic: boolean;
    FLinuxDateTime: TLinuxDateTime;
    FRadioStation: integer;
    FEmailReminders: boolean;
    FFormShown: boolean;

    FConfigFilename: string;

    FPlayer: TPlayer;
 	  FMusicState: TMusicState;
    FMusicSource: TMusicSource;
    FMusicNames: array [0..3] of string;
    FMusic2Source: TMusicSource;

    FSources: TSourceArray;

    FCOMServer: TCOMServer;

    FDisplay: PDisplay;
    FAlarmActive: boolean;

    FRadioPicker: TfrmSourcePicker;
    FMusicPicker: TfrmSourcePicker;

    FFavoritesAuto: boolean;

    procedure AddToFavorites;
    procedure ApplyMplayerEQ;
    procedure BacklightBright;
    procedure BacklightDim;
    procedure CreateMusicPicker;

    function DayOfWeekStr(Date: TDateTime): string;
    procedure DisplayVolume;
    function FormShowModal(MyForm: TForm): integer;
    function GetMonitorState: boolean;
    procedure HideForm(MyForm: TForm);
    procedure LoadRadioStations;
    procedure Log(Message: string);
    procedure PauseMusic;
    procedure PlayAlbums;
    procedure PlayMusic;
    procedure PlayNextMusic;
    procedure PlayPreviousMusic;
    procedure ProcessCommand(Command: TRemoteCommand);
    procedure SendReminders(Reminders: String);
    procedure SetCursorType(MyForm: TForm);
    procedure SetMonitorState(State: boolean);
    procedure SetMusicSource(Source: TMusicSource);
    procedure ShowForm(MyForm: TForm);
    procedure StartDiscoverServer;
    procedure UpdatingMusic(Player: TPlayer);

{$IFDEF GRABXKEYS}
    procedure GrabMediaKeys;
    procedure ReleaseMediaKeys;
    function GetMediaKeyPress: TMediaKey;
    function XErrorHandler(para1: PDisplay; para2: PXErrorEvent): cint; cdecl;
{$ENDIF}

    procedure UpdateSettings;
    procedure UpdateReminders;

    procedure BeforeAlarm;
    procedure AfterAlarm;
  public
    { public declarations }
    HTTPBuffer: string;
    function WaitForMedia(Path: string): boolean;
    procedure SignalCallback(Command: TRemoteCommand);
    procedure CustomExceptionHandler(Sender: TObject; E: Exception);
  published
  end;

var
  frmClockMain: TfrmClockMain;

implementation

{$R *.lfm}

{ TfrmClockMain }

procedure TfrmClockMain.CustomExceptionHandler(Sender: TObject; E: Exception);
begin
  DumpExceptionCallStack(E);
  Halt; // End of program execution
end;

procedure TfrmClockMain.ApplyMplayerEQ;
var
  IniFile: TIniFile;
  i: Integer;
begin
  try
    IniFile := TIniFile.Create(FConfigFilename);
    for i := 0 to 9  do
    begin
      IniFile.WriteInteger('Equaliser', EqBands[i], FMplayerEQ[i]);
    end;

    IniFile.Free;
  except
  end;

  if FPlayer.State = psPlaying then
  begin
    FPlayer.Pause;
    FPlayer.Pause;
  end;
end;

procedure TfrmClockMain.DisplayVolume;
begin
  tmrTime.Enabled := False;
  lblTime.Caption := 'VOL: ' + IntToStr(FMPGPlayer.GetVolume) + ' %';
  FCOMServer.Playing := lblTime.Caption;
  tmrTime.Interval := 3000;
  tmrTime.Enabled := True;
end;

procedure TfrmClockMain.PauseMusic;
begin
  FPlayer.Stop;
  FMusicState := msPaused;
end;

procedure TfrmClockMain.PlayMusic;
begin
  FPlayer.Play;
  FMusicState := msPlaying;

  // Display volume when play back is started.
  DisplayVolume;
end;

procedure TfrmClockMain.PlayNextMusic;
begin
  FPlayer.Next;
  FMusicState := msPlaying;
end;

procedure TfrmClockMain.PlayPreviousMusic;
begin
  FPlayer.Previous;
  FMusicState := msPlaying;
end;

function TfrmClockMain.WaitForMedia(Path: string): boolean;
var
  Timeout: TDateTime;
  frmWait: TfrmWaitForMedia;
begin
  Result := False;

  if (Path <> '') and not DirectoryExists(Path) then
  begin
    Timeout := EncodeTime(0,1,0,0);

    frmWait := TfrmWaitForMedia.Create(Self, Path, Timeout);
    Result := FormShowModal(frmWait) = mrOk;
    frmWait.Free;
  end
  else Result := True;
end;

procedure TfrmClockMain.SetMusicSource(Source: TMusicSource);
begin
  if Assigned(FPlayer) then FreeAndNil(FPlayer);

  //Create new source
  case Source of
    msrcSleep:
      begin
        FPlayer := TPlayer.Create(FMPGPlayer,
          ChangeFileExt(FConfigFilename, '_sleep.cfg'), frmSettings.edtSleepPath.Text, True);
      end;
    msrcMusic:
      begin
        FPlayer := TPlayer.Create(FMPGPlayer,
          ChangeFileExt(FConfigFilename, '_music.cfg'), frmSettings.edtMusicPath.Text, True);
      end;
    msrcMeditation:
      begin
        FPlayer := TPlayer.Create(FMPGPlayer,
          ChangeFileExt(FConfigFilename, '_meditation.cfg'), frmSettings.edtMeditationPath.Text, True);
      end;
    else
    begin
      FPlayer := TPlayer.Create(FMPGPlayer, '', '', True);
      Source := msrcRadio;
    end;
  end;

  FMusicSource := Source;
end;

procedure TfrmClockMain.BeforeAlarm;
begin
  FAlarmActive := True;
  FAfterAlarmResumeMusic := FMusicState = msPlaying;
  PauseMusic;
end;

procedure TfrmClockMain.AfterAlarm;
begin
  // Possibly start music after alarm
  if frmSettings.cbxPlayMusic.Checked or FAfterAlarmResumeMusic then
  begin
    PlayMusic;
  end;

  FAlarmActive := False;
end;

function TfrmClockMain.DayOfWeekStr(Date: TDateTime): string;
var
  DOW: Integer;
begin
  DOW := DayOfWeek(Date);

  case DOW of
    1: Result := 'Sunday';
    2: Result := 'Monday';
    3: Result := 'Tuesday';
    4: Result := 'Wednesday';
    5: Result := 'Thursday';
    6: Result := 'Friday';
    else Result := 'Saturday';
  end;
end;

procedure TfrmClockMain.SendReminders(Reminders: String);
var
  Mail: TEmail;
  Titles: TStringList;
begin
  Mail := TEmail.Create(frmSettings.edtSMTPAccount.Text, frmSettings.edtSMTPPassword.Text);
  Titles := TStringList.Create;

  try
    Titles.Text := Reminders;

    if Titles.Count > 0 then
    begin
      // ToDo: Errors
      Mail.Send(Trim(frmSettings.edtSMTPAccount.Text), Trim(frmSettings.edtEmailAddress.Text),
        'Clock2 Reminders', Titles.Text);
    end;
  except
    on E: Exception do
    begin
    end;
  end;

  Titles.Free;
  Mail.Free;
end;

procedure TfrmClockMain.tmrTimeTimer(Sender: TObject);
var
  Current: TDateTime;
  DayStr: string;
  TimeCaption: string;
  ReminderList: TStringList;
  i: Integer;
  Key: Char;
  PlayerName, Song: string;
  Day, Month, Year: word;
  Hour, Min, Sec, MSec: word;
begin
  tmrTime.Interval := 1000;
  Current := FLinuxDateTime.GetLocalTime;

  DayStr := Copy(DayOfWeekStr(Current), 1, 3);
  DecodeDate(Current, Year, Month, Day);
  DecodeTime(Current, Hour, Min, Sec, MSec);
  TimeCaption := Format('%s %.2d/%.2d %.2d:%.2d:%.2d ', [DayStr, Day, Month, Hour, Min, Sec]);

  if TimeCaption <> lblTime.Caption then
    lblTime.Caption := TimeCaption;

  // Turn off timer
  if Current > FTimer.AlarmTime + EncodeTime(0, 5, 0, 0) then
  begin
    for i := 0 to High(FTimer.Days) do
    begin
      if FTimer.Days[i] <> False then
        FTimer.Days[i] := False;
    end;
  end;

  FReminderAlarm.Silent:= not frmSettings.cbxReminderAlarm.Checked;

  FAlarm.Tick(Current);
  FReminderAlarm.Tick(Current);
  FTimer.Tick(Current);

  if (FReminderAlarm.State = asActive) and (not FReminderAlarm.Acknowledged) then
  begin
    FReminderAlarm.Acknowledged := True;
    lbReminderSummary.Font.Color := clYellow;
    ReminderList := TStringList.Create;
    frmReminders.SortReminders(FCurrentReminders);
    frmReminders.PopulateList(FCurrentReminders, ReminderList);
    lbReminderSummary.Caption := ReminderList.Text;

    if FEmailReminders then
    begin
      SendReminders(ReminderList.Text);
    end;

    ReminderList.Free;
  end;

  if (FTimer.State = asSet) and (FTimer.AlarmTime > Current) then
  begin
    DecodeTime(FTimer.AlarmTime - Current, Hour, Min, Sec, MSec);
    TimeCaption := Format('%.2d:%.2d', [Min, Sec]);

    if TimeCaption <> btnStopAlarm.Caption then
      btnStopAlarm.Caption := TimeCaption;

    btnStopAlarm.Visible := True;
  end
  else
  begin
    btnStopAlarm.Caption := 'Stop';
    btnStopAlarm.Visible := ((FReminderAlarm.State = asActive) and (not FReminderAlarm.Silent))
      or (FTimer.State = asActive) or (FAlarm.State = asActive);
  end;

  case FMusicSource of
    msrcSleep, msrcMeditation, msrcMusic, msrcRadio:
      begin
        PlayerName := FMusicNames[Ord(FMusicSource)];
      end;
    else PlayerName := '';
  end;

  Song := 'Shaun''s Clock Version: ' + VERSION;

  if (PlayerName <> '') then
  begin
    i := FPlayer.Tick;

    if i >= 0  then
      Song := Format('Updating %s list ... %d', [PlayerName, i])
    else
    begin
      if FPlayer.State = psPlaying then
      begin
        if FMusicSource = msrcRadio then
          Song := FPlayer.SongArtist + ': ' + FPlayer.SongTitle
        else
          Song := FPlayer.SongArtist + ' - ' + FPlayer.SongTitle;
      end;
    end;
  end;

  if labSong.Caption <> Song then
  begin
    labSong.Font.Color := clWhite;
    labSongPrev2.Caption := labSongPrev1.Caption;
    labSongPrev1.Caption := labSong.Caption;
    labSong.Caption := Song;
  end;

  if Assigned(FCOMServer) then
  begin
    FCOMServer.Playing := LabSong.Caption;
  end;

{$IFDEF GRABXKEYS}
  case GetMediaKeyPress of
    mkAudioPlay:
      begin
        Key := 'p';
        FormKeyPress(Self, Key);
      end;
    mkAudioNext:
      begin
        case FMusicSource of
          msrcSleep: Key := 's';
          msrcMeditation: Key := 'd';
          msrcRadio: Key := 'n';
          else Key := 'm';
        end;

        FormKeyPress(Self, Key);
      end;
  end;
{$ENDIF}
end;

procedure TfrmClockMain.tmrMinuteTimer(Sender: TObject);
var
  Rems: string;
  CurrentList: TStringList;
  H, M, S, Ms: word;
  Error: String;
  i: Integer;
begin
  tmrMinute.Enabled := False;
  tmrMinute.Interval := 60000;

  // Update discover server with Clock name
  if Assigned(FDiscoverServer) then
  begin
    if FDiscoverServer.Error <> desOK then
      StartDiscoverServer;
  end;

  DecodeTime(Now, H, M, S, Ms);

  // Try and send favorites every week for 1 munute.
  if FFavoritesAuto and (DayOfWeek(Now) = 7) and ((H * 10) + M < 2) then
    frmSettings.SendFavorites(Error);

  // Reminders every two minutes
  if tmrMinute.Tag >= 1 then
  begin
    tmrMinute.Tag := 0;

    if Assigned(FSyncServer) then
      FSyncServer.RemindersFile(frmReminders.Filename)
    else if Assigned(FSyncClient) then
    begin
      if FSyncClient.GetReminders(FServerAddress, FServerPort, Rems) then
      begin
        CurrentList := TStringList.Create;

        try
          if FileExists(frmReminders.Filename) then
            CurrentList.LoadFromFile(frmReminders.Filename);

          if CurrentList.Text <> Rems then
          begin
            CurrentList.Text := Rems;
            CurrentList.SaveToFile(frmReminders.Filename);
            frmReminders.ReadReminders;
          end;
        except
        end;

        CurrentList.Free;

        frmReminders.ReadReminders;
      end;
    end;

    frmReminders.RefreshReminders;
    UpdateReminders;

    Rems := '';
    CurrentList := TStringList.Create;
    frmReminders.SortReminders(FCurrentReminders);
    frmReminders.PopulateList(FCurrentReminders, CurrentList);
    for i:= 0 to CurrentList.Count - 1 do
    begin
      Rems := Rems + CurrentList.Strings[i] + ';';
    end;
    FComServer.Reminders := Rems;

    lbReminderSummary.Caption := CurrentList.Text;
    CurrentList.Free;
  end
  else tmrMinute.Tag := tmrMinute.Tag + 1;

  tmrMinute.Enabled := True;
end;

// True = Monitor On
function TfrmClockMain.GetMonitorState: boolean;
var
  Output: string;
  Commands: array [0..0] of string;
begin
  Result := False;
  Commands[0] := 'q';

  if RunCommand('xset', Commands, Output) then
  begin
    Result := (Pos('monitor is on', Lowercase(Output)) > 0);
  end;
end;

// True = Monitor On
procedure TfrmClockMain.SetMonitorState(State: boolean);
begin
  if State then
  begin
    fpSystem('xset -dpms');
    fpSystem('xdotool mousemove 1 1');
    fpSystem('xdotool mousemove 100 100');
    fpSystem('xset +dpms');
  end
  else
  begin
    fpSystem('xset dpms force off');
  end;
end;

procedure TfrmClockMain.BacklightDim;
begin
  try
    fpSystem('sudo sh -c "echo 4 > /sys/class/backlight/openframe-bl/brightness"');
  except
  end;
end;

procedure TfrmClockMain.BacklightBright;
begin
  try
    fpSystem('sudo sh -c "echo 32 > /sys/class/backlight/openframe-bl/brightness"');
  except
  end;
end;

procedure TfrmClockMain.LoadRadioStations;
var
  Stations: TStringList;
  ConfigFile: string;
  i, s, t: Integer;
  RadioStations: String;
begin
  if frmRadioStations.Changed then
  begin
    SetLength(FSources, 0);

    // Load custom stations and add them
    ConfigFile := ChangeFileExt(FConfigFilename, '_radio_stations.cfg');

    frmRadioStations.ConfigFile := ConfigFile;

    Stations := TStringList.Create;

    if not FileExists(ConfigFile) then
    begin
      // Save default stations
      try
        Stations.Text := frmRadioStations.mmoRadioDefaults.Text;
        Stations.SaveToFile(ConfigFile);
      except
      end;
    end
    else
    begin
      Stations := TStringList.Create;
      try
        Stations.LoadFromFile(ConfigFile);
      except
      end;
    end;

    frmRadioStations.mmoRadioStations.Text := Stations.Text;

    try
      for i := Stations.Count - 1 downto 0 do
      begin
        Stations.Strings[i] := Trim(Stations.Strings[i]);

        if Stations.Strings[i] = '' then
          Stations.Delete(i);
      end;

      t := Stations.Count div 2;
      SetLength(FSources, t);

      for i := 0 to t - 1 do
      begin
        FSources[i].Title := Stations.Strings[i*2];
        FSources[i].Resource := Stations.Strings[(i*2)+1];
      end;
    finally
      Stations.Free;
    end;

    // Load stations into Command Server
    RadioStations := '';
    for i := 0 to High(FSources) do
    begin
      RadioStations := RadioStations + FSources[i].Title + ';';
    end;
    FComServer.RadioStations := RadioStations;

    if Assigned(FRadioPicker) then FreeAndNil(FRadioPicker);

    FRadioPicker := TfrmSourcePicker.Create(Self, FSources);

    frmRadioStations.Changed := False;
  end;
end;

procedure TfrmClockMain.FormCreate(Sender: TObject);
var
  i: Integer;
  MixerControl, SerialDevice : string;
  UsePulseVol: boolean;
  UseVol: string;
  VolControl: TVolumeControl;
  IniFile: TIniFile;
begin
  FFormShown := False;
  Self.Color := clBlack;

  // Gets created when settings are updated
  FDiscoverServer := nil;

  FRadioPicker := nil;

  labSong.Caption := '';
  labSongPrev1.Caption := '';
  labSongPrev2.Caption := '';

  FMusicNames[Ord(msrcSleep)] := 'Sleep';
  FMusicNames[Ord(msrcMeditation)] := 'Meditation';
  FMusicNames[Ord(msrcMusic)] := 'Music';
  FMusicNames[Ord(msrcRadio)] := 'Radio';
  FMusic2Source := msrcSleep;

  FConfigFilename := GetAppConfigFile(False);
  VolControl := vcAlsa;
  UsePulseVol := True;
  MixerControl := 'Master';

  SetLength(FMplayerEQ, 10);

  for i := 0 to 9  do
  begin
    FMplayerEQ[i] := 0;
  end;

  try
    IniFile := TIniFile.Create(FConfigFilename);
    UseVol := IniFile.ReadString('Volume', 'UseVolControl', '');
    UsePulseVol := IniFile.ReadBool('Volume', 'UsePulse', UsePulseVol);
    MixerControl := IniFile.ReadString('Volume', 'MixerControl', MixerControl);
    SerialDevice := IniFile.ReadString('Control', 'SerialDevice', '/dev/ttyUSB0');

    for i := 0 to 9  do
    begin
      FMplayerEQ[i] := IniFile.ReadInteger('Equaliser', EqBands[i], 0);
    end;

    IniFile.Free;
  except
  end;

  if (LowerCase(UseVol) = '') then
  begin
    if UsePulseVol then
       VolControl := vcPulse
    else VolControl := vcAlsa;
  end
  else if (LowerCase(UseVol) = 'alsa') then
    VolControl := vcAlsa
  else if (LowerCase(UseVol) = 'pulse') then
    VolControl := vcPulse
  else VolControl := vcSoftVol;

  FMPGPlayer := TMusicPlayer.Create(MixerControl, VolControl, FMplayerEQ);

  FAlarm := TAlarm.Create(FMPGPlayer);
  FAlarm.Path := ExtractFilePath(Application.ExeName);

  FReminderAlarm := TAlarm.Create(FMPGPlayer);
  FReminderAlarm.Path := ExtractFilePath(Application.ExeName);

  FTimer := TAlarm.Create(FMPGPlayer);
  FTimer.Path := ExtractFilePath(Application.ExeName);

  FAlarm.OnBeforeAlarm := BeforeAlarm;
  FReminderAlarm.OnBeforeAlarm := BeforeAlarm;
  FTimer.OnBeforeAlarm := BeforeAlarm;

  FAlarm.OnAfterAlarm := AfterAlarm;
  FReminderAlarm.OnAfterAlarm := AfterAlarm;
  FTimer.OnAfterAlarm := AfterAlarm;

  FAlarmActive := False;

  FSyncServer := nil;
  FSyncClient := nil;

  FCOMServer := TCOMServer.Create(44558, SerialDevice);
  tmrCommand.Enabled := True;

  FLinuxDateTime := TLinuxDateTime.Create;

  frmRadioStations := TfrmRadioStations.Create(Self);
  LoadRadioStations;

  FMusicPicker := nil;
  CreateMusicPicker;

  FPlayer := nil;
  FMusicState := msPaused;
  FMusicSource := msrcRadio;

  SetMusicSource(msrcRadio);

  FRadioStation := 0;
  FPlayer.StreamTitle := FSources[FRadioStation].Title;
  FPlayer.StreamURL := FSources[FRadioStation].Resource;

  // Activate Reminder update.
  tmrMinute.Interval := 10000;
  tmrMinute.Tag := 9999;
{$IFDEF GRABXKEYS}
  GrabMediaKeys;
{$ENDIF}
end;

procedure TfrmClockMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  FAlarm.ResetAlarm;
  FTimer.ResetAlarm;
  FReminderAlarm.ResetAlarm;
  FLinuxDateTime.Free;
{$IFDEF GRABXKEYS}
  ReleaseMediaKeys;
{$ENDIF}
  FRadioPicker.Free;
  FMusicPicker.Free;
end;

procedure TfrmClockMain.FormActivate(Sender: TObject);
begin
  Randomize;

  if frmSettings.cbxForceFullscreen.Checked then
    gdk_window_fullscreen(PGtkWidget(Handle)^.window);
end;

procedure TfrmClockMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FPlayer) then FPlayer.Free;

  FMPGPlayer.Free;

  if Assigned(FSyncClient) then
    FreeAndNil(FSyncClient);

  if Assigned(FSyncServer) then
    FreeAndNil(FSyncServer);

  FAlarm.Free;
  FReminderAlarm.Free;
  FTimer.Free;
  FCOMServer.Free;
  frmRadioStations.Free;
end;

procedure TfrmClockMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = 112) or (key = 72) then
  begin
    Key := 0;

    ShowMessage('Keyboard Shortcuts' +  LineEnding +
      'Power - Powerdown' + LineEnding +
      'Power + Shift (...) - Reboot' + LineEnding + LineEnding +
      'W - Configure Wifi' + LineEnding + LineEnding +
      'Enter - Settings' + LineEnding +
      'Space - Stop alarm' + LineEnding +
      'M - Play music / skip song' + LineEnding +
      'L - List and select music' + LineEnding +
      'S - Play sleep music / skip song' + LineEnding +
      'P - Stop music' + LineEnding +
      'U - Update music' + LineEnding + LineEnding +
      'R - Reminders'  + LineEnding + LineEnding +
      'Smiley :-) - Exit');
  end
  {$IFNDEF PICSHOW}
  else if (Key = 43) and (ssShift in Shift) then
  begin
    Key := 0;
    Shutdown(True);
  end
  else if Key = 43 then
  begin
    Key := 0;
    Shutdown(False);
  end
  else if Key = 87 then
  begin
    Key := 0;
    ConfigureWifi;
  end
  else if (Key = 27) and (ssShift in Shift) then
  begin
    Key := 0;
    CloseApp;
  end;
  {$ELSE};
  {$ENDIF}
end;

procedure TfrmClockMain.FormKeyPress(Sender: TObject; var Key: char);
begin
  tmrMinute.Enabled := False;

  FAlarm.ResetAlarm;
  FReminderAlarm.ResetAlarm;
  FTimer.ResetAlarm;

  if Key = #27 then {$IFDEF PICSHOW} Close {$ENDIF}
  else if Key = #13 then
  begin
    FormShowModal(frmSettings);
    UpdateSettings;
  end
  else if (Key = 'l') or (Key='L') then
  begin
    PlayAlbums;
  end
  else if (Key = 'r') or (Key = 'R') then frmReminderList.Show
  else if (Key = 'n') or (Key = 'N') then
  begin
    if FMusicSource = msrcRadio then
    begin
      if (FRadioStation > High(FSources)) then
        FRadioStation := 0;
    end;

    SetMusicSource(msrcRadio);

    FPlayer.StreamTitle := FSources[FRadioStation].Title;
    FPlayer.StreamURL := FSources[FRadioStation].Resource;

    if not FAlarmActive then PlayMusic
    else PauseMusic;
  end
  else if (Key = 'm') or (Key = 'M') then
  begin
    SetMusicSource(msrcMusic);

    if not FAlarmActive then PlayMusic
    else PauseMusic;
  end
  else if (Key = 's') or (Key = 'S') then
  begin
    SetMusicSource(msrcSleep);

    if not FAlarmActive then PlayMusic
    else PauseMusic;
  end
  else if (Key = 'd') or (Key = 'D') then
  begin
    SetMusicSource(msrcMeditation);

    if not FAlarmActive then PlayMusic
    else PauseMusic;
  end
  else if (Key = 't') or (Key = 'T') then
  begin
    lbDisplayClick(Self);
  end
  else if (Key = 'p') or (Key = 'P') then
  begin
    case FMusicState of
      msPlaying: PauseMusic;
      else PlayMusic;
    end;
  end
  else if (Key = 'u') or (Key = 'U') then
  begin
    if (FMusicSource <> msrcRadio) then
    begin
      FPlayer.RescanSearchPath;
    end;
  end
  else if (Key = '.') then
  begin
    FMPGPlayer.VolumeUp;
    DisplayVolume;
  end
  else if (Key = ',') then
  begin
    FMPGPlayer.VolumeDown;
    DisplayVolume;
  end;

  tmrMinute.Enabled := True;
end;

procedure TfrmClockMain.SetCursorType(MyForm: TForm);
var
  i: Integer;
begin

  // Disable cursor if in Touchscreen mode
  if frmSettings.cbxTouchScreen.Checked then
    MyForm.Cursor := crNone
  else MyForm.Cursor := crDefault;

  for i := 0 to MyForm.ComponentCount - 1 do
  begin
    if MyForm.Components[i] is TControl then
    begin
      if frmSettings.cbxTouchScreen.Checked then
        TControl(MyForm.Components[i]).Cursor := crNone
      else TControl(MyForm.Components[i]).Cursor := crDefault;
    end;
  end;
end;

procedure TfrmClockMain.FormShow(Sender: TObject);
begin
  if not FFormShown then
  begin
    FFormShown := True;

    if not FileExists(ExtractFilePath(Application.ExeName) + 'alarm.mp3')
      and not FileExists('/usr/share/clock/alarm.mp3') then
      ShowMessage('Alarm Failure' + LineEnding
      + 'The mp3 file "alarm.mp3" can not be found.' + LineEnding
      + 'Please copy the file "alarm.mp3" to the location:' + LineEnding
      + '/usr/share/clock/alarm.mp3')
    else if not FileExists('/usr/bin/mplayer') then
      ShowMessage('Audio Playback Failure' + LineEnding
      + 'The package mplayer was not found on this system.' + LineEnding
      + 'Please install mplayer to enable audio playback:' + LineEnding
      + 'sudo apt-get install mplayer');

    UpdateSettings;
    frmReminderList.FReminders := frmReminders;

    tmrMinute.Enabled := True;
  end;
end;

procedure TfrmClockMain.imgExitClick(Sender: TObject);
begin
  imgExit.Picture.Assign(imgOff.Picture);
  Application.ProcessMessages;
  Close;
end;

procedure TfrmClockMain.CreateMusicPicker;
var
  Sources: TSourceArray;
begin
  if Assigned(FMusicPicker) then
    FreeAndNil(FMusicPicker);

  SetLength(Sources, 3);
  Sources[0].Title := FMusicNames[Ord(msrcMusic)];
  Sources[1].Title := FMusicNames[Ord(msrcSleep)];
  Sources[2].Title := FMusicNames[Ord(msrcMeditation)];
  Sources[0].Resource := '';
  Sources[1].Resource := '';
  Sources[2].Resource := '';

  FMusicPicker := TfrmSourcePicker.Create(Self, Sources);
end;

procedure TfrmClockMain.imgMusicClick(Sender: TObject);
var
  Sources: TSourceArray;
begin
  if DirectoryExists(frmSettings.edtMeditationPath.Text) then
  begin
    SetCursorType(FMusicPicker);

    if FMusicPicker.ShowModal = mrOK then
    begin
      case FMusicPicker.ItemIndex of
        Ord(msrcMusic):   SetMusicSource(msrcMusic);
        Ord(msrcSleep):   SetMusicSource(msrcSleep);
        Ord(msrcMeditation):   SetMusicSource(msrcMeditation);
        Ord(msrcRadio):   SetMusicSource(msrcRadio);
      end;

      if not FAlarmActive then PlayMusic
        else PauseMusic;
    end;
  end
  else
  begin
    imgMusic.Picture.Assign(imgOff.Picture);
    Application.ProcessMessages;

    SetMusicSource(msrcMusic);

    if not FAlarmActive then PlayMusic
      else PauseMusic;

    imgMusic.Picture.Assign(imgOn.Picture);
  end;
end;

procedure TfrmClockMain.imgRemindersClick(Sender: TObject);
begin
  if (FAlarm.State = asActive)
    or (FTimer.State = asActive)
    or (FReminderAlarm.State = asActive) then
  begin
    FAlarm.ResetAlarm;
    FTimer.ResetAlarm;
    FReminderAlarm.ResetAlarm;
  end
  else
  begin
    FormShowModal(frmReminderList);
  end;
end;

procedure TfrmClockMain.imgVolDownClick(Sender: TObject);
begin
  FMPGPlayer.VolumeDown;
  DisplayVolume;
end;

procedure TfrmClockMain.imgVolUpClick(Sender: TObject);
begin
  FMPGPlayer.VolumeUp;
  DisplayVolume;
end;


procedure TfrmClockMain.UpdatingMusic(Player: TPlayer);
var
  i: Integer;
begin
  if Assigned(Player) then
  begin
    repeat
      Sleep(500);
      i := Player.Tick;
      Application.ProcessMessages;
    until i < 0;
  end;
end;

procedure TfrmClockMain.imgUpdateMusicClick(Sender: TObject);
var
  MusicSource: TMusicSource;
begin
  imgUpdateMusic.Picture.Assign(imgOff.Picture);
  Self.Enabled := False;

  Application.ProcessMessages;

  if DirectoryExists(frmSettings.edtMeditationPath.Text) then
  begin
    SetMusicSource(msrcMeditation);

    if (FPlayer.SearchPath <> '') then
    begin
      FPlayer.RescanSearchPath;
      UpdatingMusic(FPlayer);
    end;
  end;

  if DirectoryExists(frmSettings.edtSleepPath.Text) then
  begin
    SetMusicSource(msrcSleep);

    if (FPlayer.SearchPath <> '') then
    begin
      FPlayer.RescanSearchPath;
      UpdatingMusic(FPlayer);
    end;
  end;

  if DirectoryExists(frmSettings.edtMusicPath.Text) then
  begin
    SetMusicSource(msrcMusic);

    if (FPlayer.SearchPath <> '') then
    begin
      FPlayer.RescanSearchPath;
      UpdatingMusic(FPlayer);
    end;
  end;

  Self.Enabled := True;
  imgUpdateMusic.Picture.Assign(imgOn.Picture);
end;

procedure TfrmClockMain.AddToFavorites;
var
  Titles: TStringList;
  FavFile, TimeStr: string;
begin
  if labSong.Font.Color = clYellow then Exit;
  // Signal that the favorite has been added.
  labSong.Font.Color := clYellow;

  FavFile := ChangeFileExt(FConfigFilename, '_favorites.txt');
  Titles := TStringList.Create;

  try
    DateTimeToString(TimeStr, 'yyyy/mm/dd hh:nn:ss :- ', Now);
    if FileExists(FavFile) then
      Titles.LoadFromFile(FavFile);
    Titles.Insert(0, TimeStr + LabSong.Caption);
    Titles.SaveToFile(FavFile);
  except
    on E: Exception do
    begin
      ShowMessage('Exception: ' + E.Message);
    end;
  end;

  Titles.Free;
end;

procedure TfrmClockMain.labSongClick(Sender: TObject);
begin
  AddToFavorites;
end;

procedure TfrmClockMain.lbDisplayClick(Sender: TObject);
begin
  SetMonitorState(False);
end;

procedure TfrmClockMain.lbEqualiserClick(Sender: TObject);
var
  frmMplayerEQ: TfrmMplayerEQ;
  i: integer;
begin
  frmMplayerEQ := TfrmMplayerEQ.Create(Self);
  frmMplayerEQ.Levels := FMplayerEQ;

  if frmSettings.cbxForceFullscreen.Checked then
  begin
    if frmMplayerEQ.BorderStyle <> bsNone then
      frmMplayerEQ.BorderStyle := bsNone;
  end
  else
  begin
    if frmMplayerEQ.BorderStyle <> bsSingle then
      frmMplayerEQ.BorderStyle := bsSingle;
  end;

  FormShowModal(frmMplayerEQ);
  frmMplayerEQ.Free;
  ApplyMplayerEQ;
end;

procedure TfrmClockMain.imgRadioClick(Sender: TObject);
begin
  LoadRadioStations;

  if frmSettings.cbxForceFullscreen.Checked then
  begin
    if FRadioPicker.BorderStyle <> bsNone then
      FRadioPicker.BorderStyle := bsNone;
  end
  else
  begin
    if FRadioPicker.BorderStyle <> bsSingle then
      FRadioPicker.BorderStyle := bsSingle;
  end;

  SetCursorType(FRadioPicker);

  if FRadioPicker.ShowModal = mrOK then
  begin
    PauseMusic;
    SetMusicSource(msrcRadio);
    FPlayer.StreamTitle := FSources[FRadioPicker.ItemIndex].Title;
    FPlayer.StreamURL := FSources[FRadioPicker.ItemIndex].Resource;

    if not FAlarmActive then
      PlayMusic;
  end;
end;

procedure TfrmClockMain.lbMusic2Click(Sender: TObject);
begin
  imgSleep.Picture.Assign(imgOff.Picture);
  Application.ProcessMessages;

  SetMusicSource(FMusic2Source);

  if not FAlarmActive then PlayMusic
  else PauseMusic;

  imgSleep.Picture.Assign(imgOn.Picture);
end;

procedure TfrmClockMain.tmrCommandTimer(Sender: TObject);
var
  Command: TRemoteCommand;
begin
  tmrCommand.Enabled := False;

  Command := FComServer.Command;
  if (Command <> rcomNone) then
     ProcessCommand(Command);

  tmrCommand.Enabled := True;
end;

procedure TfrmClockMain.SignalCallback(Command: TRemoteCommand);
begin
  ProcessCommand(Command);
end;

procedure TfrmClockMain.ProcessCommand(Command: TRemoteCommand);
var
  Key: Char;
begin
  case Command of
    rcomNext:
      begin
        FPlayer.Next;
      end;
    rcomPrevious:
      begin
        FPlayer.Previous;
      end;
    rcomMusic:
      begin
        Key := 'm';
        FormKeyPress(Self, Key);
      end;
    rcomSleep:
      begin
        Key := 's';
        FormKeyPress(Self, Key);
      end;
    rcomMeditation:
      begin
        Key := 'd';
        FormKeyPress(Self, Key);
      end;
    rcomDisplayToggle:
      begin
        Key := 't';
        FormKeyPress(Self, Key);
      end;
    rcomRadio:
      begin
        Key := 'n';
        FormKeyPress(Self, Key);
      end;
    rcomSetRadioStation:
      begin
        FRadioStation := FComServer.RadioStation;
        Key := 'n';
        FormKeyPress(Self, Key);
      end;
    rcomPause:
      begin
        case FMusicState of
          msPlaying: PauseMusic;
          else PlayMusic;
        end;
      end;
    rcomVolumeUp:
      begin
        FMPGPlayer.VolumeUp;
        DisplayVolume;
      end;
    rcomVolumeDown:
      begin
        FMPGPlayer.VolumeDown;
        DisplayVolume;
      end;
    rcomFavorite:
      begin
        AddToFavorites;
      end;
  end;
end;

procedure TfrmClockMain.lbPlayClick(Sender: TObject);
begin
  imgPlay.Picture.Assign(imgOff.Picture);
  Application.ProcessMessages;

  case FMusicState of
    msPlaying: PauseMusic;
    else PlayMusic;
  end;

  imgPlay.Picture.Assign(imgOn.Picture);
end;

function TfrmClockMain.FormShowModal(MyForm: TForm): integer;
begin
  gdk_window_unfullscreen(PGtkWidget(Handle)^.window);

  SetCursorType(MyForm);
  Result := MyForm.ShowModal;

  if frmSettings.cbxForceFullscreen.Checked then
    gdk_window_fullscreen(PGtkWidget(Handle)^.window);
end;

procedure TfrmClockMain.ShowForm(MyForm: TForm);
begin
  gdk_window_unfullscreen(PGtkWidget(Handle)^.window);

  SetCursorType(MyForm);
  MyForm.Show;
end;

procedure TfrmClockMain.HideForm(MyForm: TForm);
begin
  MyForm.Hide;
  MyForm.Close;
end;

procedure TfrmClockMain.lbSettingsClick(Sender: TObject);
begin
  if frmSettings.cbxForceFullscreen.Checked then
  begin
    if frmSettings.BorderStyle <> bsNone then
      frmSettings.BorderStyle := bsNone;
  end
  else
  begin
    if frmSettings.BorderStyle <> bsSingle then
      frmSettings.BorderStyle := bsSingle;
  end;

  FormShowModal(frmSettings);
  Application.ProcessMessages;

  UpdateSettings;
end;

procedure TfrmClockMain.lbNextClick(Sender: TObject);
begin
  imgNext.Picture.Assign(imgOff.Picture);
  Application.ProcessMessages;

  PlayNextMusic;

  imgNext.Picture.Assign(imgOn.Picture);
end;

procedure TfrmClockMain.lbPlayAlbumsClick(Sender: TObject);
begin
  PlayAlbums;
end;

procedure TfrmClockMain.lbPreviousClick(Sender: TObject);
begin
  imgPrevious.Picture.Assign(imgOff.Picture);
  Application.ProcessMessages;

  PlayPreviousMusic;

  imgPrevious.Picture.Assign(imgOn.Picture);
end;

procedure TfrmClockMain.lbPicturesClick(Sender: TObject);
begin
  imgPictures.Picture.Assign(imgOff.Picture);
  Application.ProcessMessages;

  if frmSettings.edtPicturePath.Text = '' then
  begin
    frmSettings.PageControl1.TabIndex := 0;

    if frmSettings.cbxForceFullscreen.Checked then
    begin
      if frmSettings.BorderStyle <> bsNone then
        frmSettings.BorderStyle := bsNone;
    end
    else
    begin
      if frmSettings.BorderStyle <> bsSingle then
        frmSettings.BorderStyle := bsSingle;
    end;

    FormShowModal(frmSettings);
  end;

  if frmSettings.edtPicturePath.Text <> '' then
  begin
    if WaitForMedia(frmSettings.edtPicturePath.Text) then
    begin
      FormShowModal(frmPictures);
    end;
  end;

  imgPictures.Picture.Assign(imgOn.Picture);
end;

procedure TfrmClockMain.lblTimeClick(Sender: TObject);
var
  i: Integer;
begin
  if (FAlarm.State = asActive)
    or (FTimer.State = asActive)
    or (FReminderAlarm.State = asActive) then
  begin
    // Stop alarm ringing.
    FAlarm.ResetAlarm;
    FTimer.ResetAlarm;
    FReminderAlarm.ResetAlarm;
  end
  else
  begin
    // Turn off timer alarm.
    for i := 0 to High(FTimer.Days) do
      FTimer.Days[i] := False;
  end;
end;

procedure TfrmClockMain.UpdateSettings;
var
  i: Integer;
  Minutes: LongInt;
  Hours: Integer;
begin
  // Get music source names
  FMusicNames[Ord(msrcSleep)] := Trim(frmSettings.edtSleep.Text);
  FMusicNames[Ord(msrcMeditation)] := Trim(frmSettings.edtMeditation.Text);
  FMusicNames[Ord(msrcMusic)] := Trim(frmSettings.edtMusic.Text);
  FMusicNames[Ord(msrcRadio)] := Trim('Radio');

  // Update button names
  lbRadio.Caption := FMusicNames[Ord(msrcRadio)];

  if DirectoryExists(frmSettings.edtMeditationPath.Text) then
  begin
    // Recreate music picker
    CreateMusicPicker;
    lbMusic.Caption := 'Music';

    lbMusic2.Caption := FMusicNames[Ord(msrcMusic)];
    FMusic2Source := msrcMusic;
  end
  else
  begin
    lbMusic.Caption := FMusicNames[Ord(msrcMusic)];
    lbMusic2.Caption := FMusicNames[Ord(msrcSleep)];
    FMusic2Source := msrcSleep;
  end;


  FFavoritesAuto := frmSettings.cbxFavoritesAuto.Checked;

  FAlarm.Days[1] := frmSettings.cbxSun.Checked;
  FAlarm.Days[2] := frmSettings.cbxMon.Checked;
  FAlarm.Days[3] := frmSettings.cbxTue.Checked;
  FAlarm.Days[4] := frmSettings.cbxWed.Checked;
  FAlarm.Days[5] := frmSettings.cbxThu.Checked;
  FAlarm.Days[6] := frmSettings.cbxFri.Checked;
  FAlarm.Days[7] := frmSettings.cbxSat.Checked;

  FAlarm.AlarmTime := Date + EncodeTime(frmSettings.edtHour.Value,
    frmSettings.edtMinute.Value, 0, 0);

  FAlarm.Silent := frmSettings.cbxSilentAlarm.Checked;

  FEmailReminders := frmSettings.cbxEmailReminders.Checked;

  if frmSettings.TimerUpdate then
  begin
    for i := 0 to High(FTimer.Days) do
      FTimer.Days[i] := False;

    Minutes := frmSettings.TimerTime;

    if Minutes > 0 then
    begin
      Hours := Minutes div 60;
      Minutes := Minutes mod 60;

      FTimer.AlarmTime := Now + EncodeTime(Hours,
        Minutes, 0, 0);

      FTimer.Days[DayOfWeek(FTimer.AlarmTime)] := True;
    end;

    frmSettings.TimerUpdate := False;
  end;

  if Assigned(FSyncServer) then
    FreeAndNil(FSyncServer);

  if Assigned(FSyncClient) then
    FreeAndNil(FSyncClient);

  if frmSettings.cbxEnableReminders.Checked then
  begin
    if frmSettings.cbxGetReminders.Checked then
    begin
      frmReminderList.CanEdit := False;

      if Assigned(FSyncServer) then
        FreeAndNil(FSyncServer);

      if not Assigned(FSyncClient) then
        FSyncClient := TSyncClient.Create;
    end
    else
    begin
      frmReminderList.CanEdit := True;

      if Assigned(FSyncClient) then
        FreeAndNil(FSyncClient);

      if not Assigned(FSyncServer) then
      begin
        FSyncServer := TSyncServer.Create;
        FSyncServer.RemindersFile(frmReminders.Filename);
      end;
    end;
  end;

  FServerAddress := frmSettings.edtServerAddress.Text;
  FServerPort := frmSettings.edtServerPort.Text;

  SetCursorType(Self);

  StartDiscoverServer;

  UpdateReminders;

  if frmSettings.cbxForceFullscreen.Checked then
  begin
    BorderStyle := bsNone;
  end
  else
  begin
    BorderStyle := bsSingle;
  end;
end;

procedure TfrmClockMain.StartDiscoverServer;
var
  ClockName: String;
begin
  // Update discover server with Clock name
  if Assigned(FDiscoverServer) then
    FreeAndNil(FDiscoverServer);

  ClockName := Trim(frmSettings.edtClockName.Text);
  if LowerCase(ClockName) = 'hostname' then
    ClockName := GetHostName;

  if ClockName = '' then
    ClockName := 'no-name-set';

  FDiscoverServer := TDiscoverServer.Create(44557, ClockName);
end;

procedure TfrmClockMain.UpdateReminders;
var
  i: Integer;
begin
  if frmSettings.cbxEnableReminders.Checked then
    FCurrentReminders := frmReminders.GetCurrentReminders
  else SetLength(FCurrentReminders, 0);

  // Reminders
  for i := 1 to 7 do
    FReminderAlarm.Days[i] := Length(FCurrentReminders) > 0;

  FReminderAlarm.AlarmTime := Date + EncodeTime(frmSettings.edtRemHour.Value,
    frmSettings.edtRemMinute.Value, 0, 0);
end;

procedure TfrmClockMain.PlayAlbums;
var
  SongFile: String;
  i: Integer;
begin
  // Test is the search path has changed
  case FMusicSource of
    msrcSleep:
      begin
        SongFile := ChangeFileExt(FConfigFilename, '_sleep.cfg');
      end;
    msrcMeditation:
      begin
        SongFile := ChangeFileExt(FConfigFilename, '_meditation.cfg');
      end;
    else
    begin
      if FMusicSource <> msrcMusic then
      begin
        SetMusicSource(msrcMusic);
      end;

      SongFile := ChangeFileExt(FConfigFilename, '_music.cfg');
    end
  end;

  frmPlaylist := TfrmPlaylist.Create(Self);

  frmPlaylist.LoadSongs(SongFile, FPlayer.SearchPath);

  if frmSettings.cbxForceFullscreen.Checked then
  begin
    if frmPlaylist.BorderStyle <> bsNone then
      frmPlaylist.BorderStyle := bsNone;
  end
  else
  begin
    if frmPlaylist.BorderStyle <> bsSingle then
      frmPlaylist.BorderStyle := bsSingle;
  end;

  if FormShowModal(frmPlaylist) = mrOk then
  begin
    if FPlayer.PlaySelection(frmPlaylist.lstSelected.Items.Text, frmPlaylist.Random) > 0 then
    begin
      PlayMusic;
    end
    else ShowMessage('Error, could not find any music files!');
  end;

  frmPlaylist.Free;
end;

procedure TfrmClockMain.Log(Message: string);
begin
{$IFDEF LOGGING}
  end;
  DebugLn(Self.ClassName + #9#9 + Message);
{$ENDIF}
end;

{$IFDEF GRABXKEYS}
{
keycode 121 = XF86AudioMute
keycode 122 = XF86AudioLowerVolume
keycode 123 = XF86AudioRaiseVolume
keycode 171 = XF86AudioNext
Keycode 172 = XF86AudioPlay
Keycode 173 = XF86AudioPrev
Keycode 174 = XF86AudioStop
Keycode 180 = XF86HomePage
}
procedure TfrmClockMain.GrabMediaKeys;
var
  Error: Integer;
begin
  FDisplay := nil;

  Log('XOpenDisplay ...');
  FDisplay := XOpenDisplay(nil);

  if FDisplay <> nil then
  begin
    XSetErrorHandler(TXErrorHandler(XErrorHandler));

    Log('XGrabKey ...');
    Error := XGrabKey(FDisplay, XKeysymToKeycode(FDisplay, XStringToKeysym('XF86AudioPlay')),
      {ShiftMask} 0, DefaultRootWindow(FDisplay), 0, GrabModeAsync, GrabModeAsync);

    if Error = 1 then
    begin
      Log('XGrabKey ...');
      Error := XGrabKey(FDisplay, XKeysymToKeycode(FDisplay, XStringToKeysym('XF86AudioNext')),
         0, DefaultRootWindow(FDisplay), 0, GrabModeAsync, GrabModeAsync);
    end;

    if Error = 1 then
    begin
      Log('XGrabKey ...');
      Error := XGrabKey(FDisplay, XKeysymToKeycode(FDisplay, XStringToKeysym('XF86AudioMute')),
         0, DefaultRootWindow(FDisplay), 0, GrabModeAsync, GrabModeAsync);
    end;

    if Error = 1 then
    begin
      Log('XGrabKey ...');
      Error := XGrabKey(FDisplay, XKeysymToKeycode(FDisplay, XStringToKeysym('XF86HomePage')),
         0, DefaultRootWindow(FDisplay), 0, GrabModeAsync, GrabModeAsync);
    end;

    if Error = 1 then
    begin
      { select kind of events we are interested in }
      Log('XSelectInput ...');
      Error := XSelectInput(FDisplay, DefaultRootWindow(FDisplay), KeyPressMask);
      Log('XSelectInput.');
    end;

    if Error <> 1 then
    begin
      Log('GrabMediaKeys Error: ' + IntToStr(Error));
      ReleaseMediaKeys;
    end;
  end;
end;

procedure TfrmClockMain.ReleaseMediaKeys;
var
  Error: Integer;
begin
  if FDisplay = nil then
    Exit;

  Log('XCloseDisplay ...');
  Error := XCloseDisplay(FDisplay);
  Log('XCloseDisplay.');

  if Error <> 0 then
  begin
    Log('ReleaseMediaKeys Error: ' + IntToStr(Error));
  end;

  FDisplay := nil;
end;

function TfrmClockMain.GetMediaKeyPress: TMediaKey;
var
  Event: TXEvent;
begin
  Result := mkNone;

  if FDisplay = nil then
    Exit;

  Log('XCheckMaskEvent ...');
  try
    if XCheckMaskEvent(FDisplay, KeyPressMask, @Event) then
    begin
      Log('XEvent = ' + IntToStr(Event._type));

      if Event._type = 2 then
      begin
        case Event.xkey.keycode of
          171, 180: Result := mkAudioNext;
          172, 121: Result := mkAudioPlay;
        end;
      end;
    end;
  except
    on E: Exception do
    begin
      Log('Exception GetMediaKeyPress: ' + E.Message);
      ReleaseMediaKeys;
    end;
  end;

  Log('XCheckMaskEvent.');
end;

function TfrmClockMain.XErrorHandler(para1:PDisplay; para2:PXErrorEvent):cint;cdecl;
begin
  // do nothing with error
end;

{$ENDIF}

end.

