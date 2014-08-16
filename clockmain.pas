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
  gtk2, gdk2,
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, {MetOffice,} Alarm, Settings, Reminders, ReminderList, LCLProc,
  Music, Sync, Process, MusicPlayer, PlaylistCreator, commandserver,
  X, Xlib, CTypes, Black, WaitForMedia, Pictures, DateTime, SourcePicker,
  ConnectionHealth, Unix;

const
  VERSION = '2.3.2';

type
  TMusicState = (msOff, msPlaying, msPaused);
  TMusicSource = (msrcNone, msrcSleep, msrcMusic, msrcMeditation, msrcRadio);
  TMediaKey = (mkNone, mkAudioPlay, mkAudioNext);

  { TfrmClockMain }

  TfrmClockMain = class(TForm)
    imgExit: TImage;
    imgPlayAlbums: TImage;
    imgPrevious: TImage;
    imgOn: TImage;
    imgOff: TImage;
    imgDisplay: TImage;
    imgPlay: TImage;
    imgReminders: TImage;
    imgVolUp: TImage;
    imgVolDown: TImage;
    imgNext: TImage;
    imgMusic: TImage;
    imgRadio: TImage;
    ImgSleep: TImage;
    imgPictures: TImage;
    imgUpdateMusic: TImage;
    imgSettings: TImage;
    Label22: TLabel;
    labSongPrev: TLabel;
    lbPlayAlbums: TLabel;
    lbWeatherSummary: TLabel;
    lbExit: TLabel;
    lbMusic: TLabel;
    Radio: TLabel;
    lbPrevious: TLabel;
    lbPlay: TLabel;
    lbReminders: TLabel;
    lbSleep: TLabel;
    lbVolUp: TLabel;
    lbVolDown: TLabel;
    lbNext: TLabel;
    lbPictures: TLabel;
    lbDisplay: TLabel;
    labSong: TLabel;
    lblTime: TLabel;
    UpdateMusic: TLabel;
    lbSettings: TLabel;
    tmrMinute: TTimer;
    tmrWeather: TTimer;
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
    procedure lbDisplayClick(Sender: TObject);
    procedure lblTimeClick(Sender: TObject);
    procedure lbNextClick(Sender: TObject);
    procedure lbPlayAlbumsClick(Sender: TObject);
    procedure lbPicturesClick(Sender: TObject);
    procedure lbPlayClick(Sender: TObject);
    procedure lbPreviousClick(Sender: TObject);
    procedure lbSettingsClick(Sender: TObject);
    procedure lbSleepClick(Sender: TObject);
    procedure tmrTimeTimer(Sender: TObject);
    procedure tmrWeatherTimer(Sender: TObject);
    procedure tmrMinuteTimer(Sender: TObject);
  private
    { private declarations }
    FMPGPlayer: TMusicPlayer;
//    FMetOffice: TMetOffice;
    FAlarm, FReminderAlarm: TAlarm;
    FCurrentReminders: TReminders;
    FTimer: TAlarm;
    FSyncServer: TSyncServer;
    FSyncClient: TSyncClient;
    FServerAddress, FServerPort: String;
    FAfterAlarmResumeMusic: boolean;
    FLinuxDateTime: TLinuxDateTime;
    FConnectionHealth: TConnectionHealth;
    FBlackForm: TfrmBlack;
    FRadioStation: integer;

    FConfigFilename: string;

    FMusicPlayer, FSleepPlayer, FMeditationPlayer, FRadioPlayer: TPlayer;
 	  FMusicState: TMusicState;
    FMusicSource: TMusicSource;

    FSources: TSourceArray;

    FCOMServer: TCOMServer;
    FWeatherReport: string;
    FWeatherReports: array [0..4] of string;

    FDisplay: PDisplay;
    FAlarmActive: boolean;

    procedure BacklightBright;
    procedure BacklightDim;

    function DayOfWeekStr(Date: TDateTime): string;
    function FormShowModal(MyForm: TForm): integer;
    function GetMonitorState: boolean;
    procedure HideForm(MyForm: TForm);
    procedure Log(Message: string);
    procedure PauseMusic;
    procedure PlayAlbums;
    procedure PlayMusic;
    procedure PlayPreviousMusic;
    procedure SetCursorType(MyForm: TForm);
    procedure SetMonitorState(State: boolean);
    procedure SetMusicSource(Source: TMusicSource);
    procedure ShowForm(MyForm: TForm);
    procedure UpdatingMusic(Player: TPlayer);
    procedure ComServerCallback;

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

  published
  end;

var
  frmClockMain: TfrmClockMain;

implementation

{$R *.lfm}

{ TfrmClockMain }

procedure TfrmClockMain.PauseMusic;
begin
  case FMusicState of
    msPlaying:
      begin
        case FMusicSource of
          msrcSleep: FSleepPlayer.Stop;
          msrcMusic: FMusicPlayer.Stop;
          msrcMeditation: FMeditationPlayer.Stop;
          msrcRadio: FRadioPlayer.Stop;
        end;

	      FMusicState := msPaused;
	    end;
  end;
end;

procedure TfrmClockMain.PlayMusic;
var
  Player: TPlayer;
begin
  case FMusicSource of
    msrcSleep: Player := FSleepPlayer;
    msrcMeditation: Player := FMeditationPlayer;
    msrcMusic: Player := FMusicPlayer;
    msrcRadio: Player := FRadioPlayer;
    else Player := nil;
  end;

  if WaitForMedia(Player.SearchPath) then
  begin
    if Assigned(Player) then
    begin
      case FMusicState of
        msOff, msPaused:
          begin
            Player.Play;
          end;
        msPlaying:
          begin
            Player.Next; // if playing play next track
	        end;
      end;

      FMusicState := msPlaying;
    end;
  end;
end;

procedure TfrmClockMain.PlayPreviousMusic;
var
  Player: TPlayer;
begin
  case FMusicSource of
    msrcSleep: Player := FSleepPlayer;
    msrcMeditation: Player := FMeditationPlayer;
    msrcMusic: Player := FMusicPlayer;
    msrcRadio: Player := FRadioPlayer;
    else Player := nil;
  end;

  if WaitForMedia(Player.SearchPath) then
  begin
    if Assigned(Player) then
    begin
      case FMusicState of
        msOff, msPaused, msPlaying:
          begin
            Player.Previous;
	        end;
      end;

      FMusicState := msPlaying;
    end;
  end;
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
var
  Player: TPlayer;
  PlayerPath: String;
begin
  case Source of
    msrcSleep:
      begin
        Player := FSleepPlayer;
        PlayerPath := frmSettings.edtSleepPath.Text;
      end;
    msrcMeditation:
      begin
        Player := FMeditationPlayer;
        PlayerPath := frmSettings.edtMeditationPath.Text;
      end;
    msrcMusic:
      begin
        Player := FMusicPlayer;
        PlayerPath := frmSettings.edtMusicPath.Text;
      end;
    msrcRadio:
      begin
        Player := FRadioPlayer;
        PlayerPath := '';
      end
    else
      begin
        Player := nil;
        PlayerPath := '';
      end;
  end;

  // Test is the search path has changed
  if (Assigned(Player) and (PlayerPath <> Player.SearchPath)) then
    FreeAndNil(Player);

  if not Assigned(Player) then
  begin
    //Create new source
    case Source of
      msrcSleep:
        begin
          FSleepPlayer := TPlayer.Create(FMPGPlayer,
            ChangeFileExt(FConfigFilename, '_sleep.cfg'), frmSettings.edtSleepPath.Text);
        end;
      msrcMusic:
        begin
          FMusicPlayer := TPlayer.Create(FMPGPlayer,
            ChangeFileExt(FConfigFilename, '_music.cfg'), frmSettings.edtMusicPath.Text);
        end;
      msrcMeditation:
        begin
          FMeditationPlayer := TPlayer.Create(FMPGPlayer,
            ChangeFileExt(FConfigFilename, '_meditation.cfg'), frmSettings.edtMeditationPath.Text);
        end;
      msrcRadio:
        begin
          FRadioPlayer := TPlayer.Create(FMPGPlayer, '', '');
        end;
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
    case FMusicSource of
      msrcSleep: FSleepPlayer.Play;
      msrcMusic: FMusicPlayer.Play;
      msrcMeditation: FMeditationPlayer.Play;
      msrcRadio: FRadioPlayer.Play;
      else
      begin
        SetMusicSource(msrcMusic);
        FMusicPlayer.Play;
      end;
    end;

    FMusicState := msPlaying;
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

procedure TfrmClockMain.tmrTimeTimer(Sender: TObject);
var
  Current: TDateTime;
  DayStr: string;
  TimeCaption: string;
  ReminderList: TStringList;
  i: Integer;
  Key: Char;
  Player: TPlayer;
  PlayerName, Song: string;
  ReminderState: TAlarmState;
  Day, Month, Year: word;
  Hour, Min, Sec, MSec: word;
begin
  Current := FLinuxDateTime.GetLocalTime;

  DayStr := Copy(DayOfWeekStr(Current), 1, 3);
  DecodeDate(Current, Year, Month, Day);
  DecodeTime(Current, Hour, Min, Sec, MSec);
  TimeCaption := Format('%s %.2d/%.2d %.2d:%.2d:%.2d ', [DayStr, Day, Month, Hour, Min, Sec]);

  if TimeCaption <> lblTime.Caption then
    lblTime.Caption := TimeCaption;

  tmrTime.Tag := tmrTime.Tag + 1;

  if tmrTime.Tag >= 1 then
  begin
    tmrWeather.Enabled := True;

    // Disable weather update around alarm times
    if (Current > FAlarm.AlarmTime - EncodeTime(0, 5, 0, 0))
      and (Current < FAlarm.AlarmTime + EncodeTime(0, 5, 0, 0)) then
    begin
      tmrWeather.Enabled := False;
    end;

    // Disable weather when reminder alarm about to be activated
    if (Length(FCurrentReminders) > 0) and (Current > FReminderAlarm.AlarmTime - EncodeTime(0, 2, 0, 0))
      and (Current < FReminderAlarm.AlarmTime + EncodeTime(0, 0, 10, 0)) then
    begin
      tmrWeather.Enabled := False;
    end;

    // Do not update weather if reminder is displayed
    if (lbWeatherSummary.Font.Color = clYellow) then
    begin
      tmrWeather.Enabled := False;
    end;

    // Turn off timer
    if Current > FTimer.AlarmTime + EncodeTime(0, 5, 0, 0) then
    begin
      for i := 0 to High(FTimer.Days) do
      begin
        if FTimer.Days[i] <> False then
          FTimer.Days[i] := False;
      end;
    end;

    ReminderState := FReminderAlarm.State;

    FAlarm.Tick(Current);
    FReminderAlarm.Tick(Current);
    FTimer.Tick(Current);

    if (FReminderAlarm.State = asActive) and (ReminderState <> asActive) then
    begin
      lbWeatherSummary.Font.Color := clYellow;
      ReminderList := TStringList.Create;
      frmReminders.SortReminders(FCurrentReminders);
      frmReminders.PopulateList(FCurrentReminders, ReminderList);
      lbWeatherSummary.Caption := ReminderList.Text;
      ReminderList.Free;
      tmrWeather.Enabled := False;
    end;

    case FMusicSource of
      msrcSleep:
        begin
          Player := FSleepPlayer;
          PlayerName := 'sleep';
        end;
      msrcMeditation:
        begin
          Player := FMeditationPlayer;
          PlayerName := 'meditation';
        end;
      msrcMusic:
        begin
          Player := FMusicPlayer;
          PlayerName := 'music';
        end;
      msrcRadio:
        begin
          Player := FRadioPlayer;
          PlayerName := 'radio';
        end;
      else Player := nil;
    end;

    Song := 'Shaun''s Clock Version: ' + VERSION;

    if Assigned(Player) then
    begin
      i := Player.Tick;

      if i >= 0  then
        Song := Format('Updating %s list ... %d', [PlayerName, i])
      else
      begin
        if Player.State = psPlaying then
          Song := Player.SongArtist + ' - ' + Player.SongTitle;
      end;
    end;

    if labSong.Caption <> Song then
    begin
      labSongPrev.Caption := labSong.Caption;
      labSong.Caption := Song;
    end;

    if Assigned(FCOMServer) then
    begin
      FCOMServer.Playing := LabSong.Caption;
    end;

    tmrTime.Tag := 0;
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

procedure TfrmClockMain.tmrWeatherTimer(Sender: TObject);
begin
  tmrWeather.Enabled := False;
  // Every 60 Minutes
  tmrWeather.Interval := 60 * 60 * 1000;
  tmrWeather.Enabled := True;
end;

procedure TfrmClockMain.tmrMinuteTimer(Sender: TObject);
var
  Rems: string;
  CurrentList: TStringList;
begin
  tmrMinute.Enabled := False;

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

    if Assigned(FConnectionHealth) then
    begin
      FConnectionHealth.TestConnection;
    end;
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
    Shell('xset -dpms');
    Shell('xdotool mousemove 1 1');
    Shell('xdotool mousemove 100 100');
    Shell('xset +dpms');
  end
  else
  begin
    Shell('xset dpms force off');
  end;
end;

procedure TfrmClockMain.BacklightDim;
begin
  try
    Shell('sudo sh -c "echo 4 > /sys/class/backlight/openframe-bl/brightness"');
  except
  end;
end;

procedure TfrmClockMain.BacklightBright;
begin
  try
    Shell('sudo sh -c "echo 32 > /sys/class/backlight/openframe-bl/brightness"');
  except
  end;
end;

procedure TfrmClockMain.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  FConfigFilename := GetAppConfigFile(False);

  Self.Color := clBlack;

//  FMetOffice := nil;

  labSong.Caption := '';
  labSongPrev.Caption := '';


  FMPGPlayer := TMusicPlayer.Create;

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

  FMusicPlayer := nil;
  FSleepPlayer := nil;
  FMeditationPlayer := nil;
  FRadioPlayer := nil;
  FSyncServer := nil;
  FSyncClient := nil;

  FConnectionHealth := nil;

  FMusicState := msOff;
  FMusicSource := msrcNone;
  FAlarmActive := False;

  FCOMServer := TCOMServer.Create(44558);
  FCOMServer.OnCommand := ComServerCallback;

  FWeatherReport := '';

  FLinuxDateTime := TLinuxDateTime.Create;

  FBlackForm := nil;
  FRadioStation := 0;

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
end;

procedure TfrmClockMain.FormActivate(Sender: TObject);
begin
  if frmSettings.cbxForceFullscreen.Checked then
    gdk_window_fullscreen(PGtkWidget(Handle)^.window);
end;

procedure TfrmClockMain.FormDestroy(Sender: TObject);
begin
  if Assigned(FMusicPlayer) then
    FMusicPlayer.Free;

  if Assigned(FSleepPlayer) then
    FSleepPlayer.Free;

  if Assigned(FMeditationPlayer) then
    FMeditationPlayer.Free;

  if Assigned(FRadioPlayer) then
    FRadioPlayer.Free;

  FMPGPlayer.Free;

  if Assigned(FSyncClient) then
    FreeAndNil(FSyncClient);

  if Assigned(FSyncServer) then
    FreeAndNil(FSyncServer);

  if Assigned(FConnectionHealth) then
    FreeAndNil(FConnectionHealth);

  if Assigned(FBlackForm) then
    FreeAndNil(FBlackForm);

  FAlarm.Free;
  FReminderAlarm.Free;
  FTimer.Free;
  FCOMServer.Free;
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
      'N - Next weather location' + LineEnding +
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
var
  Player: TPlayer;
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
    imgRadioClick(nil);
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
    case FMusicSource of
      msrcSleep: Player := FSleepPlayer;
      msrcMeditation: Player := FMeditationPlayer;
      msrcMusic: Player := FMusicPlayer;
      msrcRadio: Player := FRadioPlayer;
      else Player := nil;
    end;

    if Assigned(Player) then
    begin
      Player.RescanSearchPath;
    end;
  end
  else if (Key = '.') then
  begin
    if Assigned(FMPGPlayer) then
    begin
      FMPGPlayer.VolumeUp;
    end;
  end
  else if (Key = ',') then
  begin
    if Assigned(FMPGPlayer) then
    begin
      FMPGPlayer.VolumeDown;
    end;
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
  if not FileExists('/usr/bin/mpg123') and not FileExists('/usr/bin/mpg321') then
    ShowMessage('Alarm Not Working' + LineEnding
    + 'The package mpg123 was not found on this system.' + LineEnding
    + 'Please install mpg123 to enable the alarm by running the command:' + LineEnding
    + 'sudo apt-get install mpg123');

  if not FileExists(ExtractFilePath(Application.ExeName) + 'alarm.mp3')
    and not FileExists('/usr/share/clock/alarm.mp3') then
    ShowMessage('Alarm Not Working' + LineEnding
    + 'The mp3 file "alarm.mp3" can not be found.' + LineEnding
    + 'Please copy the file "alarm.mp3" to the location:' + LineEnding
    + '/usr/share/clock/alarm.mp3');

  UpdateSettings;
  frmReminderList.FReminders := frmReminders;

  // Do only once
  if FMusicSource = msrcNone then
  begin
    // Initialise music sources
    SetMusicSource(msrcSleep);
    SetMusicSource(msrcMeditation);
    SetMusicSource(msrcMusic);
  end;

  tmrMinute.Enabled := True;
end;

procedure TfrmClockMain.imgExitClick(Sender: TObject);
begin
  imgExit.Picture.Assign(imgOff.Picture);
  Application.ProcessMessages;
  Close;
end;

procedure TfrmClockMain.imgMusicClick(Sender: TObject);
var
  Picker: TfrmSourcePicker;
  Sources: TSourceArray;
begin
  imgMusic.Picture.Assign(imgOff.Picture);
  Application.ProcessMessages;

  SetLength(Sources, 3);
  Sources[0].Title := 'Music';
  Sources[1].Title := 'Meditation';
  Sources[2].Title := 'Sleep';
  Sources[0].Resource := '';
  Sources[1].Resource := '';
  Sources[2].Resource := '';

  Picker := TfrmSourcePicker.Create(Self, Sources);

  if frmSettings.cbxForceFullscreen.Checked then
  begin
    Picker.BorderStyle := bsNone;
  end
  else
  begin
    Picker.BorderStyle := bsSingle;
  end;

  SetCursorType(Picker);

  if Picker.ShowModal = mrOK then
  begin
    case Picker.ItemIndex of
      0:   SetMusicSource(msrcMusic);
      1:   SetMusicSource(msrcMeditation);
      2:   SetMusicSource(msrcSleep);
    end;

    if not FAlarmActive then PlayMusic
    else PauseMusic;
  end;

  Picker.Free;

  imgMusic.Picture.Assign(imgOn.Picture);
end;

procedure TfrmClockMain.imgRemindersClick(Sender: TObject);
begin
  imgReminders.Picture.Assign(imgOff.Picture);
  Application.ProcessMessages;

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

  imgReminders.Picture.Assign(imgOn.Picture);
end;

procedure TfrmClockMain.imgVolDownClick(Sender: TObject);
begin
  imgVolDown.Picture.Assign(imgOff.Picture);
  Application.ProcessMessages;

  if Assigned(FMPGPlayer) then
  begin
    FMPGPlayer.VolumeDown;
  end;

  imgVolDown.Picture.Assign(imgOn.Picture);
end;

procedure TfrmClockMain.imgVolUpClick(Sender: TObject);
begin
  imgVolUp.Picture.Assign(imgOff.Picture);
  Application.ProcessMessages;

  if Assigned(FMPGPlayer) then
  begin
    FMPGPlayer.VolumeUp;
  end;

  imgVolUp.Picture.Assign(imgOn.Picture);
end;


procedure TfrmClockMain.UpdatingMusic(Player: TPlayer);
var
  i: Integer;
begin
  if Assigned(Player) then
  begin
    repeat
      i := Player.Tick;
      Application.ProcessMessages;
    until i < 0;
  end;
end;

procedure TfrmClockMain.imgUpdateMusicClick(Sender: TObject);
begin
  imgUpdateMusic.Picture.Assign(imgOff.Picture);
  Self.Enabled := False;

  Application.ProcessMessages;

  if Assigned(FSleepPlayer) then
  begin
    FSleepPlayer.RescanSearchPath;
    UpdatingMusic(FSleepPlayer);
  end;

  if Assigned(FMeditationPlayer) then
  begin
    FMeditationPlayer.RescanSearchPath;
    UpdatingMusic(FMeditationPlayer);
  end;

  if Assigned(FMusicPlayer) then
  begin
    FMusicPlayer.RescanSearchPath;
    UpdatingMusic(FMusicPlayer);
  end;

  Self.Enabled := True;
  imgUpdateMusic.Picture.Assign(imgOn.Picture);
end;

procedure TfrmClockMain.lbDisplayClick(Sender: TObject);
begin
  if FBlackForm = nil then
  begin
    SetMonitorState(False);
    FBlackForm := TfrmBlack.Create(Self);
    FBlackForm.OnClick := lbDisplayClick;
    ShowForm(FBlackForm);
  end
  else if not FBlackForm.CanFocus then
  begin
    SetMonitorState(False);
    ShowForm(FBlackForm);
  end
  else
  begin
    SetMonitorState(True);
    HideForm(FBlackForm);
  end;
end;

procedure TfrmClockMain.imgRadioClick(Sender: TObject);
var
  Picker: TfrmSourcePicker;
begin
  imgRadio.Picture.Assign(imgOff.Picture);
  Application.ProcessMessages;

  SetLength(FSources, 48);

  FSources[0].Title := 'Oldies';
  FSources[0].Resource := 'http://pub1.sky.fm/sky_oldies';
  FSources[1].Title := '60''s Rock';
  FSources[1].Resource := 'http://pub1.sky.fm/sky_60srock';
  FSources[2].Title := '60''s Hits';
  FSources[2].Resource := 'http://pub1.sky.fm/sky_hit60s';
  FSources[3].Title := '80''s Rock';
  FSources[3].Resource := 'http://pub1.sky.fm/sky_80srock';
  FSources[4].Title := '80''s Hits';
  FSources[4].Resource := 'http://pub1.sky.fm/sky_the80s';
  FSources[5].Title := '80''s Dance';
  FSources[5].Resource := 'http://pub1.sky.fm/sky_80sdance';
  FSources[6].Title := '90''s Hits';
  FSources[6].Resource := 'http://pub1.sky.fm/sky_hit90s';
  FSources[7].Title := 'Soft Rock';
  FSources[7].Resource := 'http://pub1.sky.fm/sky_softrock';
  FSources[8].Title := 'Classic Rock';
  FSources[8].Resource := 'http://pub1.sky.fm/sky_classicrock';
  FSources[9].Title := 'Modern Rock';
  FSources[9].Resource := 'http://pub1.sky.fm/sky_modernrock';
  FSources[10].Title := 'Indie Rock';
  FSources[10].Resource := 'http://pub1.sky.fm/sky_indierock';
  FSources[11].Title := 'Alt Rock';
  FSources[11].Resource := 'http://pub1.sky.fm/sky_altrock';
  FSources[12].Title := 'Hard Rock';
  FSources[12].Resource := 'http://pub1.sky.fm/sky_hardrock';
  FSources[13].Title := 'Metal';
  FSources[13].Resource := 'http://pub1.sky.fm/sky_metal';
  FSources[14].Title := 'Pop Punk';
  FSources[14].Resource := 'http://pub1.sky.fm/sky_poppunk';
  FSources[15].Title := 'Pop Rock';
  FSources[15].Resource := 'http://pub1.sky.fm/sky_poprock';
  FSources[16].Title := 'Roots Reggae';
  FSources[16].Resource := 'http://pub8.sky.fm/sky_rootsreggae';
  FSources[17].Title := 'Roots Legacy Reggae';
  FSources[17].Resource := 'http://88.191.164.141:443/stream/1/';
  FSources[18].Title := 'Ska';
  FSources[18].Resource := 'http://pub7.sky.fm/sky_ska';
  FSources[19].Title := 'New Age';
  FSources[19].Resource := 'http://pub1.sky.fm/sky_newage';
  FSources[20].Title := 'Vocal New Age';
  FSources[20].Resource := 'http://pub1.sky.fm/sky_vocalnewage';
  FSources[21].Title := 'Dreamscapes';
  FSources[21].Resource := 'http://pub7.sky.fm/sky_dreamscapes';
  FSources[22].Title := 'Relaxation';
  FSources[22].Resource := 'http://pub6.sky.fm/sky_relaxation';
  FSources[23].Title := 'Nature';
  FSources[23].Resource := 'http://pub1.sky.fm/sky_nature';
  FSources[24].Title := 'Salsa';
  FSources[24].Resource := 'http://pub1.sky.fm/sky_salsa';
  FSources[25].Title := 'Bossa Nova';
  FSources[25].Resource := 'http://pub1.sky.fm/sky_bossanova';
  FSources[26].Title := 'Smooth Bossa Nova';
  FSources[26].Resource := 'http://pub1.sky.fm/sky_smoothbossanova';
  FSources[27].Title := 'American Songs';
  FSources[27].Resource := 'http://pub1.sky.fm/sky_americansongbook';
  FSources[28].Title := 'Classical Guitar';
  FSources[28].Resource := 'http://pub1.sky.fm/sky_guitar';
  FSources[29].Title := 'Classical Piano';
  FSources[29].Resource := 'http://pub1.sky.fm/sky_classicalpianotrios';
  FSources[30].Title := 'Solo Piano';
  FSources[30].Resource := 'http://pub1.sky.fm/sky_solopiano';
  FSources[31].Title := 'Country';
  FSources[31].Resource := 'http://pub1.sky.fm/sky_country';
  FSources[32].Title := 'Date Tempo Lounge';
  FSources[32].Resource := 'http://pub1.sky.fm/sky_datempolounge';
  FSources[33].Title := '90''s R&B';
  FSources[33].Resource := 'http://pub1.sky.fm/sky_90srnb';
  FSources[34].Title := '00''s R&B';
  FSources[34].Resource := 'http://pub1.sky.fm/sky_00srnb';
  FSources[35].Title := 'Hip-Hop';
  FSources[35].Resource := 'http://pub1.sky.fm/sky_classicrap';
  FSources[36].Title := 'Motown';
  FSources[36].Resource := 'http://pub1.sky.fm/sky_classicmotown';
  FSources[37].Title := 'Jazz Clasics';
  FSources[37].Resource := 'http://pub1.sky.fm/sky_jazzclassics';
  FSources[38].Title := 'Smooth Jazz';
  FSources[38].Resource := 'http://pub1.sky.fm/sky_davekoz';
  FSources[39].Title := 'Uptempo Smooth Jazz';
  FSources[39].Resource := 'http://pub1.sky.fm/uptemposmoothjazz';
  FSources[40].Title := 'BBC Radio 5 live Sports Extra';
  FSources[40].Resource := 'http://www.bbc.co.uk/radio/listen/live/r5lsp_heaacv2.pls';
  FSources[41].Title := 'Radio 2000';
  FSources[41].Resource := 'http://216.246.37.51/pbs-radio2000-live';
  FSources[42].Title := 'BBC Radio 1';
  FSources[42].Resource := 'http://www.bbc.co.uk/radio/listen/live/r1_heaacv2.pls';
  FSources[43].Title := 'BBC Radio 1Xtra';
  FSources[43].Resource := 'http://www.bbc.co.uk/radio/listen/live/r1x_heaacv2.pls';
  FSources[44].Title := 'BBC Radio 2';
  FSources[44].Resource := 'http://www.bbc.co.uk/radio/listen/live/r2_heaacv2.pls';
  FSources[45].Title := 'BBC Radio 3';
  FSources[45].Resource := 'http://www.bbc.co.uk/radio/listen/live/r3_heaacv2.pls';
  FSources[46].Title := 'BBC Radio 4';
  FSources[46].Resource := 'http://www.bbc.co.uk/radio/listen/live/r4_heaacv2.pls';
  FSources[47].Title := 'BBC Radio 5 live';
  FSources[47].Resource := 'http://www.bbc.co.uk/radio/listen/live/r5l_heaacv2.pls';

  if Sender = nil then
  begin
    if FMusicSource = msrcRadio then
    begin
      Inc(FRadioStation);
      if FRadioStation > High(FSources) then
        FRadioStation := 0;
    end;

    SetMusicSource(msrcRadio);
    FRadioPlayer.StreamTitle := FSources[FRadioStation].Title;
    FRadioPlayer.StreamURL := FSources[FRadioStation].Resource;

    if not FAlarmActive then PlayMusic
    else PauseMusic;
  end
  else
  begin
    Picker := TfrmSourcePicker.Create(Self, FSources);

    if frmSettings.cbxForceFullscreen.Checked then
    begin
      Picker.BorderStyle := bsNone;
    end
    else
    begin
      Picker.BorderStyle := bsSingle;
    end;

    SetCursorType(Picker);

    if Picker.ShowModal = mrOK then
    begin
      SetMusicSource(msrcRadio);
      FRadioPlayer.StreamTitle := FSources[Picker.ItemIndex].Title;
      FRadioPlayer.StreamURL := FSources[Picker.ItemIndex].Resource;

      if not FAlarmActive then PlayMusic
      else PauseMusic;
    end;

    Picker.Free;
  end;

  imgRadio.Picture.Assign(imgOn.Picture);
end;

procedure TfrmClockMain.lbSleepClick(Sender: TObject);
begin
  imgSleep.Picture.Assign(imgOff.Picture);
  Application.ProcessMessages;

  SetMusicSource(msrcSleep);

  if not FAlarmActive then PlayMusic
  else PauseMusic;

  imgSleep.Picture.Assign(imgOn.Picture);
end;

procedure TfrmClockMain.ComServerCallback;
var
  Command: TRemoteCommand;
  Key: Char;
begin
  Command := FComServer.GetCommand;

  case Command of
    rcomNext:
      begin
        case FMusicSource of
          msrcSleep: Key := 's';
          msrcMeditation: Key := 'd';
          msrcRadio: Key := 'n';
          else Key := 'm';
        end;

        FormKeyPress(Self, Key);
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
    rcomRadioToggle:
      begin
        Key := 'n';
        FormKeyPress(Self, Key);
      end;
    rcomPause:
      begin
        Key := 'p';
        FormKeyPress(Self, Key);
      end;
    rcomVolumeUp:
      begin
        Key := '.';
        FormKeyPress(Self, Key);
      end;
    rcomVolumeDown:
      begin
        Key := ',';
        FormKeyPress(Self, Key);
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

//  if frmSettings.cbxForceFullscreen.Checked then
//    gdk_window_fullscreen(PGtkWidget(Handle)^.window);
end;

procedure TfrmClockMain.HideForm(MyForm: TForm);
begin
  MyForm.Hide;
  MyForm.Close;

//  if frmSettings.cbxForceFullscreen.Checked then
//    gdk_window_fullscreen(PGtkWidget(Handle)^.window);
end;

procedure TfrmClockMain.lbSettingsClick(Sender: TObject);
begin
  imgSettings.Picture.Assign(imgOff.Picture);
  Application.ProcessMessages;

  FormShowModal(frmSettings);

  UpdateSettings;

  imgSettings.Picture.Assign(imgOn.Picture);
end;

procedure TfrmClockMain.lbNextClick(Sender: TObject);
begin
  imgNext.Picture.Assign(imgOff.Picture);
  Application.ProcessMessages;

  PlayMusic;

  imgNext.Picture.Assign(imgOn.Picture);
end;

procedure TfrmClockMain.lbPlayAlbumsClick(Sender: TObject);
begin
  imgPlayAlbums.Picture.Assign(imgOff.Picture);
  Application.ProcessMessages;

  PlayAlbums;

  imgPlayAlbums.Picture.Assign(imgOn.Picture);
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
begin
  if (FAlarm.State = asActive)
    or (FTimer.State = asActive)
    or (FReminderAlarm.State = asActive) then
  begin
    FAlarm.ResetAlarm;
    FTimer.ResetAlarm;
    FReminderAlarm.ResetAlarm;
  end;
end;

procedure TfrmClockMain.UpdateSettings;
var
  i: Integer;
  Minutes: LongInt;
  Hours: Integer;
  Player: TPlayer;
begin
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

  if frmSettings.TimerActive then
  begin
    for i := 0 to High(FTimer.Days) do
      FTimer.Days[i] := False;

    Minutes := StrToInt(frmSettings.stxtTimer.Caption);

    if Minutes > 0 then
    begin
      Hours := Minutes div 60;
      Minutes := Minutes mod 60;

      FTimer.AlarmTime := Now + EncodeTime(Hours,
        Minutes, 0, 0);

      FTimer.Days[DayOfWeek(FTimer.AlarmTime)] := True;
    end;

    frmSettings.TimerActive := False;
  end;

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

  case FMusicSource of
    msrcSleep:
      begin
        Player := FSleepPlayer;
      end;
    msrcMeditation:
      begin
        Player := FMeditationPlayer;
      end;
    msrcMusic:
      begin
        Player := FMusicPlayer;
      end;
    msrcRadio:
      begin
        Player := FRadioPlayer;
      end
    else Player := nil;
  end;

  // Update search path if needed
  if Assigned(Player) then
  begin
    SetMusicSource(FMusicSource);
  end;

  FServerAddress := frmSettings.edtServerAddress.Text;
  FServerPort := frmSettings.edtServerPort.Text;

  UpdateReminders;
  SetCursorType(Self);

  if frmSettings.cbxForceFullscreen.Checked then
  begin
    BorderStyle := bsNone;
  end
  else
  begin
    BorderStyle := bsSingle;
  end;

  if frmSettings.cbxMonitorConnection.Checked then
  begin
    if not Assigned(FConnectionHealth) then
      FConnectionHealth := TConnectionHealth.Create;

    FConnectionHealth.Host := frmSettings.edtTestHost.Text;
    FConnectionHealth.ResetCommand := frmSettings.edtConnectionResetComand.Text;
    FConnectionHealth.AutoReset := True;
  end
  else
  begin
    if Assigned(FConnectionHealth) then
      FreeAndNil(FConnectionHealth);
  end;
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
  Player: TPlayer;
  SongFile: String;
begin
  // Test is the search path has changed
  case FMusicSource of
    msrcSleep:
      begin
        Player := FSleepPlayer;
        SongFile := ChangeFileExt(FConfigFilename, '_sleep.cfg');
      end;
    msrcMeditation:
      begin
        Player := FMeditationPlayer;
        SongFile := ChangeFileExt(FConfigFilename, '_meditation.cfg');
      end;
    msrcMusic:
      begin
        Player := FMusicPlayer;
        SongFile := ChangeFileExt(FConfigFilename, '_music.cfg');
      end
    else
    begin
      Exit;
    end;
  end;

  frmPlaylist := TfrmPlaylist.Create(Self);

  frmPlaylist.LoadSongs(SongFile, Player.SearchPath);

  if FormShowModal(frmPlaylist) = mrOk then
  begin
    Player.PlaySelection(frmPlaylist.lstSelected.Items.Text, frmPlaylist.Random);
  end;

  frmPlaylist.Free;
end;

procedure TfrmClockMain.Log(Message: string);
begin
{$IFDEF LOGGING}
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

