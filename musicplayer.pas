//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit MusicPlayer;

{$mode objfpc}{$H+}

interface

// Code to support older Lazarus/FPC
//{$define LEGACY}
//{$define LOGGING}

uses
  Process,
  {$ifndef LEGACY}
  LCLProc, MplayerEQ,
  {$else}
  process_legacy,
  {$endif}
  Classes, SysUtils, ID3v1Library, ID3v2Library, unix, Pipes;

type
  {$ifdef LEGACY}
  TMplayerEQ = array of integer;
  {$endif}

  TMusicPlayerState = (mpsStopped, mpsPlaying);
  TAnnouncements = (anOff, anQuiet, anMute);

  { TMusicPlayer }

  TMusicPlayer  = class
  private
    FNextTick: TDateTime;
    FUsePulseVol: boolean;
    FVolAttenuation: integer;
    FMplayerEQ: TMplayerEQ;
    FMixerControl: string;
    FVolume: integer;
    FPlayProcess: TProcess;
    FPlayTimeout: TDateTime;
    FPlayProcessList: string;
    FSongArtist: string;
    FSongTitle: string;
    FState: TMusicPlayerState;
    FID3v1: TID3v1Tag;
    FID3v2: TID3v2Tag;

    FRadioTitle: string;
    FNewRadioTitle: string;
    FRadioTitleTime: TDateTime;
    FRadioPlaying: boolean;
    FAdDelay: integer;
    FAdvertType: string;
    FMuteLevel: integer;

    FAnnouncementVol: integer;
    FAnnouncement: boolean;
    FAnnouncementStart: TDateTime;
    FAnnouncementStop: TDateTime;

    procedure DestroyPlayProcess;
    function GetRadioTitle: string;
    function GetState: TMusicPlayerState;
    procedure PlaySong(Song: string);
    procedure ProcessAnnouncement;
    function ProcessRadio: string;
    function ReadProcessData: string;
    procedure SetAnnouncements(AValue: TAnnouncements);
    procedure SetVolAttenuation(AValue: integer);
    procedure SetVolume(Volume: integer);
    procedure StartAnnouncement(MuteLevel: integer);
    procedure StartPlayProcess(Song: string; out Process: TProcess);
    procedure StopAnnouncement(MuteLevel: integer);
    procedure StopSong;
  public
    constructor Create(MixerControl : string; UsePulseVol: boolean; EQ: TMplayerEQ);
    destructor Destroy; override;

    procedure Tick;     // Used to process radio stream information

    procedure Play(Filename: string);
    procedure VolumeUp;
    procedure VolumeDown;
    procedure Stop;

    function GetVolume: integer;
  published
    property SongArtist: string read FSongArtist;
    property SongTitle: string read FSongTitle;
    property State: TMusicPlayerState read GetState;
    property RadioTitle: string read GetRadioTitle;
    property VolAttenuation: integer read FVolAttenuation write SetVolAttenuation;
    property Announcements: TAnnouncements write SetAnnouncements;
  end;

implementation

procedure TMusicPlayer.PlaySong(Song: string);
begin
  // Ensure that song is not playing
  StopSong;

  {$ifdef LOGGING} WriteLn('PLAY: ' + Song); {$endif}

  FSongTitle := '';
  FSongArtist := '';
  FRadioTitle := '';
  FNewRadioTitle := '';

  if FileExists(Song) then
  begin
    try
      if FID3v2.LoadFromFile(Song) = ID3V2LIBRARY_SUCCESS then
      begin
        //* Get Title
        FSongTitle := FID3v2.GetUnicodeText('TIT2');

        //* Get Artist
        FSongArtist := FID3v2.GetUnicodeText('TPE1');
      end
      else if FID3v1.LoadFromFile(Song) = ID3V1LIBRARY_SUCCESS then
      begin
        //* Get Title
        FSongTitle := FID3v1.Title;

        //* Get Artist
        FSongArtist := FID3v1.Artist;
      end;
    except
      on E: Exception do
      begin
        {$ifdef LOGGING}
        WriteLn(Self.ClassName + #9#9 + 'Failed to get ID3 Tags for "'
          + ExtractFilename(Song) + '"');
        WriteLn(Self.ClassName + #9#9 + E.Message);
        {$endif}
      end;
    end;
  end
  else
  begin
    //* Get Title
    FSongTitle := 'Unknown';

    //* Get Artist
    FSongArtist := 'Unknown';
  end;

  try
    if Assigned(FPlayProcess) then DestroyPlayProcess;

    StartPlayProcess(Song, FPlayProcess);

    if Trim(FSongTitle) = '' then FSongTitle := ExtractFilename(Song);

    FState := mpsPlaying;
  except
    on E: Exception do
    begin
      Writeln('EXCEPTION: Function TMusicPlayer.PlaySong');
      Writeln('MESSAGE: ' + E.Message);
    end;
  end;
end;

procedure TMusicPlayer.StartPlayProcess(Song: string; out Process: TProcess);
var
  PlaylistTypes: array [0..4] of string = ('.pls', '.m3u', '.asx', '.wpl', '.xspf');
  Vol: integer;
  EQ, FileExt: String;
  i: Integer;
begin
  Process := TProcess.Create(nil);

  FRadioPlaying := False;

  // Create equaliser commandline params from input array.
  EQ := '';

  if (Length(FMplayerEQ) = 10) then
  begin
    EQ := '-af-add equalizer=';

    for i := 0 to 8 do
        EQ := EQ + IntToStr(FMplayerEQ[i]) + ':';

    // Last one gets no ':'
    EQ := EQ + IntToStr(FMplayerEQ[9]) + ' ';
  end;

  Process.CommandLine := 'mplayer ' + EQ + ' -af-add format=s16le -softvol -volume ' + IntToStr(100-FVolAttenuation);

  { If the song name begins with 'http://' then it assumed to be a URL of a stream. }
  if ((Pos('http://', Trim(Lowercase(Song))) = 1) or (Pos('https://', Trim(Lowercase(Song))) = 1)) then
  begin
    FRadioPlaying := True;

    // Announcement removal requires messages
    Process.CommandLine := Process.CommandLine + ' -msglevel all=4';

    // AdDelay is used to mute adverts/announcements.
    // Some stations have a delay between the title text change and the audio stream change.
    FAdDelay := 8;

    // Support playlists
    for i := 0 to High(PlaylistTypes) do
    begin
      if Pos(PlaylistTypes[i], LowerCase(Song)) > 0 then
      begin
        Process.CommandLine := Process.CommandLine + ' -playlist';
        break;
      end;
    end;
  end;

  Process.CommandLine := Process.CommandLine  + ' "' + Song + '"';

  {$ifdef LOGGING} Writeln('EXECUTE: ' + Process.CommandLine); {$endif}

  if FRadioPlaying then
    Process.Options := Process.Options + [poStderrToOutPut, poUsePipes]
  else
    Process.Options := Process.Options + [poStderrToOutPut];

  Process.Execute;
end;

function TMusicPlayer.GetState: TMusicPlayerState;
begin
  if FState = mpsPlaying then
  begin
    if not FPlayProcess.Running then
    begin
      {$ifdef LOGGING} WriteLn('DESTROY: Play process'); {$endif}
      DestroyPlayProcess;
    end;
  end;

  Result := FState;
end;

procedure TMusicPlayer.StopSong;
begin
  if FState = mpsPlaying then
  begin
    DestroyPlayProcess;
  end;
end;

procedure TMusicPlayer.DestroyPlayProcess;
begin
  if Assigned(FPlayProcess) then
  begin
    if FPlayProcess.Running then
    begin
      // Kill mplayer running in bash shell
      Shell('killall -9 mplayer');
      FPlayProcess.Terminate(1);
    end;

    FreeAndNil(FPlayProcess);

    FAnnouncement := False;
    FAnnouncementStart := 0;
    FState := mpsStopped;
    FRadioPlaying := False;
    FPlayProcessList := '';
  end;
end;

procedure TMusicPlayer.ProcessAnnouncement;
begin
  // Look for announcement start if not currntly in progress
  if not FAnnouncement and (FAnnouncementStart > 0)
    and (FAnnouncementStart < Now) then
  begin
    {$ifdef LOGGING} Writeln('ANNOUNCE: StartAnnouncement ' + TimeToStr(Now)); {$endif}
    FAnnouncementStart := 0;
    StartAnnouncement(FMuteLevel);
    FAnnouncement := True;
  end;

  // If announcement in progress look for stop
  if FAnnouncement and (FAnnouncementStop < Now) then
  begin
    {$ifdef LOGGING} Writeln('ANNOUNCE: StopAnnouncement ' + TimeToStr(Now)); {$endif}
    StopAnnouncement(FMuteLevel);
    FAnnouncement := False;
  end;
end;

procedure TMusicPlayer.SetVolAttenuation(AValue: integer);
begin
  if (AValue < 0) then FVolAttenuation := 0
  else if (AValue > 100) then FVolAttenuation := 100
  else FVolAttenuation:=AValue;
end;

// Mute the volume during an announcement
procedure TMusicPlayer.StartAnnouncement(MuteLevel: integer);
var
  Buffer: array[0..1] of char;
  i: Integer;
begin
  {$ifdef LOGGING} Writeln('MPLAYER: Setting volume low ...'); {$endif}

  if Assigned(FPlayProcess) then
  begin
    // Mplayer volume down
    Buffer[0] := '9';

    for i := 0 to MuteLevel do
    begin
      FPlayProcess.Input.Write(Buffer, 1);
      Sleep(33);
    end;
  end;

  {$ifdef LOGGING} Writeln('MPLAYER: Set volume low.'); {$endif}
end;

// Restores the volume after the announcement
procedure TMusicPlayer.StopAnnouncement(MuteLevel: integer);
var
  Buffer: array[0..1] of char;
  i: Integer;
begin
  {$ifdef LOGGING} Writeln('MPLAYER: Setting volume high ...'); {$endif}

  if Assigned(FPlayProcess) then
  begin
    // Mplayer volume up
    Buffer[0] := '0';

    for i := 0 to MuteLevel do
    begin
      FPlayProcess.Input.Write(Buffer, 1);
      Sleep(33);
    end;
  end;

  {$ifdef LOGGING} Writeln('MPLAYER: Volume high.'); {$endif}
end;

function TMusicPlayer.ReadProcessData: string;
var
  Output: string;
  Len: integer;
begin
  Result := '';

  if (FPlayProcess.Output.NumBytesAvailable > 0) then
  begin
    Len := FPlayProcess.Output.NumBytesAvailable;
    SetLength(Output, Len);

    FPlayProcess.Output.Read(PChar(@Output[1])^, Len);
{
{$ifdef LOGGING}
    Writeln('MPLAYER: ------------------DATA----------------------');
    Writeln(Output);
    Writeln('MPLAYER: --------------------------------------------');
{$endif}
}
    Result := Output;
  end;
end;

procedure TMusicPlayer.SetAnnouncements(AValue: TAnnouncements);
begin
  if AValue = anOff then
    FMuteLevel := 0
  else if AValue = anQuiet then
    FMuteLevel := 12
  else FMuteLevel := 30;
end;

function TMusicPlayer.ProcessRadio: string;
const
  AdTypes: array [0..4] of string = ('sky.fm', 'adw_ad=''true''',
    'back-soon', 'ICY Info: StreamTitle='';StreamUrl='';', 'radiotunes');
var
  Title: string;
  TitleList: TStringList;
  Len, p, i, v: integer;
  Announcement: integer;
  AnnouncmentInProgress: boolean;
  H, M, S, MS: word;
  j: Integer;
begin
  // ICY Info: StreamTitle='Enos McLeod - Jericho';StreamUrl='';

  TitleList := TStringList.Create;
  Title := '';

  AnnouncmentInProgress := False;

  try
    if Assigned(FPlayProcess) then
    begin
      if (FPlayProcess.Output.NumBytesAvailable > 0) then
      begin
        FPlayProcessList := FPlayProcessList + ReadProcessData;
      end;

      TitleList.Text := FPlayProcessList;

      // Grep StreamTitle
      for i := TitleList.Count-1 downto 0 do
      begin
        if Pos('StreamTitle', TitleList.Strings[i]) = 0 then
          TitleList.Delete(i);
      end;

      // Process announcements if required
      if FMuteLevel > 0 then
      begin
        // Detect announcments (Adverts)
        i := TitleList.Count - 1;
        if (i >= 0) then
        begin
          // Advert detection search
          for j := 0 to High(AdTypes) do
          begin
            Announcement := Pos(LowerCase(AdTypes[j]), LowerCase(TitleList.Strings[i]));
            if (Announcement > 0) then
            begin
              FAdvertType := AdTypes[j];
              break;
            end
          end;

          // Is there an announcement?
          if (Announcement > 0) then
          begin
            AnnouncmentInProgress := True;

            // Is an announcement, and is it known about and sheduled?
            if not FAnnouncement and (FAnnouncementStart = 0) then
            begin
              // Set announcment start time in the future
              FAnnouncementStart := Now + EncodeTime(0, 0, FAdDelay, 0);

              {$ifdef LOGGING} Writeln('ANNOUNCE: Start detected "' + FAdvertType + '"'); {$endif}
            end;

            // Update the end time
            FAnnouncementStop := Now + EncodeTime(0, 0, 2 + FAdDelay, 0);
          end
          else
          begin
            // Cancel if only short announcement
            if (FAnnouncementStart <> 0) and not FAnnouncement then
            begin
              // Calculate length of announcement
              DecodeTime(FAnnouncementStart - FAnnouncementStop, H, M, S, MS);

              // Cancel if announcement < 4 seconds
              if S <= 5 then
              begin
                AnnouncmentInProgress := False;
                FAnnouncementStart := 0;

                {$ifdef LOGGING} Writeln('ANNOUNCE: Stopped - too short.'); {$endif}
              end;
            end;
          end;
        end;
      end;

      if not AnnouncmentInProgress and not FAnnouncement
        and (TitleList.Count > 0) then
      begin
        Title := TitleList.Strings[TitleList.Count-1];

        // Remove unwanted beginning
        p := Pos('''', Title);
        Len :=  Length(Title);
        if (p > 0) and (p < Length(Title)) then
         Title := Copy(Title, p + 1, Len);

        // Remove unwanted end
        p := Pos(';', Title);
        if (p > 1) then
         Title := Copy(Title, 1, p-2);

        if Title = '' then
        begin
          Title := '';
        end
        else
        begin
          FNewRadioTitle := Title;
          FRadioTitleTime := Now + EncodeTime(0, 0, FAdDelay, 0);
        end;

        // Do not let the list grow
        FPlayProcessList := '';
      end;

      ProcessAnnouncement;
    end;
  except
    on E: exception do
    begin
      Writeln('EXCEPTION: Function TMusicPlayer.GetRadioTitle');
      Writeln('MESSAGE: ' + E.Message);
      TitleList.Free;
      Exit;
    end
  end;

  TitleList.Free;

  // Only update title after delay. This prevents showing the new song
  // before the old song has ended (due to buffering).
  if (FRadioTitleTime < Now) or (FRadioTitle = '') then
    FRadioTitle := FNewRadioTitle;
end;

function TMusicPlayer.GetRadioTitle: string;
begin
  // Override the song title
  if FAnnouncement then
  begin
    Result := 'MUTING ADVERT ...' + FAdvertType;
  end
  else Result := FRadioTitle;
end;

procedure TMusicPlayer.Tick;
begin
  if FNextTick < Now then
  begin
    if FRadioPlaying then ProcessRadio;

    // Limit ticks to 1 a second
    FNextTick := Now + EncodeTime(0, 0, 1, 0);
  end;
end;

constructor TMusicPlayer.Create(MixerControl : string; UsePulseVol: boolean; EQ: TMplayerEQ);
var
  i: Integer;
begin
  FNextTick := 0;
  FMixerControl := MixerControl;
  FUsePulseVol := UsePulseVol;
  FVolAttenuation := 0;

  FMuteLevel := 0;

  FMplayerEQ := EQ;

  FPlayProcess := nil;
  FPlayProcessList := '';

  // Force getvolume to read the volume.
  FVolume := -1;
  FVolume := GetVolume;

  FAdvertType := '';

  FID3v1 := TID3v1Tag.Create;
  FID3v2 := TID3v2Tag.Create;
end;

destructor TMusicPlayer.Destroy;
begin
  FID3V1.Free;
  FID3v2.Free;

  DestroyPlayProcess;

  inherited Destroy;
end;

procedure TMusicPlayer.Play(Filename: string);
begin
  PlaySong(Filename);
end;

procedure TMusicPlayer.VolumeUp;
var
  Value: Integer;
begin
  case FVolume of
    0..1: Inc(FVolume); // for better low volume control
    2..4: FVolume := 5;
    5..100:
      begin
        Value := 5 - (FVolume mod 5);
        if Value = 0 then Value := 5;

        FVolume := FVolume + Value;
        if FVolume > 100 then FVolume := 100;
      end;
  end;

  SetVolume(FVolume);
end;

procedure TMusicPlayer.VolumeDown;
var
  Value: Integer;
begin
  case FVolume of
    1..2: Dec(FVolume); // for better low volume control
    3..5: FVolume := 2;
    6..100:
      begin
        Value := FVolume mod 5;
        if Value = 0 then Value := 5;

        FVolume := FVolume - Value;
        if FVolume < 0 then FVolume := 0;
      end;
  end;

  SetVolume(FVolume);
end;

procedure TMusicPlayer.SetVolume(Volume: integer);
var
  Output: string;
  CommandLine: string;
begin
  if Volume > 100 then Volume := 100
  else if Volume < 0 then Volume := 0;

  if FUsePulseVol then
    CommandLine := 'amixer -D pulse sset ''' + FMixerControl + ''' '
  else
    CommandLine := 'amixer -- sset ''' + FMixerControl + ''' playback ';

  CommandLine := CommandLine + IntToStr(Volume) + '%';

  RunCommand(CommandLine, Output);
end;

function TMusicPlayer.GetVolume: integer;
var
  Output: string;
  CommandLine: string;
  PStart, PEnd: Integer;
begin
  // Only get the volume once.
  if FVolume > 0 then Result := FVolume
  else
  begin
    Result := 50;

    if FUsePulseVol then
      CommandLine := 'amixer -D pulse sget ''' + FMixerControl
    else
      CommandLine := 'amixer -- sget ''' + FMixerControl + ''' playback';

    if RunCommand(CommandLine, Output) then
    begin
      PStart := Pos('[', Output) + 1;
      PEnd := Pos('%', Output);
      if (PEnd > PStart) and (PStart > 1) then
      begin
        Output := Copy(Output, PStart, PEnd - PStart);
        Result := StrToIntDef(Output, 50);
      end;
    end;
  end;
end;

procedure TMusicPlayer.Stop;
begin
  StopSong;
end;

end.

