//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit MusicPlayer;

{$mode objfpc}{$H+}

interface

// Code to support older Lazarus/FPC
//{$define LEGACY}

uses
  Process,
  {$ifndef LEGACY}
  LCLProc,
  {$else}
  process_legacy,
  {$endif}
  Classes, SysUtils, ID3v1Library, ID3v2Library, unix, Math;

type

  TMusicPlayerState = (mpsStopped, mpsPlaying);

  { TMusicPlayer }

  TMusicPlayer  = class
  private
    FUsePulseVol, FVolAttenuation: boolean;
    FMixerControl: string;
    FVolume: integer;
    FPlayProcess: TProcess;
    FPlayTimeout: TDateTime;
    FSongArtist: string;
    FSongTitle: string;
    FState: TMusicPlayerState;
    FID3v1: TID3v1Tag;
    FID3v2: TID3v2Tag;

    FRadioTitle: string;
    FNewRadioTitle: string;
    FRadioTitleTime: TDateTime;
    FRadioPlaying: boolean;

    FAnnouncementVol: integer;
    FAnnouncement: boolean;
    FAnnouncementStart: TDateTime;
    FAnnouncementStop: TDateTime;

    procedure DestroyPlayProcess;
    function GetRadioTitle: string;
    function GetState: TMusicPlayerState;
    procedure PlaySong(Song: string);
    procedure ProcessAnnouncement;
    procedure SetVolume(Volume: integer);
    procedure StartAnnouncement;
    procedure StartPlayProcess(Song: string; out Process: TProcess);
    procedure StopAnnouncement;
    procedure StopSong;
  public
    constructor Create(MixerControl : string; UsePulseVol: boolean);
    destructor Destroy; override;

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
    property VolAttenuation: boolean read FVolAttenuation write FVolAttenuation;
  end;

implementation

procedure TMusicPlayer.PlaySong(Song: string);
begin
  // Ensure that song is not playing
  StopSong;

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
        {$ifndef LEGACY}
        DebugLn(Self.ClassName + #9#9 + 'Failed to get ID3 Tags for "'
          + ExtractFilename(Song) + '"');
        DebugLn(Self.ClassName + #9#9 + E.Message);
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
      {$ifndef LEGACY}
      DebugLn(Self.ClassName + #9#9 + E.Message);
      {$endif}
    end;
  end;
end;

procedure TMusicPlayer.StartPlayProcess(Song: string; out Process: TProcess);
begin
  Process := TProcess.Create(nil);

  FRadioPlaying := False;

  // If announcement in progress stop it.
  if FAnnouncement then
  begin
    SetVolume(FAnnouncementVol);
    FAnnouncement := False;
  end;

  FAnnouncementStart := 0;

  { If the file does not exist then it could be a URL of a stream.
    Use mplayer to play streams. Prefer MPG123 for MP3 files as it
    supports replaygain. }

  if not FileExists(Song) or not FileExists('/usr/bin/mpg123')
    or not (LowerCase(ExtractFileExt(Song)) = '.mp3') or FVolAttenuation then
  begin
    if not FileExists(Song) then
    begin
      FRadioPlaying := True;

      Process.CommandLine := 'bash -c ''mplayer -cache 256 ';

      // Lower the volume if required.
      // Attenuation will attenuate the playback volume level by 18dB to compensate for headphone amp.
      if FVolAttenuation then
         Process.CommandLine := Process.CommandLine + '-af volume=-18:0 ';

      Process.CommandLine := Process.CommandLine + '"' + Song + '" '
        + '| grep --line-buffered "StreamTitle" > /tmp/radio-song-titles.txt'''
    end
    else
    begin
      Process.CommandLine := 'mplayer -cache 256 ';

      if FVolAttenuation then
         Process.CommandLine := Process.CommandLine + '-af volume=-18:0 ';

      Process.CommandLine := Process.CommandLine + '"' + Song + '"';
    end;
  end
  else
  begin
    Process.CommandLine := 'mpg123 --rva-mix "' + Song + '"';
  end;

  Process.Execute;
end;

function TMusicPlayer.GetState: TMusicPlayerState;
begin
  if FState = mpsPlaying then
  begin
    if not FPlayProcess.Running then
    begin
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
    FState := mpsStopped;

    if FPlayProcess.Running then
    begin
      // Kill mplayer running in bash shell
      if FRadioPlaying then Shell('killall -9 mplayer');
      FPlayProcess.Terminate(1);
    end;

    FRadioPlaying := False;
    FreeAndNil(FPlayProcess);
  end;
end;

procedure TMusicPlayer.ProcessAnnouncement;
begin
  // Look for announcement start if not currntly in progress
  if not FAnnouncement and (FAnnouncementStart > 0)
    and (FAnnouncementStart < Now) then
  begin
    FAnnouncementStart := 0;
    StartAnnouncement;
    FAnnouncement := True;
  end;

  // If announcement in progress look for stop
  if FAnnouncement and (FAnnouncementStop < Now) then
  begin
    StopAnnouncement;
    FAnnouncement := False;
  end;
end;

// Turns down the volume during an announcement
procedure TMusicPlayer.StartAnnouncement;
begin
  FAnnouncementVol := GetVolume;
  SetVolume(FAnnouncementVol div 10);
end;

// Turns up the volume during an announcement
procedure TMusicPlayer.StopAnnouncement;
begin
  SetVolume(FAnnouncementVol);
end;

function TMusicPlayer.GetRadioTitle: string;
var
  Title: string;
  TitleList: TStringList;
  p, Len, i, v: integer;
  Announcement: integer;
  AnnouncmentInProgress: boolean;
  H, M, S, MS: word;
begin
  // ICY Info: StreamTitle='Enos McLeod - Jericho';StreamUrl='';

  TitleList := TStringList.Create;
  Title := '';

  AnnouncmentInProgress := False;

  try
    if FileExists('/tmp/radio-song-titles.txt') then
    begin
      TitleList.LoadFromFile('/tmp/radio-song-titles.txt');

       // Detect announcments (Adverts)
       i := TitleList.Count - 1;
       if (i >= 0) then
       begin
         // Advert detection search for SKY.FM
         Announcement := Pos('SKY.FM', UpperCase(TitleList.Strings[i]));
         if (Announcement = 0) then
           Announcement := Pos('adw_ad=''true''', TitleList.Strings[i]);

         // Is there an announcement?
         if (Announcement > 0) then
         begin
           AnnouncmentInProgress := True;

           // Is an announcement, and is it known about and sheduled?
           if not FAnnouncement and (FAnnouncementStart = 0) then
           begin
             // Set announcment start time in the future
             FAnnouncementStart := Now + EncodeTime(0, 0, 2, 0);
             FAnnouncementStop := Now + EncodeTime(0, 0, 10, 0);
           end
           else
           begin
             // Update the end time
             FAnnouncementStop := Now + EncodeTime(0, 0, 8, 0);
{
             // Is this a real announcement or false positive?
             // Cancel it if it has not started, and is not and announcement,
             // and an anouncement start time has been triggered.
             if not FAnnouncement and (Announcement <= 0)
               and (FAnnouncementStart > 0 )
               and (FAnnouncementStop > FAnnouncementStart) then
             begin
               DecodeTime(FAnnouncementStop - FAnnouncementStart, H, M, S, MS);

               // If the length of the announcement is less than 5 seconds cancel it.
               if (H + M <= 0) and (S < 5) then FAnnouncementStart := 0;
             end;
}
           end;
         end;
       end;

       if not AnnouncmentInProgress and (TitleList.Count > 0) then
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

         FNewRadioTitle := Title;
         FRadioTitleTime := Now + EncodeTime(0, 0, 4, 0);

         // Do not let the file grow
         TitleList.Clear;
         TitleList.SaveToFile('/tmp/radio-song-titles.txt');
       end;

       ProcessAnnouncement;
     end;
  finally
     TitleList.Free;
  end;

  // Only update title after delay. This prevents showing the new song
  // before the old song has ended (due to buffering).
  if (FRadioTitleTime < Now) or (FRadioTitle = '') then
    FRadioTitle := FNewRadioTitle;

  // Override the song title
  if FAnnouncement then
  begin
    Result := 'MUTING ADVERT ...';
  end
  else Result := FRadioTitle;
end;

constructor TMusicPlayer.Create(MixerControl : string; UsePulseVol: boolean);
begin
  FMixerControl := MixerControl;
  FUsePulseVol := UsePulseVol;
  FVolAttenuation := False;

  FPlayProcess := nil;

  // Force getvolume to read the volume.
  FVolume := -1;
  FVolume := GetVolume;

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
  Value := 5 - (FVolume mod 5);
  if Value = 0 then Value := 5;

  FVolume := FVolume + Value;
  if FVolume > 100 then FVolume := 100;

  SetVolume(FVolume);
end;

procedure TMusicPlayer.VolumeDown;
var
  Value: Integer;
begin
  Value := FVolume mod 5;
  if Value = 0 then Value := 5;

  FVolume := FVolume - Value;
  if FVolume < 0 then FVolume := 0;

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

