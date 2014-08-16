//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit MusicPlayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, ID3v1Library, ID3v2Library, LCLProc, unix;

type

  TMusicPlayerState = (mpsStopped, mpsPlaying);

  { TMusicPlayer }

  TMusicPlayer  = class
  private
    FVolume: integer;
    FPlayProcess: TProcess;
    FPlayTimeout: TDateTime;
    FSongArtist: string;
    FSongTitle: string;
    FState: TMusicPlayerState;
    FID3v1: TID3v1Tag;
    FID3v2: TID3v2Tag;

    FRadioTitle: string;
    FRadioTitleTime: TDateTime;
    FRadioPlaying: boolean;

    FAnnouncementVol: integer;
    FAnnouncement: boolean;
    FAnnouncementStart: TDateTime;
    FAnnouncementStop: TDateTime;

    procedure DestroyPlayProcess;
    function GetRadioTitle: string;
    function GetState: TMusicPlayerState;
    function GetVolume: integer;
    procedure PlaySong(Song: string);
    procedure ProcessAnnouncement;
    procedure SetVolume(Volume: integer);
    procedure StartAnnouncement;
    procedure StartPlayProcess(Song: string; out Process: TProcess);
    procedure StopAnnouncement;
    procedure StopSong;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Play(Filename: string);
    procedure VolumeUp;
    procedure VolumeDown;
    procedure Stop;
  published
    property SongArtist: string read FSongArtist;
    property SongTitle: string read FSongTitle;
    property State: TMusicPlayerState read GetState;
    property RadioTitle: string read GetRadioTitle;
  end;

implementation

procedure TMusicPlayer.PlaySong(Song: string);
begin
  // Ensure that song is not playing
  StopSong;

  FSongTitle := '';
  FSongArtist := '';
  FRadioTitle := '';

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
        DebugLn(Self.ClassName + #9#9 + 'Failed to get ID3 Tags for "'
          + ExtractFilename(Song) + '"');
        DebugLn(Self.ClassName + #9#9 + E.Message);
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
      DebugLn(Self.ClassName + #9#9 + E.Message);
    end;
  end;
end;

procedure TMusicPlayer.StartPlayProcess(Song: string; out Process: TProcess);
begin
  Process := TProcess.Create(nil);
  Process.Options := Process.Options;

  FRadioPlaying := False;
  FRadioTitleTime := Now;

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
    or not (LowerCase(ExtractFileExt(Song)) = '.mp3') then
  begin
    if not FileExists(Song) then
    begin
      FRadioPlaying := True;

      Process.CommandLine := 'bash -c ''mplayer -cache 256 -cache-min 50 "' + Song + '" '
        + '| grep --line-buffered "StreamTitle" > /tmp/radio-song-titles.txt'''
    end
    else
    begin
      Process.CommandLine := 'mplayer -cache 256 -cache-min 50 "' + Song + '"'
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
      if FRadioPlaying then Shell('killall mplayer');
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

    // Set the end time for a time after the start
    FAnnouncementStop := Now + EncodeTime(0, 0, 5, 0);
  end;

  // If announcement in progress look for stop
  if FAnnouncement and (FAnnouncementStop < Now) then
  begin
    StopAnnouncement;
    FAnnouncement := False;
  end;

  // Override the sing title
  if FAnnouncement then
  begin
    FRadioTitle := 'MUTING ADVERT ...'
  end;
end;

// Turns down the volume during an announcement
procedure TMusicPlayer.StartAnnouncement;
var
  v, Vol, StepSize: integer;
begin
  FAnnouncementVol := GetVolume;
  Vol := FAnnouncementVol div 10;

  StepSize := (FAnnouncementVol - Vol) div 10;
  if StepSize < 1 then StepSize := 1;

  // Turn up volume
  for v := FAnnouncementVol downto Vol do
  begin
    if v mod StepSize = 0 then
    begin
      SetVolume(v);
      Sleep(200);
    end;
  end;

  SetVolume(Vol);
end;

// Turns up the volume during an announcement
procedure TMusicPlayer.StopAnnouncement;
var
  v, Vol, StepSize: integer;
begin
  Vol := GetVolume;

  if FAnnouncementVol > Vol then
  begin
    StepSize := (FAnnouncementVol - Vol) div 10;
    if StepSize < 1 then StepSize := 1;

    // Turn up volume
    for v := GetVolume to FAnnouncementVol do
    begin
      if v mod StepSize = 0 then
      begin
        SetVolume(v);
        Sleep(200);
      end;
    end;
  end;
  SetVolume(FAnnouncementVol);
end;

function TMusicPlayer.GetRadioTitle: string;
var
  Title: string;
  TitleList: TStringList;
  p, Len, i, v: integer;
  Announcement, StreamTitle: integer;
  AnnouncmentInProgress: boolean;
  H, M, S, MS: word;
begin
  // ICY Info: StreamTitle='Enos McLeod - Jericho';StreamUrl='';

  if FRadioTitleTime < Now then
  begin
    TitleList := TStringList.Create;
    Title := '';

    AnnouncmentInProgress := False;

    try
      if FileExists('/tmp/radio-song-titles.txt') then
      begin
        TitleList.LoadFromFile('/tmp/radio-song-titles.txt');

         // Detect announcments (Adverts)
         for i := TitleList.Count - 1 downto 0 do
         begin
           // Advert detection  needs a stream tile and a SKY.FM
           StreamTitle := Pos('StreamTitle=', TitleList.Strings[i]);
           if StreamTitle > 0 then
             Announcement := Pos('SKY.FM', TitleList.Strings[i]);

           // Is there a stream title?
           if (StreamTitle > 0) then
           begin
             // Is this a current message (latest on the list)?
             if (i = TitleList.Count - 1) then
             begin
               AnnouncmentInProgress := Announcement > 0;

               // Is an announcement, and is it known about and sheduled?
               if (Announcement > 0) and not FAnnouncement and (FAnnouncementStart <= 0) then
               begin
                 // Set announcment start time in the future
                 FAnnouncementStart := Now + EncodeTime(0, 0, 10, 0);
               end
               else
               begin
                 // Update the end time
                 FAnnouncementStop := Now + EncodeTime(0, 0, 10, 0);

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
               end;
             end;

             Break;
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
           p := Pos('''', Title);
           if (p > 1) then
             Title := Copy(Title, 1, p-1);

           FRadioTitle := Title;

           // Update every second
           FRadioTitleTime := Now + EncodeTime(0, 0, 1, 0);

           // Do not let the file grow
           TitleList.Clear;
           TitleList.Add(Title);
           TitleList.SaveToFile('/tmp/radio-song-titles.txt');
         end;

         ProcessAnnouncement;
       end;
    finally
       TitleList.Free;
    end;
  end;

  Result := FRadioTitle;
end;

constructor TMusicPlayer.Create;
begin
  FPlayProcess := nil;
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
begin
  FVolume := FVolume + 5;
  if FVolume > 100 then FVolume := 100;

  SetVolume(FVolume);
end;

procedure TMusicPlayer.VolumeDown;
begin
  FVolume := FVolume - 5;
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

  CommandLine := 'amixer -D pulse sset Master ' + IntToStr(Volume) + '%';
  RunCommand(CommandLine, Output);
end;

function TMusicPlayer.GetVolume: integer;
var
  Output: string;
  CommandLine: string;
  PStart, PEnd: Integer;
begin
  Result := 50;

  CommandLine := 'amixer -D pulse sget Master';

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

procedure TMusicPlayer.Stop;
begin
  StopSong;
end;

end.

