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

    procedure DestroyPlayProcess;
    function GetRadioTitle: string;
    function GetState: TMusicPlayerState;
    procedure PlaySong(Song: string);
    procedure SetVolume(Volume: integer);
    procedure StartPlayProcess(Song: string; out Process: TProcess);
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

function TMusicPlayer.GetRadioTitle: string;
var
  Title: string;
  TitleList: TStringList;
  p, Len, i: integer;
begin
  // ICY Info: StreamTitle='Enos McLeod - Jericho';StreamUrl='';

  if FRadioTitleTime < Now then
  begin
    TitleList := TStringList.Create;
    Title := '';

    try
      if FileExists('/tmp/radio-song-titles.txt') then
      begin
        TitleList.LoadFromFile('/tmp/radio-song-titles.txt');

         // Delete unwanted strings
         for i := TitleList.Count - 1 downto 0 do
         begin
           if Pos('StreamTitle=''SKY.FM', TitleList.Strings[i]) > 0 then TitleList.Delete(i);
         end;

         if TitleList.Count > 0 then
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

           FRadioTitleTime := Now + EncodeTime(0, 0, 5, 0);

           // Do not let the file grow
           TitleList.Clear;
           TitleList.Add(Title);
           TitleList.SaveToFile('/tmp/radio-song-titles.txt');
         end;
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
  FVolume := 50;
//  SetVolume(FVolume);

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
  Process: TProcess;
begin
  if Volume > 100 then Volume := 100
  else if Volume < 0 then Volume := 0;

  try
    Process := TProcess.Create(nil);
    Process.Options := Process.Options + [poWaitOnExit];

    Process.CommandLine := 'amixer set Master ' + IntToStr(FVolume) + '%';
    Process.Execute;
  except
    on E: Exception do
    begin
      DebugLn(Self.ClassName + #9#9 + E.Message);
    end;
  end;
end;

procedure TMusicPlayer.Stop;
begin
  StopSong;
end;

end.

