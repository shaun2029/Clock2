//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit MusicPlayer;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Process, ID3v1Library, ID3v2Library, LCLProc;

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

    procedure DestroyPlayProcess;
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
  end;

implementation

procedure TMusicPlayer.PlaySong(Song: string);
begin
  // Ensure that song is not playing
  StopSong;

  FSongTitle := '';
  FSongArtist := '';

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

  try
    if FileExists(Song) then
    begin
      if Assigned(FPlayProcess) then DestroyPlayProcess;

      StartPlayProcess(Song, FPlayProcess);

      if Trim(FSongTitle) = '' then FSongTitle := ExtractFilename(Song);

      FState := mpsPlaying;
    end;
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

  Process.CommandLine := 'mplayer ' + '"' + Song + '"';

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
      FPlayProcess.Terminate(1);
    end;

    FreeAndNil(FPlayProcess);
  end;
end;

constructor TMusicPlayer.Create;
begin
  FPlayProcess := nil;
  FVolume := 50;
  SetVolume(FVolume);

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

