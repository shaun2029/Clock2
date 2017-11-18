//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit MusicPlayer;

{$mode objfpc}{$H+}

interface

// Code to support older Lazarus/FPC
//{$define NOGUI}
//{$define LOGGING}

uses
  Process,
  {$ifndef NOGUI}
  LCLProc, MplayerEQ,
  {$else}
  process_legacy,
  {$endif}
  Classes, SysUtils, ID3v1Library, ID3v2Library, unix, Pipes, MPD;

type
  {$ifdef NOGUI}
  TMplayerEQ = array of integer;
  {$endif}

  TMusicPlayerState = (mpsStopped, mpsPlaying);
//  TAnnouncements = (anOff, anQuiet, anMute);
  TVolumeControl = (vcPulse, vcAlsa, vcSoftVol);

  { TMusicPlayer }

  TMusicPlayer  = class
  private
    FNextTick: TDateTime;
    FVolumeControl: TVolumeControl;
    FVolAttenuation: integer;
    FMplayerEQ: TMplayerEQ;
    FMixerControl: string;
    FVolume: integer;
    FPlayProcess: TProcess;
    FPlayTimeout: TDateTime;
    FPlayProcessList: string;
    FSongArtist: string;
    FSongTitle: string;
    FSongURL: string;
    FState: TMusicPlayerState;
    FID3v1: TID3v1Tag;
    FID3v2: TID3v2Tag;

    FRadioTitle: string;
    FNewRadioTitle: string;
    FRadioTitleTime: TDateTime;
    FRadioPlaying: boolean;
    FMuteLevel: integer;
    procedure DestroyPlayProcess;
    function GetRadioTitle: string;
    function GetState: TMusicPlayerState;
    procedure PlaySong(Song, URL: string);
    procedure ProcessRadio;
    function ReadProcessData: string;
    procedure SetVolAttenuation(AValue: integer);
    procedure SetVolume(Volume: integer);
    procedure StartPlayProcess(Song: string; out Process: TProcess);
    procedure StartPlayApplication(CommandLine: string; out Process: TProcess);
    procedure StopSong;
    procedure WriteProcessData(Command: string);
  public
    constructor Create(MixerControl : string; VolControl: TVolumeControl; EQ: TMplayerEQ);
    destructor Destroy; override;

    procedure Tick;     // Used to process radio stream information

    procedure Play(Filename, URL: string);
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
  end;

implementation

procedure TMusicPlayer.PlaySong(Song, URL: string);
begin
  // Ensure that song is not playing
  StopSong;

  {$ifdef LOGGING} WriteLn('PLAY: ' + Song); {$endif}

  FSongTitle := '';
  FSongArtist := '';
  FRadioTitle := '';
  FNewRadioTitle := '';
  FSongURL := URL;

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

    // If the song name starts with cmd: then treat the song as a command.
    if (Pos('cmd://', Song) = 1) then
    begin
      StartPlayApplication(Copy(Song, 7, Length(Song)), FPlayProcess);
    end
    else StartPlayProcess(Song, FPlayProcess);

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
  EQ: String;
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


  if FVolumeControl = vcPulse then
     Process.CommandLine := 'mplayer -ao pulse ' + EQ
  else if FVolumeControl = vcPulse then
     Process.CommandLine := 'mplayer -ao alsa ' + EQ
  else
     Process.CommandLine := 'mplayer ' + EQ;

  Process.CommandLine := Process.CommandLine + ' -slave -af-add format=s16le -softvol -volume ' + IntToStr(100-FVolAttenuation);

  { If the song name begins with 'http://' then it assumed to be a URL of a stream. }
  if ((Pos('http://', Trim(Lowercase(Song))) > 0) or (Pos('https://', Trim(Lowercase(Song))) > 0)) then
  begin
    FRadioPlaying := True;

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

  Process.Options := [poStderrToOutPut, poUsePipes];

  Process.Execute;
end;

procedure TMusicPlayer.StartPlayApplication(CommandLine: string; out Process: TProcess);
begin
  Process := TProcess.Create(nil);

  FRadioPlaying := True;
  Process.CommandLine := CommandLine;

  {$ifdef LOGGING} Writeln('EXECUTE: ' + Process.CommandLine); {$endif}

  Process.Options := [poStderrToOutPut, poUsePipes];

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
var
  Output: string;
begin
  if Assigned(FPlayProcess) then
  begin
    if FPlayProcess.Running then
    begin
      // Kill mplayer running in bash shell
      FPlayProcess.Terminate(0);
      RunCommand('killall -9 mplayer', Output);
    end;

    FreeAndNil(FPlayProcess);
    FState := mpsStopped;
    FRadioPlaying := False;
    FPlayProcessList := '';
  end;
end;

procedure TMusicPlayer.SetVolAttenuation(AValue: integer);
begin
  if (AValue < 0) then FVolAttenuation := 0
  else if (AValue > 100) then FVolAttenuation := 100
  else FVolAttenuation:=AValue;
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

{$ifdef LOGGING}
    Writeln(stderr, 'MPLAYER: ------------------DATA----------------------');
    Writeln(stderr, Output);
    Writeln(stderr, 'MPLAYER: --------------------------------------------');
{$endif}

    Result := Output;
  end;
end;

procedure TMusicPlayer.WriteProcessData(Command: string);
var
  Len: integer;
begin
  Command := Command + LineEnding;
  FPlayProcess.Input.Write(Command[1], Length(Command));
end;

procedure TMusicPlayer.ProcessRadio;
var
  Title: string;
  TitleList: TStringList;
  Len, p, i, v: integer;
  H, M, S, MS: word;
  j: Integer;
begin
  TitleList := TStringList.Create;
  Title := '';

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

      if (TitleList.Count > 0) then
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
          FRadioTitleTime := Now + EncodeTime(0, 0, 3, 0);
        end;

        // Do not let the list grow
        FPlayProcessList := '';
      end;
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
var
  URL, Host, Port: string;
begin
  URL := MPDParseURL(FSongURL, Host, Port);

  if Host <> '' then
  begin
    Result := MPDGetTrackTitle(Host, Port);
  end
  else Result := FRadioTitle;
end;

procedure TMusicPlayer.Tick;
begin
  if FNextTick < Now then
  begin
    if FRadioPlaying then ProcessRadio
    else
    begin
      if (FPlayProcess.Output.NumBytesAvailable > 0) then
      begin
        ReadProcessData;
      end;
    end;

    // Limit ticks to 1 a second
    FNextTick := Now + EncodeTime(0, 0, 1, 0);
  end;
end;

constructor TMusicPlayer.Create(MixerControl : string; VolControl: TVolumeControl; EQ: TMplayerEQ);
var
  i: Integer;
begin
  FNextTick := 0;
  FMixerControl := MixerControl;
  FVolumeControl := VolControl;
  FVolAttenuation := 0;

  FMuteLevel := 0;

  FMplayerEQ := EQ;

  FPlayProcess := nil;
  FPlayProcessList := '';

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

procedure TMusicPlayer.Play(Filename, URL: string);
begin
  PlaySong(Filename, URL);
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

  if FVolumeControl = vcSoftVol then
  begin
    WriteProcessData('set_property volume ' + IntToStr(Volume));
  end
  else
  begin
    if FVolumeControl = vcPulse then
      CommandLine := 'amixer -D pulse sset ''' + FMixerControl + ''' '
    else
      CommandLine := 'amixer -- sset ''' + FMixerControl + ''' playback ';

    CommandLine := CommandLine + IntToStr(Volume) + '%';

    RunCommand(CommandLine, Output);
  end;
end;

function TMusicPlayer.GetVolume: integer;
var
  Output: string;
  CommandLine: string;
  PStart, PEnd: Integer;
begin
  // Only get the volume once.
  if FVolume > 0 then Result := FVolume
  else if FVolumeControl = vcSoftVol then
  begin
    FVolume := 100;
    Result := FVolume;
  end
  else
  begin
    Result := 100;

    if FVolumeControl = vcPulse then
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

