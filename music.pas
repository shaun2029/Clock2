//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit music;

{$mode objfpc}{$H+}

interface

uses
  {$ifndef LEGACY}
  FindThread,
  LCLProc,
  {$endif}
  Classes, SysUtils, MusicPlayer, IniFiles, MPD;

type

  TPlayerState = (psStopped, psPlaying);
  TPlayDirection = (pdCurrent, pdPrevious, pdNext);

  { TPlayer }

  TPlayer = class
  private
    FSongList, FPathList: TStringList;
    FFindSongList, FFindPathList: TStringList;
    FPlaySongList, FPlayPathList: TStringList;
    FFindPlaySongList, FFindPlayPathList: TStringList;

    {$ifndef LEGACY}
    FFindFiles: TFindFilesThread;
    {$endif}

    FMusicPlayer: TMusicPlayer;
    FConfigFile, FSearchPath: string;
    FSongIndex: integer;
    FPlaylistIndex: integer;
    FState: TPlayerState;
    FStreamTitle: string;
    FStreamURL: string;

    procedure FilterAudioFiles(var List: TStringList);
    function GetFileName(Index: integer; SongList, PathList: TStringList): string;
    function GetSongArtist: string;
    function GetSongTitle: string;
    procedure LoadSettings;
    function PlayPlaylistSong(Play: TPlayDirection): boolean;
    procedure PlaySong(Play: TPlayDirection);
    procedure RandomiseList(var List: TStringList);
    procedure SaveSettings;
//    procedure SetAnnouncements(AValue: TAnnouncements);
    procedure SetStreamURL(AValue: string);
    procedure StopSong;
  public
    procedure Play;
    procedure Pause;  // Actualy stops the music

    {  Stop just sets stopped state without calling FMusicPlayer.Stop
       This is because mpg123 output buffering is an issue. }
    procedure Stop;
    procedure Next;
    procedure Previous;

    procedure RescanSearchPath;

    function Tick: integer;

    function PlaySelection(SearchPaths: string; Random: boolean): integer;

    constructor Create(MusicPlayer: TMusicPlayer; ConfigFile, SearchPath: string);
    destructor Destroy; override;
  published
    property SongArtist: string read GetSongArtist;
    property SongTitle: string read GetSongTitle;
    property State: TPlayerState read FState;
    property SearchPath: string read FSearchPath;
    property StreamURL: string write SetStreamURL;
    property StreamTitle: string read FStreamTitle write FStreamTitle;
//    property Announcements: TAnnouncements write SetAnnouncements;
  end;

implementation

{ TPlayer }

constructor TPlayer.Create(MusicPlayer: TMusicPlayer; ConfigFile, SearchPath: string);
begin
  inherited Create;

  FStreamURL := '';
  FStreamTitle := '';

  FMusicPlayer := MusicPlayer;

  FConfigFile := ConfigFile;
  FSongList := TStringList.Create;
  FPathList := TStringList.Create;
  FPlaySongList := TStringList.Create;
  FPlayPathList := TStringList.Create;
  FFindSongList := TStringList.Create;
  FFindPathList := TStringList.Create;
  FFindPlaySongList := TStringList.Create;
  FFindPlayPathList := TStringList.Create;
  FSongIndex := 0;
  FState := psStopped;

  FSearchPath := SearchPath;
  LoadSettings;
end;

destructor TPlayer.Destroy;
begin
  FMusicPlayer.Stop;

  {$ifndef LEGACY}
  if Assigned(FFindFiles) then
  begin
    FFindFiles.Terminate;
    FFindFiles.WaitFor;
    FreeAndNil(FFindFiles);
  end
  else
  {$endif} SaveSettings;

  FSongList.Free;
  FPathList.Free;
  FPlaySongList.Free;
  FPlayPathList.Free;
  FFindSongList.Free;
  FFindPathList.Free;
  FFindPlaySongList.Free;
  FFindPlayPathList.Free;

  inherited Destroy;
end;

procedure TPlayer.StopSong;
begin
  FMusicPlayer.Stop;
end;

procedure TPlayer.Stop;
begin
  if FState = psPlaying then
  begin
//DebugLn('Music: Stop');
    FState := psStopped;
    StopSong;
  end;

  Tick;
end;

procedure TPlayer.Pause;
begin
  if FState = psPlaying then
  begin
//DebugLn('Music: Pause');
    FState := psStopped;
    StopSong;

    Tick;
  end
  else Play;
end;

procedure TPlayer.Play;
begin
//DebugLn('Music: Play');
  FState := psPlaying;
  PlaySong(pdCurrent);
  
  Tick;
end;

procedure TPlayer.Next;
begin
//DebugLn('Music: Next');
  FState := psPlaying;
  PlaySong(pdNext);

  Tick;
end;

procedure TPlayer.Previous;
begin
//DebugLn('Music: Next');
  FState := psPlaying;
  PlaySong(pdPrevious);

  Tick;
end;

// Returns true if scanning for music files

function TPlayer.Tick: integer;
begin
  Result := -1;

  {$ifndef LEGACY}
  if Assigned(FFindFiles) then
  begin
    if FFindFiles.Complete then
    begin
      FFindFiles.Terminate;
      FFindFiles.WaitFor;
      FreeAndNil(FFindFiles);
      FilterAudioFiles(FFindSongList);
      RandomiseList(FFindSongList);
      FSongList.Text := FFindSongList.Text;
      FPathList.Text := FFindPathList.Text;
    end
    else Result := FFindFiles.Count;
  end
  else {$endif} if (FState = psPlaying) then
  begin
    FMusicPlayer.Tick;

    if FMusicPlayer.State = mpsStopped then
    begin
      if FStreamURL <> '' then PlaySong(pdCurrent) // If the radio has stopped restart it.
      else PlaySong(pdNext); // Play the next song
    end;
  end;
end;

procedure TPlayer.PlaySong(Play: TPlayDirection);
var
  Filename: string;
  URL, Host, Port: string;
begin
  if FStreamURL <> '' then
  begin
    URL := MPDParseURL(FStreamURL, Host, Port);

    if Host <> '' then
    begin
      case Play of
        pdPrevious: MPDCommand(Host, Port, 'prev');
        pdNext:  MPDCommand(Host, Port, 'next');
        else FMusicPlayer.Play(URL);
      end;
    end
    else FMusicPlayer.Play(URL);
  end
  else if not PlayPlaylistSong(Play) then
  begin
    case Play of
      pdPrevious: Dec(FSongIndex , 1);
      pdNext: Inc(FSongIndex , 1);
    end;

    if FSongIndex < 0 then FSongIndex := FSongList.Count -1;
    if FSongIndex >= FSongList.Count then FSongIndex := 0;

    try
      if (FSongIndex < FSongList.Count) then
      begin
        Filename := GetFilename(FSongIndex, FSongList, FPathList);

        if FileExists(Filename) then FMusicPlayer.Play(Filename)
        {$ifndef LEGACY}
        else DebugLn('Music: Failed to find "' + Filename + '"')
        {$endif};
      end;
    except
      on E: Exception do
      begin
        {$ifndef LEGACY}
        DebugLn(Self.ClassName + #9#9 + E.Message);
        {$endif}
      end;
    end;

    SaveSettings;
  end;
end;

function TPlayer.PlayPlaylistSong(Play: TPlayDirection): boolean;
var
  Filename: string;
begin
  Result := False;

  if FPlaylistIndex <= FPlaySongList.Count -1 then
  begin
    case Play of
      pdPrevious: Dec(FPlaylistIndex , 1);
      pdNext: Inc(FPlaylistIndex , 1);
    end;

    if FPlaylistIndex < 0 then FPlaylistIndex := 0;

    if FPlaylistIndex <= FPlaySongList.Count -1 then
    begin
      Result := True;

      try
        Filename := GetFilename(FPlaylistIndex, FPlaySongList, FPlayPathList);

        if FileExists(Filename) then FMusicPlayer.Play(Filename)
        {$ifndef LEGACY}
        else DebugLn('Music: Failed to find "' + Filename + '"')
        {$endif};
      except
        on E: Exception do
        begin
          {$ifndef LEGACY}
          DebugLn(Self.ClassName + #9#9 + E.Message);
          {$endif}
        end;
      end;
    end;
  end;
end;

procedure TPlayer.SaveSettings;
var
  IniFile: TIniFile;
begin
  if FConfigFile = '' then Exit;

  IniFile := TIniFile.Create(FConfigFile);

  try
    IniFile.WriteString('Settings', 'MusicPath', FSearchPath);
    IniFile.WriteInteger('Settings', 'Position', FSongIndex);

    if (FSongList.Count > 0) then
    begin
      FSongList.SaveToFile(ChangeFileExt(FConfigFile, '.pl'));
      FPathList.SaveToFile(ChangeFileExt(FConfigFile, '.plp'));
    end;
  finally
    IniFile.Free;
  end;
end;
(*
procedure TPlayer.SetAnnouncements(AValue: TAnnouncements);
begin
  FMusicPlayer.Announcements := AValue;
end;
*)
procedure TPlayer.SetStreamURL(AValue: string);
begin
  FStreamURL := AValue;
  FState := psStopped;
end;

procedure TPlayer.LoadSettings;
var
  IniFile: TIniFile;
begin
  if FConfigFile = '' then Exit;

  IniFile := TIniFile.Create(FConfigFile);

  try
    if FSearchPath = '' then
      FSearchPath := IniFile.ReadString('Settings', 'MusicPath', '~/Music');

    FSongIndex := IniFile.ReadInteger('Settings', 'Position', 0);

    if FileExists(ChangeFileExt(FConfigFile, '.pl'))
      and FileExists(ChangeFileExt(FConfigFile, '.plp')) then
    begin
      FSongList.LoadFromFile(ChangeFileExt(FConfigFile, '.pl'));
      FPathList.LoadFromFile(ChangeFileExt(FConfigFile, '.plp'));
    end;
  finally
    IniFile.Free;
  end;
end;

function TPlayer.GetSongArtist: string;
begin
  if FStreamURL <> '' then Result := StreamTitle
  else Result := FMusicPlayer.SongArtist;
end;

function TPlayer.GetSongTitle: string;
begin
  if FStreamURL <> '' then Result := FMusicPlayer.RadioTitle
  else Result := FMusicPlayer.SongTitle;
end;

procedure TPlayer.FilterAudioFiles(var List: TStringList);
var
  i: integer;
  Ext: String;
begin
  for i := List.Count -1 downto  0 do
  begin
    Ext := Lowercase(ExtractFileExt(List.Strings[i]));

    if (Ext <> '.mp3') and (Ext <> '.m4a') and (Ext <> '.wma')
      and (Ext <> '.ogg') and (Ext <> '.oga') and (Ext <> '.flac') then
      List.Delete(i);
  end;
end;

procedure TPlayer.RandomiseList(var List: TStringList);
var
  i, r: integer;
  Str: string;
begin
  RandSeed := 1;

  for i := 0 to  List.Count -1 do
  begin
    r := Random(List.Count);
    Str := List.Strings[r];
    List.Strings[r] := List.Strings[i];
    List.Strings[i] := Str;
  end;
end;

procedure TPlayer.RescanSearchPath;
begin
  {$ifndef LEGACY}
  if Assigned(FFindFiles) then
  begin
    FFindFiles.Terminate;
    FFindFiles.WaitFor;
    FreeAndNil(FFindFiles);
    FilterAudioFiles(FFindPlaySongList);
    RandomiseList(FFindSongList);
    FSongList.Text := FFindSongList.Text;
    FPathList.Text := FFindPathList.Text;
  end
  else
  begin
    FFindFiles := TFindFilesThread.Create(FFindSongList, FFindPathList, FSearchPath, '.mp3');
    FFindFiles.Resume;
  end;
  {$endif}
end;

function TPlayer.GetFileName(Index: integer; SongList, PathList: TStringList) : string;
var
  PathIndex: integer;
  StartPos: integer;
begin
  Result := '';

  if Index < SongList.Count then
  begin
    Result := SongList.Strings[Index];
    StartPos := Pos(':', Result);

    if StartPos > 0 then
    begin
      PathIndex := StrToIntDef(Copy(Result, 1, StartPos - 1), -1);

      if PathIndex >= 0 then
      begin
        Result := PathList.Strings[PathIndex]
          + Copy(Result, StartPos + 1, Length(Result));

        //DebugLn('GetFileName: Song ' + Result);
      end;
    end;
  end;
end;

function TPlayer.PlaySelection(SearchPaths: string; Random: boolean): integer;
{$ifndef LEGACY}
var
  FindPlaySelection: TFindFilesThread;
{$endif}
begin
  {$ifndef LEGACY}

  FindPlaySelection := TFindFilesThread.Create(FFindPlaySongList, FFindPlayPathList, SearchPaths, '.*');
  FindPlaySelection.Resume;

  while not (FindPlaySelection.Complete) do
  begin
    Sleep(100);
  end;

  FindPlaySelection.Terminate;
  FindPlaySelection.WaitFor;
  FreeAndNil(FindPlaySelection);
  FilterAudioFiles(FFindPlaySongList);
  if Random then RandomiseList(FFindPlaySongList);
  FPlaySongList.Text := FFindPlaySongList.Text;
  FPlayPathList.Text := FFindPlayPathList.Text;
  FPlaylistIndex := -1;

  Result := FFindPlaySongList.Count;
  {$endif}
end;

end.

