//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit music;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, MusicPlayer, IniFiles, FindThread, LCLProc, MPD;

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
    FFindFiles: TFindFilesThread;

    FMusicPlayer: TMusicPlayer;
    FConfigFile, FSearchPath: string;
    FSongIndex: integer;
    FPlaylistIndex: integer;
    FState: TPlayerState;
    FStreamTitle: string;
    FStreamURL: string;
    FRandom: boolean;

    procedure FilterAudioFiles(var List: TStringList);
    function GetFileName(Index: integer; SongList, PathList: TStringList): string;
    function GetSongArtist: string;
    function GetSongTitle: string;
    function LoadSettings: boolean;
    function PlayPlaylistSong(Play: TPlayDirection): boolean;
    procedure PlaySong(Play: TPlayDirection);
    procedure RandomiseList(var List: TStringList);
    procedure SaveSettings;
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

    constructor Create(MusicPlayer: TMusicPlayer; ConfigFile,
      SearchPath: string; Random: boolean);

    destructor Destroy; override;
  published
    property SongArtist: string read GetSongArtist;
    property SongTitle: string read GetSongTitle;
    property State: TPlayerState read FState;
    property SearchPath: string read FSearchPath;
    property StreamURL: string read FStreamURL write SetStreamURL;
    property StreamTitle: string read FStreamTitle write FStreamTitle;
    property SongIndex: integer read FSongIndex;
  end;

implementation

{ TPlayer }

constructor TPlayer.Create(MusicPlayer: TMusicPlayer; ConfigFile,
  SearchPath: string; Random: boolean);
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
  FRandom := Random;
  FSearchPath := SearchPath;

  if (FStreamURL = '') and not LoadSettings and DirectoryExists(FSearchPath) then
  begin
    PlaySelection(FSearchPath, FRandom);
    FSongList.Text := FPlaySongList.Text;
    FPathList.Text := FPlayPathList.Text;
    SaveSettings;
  end;
end;

destructor TPlayer.Destroy;
begin
  FMusicPlayer.Stop;

  if Assigned(FFindFiles) then
  begin
    FFindFiles.Terminate;
    FFindFiles.WaitFor;
    FreeAndNil(FFindFiles);
  end;
//Crash???  else SaveSettings;

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

  if Assigned(FFindFiles) then
  begin
    if FFindFiles.Complete then
    begin
      FFindFiles.Terminate;
      FFindFiles.WaitFor;
      FreeAndNil(FFindFiles);
      FilterAudioFiles(FFindSongList);

      if FRandom then
         RandomiseList(FFindSongList);

      FSongList.Text := FFindSongList.Text;
      FPathList.Text := FFindPathList.Text;
    end
    else Result := FFindFiles.Count;
  end
  else if (FState = psPlaying) then
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
        else FMusicPlayer.Play(URL, FStreamURL);
      end;
    end
    else FMusicPlayer.Play(URL, FStreamURL);
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

        if FileExists(Filename) then FMusicPlayer.Play(Filename, '')
        else DebugLn('Music: Failed to find "' + Filename + '"')
      end;
    except
      on E: Exception do
      begin
        DebugLn(Self.ClassName + #9#9 + E.Message);
      end;
    end;

//Crash???    SaveSettings;
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

        if FileExists(Filename) then FMusicPlayer.Play(Filename, '')
        else DebugLn('Music: Failed to find "' + Filename + '"')
      except
        on E: Exception do
        begin
          DebugLn(Self.ClassName + #9#9 + E.Message);
        end;
      end;
    end;
  end;
end;

procedure TPlayer.SaveSettings;
var
  IniFile: TIniFile = nil;
begin
  if FConfigFile = '' then Exit;

  try
    IniFile := TIniFile.Create(FConfigFile);
    IniFile.WriteString('Settings', 'MusicPath', FSearchPath);
    IniFile.WriteInteger('Settings', 'Position', FSongIndex);

    if (FSongList.Count > 0) then
    begin
      FSongList.SaveToFile(ChangeFileExt(FConfigFile, '.pl'));
      FPathList.SaveToFile(ChangeFileExt(FConfigFile, '.plp'));
    end;
    IniFile.Free;
  except
    on E: Exception do
       writeln('EXCEPTION: TPlayer.SaveSettings IniFile - ' + E.ClassName + #13#10 + E.Message);
  end;
end;

procedure TPlayer.SetStreamURL(AValue: string);
begin
  FStreamURL := AValue;
  FState := psStopped;
end;

function TPlayer.LoadSettings: boolean;
var
  IniFile: TIniFile;
begin
  Result := False;
  if FConfigFile = '' then Exit;

  try
    IniFile := TIniFile.Create(FConfigFile);

    if FSearchPath = '' then
      FSearchPath := IniFile.ReadString('Settings', 'MusicPath', '~/Music');

    FSongIndex := IniFile.ReadInteger('Settings', 'Position', 0);

    if FileExists(ChangeFileExt(FConfigFile, '.pl'))
      and FileExists(ChangeFileExt(FConfigFile, '.plp')) then
    begin
      FSongList.LoadFromFile(ChangeFileExt(FConfigFile, '.pl'));
      FPathList.LoadFromFile(ChangeFileExt(FConfigFile, '.plp'));
      Result := True;
    end;

    IniFile.Free;
  except
    on E: Exception do
       writeln('EXCEPTION: TPlayer.LoadSettings IniFile - ' + E.ClassName + #13#10 + E.Message);
  end;
end;

function TPlayer.GetSongArtist: string;
begin
  if FStreamURL <> '' then Result := StreamTitle
  else Result := FMusicPlayer.SongArtist;
end;

function TPlayer.GetSongTitle: string;
var
  URL, Host, Port: string;
begin
  if FStreamURL <> '' then
    Result := FMusicPlayer.RadioTitle
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
  if Assigned(FFindFiles) then
  begin
    FFindFiles.Terminate;
    FFindFiles.WaitFor;
    FreeAndNil(FFindFiles);
  end
  else
  begin
    FFindFiles := TFindFilesThread.Create(FFindSongList, FFindPathList, FSearchPath, '.*', False, 0);
    FFindFiles.Resume;
  end;
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
var
  FindPlaySelection: TFindFilesThread;
begin
  FindPlaySelection := TFindFilesThread.Create(FFindPlaySongList, FFindPlayPathList, SearchPaths, '.*', False, 0);
  FindPlaySelection.Resume;

  while not (FindPlaySelection.Complete) do
  begin
    Sleep(100);
  end;

  FindPlaySelection.Terminate;
  FindPlaySelection.WaitFor;
  FreeAndNil(FindPlaySelection);
  FilterAudioFiles(FFindPlaySongList);
  if Random then RandomiseList(FFindPlaySongList)
  else FFindPlaySongList.Sort;
  FPlaySongList.Text := FFindPlaySongList.Text;
  FPlayPathList.Text := FFindPlayPathList.Text;
  FPlaylistIndex := -1;
  FRandom := Random;

  Result := FFindPlaySongList.Count;
end;

end.

