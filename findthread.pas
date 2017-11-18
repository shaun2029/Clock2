//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit FindThread;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LCLProc, SyncObjs;

type

  { TFindFilesThread }

  TFindFilesThread = class(TThread)
  private
    FSearchPaths: string;
    FExtension: string;
    FComplete: boolean;
    FCritical: TCriticalSection;
    FFileList, FPathList: TStringList;
    FOnlyDirectories: boolean;
    FDepth: integer;

    procedure FindFiles(FilesList, PathList: TStringList; StartDir, Extension: string);
    function GetComplete: boolean;
    function GetCount: integer;
  public
    constructor Create(FileList, PathList: TStringList; SearchPaths, Extension: string;
      OnlyDirectories: boolean; Depth: integer);
    destructor Destroy; override;

    procedure Execute; override;
  published
    property Complete: boolean read GetComplete;
    property SearchPaths: string read FSearchPaths;
    property Count: integer read GetCount;
  end;

implementation

{ TFindFilesThread }

function TFindFilesThread.GetComplete: boolean;
begin
  Result := FComplete;
end;

function TFindFilesThread.GetCount: integer;
begin
  FCritical.Enter;

  if Assigned(FFileList) then
    Result := FFileList.Count
  else
    Result := FPathList.Count;

  FCritical.Leave;
end;

{ The FileList and PathList are used in combination when producing a file file path.
  This is done to save memory when storing files.}
constructor TFindFilesThread.Create(FileList, PathList: TStringList;
  SearchPaths, Extension: string; OnlyDirectories: boolean; Depth: integer);
begin
  inherited Create(True);
  FSearchPaths := SearchPaths;
  FComplete := False;
  FOnlyDirectories := OnlyDirectories;
  FExtension := Extension;
  FDepth := Depth;

  FFileList := FileList;
  FPathList := PathList;

  if Assigned(FFileList) then
    FFileList.Clear;

  FPathList.Clear;

  FCritical := TCriticalSection.Create;
end;

destructor TFindFilesThread.Destroy;
begin
  FCritical.Free;

  inherited Destroy;
end;

procedure TFindFilesThread.Execute;
var
  Paths: TStringList;
  i: Integer;
begin
  Paths := TStringList.Create;

  Paths.Text := FSearchPaths;

  for i := 0 to Paths.Count - 1 do
  begin
    if DirectoryExists(Paths.Strings[i]) then
    begin
      FindFiles(FFileList, FPathList, Paths.Strings[i], FExtension);
    end;
  end;

  if Assigned(FFileList) then
    FFileList.Sort;

  Paths.Free;
  FComplete := True;
end;

// Recursive procedure to build a list of files
procedure TFindFilesThread.FindFiles(FilesList, PathList: TStringList;
  StartDir, Extension: string);
var
  SR: TSearchRec;
  DirList: TStringList;
  IsFound: Boolean;
  i: integer;
  PathIndex: integer;
  PathIndexStr: string;
  Depth, StartLen: integer;
begin
  if Terminated then Exit;

  StartLen := Length(StartDir);

  if StartDir[StartLen] <> '/' then
  begin
    StartDir := StartDir + '/';
    Inc(StartLen);
  end;

  { Calculate current depth. }
  Depth := -1;
  for i := 1 to StartLen do
  begin
    if StartDir[i] = '/' then
      Inc(Depth);
  end;

  // Build a list of the files in the directory StartDir

  if Assigned(FilesList) then
  begin
    PathIndex := PathList.IndexOf(StartDir);
    if PathIndex < 0 then
    begin
      PathIndex := PathList.Count;
      PathList.Add(StartDir);
    end;

    PathIndexStr := IntToStr(PathIndex);

    IsFound :=
      FindFirst(StartDir+'*' + Extension, faAnyFile-faDirectory, SR) = 0;
    while IsFound do
    begin
      FCritical.Enter;
      FilesList.Add(PathIndexStr + ':' + SR.Name);
      FCritical.Leave;

      IsFound := FindNext(SR) = 0;
    end;

    FindClose(SR);
  end
  else if Extension <> '' then
  begin
    { If an extension is specified, include the directoy if at leat one file of that type exists in it. }

    if Extension[Length(Extension)] <> ';' then
       Extension := Extension + ';';

    IsFound :=
      FindFirst(StartDir+'*.*', faAnyFile-faDirectory, SR) = 0;
    while IsFound do
    begin
      if Pos(Lowercase(ExtractFileExt(SR.Name)) + ';', Lowercase(Extension)) > 0 then
      begin
        PathIndex := PathList.IndexOf(StartDir);
        if PathIndex < 0 then
        begin
          PathList.Add(StartDir);
        end;

        IsFound := False;
      end
      else IsFound := FindNext(SR) = 0;
    end;

    FindClose(SR);
  end
  else
  begin
    PathIndex := PathList.IndexOf(StartDir);
    if PathIndex < 0 then
    begin
      PathList.Add(StartDir);
    end;
  end;

  if (FDepth = 0) or (Depth < FDepth) then
  begin
    // Build a list of subdirectories
    DirList := TStringList.Create;
    IsFound := FindFirst(StartDir+'*', faAnyFile and faDirectory, SR) = 0;
    while IsFound and not Terminated do begin
      if ((SR.Attr and faDirectory) <> 0)
        and (SR.Name <> '') and (SR.Name[1] <> '.') then
      begin
        DirList.Add(StartDir + SR.Name);
      end;
      IsFound := FindNext(SR) = 0;
    end;
    FindClose(SR);

    // Scan the list of subdirectories
    for i := 0 to DirList.Count - 1 do
      FindFiles(FilesList, PathList, DirList[i], Extension);

    DirList.Free;
  end;
end;


end.

