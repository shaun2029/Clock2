unit FindPicsThread;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil;

type

  { TFindFilesThread }

  TFindPicsThread = class(TThread)
  private
    FSearchPath: string;
    FComplete: boolean;

    procedure FindFiles(FilesList: TStringList; StartDir, FileMask: string);
    function GetComplete: boolean;
  public
    FileList: TStringList;

    constructor Create(SearchPath: string);
    destructor Destroy; override;

    procedure Execute; override;
  published
    property Complete: boolean read GetComplete;
    property SearchPath: string read FSearchPath;
  end;

implementation

{ TFindFilesThread }

function TFindPicsThread.GetComplete: boolean;
begin
//  FCritical.Enter;
  Result := FComplete;
//  FCritical.Leave;
end;

constructor TFindPicsThread.Create(SearchPath: string);
begin
  inherited Create(True);
  FSearchPath := SearchPath;
  FComplete := False;
  FileList := TStringList.Create;
end;

destructor TFindPicsThread.Destroy;
begin
  FileList.Free;
  inherited Destroy;
end;

procedure TFindPicsThread.Execute;
var
  total: integer;
begin
  FindFiles(FileList, FSearchPath, '*.jpg');
  FindFiles(FileList, FSearchPath, '*.JPG');

  FComplete := True;
end;

// Recursive procedure to build a list of files
procedure TFindPicsThread.FindFiles(FilesList: TStringList; StartDir, FileMask: string);
var
  SR: TSearchRec;
  DirList: TStringList;
  IsFound: Boolean;
  i: integer;
begin
  if Terminated then Exit;

  if StartDir[length(StartDir)] <> '/' then
    StartDir := StartDir + '/';

  { Build a list of the files in directory StartDir
     (not the directories!)                         }

  IsFound :=
    FindFirst(StartDir+FileMask, faAnyFile-faDirectory, SR) = 0;
  while IsFound do begin
    FilesList.Add(StartDir + SR.Name);
    IsFound := FindNext(SR) = 0;
  end;
  FindClose(SR);

  // Build a list of subdirectories
  DirList := TStringList.Create;
  IsFound := FindFirst(StartDir+'*', faAnyFile and faDirectory, SR) = 0;
  while IsFound and not Terminated do begin
    if ((SR.Attr and faDirectory) <> 0) and
         (SR.Name[1] <> '.') then
      DirList.Add(StartDir + SR.Name);

    IsFound := FindNext(SR) = 0;

    Sleep(1);
  end;
  FindClose(SR);

  // Scan the list of subdirectories
  for i := 0 to DirList.Count - 1 do
    FindFiles(FilesList, DirList[i], FileMask);

  DirList.Free;
end;


end.

