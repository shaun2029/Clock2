//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit PlaylistCreator;

{$mode Delphi}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, IniFiles, TouchList;

type

  TPathList = record
    Path: string;
    List: string;
  end;

  { TfrmPlaylist }

  TfrmPlaylist = class(TForm)
    BitBtn1: TBitBtn;
    btnAddAll: TBitBtn;
    btnBack: TBitBtn;
    btnOk: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    Panel1: TPanel;
    Panel2: TPanel;
    Splitter1: TSplitter;
    tbxRandom: TToggleBox;
    procedure btnAddAllClick(Sender: TObject);
    procedure btnBackClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
  private
    { private declarations }

    FLevel, FMinLevel: integer;
    FPath: TStringList;
    FPathList: TStringList;
    FStartPath: string;
    lstDisplay: TTouchList;
    FItemIndex: integer;
    FPathLists: array of TPathList;

    procedure Display(Next: boolean);

    function GetLevel(Level: integer; const Path: string; out Data: string): boolean;
    function GetMusicPath: string;
    function GetRandomOrder: boolean;
    procedure OnItemSelected(Sender: TObject; Index: integer);
    procedure OnDeleteSelected(Sender: TObject; Index: integer);
    function PathCompare(CurrPath, NewPath: string): boolean;
  public
    { public declarations }
    lstSelected: TTouchList;

    procedure LoadSongs(const ConfigFile, StartPath: string);
  published
    property Random: boolean read GetRandomOrder;
  end;

var
  frmPlaylist: TfrmPlaylist;

implementation

{$R *.lfm}

{ TfrmPlaylist }

procedure TfrmPlaylist.FormCreate(Sender: TObject);
begin
  FPathList := TStringList.Create;
  FPath := TStringList.Create;
  SetLength(FPathLists, 0);

  lstDisplay := TTouchList.Create(Panel1);
  lstDisplay.Parent := Panel1;
  lstDisplay.OnItemSelected := OnItemSelected;
  lstDisplay.Font.Size := 14;

  lstSelected := TTouchList.Create(Panel2);
  lstSelected.Parent := Panel2;
  lstSelected.Font.Size := 14;
  lstSelected.TrimItems := True;
  lstSelected.OnItemSelected := OnDeleteSelected;

  FItemIndex := -1;
end;

procedure TfrmPlaylist.btnBackClick(Sender: TObject);
begin
  Display(False);
end;

procedure TfrmPlaylist.btnAddAllClick(Sender: TObject);
var
  Selected: integer;
  MusicPath: String;
  i: Integer;
begin
  btnAddAll.Enabled := False;
  Selected := lstDisplay.ItemIndex;

  for i := 0 to lstDisplay.Items.Count -1 do
  begin
    lstDisplay.ItemIndex := i;

    MusicPath := GetMusicPath;
    if lstSelected.Items.IndexOf(MusicPath) < 0 then
      lstSelected.Items.Add(MusicPath);
  end;

  lstDisplay.ItemIndex := Selected;
  btnAddAll.Enabled := True;
end;

procedure TfrmPlaylist.FormDestroy(Sender: TObject);
begin
  SetLength(FPathLists, 0);
  lstDisplay.Free;
  lstSelected.Free;
  FPathList.Free;
  FPath.Free;
end;

procedure TfrmPlaylist.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = 27 then Self.ModalResult := mrCancel
  else if Key = 13 then
  begin
    Self.ModalResult := mrOk;
  end
  else if Key = 39 then
  begin
    Display(True);
  end
  else if Key = 37 then
  begin
    Display(False);
  end;
end;

procedure TfrmPlaylist.OnItemSelected(Sender: TObject; Index: integer);
begin
  Display(True);
end;

procedure TfrmPlaylist.OnDeleteSelected(Sender: TObject; Index: integer);
begin
  if Sender is TTouchList then
  begin
    TTouchList(Sender).Items.Delete(Index);
  end;
end;

{ Claculate what needs to be displayed for file navigation.
  If Next is true display the next branch else display the previous. }
procedure TfrmPlaylist.Display(Next: boolean);
var
  i, Level, Selected: Integer;
  Found, Cached: boolean;
  Data, SelectedStr: string;
  CurrPath: string;
  SelectList: TStringList;
  MusicPath: string;
begin
  FItemIndex := -1;

  Found := False;
  SelectList := TStringList.Create;
  Selected := lstDisplay.ItemIndex;

  if (Selected < 0) then Selected := 0;

  if (Selected < lstDisplay.Items.Count) then
    SelectedStr := lstDisplay.Items.Strings[Selected]
  else
    Selectedstr := '';

  if Next then
  begin
    Level := FLevel + 1
  end
  else
  begin
    if Flevel <= FMinLevel then Level := FMinLevel
    else Level := FLevel - 1;
  end;

  CurrPath := FStartPath;

  if Next then
  begin
    for i := 0 to FPath.Count -1 do
    begin
      CurrPath := CurrPath + FPath.Strings[i] + '/';
    end;

    CurrPath := CurrPath + Selectedstr;
  end
  else
  begin
    for i := 0 to FPath.Count -2 do
    begin
      CurrPath := CurrPath + FPath.Strings[i] + '/';
    end;
  end;

  // Use cached path list if avaliable
  Cached := False;

  for i := 0 to Length(FPathLists) - 1 do
  begin
    // Search the cache.
    if FPathLists[i].Path = CurrPath then
    begin
      SelectList.Text := FPathLists[i].List;
      Cached := True;
      Break;
    end;
  end;

  if not Cached then
  begin
    // Search path list for selected paths.
    for i := 0 to FPathList.Count -1 do
    begin
      if PathCompare(IncludeTrailingPathDelimiter(CurrPath), FPathList.Strings[i])
        and GetLevel(Level, FPathList.Strings[i], Data) then
      begin
        if not Found then
        begin
          // Track if any paths are found.
          Found := True;
        end;

        // Add path if not duplicating existing entry.
        if SelectList.IndexOf(Data) < 0 then
          SelectList.Add(Data);
      end;
    end;

    if Found then
    begin
      // Add list to cache
      SetLength(FPathLists, Length(FPathLists) + 1);
      FPathLists[Length(FPathLists) - 1].Path := CurrPath;
      FPathLists[Length(FPathLists) - 1].List := SelectList.Text;
    end;
  end;

  if Found or Cached then
  begin
    lstDisplay.Items.Text := SelectList.Text;

    if Next then
    begin
      lstDisplay.ItemIndex := 0;

      if SelectedStr <> '' then
        FPath.Append(SelectedStr);
    end
    else
    begin
      lstDisplay.ItemIndex := 0;

      if FPath.Count > 0 then
      begin
        // Re-select the previously selected item.
        Selected := lstDisplay.Items.IndexOf(FPath.Strings[FPath.Count - 1]);

        if Selected >= 0 then
        begin
          lstDisplay.ItemIndex := Selected;
        end
      end;

      if FPath.Count > 0 then
        FPath.Delete(FPath.Count - 1)
    end;

    FLevel := Level;
  end
  else if Next then
  begin
    // Reached final level, make selection
    MusicPath := GetMusicPath;
    if lstSelected.Items.IndexOf(MusicPath) < 0 then
      lstSelected.Items.Add(MusicPath);
  end;

  lstDisplay.Items.Sorted := True;
  SelectList.Free;
end;

// Quickly compare paths to find if NewPath is contained within CurrPath.
function TfrmPlaylist.PathCompare(CurrPath, NewPath: string): boolean; inline;
var
  CurrLen, NewLen: integer;
  PCurrPath, PNewPath: PDWord;
  i: Integer;
begin
  Result := False;

  CurrLen := Length(CurrPath);
  if CurrLen > Length(NewPath) then Exit;

  NewLen := CurrLen - (CurrLen mod 4);

  PCurrPath := @CurrPath[1];
  PNewPath := @NewPath[1];

  // Process DWords
  for i := 0 to (NewLen div 4) - 1 do
    if PCurrPath[i] <> PNewPath[i] then Exit;

  // Process remaining single chars
  for i := NewLen+1 to CurrLen do
    if CurrPath[i] <> NewPath[i] then Exit;

  Result := True;
end;

// Load the list of paths from a playlist path file.
procedure TfrmPlaylist.LoadSongs(const ConfigFile, StartPath: string);
var
  i: Integer;
begin
  try
    if FileExists(ChangeFileExt(ConfigFile, '.plp')) then
    begin
      FPathList.LoadFromFile(ChangeFileExt(ConfigFile, '.plp'));
      FPathList.Sort;
    end;
  finally
  end;

  FStartPath := StartPath;
  if FStartPath[Length(FStartPath)] <> '/' then
    FStartPath := FStartPath + '/';

  FMinLevel := 0;

  for i := 2 to Length(FStartPath) do
  begin
    if FStartPath[i] = '/' then
      Inc(FMinLevel);
  end;

  FPath.Clear;
  lstDisplay.Items.Clear;

  FLevel := FMinLevel - 1;
  Display(True);
end;

// Get the piece of the path indicated by level.
function TfrmPlaylist.GetLevel(Level: integer; const Path: string; out Data: string): boolean;
var
  SPos, EPos, Lev: integer;
  Found: boolean;
  i: Integer;
begin
  Result := False;
  SPos := 0;
  EPos := 0;
  Data := '';

  Found := False;
  Lev := 0;

  for i := 1 to Length(Path) do
  begin
    if Path[i] = '/' then
    begin
      if Lev = Level then
      begin
        Found := True;
        SPos := i + 1;
        Break;
      end
      else Inc(Lev);
    end;
  end;

  if not Found then Exit;

  Found := False;
  Lev := 0;

  for i := SPos to Length(Path) do
  begin
    if (Path[i] = '/') then
    begin
      Found := True;
      EPos := i;
      Break;
    end;
  end;

  if SPos > 1 then
  begin
    if Epos > SPos then
    begin
      Data := Copy(Path, SPos, EPos - SPos);
      Result := True;
    end;
  end;
end;

// Generate the complete path to the selected item.
function TfrmPlaylist.GetMusicPath: string;
var
  i: integer;
  Selected: LongInt;
begin
  Result := FStartPath;

  for i := 0 to FPath.Count -1 do
  begin
    Result := Result + FPath.Strings[i] + '/';
  end;

  Selected := lstDisplay.ItemIndex;

  if Selected >= 0 then
  begin
    if (Selected < lstDisplay.Items.Count) then
    begin
      Result := Result + lstDisplay.Items.Strings[Selected] + '/';
    end;
  end;
end;

function TfrmPlaylist.GetRandomOrder: boolean;
begin
  Result := tbxRandom.Checked;
end;


end.

