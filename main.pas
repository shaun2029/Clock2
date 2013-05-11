unit Main;

{$mode delphi}
//{$DEFINE DEBUG}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, IniFiles, Process, simpleipc, FindPicsThread, Settings,
  ClockMain, ClockSettings;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    imgDisplay: TImage;
    lblLoading: TLabel;
    tmrShowClock: TTimer;
    tmrEvent: TTimer;
    procedure FormClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure tmrEventTimer(Sender: TObject);
    procedure tmrShowClockTimer(Sender: TObject);
  private
    { private declarations }
    EventDelay: TDateTime;
    LastEvent: TDateTime;
    PictureIndex: integer;
    PictureList: TStringList;
    State: integer;
    SearchPath: string;
    FindFiles: TFindPicsThread;
    RandomPictures: boolean;

    procedure ReminderCallback;
    procedure ChangeState;
    procedure LoadSettings;
    procedure RandomiseList(var List: TStringList);
    procedure SaveSettings;
    procedure Settings;
    procedure ShowClock;
    function ShowPicture: boolean;
    procedure Startup;
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

implementation

procedure TfrmMain.SaveSettings;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(GetAppConfigFile(False));

  try
    IniFile.WriteString('Settings', 'PicturePath', SearchPath);
    IniFile.WriteTime('Settings', 'Delay', EventDelay);
    IniFile.WriteInteger('Settings', 'Position', PictureIndex);
    IniFile.WriteBool('Settings', 'Random', RandomPictures);
    if Assigned(PictureList) and (PictureList.Count > 0) then
      PictureList.SaveToFile(ChangeFileExt(GetAppConfigFile(False), '.pl'));
  finally
    IniFile.Free;
  end;
end;

procedure TfrmMain.LoadSettings;
var
  IniFile: TIniFile;
begin
  IniFile := TIniFile.Create(GetAppConfigFile(False));

  try
    SearchPath := IniFile.ReadString('Settings', 'PicturePath', '');
    RandomPictures := IniFile.ReadBool('Settings', 'Random', RandomPictures);

    EventDelay := IniFile.ReadTime('Settings', 'Delay', EventDelay);
    if EventDelay = 0 then EventDelay := EncodeTime(0, 0, 10, 0);

    PictureIndex := IniFile.ReadInteger('Settings', 'Position', 0);

    if FileExists(ChangeFileExt(GetAppConfigFile(False), '.pl')) then
    begin
      if not Assigned(PictureList) then PictureList := TStringList.Create;
      PictureList.LoadFromFile(ChangeFileExt(GetAppConfigFile(False), '.pl'));
    end;
  finally
    IniFile.Free;
  end;
end;

procedure TfrmMain.Settings;
var
  frmSettings: TfrmSettings;
  h, m, s, ms: word;
begin
  tmrEvent.Enabled := False;

  frmSettings := TfrmSettings.Create(Self);

  frmSettings.edtPicturePath.Text := SearchPath;
  DecodeTime(EventDelay, h, m, s, ms);
  s := (m * 60) + s;
  frmSettings.seDelay.Value := s mod 600;

  Self.Visible := False;
  frmSettings.ShowModal;
  Self.Visible := True;
  SearchPath := frmSettings.edtPicturePath.Text;

  m := frmSettings.seDelay.Value;
  s := m mod 60;
  m := m div 60;
  EventDelay := EncodeTime(0, m, s, 0);

  frmSettings.Free;

  tmrEvent.Enabled := True;
end;

procedure TfrmMain.Startup;
begin
  if SearchPath <> 'off' then
  begin
    lblLoading.Visible := True;
    State := 1;
  end
  else State := 0;

  if Assigned(FindFiles) then
  begin
    if SearchPath <> FindFiles.SearchPath then
    begin
      FindFiles.Terminate;
      FindFiles.WaitFor;
      FindFiles.Free;

      FindFiles := TFindPicsThread.Create(SearchPath);
      FindFiles.Resume;
    end;
  end
  else
  begin
    FindFiles := TFindPicsThread.Create(SearchPath);
    FindFiles.Resume;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FindFiles := nil;
  PictureList := nil;
{$IFNDEF DEBUG}
  Self.Width := Screen.Width;
  Self.Height := Screen.Height;
  Self.WindowState := wsMaximized;
{$ENDIF}
  Mouse.CursorPos := Point(Screen.Width, Screen.Height);

  RandomPictures := True;

  State := 0;

  LoadSettings;
  if SearchPath = '' then Settings;

  if SearchPath = '' then
  begin
    Halt;
  end;

  Startup;

  frmClockMain := TfrmClockMain.Create(Self);
  frmClockMain.ReminderCallback := ReminderCallback;

  frmClockSettings := TfrmClockSettings.Create(Self);

  tmrEvent.Enabled := True;
  tmrShowClock.Enabled := True;
end;

procedure TfrmMain.ShowClock;
var
  TimerState: boolean;
begin
  Self.Hide;
  TimerState := tmrEvent.Enabled;
  tmrEvent.Enabled := False;
  frmClockMain.ShowModal;
  tmrEvent.Enabled := TimerState;
  Self.Show
end;

procedure TfrmMain.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
var
  TimerState: boolean;
begin
  if Key <> 67 then
    ChangeState
  else
  begin
    ShowClock;
  end;
end;

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveSettings;

  if Assigned(PictureList) then FreeAndNil(PictureList);

  if Assigned(FindFiles) then
  begin
    FindFiles.Terminate;
    FindFiles.WaitFor;
    FindFiles.Free;
  end;

  frmClockMain.Free;
  frmClockSettings.Free;
end;

procedure TfrmMain.FormClick(Sender: TObject);
begin
  if (Mouse.CursorPos.x > Screen.Width * 0.8)
    and (Mouse.CursorPos.Y < Screen.Height * 0.2) then
  begin
    Settings;
    Mouse.CursorPos := Point(Screen.Width, Screen.Height);
  end
  else if (Mouse.CursorPos.x > Screen.Width * 0.2) then
  begin
    ShowClock;
    Mouse.CursorPos := Point(Screen.Width, Screen.Height);
  end
  else if SearchPath = 'off' then
  begin
    // If searchpath is off then skip change state
    Close;
    Exit;
  end
  else ChangeState;
end;

function TfrmMain.ShowPicture: boolean;
var
  Dest: string;
begin
  Result := False;

  if PictureIndex < 0 then PictureIndex := PictureList.Count -1;
  if PictureIndex >= PictureList.Count then PictureIndex := 0;

  try
    if FileExists(PictureList.Strings[PictureIndex]) then
      imgDisplay.Picture.LoadFromFile(PictureList.Strings[PictureIndex]);

    Result := True;
  except
    on E: Exception do
    begin
    end;
  end;

  Inc(PictureIndex , 1);

  if PictureIndex mod 20 = 0 then
    SaveSettings;
end;

procedure TfrmMain.tmrEventTimer(Sender: TObject);
begin
  tmrEvent.Enabled := False;
  tmrEvent.Interval := 500;

  case State of
    1:
    begin
      if LastEvent + EventDelay < Now then
      begin
        if Assigned(FindFiles) then
        begin
          if FindFiles.Complete then
          begin
            if not Assigned(PictureList) then
              PictureList := TStringList.Create;

            PictureList.Text := FindFiles.FileList.Text;
            lblLoading.Visible := False;
            if RandomPictures then RandomiseList(PictureList);

            FindFiles.Terminate;
            FindFiles.WaitFor;
            FreeAndNil(FindFiles);
          end;
        end;

        if Assigned(PictureList) then
        begin
          if not imgDisplay.Visible then
            imgDisplay.Visible := True;

          if ShowPicture then
            lblLoading.Visible := False;

          LastEvent := Now;
        end;
      end;
    end;
    else
      if imgDisplay.Visible then
        imgDisplay.Visible := False;
  end;

  if State > 1 then
    Close
  else tmrEvent.Enabled := True;
end;

procedure TfrmMain.tmrShowClockTimer(Sender: TObject);
begin
  tmrShowClock.Enabled := False;
  ShowClock;
  Mouse.CursorPos := Point(Screen.Width, Screen.Height);
end;

procedure TfrmMain.ReminderCallback;
begin
  if Self.Visible then
  begin
    tmrShowClock.Enabled := True;
  end;
end;

procedure TfrmMain.ChangeState;
begin
  tmrEvent.Enabled := False;

  // Blank path, skip showing images
  if SearchPath = '/tmp' then Inc(State, 2)
  else Inc(State);

  case State of
    1:
    begin
      lblLoading.Visible := True;
    end;
    else
    begin
      lblLoading.Visible := False;
    end;
  end;

  Mouse.CursorPos := Point(Screen.Width, Screen.Height);

  tmrEvent.Enabled := True;
end;

procedure TfrmMain.RandomiseList(var List: TStringList);
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


initialization
  {$I main.lrs}

end.

