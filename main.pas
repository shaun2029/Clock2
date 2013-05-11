unit main;

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
    tmrEvent: TTimer;
    tmrShowClock: TTimer;
    procedure FormClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure tmrEventTimer(Sender: TObject);
    procedure tmrShowClockTimer(Sender: TObject);
  private
    { private declarations }
    FEventDelay: TDateTime;
    FLastEvent: TDateTime;
    FPictureIndex: integer;
    FPictureList: TStringList;
    FState: integer;
    FSearchPath: string;
    FFindFiles: TFindPicsThread;
    FRandomPictures: boolean;

    procedure ReminderCallback;
    procedure LoadSettings;
    procedure RandomiseList(var List: TStringList);
    procedure SaveSettings;
    procedure Settings;
    procedure ShowClock;
    function ShowPicture: boolean;
    procedure Startup;
    procedure WaitForMedia;
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
    IniFile.WriteString('Settings', 'PicturePath', FSearchPath);
    IniFile.WriteTime('Settings', 'Delay', FEventDelay);
    IniFile.WriteInteger('Settings', 'Position', FPictureIndex);
    IniFile.WriteBool('Settings', 'Random', FRandomPictures);
    if Assigned(FPictureList) and (FPictureList.Count > 0) then
      FPictureList.SaveToFile(ChangeFileExt(GetAppConfigFile(False), '.pl'));
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
    FSearchPath := IniFile.ReadString('Settings', 'PicturePath', '');
    FRandomPictures := IniFile.ReadBool('Settings', 'Random', FRandomPictures);

    FEventDelay := IniFile.ReadTime('Settings', 'Delay', FEventDelay);
    if FEventDelay = 0 then FEventDelay := EncodeTime(0, 0, 10, 0);

    FPictureIndex := IniFile.ReadInteger('Settings', 'Position', 0);

    if FileExists(ChangeFileExt(GetAppConfigFile(False), '.pl')) then
    begin
      if not Assigned(FPictureList) then FPictureList := TStringList.Create;
      FPictureList.LoadFromFile(ChangeFileExt(GetAppConfigFile(False), '.pl'));
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

  frmSettings.edtPicturePath.Text := FSearchPath;
  DecodeTime(FEventDelay, h, m, s, ms);
  s := (m * 60) + s;
  frmSettings.seDelay.Value := s mod 600;

  Self.Visible := False;
  frmSettings.ShowModal;
  Self.Visible := True;
  FSearchPath := frmSettings.edtPicturePath.Text;

  m := frmSettings.seDelay.Value;
  s := m mod 60;
  m := m div 60;
  FEventDelay := EncodeTime(0, m, s, 0);

  frmSettings.Free;

  tmrEvent.Enabled := True;
end;

procedure TfrmMain.Startup;
begin
  if FSearchPath <> 'off' then
  begin
    lblLoading.Visible := True;
    FState := 1;
  end
  else FState := 0;

  if Assigned(FFindFiles) then
  begin
    if FSearchPath <> FFindFiles.SearchPath then
    begin
      FFindFiles.Terminate;
      FFindFiles.WaitFor;
      FFindFiles.Free;

      FFindFiles := TFindPicsThread.Create(FSearchPath);
      FFindFiles.Resume;
    end;
  end
  else
  begin
    FFindFiles := TFindPicsThread.Create(FSearchPath);
    FFindFiles.Resume;
  end;
end;

procedure TfrmMain.FormCreate(Sender: TObject);
begin
  FFindFiles := nil;
  FPictureList := nil;
{$IFNDEF DEBUG}
  Self.Width := Screen.Width;
  Self.Height := Screen.Height;
  Self.WindowState := wsMaximized;
{$ENDIF}
  Mouse.CursorPos := Point(Screen.Width, Screen.Height);

  FRandomPictures := True;

  FState := 0;

  LoadSettings;
  if FSearchPath = '' then Settings;

  WaitForMedia;
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

procedure TfrmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  SaveSettings;

  if Assigned(FPictureList) then FreeAndNil(FPictureList);

  if Assigned(FFindFiles) then
  begin
    FFindFiles.Terminate;
    FFindFiles.WaitFor;
    FFindFiles.Free;
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
  else
  begin
    Close;
    Exit;
  end;
end;

function TfrmMain.ShowPicture: boolean;
var
  Dest: string;
begin
  Result := False;

  if FPictureIndex < 0 then FPictureIndex := FPictureList.Count -1;
  if FPictureIndex >= FPictureList.Count then FPictureIndex := 0;

  try
    if FileExists(FPictureList.Strings[FPictureIndex]) then
      imgDisplay.Picture.LoadFromFile(FPictureList.Strings[FPictureIndex]);

    Result := True;
  except
    on E: Exception do
    begin
    end;
  end;

  Inc(FPictureIndex , 1);

  if FPictureIndex mod 20 = 0 then
    SaveSettings;
end;

procedure TfrmMain.tmrEventTimer(Sender: TObject);
begin
  tmrEvent.Enabled := False;
  tmrEvent.Interval := 500;

  case FState of
    1:
    begin
      if FLastEvent + FEventDelay < Now then
      begin
        if Assigned(FFindFiles) then
        begin
          if FFindFiles.Complete then
          begin
            if not Assigned(FPictureList) then
              FPictureList := TStringList.Create;

            FPictureList.Text := FFindFiles.FileList.Text;
            lblLoading.Visible := False;
            if FRandomPictures then RandomiseList(FPictureList);

            FFindFiles.Terminate;
            FFindFiles.WaitFor;
            FreeAndNil(FFindFiles);
          end;
        end;

        if Assigned(FPictureList) then
        begin
          if not imgDisplay.Visible then
            imgDisplay.Visible := True;

          if ShowPicture then
            lblLoading.Visible := False;

          FLastEvent := Now;
        end;
      end;
    end;
    else
      if imgDisplay.Visible then
        imgDisplay.Visible := False;
  end;

  if FState > 1 then Close
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

procedure TfrmMain.WaitForMedia;
var
  Timeout: TDateTime;
begin
  if not DirectoryExists(FSearchPath) then
  begin
    Timeout := Now + EncodeTime(0,0,30,0);

    repeat
      Sleep(1000);
    until (Timeout > Now) or DirectoryExists(FSearchPath);
  end;
end;

initialization
  {$I main.lrs}

end.

