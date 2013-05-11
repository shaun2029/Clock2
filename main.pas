unit main;

{$mode delphi}
//{$DEFINE DEBUG}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  ExtCtrls, StdCtrls, IniFiles, Process, simpleipc, FindPicsThread,
  ClockMain, ClockSettings;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    imgDisplay: TImage;
    lblLoading: TLabel;
    tmrEvent: TTimer;
    tmrShowClock: TTimer;
    procedure FormActivate(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure tmrEventTimer(Sender: TObject);
    procedure tmrShowClockTimer(Sender: TObject);
  private
    { private declarations }
    FEventDelay: TDateTime;
    FLastEvent: TDateTime;
    FPictureIndex: integer;
    FPictureList: TStringList;
    FSearchPath: string;
    FFindFiles: TFindPicsThread;
    FRandomPictures: boolean;

    procedure ReminderCallback;
    procedure LoadSettings;
    procedure RandomiseList(var List: TStringList);
    procedure SaveSettings;
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
    IniFile.WriteInteger('Settings', 'Position', FPictureIndex);
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
    FSearchPath := frmClockSettings.edtPicturePath.Text;
    FRandomPictures := frmClockSettings.cbxRandomPictures.Checked;

    if frmClockSettings.seDelay.Value < 1 then FEventDelay := EncodeTime(0, 0, 10, 0)
    else FEventDelay := EncodeTime(0, 0, frmClockSettings.seDelay.Value, 0);

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

procedure TfrmMain.Startup;
begin
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
  FLastEvent := 0;
  FFindFiles := nil;
  FPictureList := nil;
{$IFNDEF DEBUG}
  Self.Width := Screen.Width;
  Self.Height := Screen.Height;
  Self.WindowState := wsMaximized;
{$ENDIF}
  Mouse.CursorPos := Point(Screen.Width, Screen.Height);

  FRandomPictures := True;

  frmClockSettings := TfrmClockSettings.Create(Self);

  LoadSettings;

  frmClockMain := TfrmClockMain.Create(Self);
  frmClockMain.ReminderCallback := ReminderCallback;

  frmClockMain.WaitForMedia(FSearchPath);
  Startup;

  tmrEvent.Enabled := True;
  tmrShowClock.Enabled := True;
end;

procedure TfrmMain.FormShow(Sender: TObject);
begin
  LoadSettings;
  Startup;
end;

procedure TfrmMain.ShowClock;
var
  TimerState: boolean;
begin
  Self.Hide;
  tmrEvent.Enabled := False;
  frmClockMain.ShowModal;
  tmrEvent.Enabled := True;

  if frmClockMain.Closing then Close
  else Self.Show
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

procedure TfrmMain.FormActivate(Sender: TObject);
begin

end;

procedure TfrmMain.FormClick(Sender: TObject);
begin
  ShowClock;
  Mouse.CursorPos := Point(Screen.Width, Screen.Height);
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

  tmrEvent.Enabled := True;
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

initialization
  {$I main.lrs}

end.

