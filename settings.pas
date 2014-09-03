//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit Settings;

{$mode objfpc}{$H+}
//{$DEFINE PICSHOW}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, XMLPropStorage, ComCtrls, ExtCtrls, Buttons, Email, BlowFish, base64;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    BitBtn1: TBitBtn;
    btnStartTimer: TButton;
    btnSelectMusic: TButton;
    btnSelectSleep: TButton;
    btnSelectMeditation: TButton;
    btnPicturePath: TButton;
    btnSendFavorites: TButton;
    cbxFri: TCheckBox;
    cbxMon: TCheckBox;
    cbxSat: TCheckBox;
    cbxSun: TCheckBox;
    cbxThu: TCheckBox;
    cbxTue: TCheckBox;
    cbxWed: TCheckBox;
    cbxEnableReminders: TCheckBox;
    cbxPlayMusic: TCheckBox;
    cbxGetReminders: TCheckBox;
    cbxSilentAlarm: TCheckBox;
    cbxRandomPictures: TCheckBox;
    cbxForceFullscreen: TCheckBox;
    cbxTouchScreen: TCheckBox;
    cbxFavoritesAuto: TCheckBox;
    edtSMTPPassword: TEdit;
    edtEmailAddress: TEdit;
    edtSMTPAccount: TEdit;
    edtPicturePath: TEdit;
    edtServerPort: TEdit;
    edtServerAddress: TEdit;
    edtMeditationPath: TEdit;
    edtMusicPath: TEdit;
    edtHour: TSpinEdit;
    edtMinute: TSpinEdit;
    edtSleepPath: TEdit;
    edtRemHour: TSpinEdit;
    edtRemMinute: TSpinEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label9: TLabel;
    PageControl1: TPageControl;
    dlgSelectDirectoryDialog: TSelectDirectoryDialog;
    seDelay: TSpinEdit;
    stxtTimer: TStaticText;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    TabSheet5: TTabSheet;
    TabSheet6: TTabSheet;
    TabSheet7: TTabSheet;
    tmrSettings: TTimer;
    udTimer: TUpDown;
    XMLPropStorage1: TXMLPropStorage;
    procedure btnPicturePathClick(Sender: TObject);
    procedure btnSelectMeditationClick(Sender: TObject);
    procedure btnSelectMusicClick(Sender: TObject);
    procedure btnSelectSleepClick(Sender: TObject);
    procedure btnSendFavoritesClick(Sender: TObject);
    procedure btnStartTimerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure tmrSettingsTimer(Sender: TObject);
    procedure XMLPropStorage1RestoreProperties(Sender: TObject);
    procedure XMLPropStorage1SaveProperties(Sender: TObject);
  private
    { private declarations }
    FTimerActive: boolean;
    function DecryptString(Key, Value: string): string;
    function EncryptString(Key, Value: string): string;
  public
    { public declarations }
    function SendFavorites(var Error: string): boolean;
  published
    property TimerActive: boolean read FTimerActive write FTimerActive;
  end;

const
  ENCRYPTKEY = '732rbnfiuHJGUTgb9yf98714hclk2ejs9867';

var
  frmSettings: TfrmSettings;

implementation

{$R *.lfm}

{ TfrmSettings }

procedure TfrmSettings.FormKeyPress(Sender: TObject; var Key: char);
begin
  if (Key = #27) or (Key = #13) then
  begin
    XMLPropstorage1.Save;
    Self.Close;
  end;
end;

procedure TfrmSettings.tmrSettingsTimer(Sender: TObject);
begin
  if stxtTimer.Caption = '0' then
  begin
    if btnStartTimer.Caption <> 'Stop Timer' then
      btnStartTimer.Caption := 'Stop Timer';
  end
  else
  begin
    if btnStartTimer.Caption <> 'Start Timer' then
      btnStartTimer.Caption := 'Start Timer';
  end;
end;

function TfrmSettings.EncryptString(Key, Value: string): string;
var
  en: TBlowFishEncryptStream;
  s1: TStringStream;
  temp: String;
  i, Len: integer;
begin
  Value := Trim(Value);
  Len := Length(Value);
  Len := 16 - (Len mod 16) + Len;

  for i := Length(Value) to Len do
  begin
    Value := Value + ' ';
  end;

  s1 := TStringStream.Create('');
  en := TBlowFishEncryptStream.Create(key,s1);
  en.Write(value[1],Length(value));

  Result := s1.DataString;

  // Make string from encoded data
  Result := EncodeStringBase64(Result);

  en.Free;
  s1.Free;
end;

function TfrmSettings.DecryptString(Key, Value: string): string;
var
  de: TBlowFishDeCryptStream;
  s2: TStringStream;
  temp: String;
begin
  // Make encoded data from string
  Value := DecodeStringBase64(Value);

  s2 := TStringStream.Create(Value);
  de := TBlowFishDeCryptStream.Create(key,s2);

  SetLength(temp,s2.Size);
  de.Read(temp[1],s2.Size);

  Result := Trim(PChar(temp));
  de.Free;
  s2.Free;
end;

procedure TfrmSettings.XMLPropStorage1RestoreProperties(Sender: TObject);
var
  Pass: TStringList;
begin
  Pass := TStringList.Create;

  try
    Pass.LoadFromFile(ChangeFileExt(GetAppConfigFile(False), '_data.txt'));
    if Pass.Count > 0 then
      edtSMTPPassword.Text := DecryptString(ENCRYPTKEY, Pass.Strings[0]);
  except
  end;

  Pass.Free;
end;

procedure TfrmSettings.XMLPropStorage1SaveProperties(Sender: TObject);
var
  Pass: TStringList;
begin
  Pass := TStringList.Create;

  try
    Pass.Add(EncryptString(ENCRYPTKEY, edtSMTPPassword.Text));
    Pass.SaveToFile(ChangeFileExt(GetAppConfigFile(False), '_data.txt'));
  except
  end;

  Pass.Free;
end;

procedure TfrmSettings.btnStartTimerClick(Sender: TObject);
begin
  FTimerActive := True;
  Self.Close;
end;

procedure TfrmSettings.btnSelectMusicClick(Sender: TObject);
begin
  if dlgSelectDirectoryDialog.Execute then
     edtMusicPath.Text := dlgSelectDirectoryDialog.Filename;
end;

procedure TfrmSettings.btnSelectMeditationClick(Sender: TObject);
begin
  if dlgSelectDirectoryDialog.Execute then
     edtMeditationPath.Text := dlgSelectDirectoryDialog.Filename;
end;

procedure TfrmSettings.btnPicturePathClick(Sender: TObject);
begin
  if dlgSelectDirectoryDialog.Execute then
     edtPicturePath.Text := dlgSelectDirectoryDialog.Filename;
end;

procedure TfrmSettings.btnSelectSleepClick(Sender: TObject);
begin
  if dlgSelectDirectoryDialog.Execute then
     edtSleepPath.Text := dlgSelectDirectoryDialog.Filename;
end;

function TfrmSettings.SendFavorites(var Error: string): boolean;
var
  Mail: TEmail;
  Titles: TStringList;
  FavFile: string;
begin
  Result := False;
  Error := '';

  Mail := TEmail.Create(edtSMTPAccount.Text, edtSMTPPassword.Text, @Application.ProcessMessages);
  FavFile := ChangeFileExt(GetAppConfigFile(False), '_favorites.txt');
  Titles := TStringList.Create;

  try
    Titles.LoadFromFile(FavFile);

    if Titles.Count > 0 then
    begin
      if Mail.Send(Trim(edtSMTPAccount.Text), Trim(edtEmailAddress.Text), 'Clock2 Favorite Songs', Titles.Text) then
      begin
        Titles.Clear;
        Titles.SaveToFile(FavFile);
        Result := True;
      end
      else Error := Mail.Error;
    end;
  except
    on E: Exception do
    begin
    end;
  end;

  Titles.Free;
  Mail.Free;
end;

procedure TfrmSettings.btnSendFavoritesClick(Sender: TObject);
var
  Titles: TStringList;
  FavFile, Error: string;
begin
  FavFile := ChangeFileExt(GetAppConfigFile(False), '_favorites.txt');
  Titles := TStringList.Create;

  try
    if FileExists(FavFile) then
    begin
      Titles.LoadFromFile(FavFile);

      if Titles.Count > 0 then
      begin
        if SendFavorites(Error) then
          ShowMessage('Successfully sent ' + IntToStr(Titles.Count) + ' favorites.')
        else
          ShowMessage('Error sending favorites.' + LineEnding + 'ERROR: ' + Error);
      end
      else ShowMessage('Nothing to send, select a favorite title.');
    end
    else ShowMessage('Nothing to send, select a favorite title.');
  except
    on E: Exception do
    begin
      ShowMessage('Exception: ' + E.Message);
    end;
  end;

  Titles.Free;
end;

procedure TfrmSettings.FormCreate(Sender: TObject);
begin
  FTimerActive := False;
  PageControl1.TabIndex := 0;

{$IFDEF PICSHOW}
  Self.BorderStyle := bsSingle;
{$ENDIF}
end;

procedure TfrmSettings.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if PageControl1.Focused then
  begin
    if Key = 39 then
    begin
      if PageControl1.ActivePageIndex = PageControl1.PageCount - 1 then
        PageControl1.ActivePageIndex := 0
      else
        PageControl1.ActivePageIndex := PageControl1.ActivePageIndex + 1;

      Key := 0;
    end
    else if Key = 37 then
    begin
      if PageControl1.ActivePageIndex = 0 then
        PageControl1.ActivePageIndex := PageControl1.PageCount - 1
      else
        PageControl1.ActivePageIndex := PageControl1.ActivePageIndex - 1;

      Key := 0;
    end;
  end;
end;


end.

