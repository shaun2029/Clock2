unit Settings;

{$mode objfpc}{$H+}
//{$DEFINE PICSHOW}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Spin, XMLPropStorage, ComCtrls, ExtCtrls, Buttons;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    BitBtn1: TBitBtn;
    btnStartTimer: TButton;
    btnSelectMusic: TButton;
    btnSelectSleep: TButton;
    btnSelectMeditation: TButton;
    btnPicturePath: TButton;
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
    edtPicturePath: TEdit;
    edtServerPort: TEdit;
    edtServerAddress: TEdit;
    edtMeditationPath: TEdit;
    edtMusicPath: TEdit;
    edtHour: TSpinEdit;
    edtLocation: TEdit;
    edtLocation1: TEdit;
    edtLocation2: TEdit;
    edtLocation3: TEdit;
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
    Label7: TLabel;
    Label8: TLabel;
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
    procedure btnStartTimerClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure tmrSettingsTimer(Sender: TObject);
  private
    { private declarations }
    FTimerActive: boolean;
  public
    { public declarations }
  published
    property TimerActive: boolean read FTimerActive write FTimerActive;
  end;

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

