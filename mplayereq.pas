// Uses Mplayer commndline option to set EQ levels
// http://www.thedigitalmachine.net/alsaequal.html

unit mplayereq;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, StdCtrls, process;

const
  EqBands : array [0..9] of string = ('31.25Hz', '62.50Hz', '125Hz', '250Hz', '500Hz',
  '1kHz', '2kHz', '4kHz', '8kHz', '16kHz');

type
  TMplayerEQ = array of integer;

  { TfrmMplayerEQ }

  TfrmMplayerEQ = class(TForm)
    BtnDown2: TBitBtn;
    BtnDown3: TBitBtn;
    BtnDown1: TBitBtn;
    BtnDown4: TBitBtn;
    BtnDown5: TBitBtn;
    btnApply: TSpeedButton;
    btnUp2: TBitBtn;
    btnUp3: TBitBtn;
    btnUp1: TBitBtn;
    btnUp4: TBitBtn;
    btnUp5: TBitBtn;
    Label1: TLabel;
    Label3: TLabel;
    btnReset: TSpeedButton;
    Label5: TLabel;
    tbarSetting1: TTrackBar;
    tbarSetting2: TTrackBar;
    tbarSetting3: TTrackBar;
    tbarSetting4: TTrackBar;
    tbarSetting5: TTrackBar;
    procedure BtnDown1Click(Sender: TObject);
    procedure btnApplyClick(Sender: TObject);
    procedure btnUp1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
    FEqLevels: TMplayerEQ;
    Setting: array [0..4] of TTrackBar;
    Down, Up: array [0..4] of TBitBtn;
    procedure ReadEqSettings;
    procedure WriteEqSettings;
  public
    { public declarations }
  published
    property Levels: TMplayerEQ read FEqLevels write FEqLevels;
  end;

const
  EqChannelName: array [0..9] of string = ('01. 31 Hz', '02. 63 Hz', '03. 125 Hz', '04. 250 Hz',
    '05. 500 Hz', '06. 1 kHz', '07. 2 kHz', '08. 4 kHz', '09. 8 kHz', '10. 16 kHz');

var
  frmMplayerEQ: TfrmMplayerEQ;

implementation

{$R *.lfm}

{ TfrmMplayerEQ }

procedure TfrmMplayerEQ.btnUp1Click(Sender: TObject);
begin
  if Setting[TBitBtn(Sender).Tag].Position < 6 then
  begin
    Setting[TBitBtn(Sender).Tag].Position := Setting[TBitBtn(Sender).Tag].Position + 1;
  end;
end;

procedure TfrmMplayerEQ.BtnDown1Click(Sender: TObject);
begin
  if Setting[TBitBtn(Sender).Tag].Position > -6 then
  begin
    Setting[TBitBtn(Sender).Tag].Position := Setting[TBitBtn(Sender).Tag].Position - 1;
  end;
end;

procedure TfrmMplayerEQ.btnApplyClick(Sender: TObject);
begin
  WriteEqSettings;
  Close;
end;

procedure TfrmMplayerEQ.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  Down[0] := BtnDown1;
  Down[1] := BtnDown2;
  Down[2] := BtnDown3;
  Down[3] := BtnDown4;
  Down[4] := BtnDown5;

  Up[0] := btnUp1;
  Up[1] := btnUp2;
  Up[2] := btnUp3;
  Up[3] := btnUp4;
  Up[4] := btnUp5;

  Setting[0] := tbarSetting1;
  Setting[1] := tbarSetting2;
  Setting[2] := tbarSetting3;
  Setting[3] := tbarSetting4;
  Setting[4] := tbarSetting5;

  SetLength(FEqLevels, 0);
end;

procedure TfrmMplayerEQ.btnResetClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 4 do
  begin
    Setting[i].Position := 0;
  end;
end;

procedure TfrmMplayerEQ.FormShow(Sender: TObject);
begin
  ReadEqSettings
end;

procedure TfrmMplayerEQ.ReadEqSettings;
var
  i: integer;
begin
  if Length(FEqLevels) = 10 then
  begin
    Setting[0].Position := FEqLevels[0];
    Setting[1].Position := FEqLevels[2];
    Setting[2].Position := FEqLevels[4];
    Setting[3].Position := FEqLevels[6];
    Setting[4].Position := FEqLevels[8];
  end;
end;

procedure TfrmMplayerEQ.WriteEqSettings;
var
  i: Integer;
begin
  if Length(FEqLevels) = 10 then
  begin
    for i := 0 to 4 do
    begin
      FEqLevels[i*2] := Setting[i].Position;
      FEqLevels[i*2+1] := Setting[i].Position;
    end;
  end;
end;

end.

