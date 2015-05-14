// Uses the Alsaequal plugin
// http://www.thedigitalmachine.net/alsaequal.html

unit equaliser;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  Buttons, StdCtrls, process;

type

  { TfrmEqualiser }

  TfrmEqualiser = class(TForm)
    BtnDown2: TBitBtn;
    BtnDown3: TBitBtn;
    BtnDown1: TBitBtn;
    BtnDown4: TBitBtn;
    BtnDown5: TBitBtn;
    btnClose: TSpeedButton;
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
    procedure btnCloseClick(Sender: TObject);
    procedure btnUp1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure tbarSetting1Change(Sender: TObject);
  private
    { private declarations }
    FSupported: boolean;
    Setting: array [0..4] of TTrackBar;
    Down, Up: array [0..4] of TBitBtn;
    EqSetting: array [0..9] of integer;
    function GetAlsaEqSupported: boolean;
    function GetEqChannel(Channel: integer): integer;
    procedure ReadEqSettings;
    procedure SetEqChannel(Channel, Value: integer);
    procedure WriteEqSettings(Channel: integer);
  public
    { public declarations }
  published
    property Supported: boolean read FSupported;
  end;

const
  EqChannelName: array [0..9] of string = ('01. 31 Hz', '02. 63 Hz', '03. 125 Hz', '04. 250 Hz',
    '05. 500 Hz', '06. 1 kHz', '07. 2 kHz', '08. 4 kHz', '09. 8 kHz', '10. 16 kHz');

var
  frmEqualiser: TfrmEqualiser;

implementation

{$R *.lfm}

{ TfrmEqualiser }

procedure TfrmEqualiser.btnUp1Click(Sender: TObject);
var
  Value: integer;
begin
  Value := Setting[TBitBtn(Sender).Tag].Position mod 5;

  if Value > 0 then Value := 5 - Value
  else Value := 5;

  Setting[TBitBtn(Sender).Tag].Position := Setting[TBitBtn(Sender).Tag].Position + Value;
end;

procedure TfrmEqualiser.BtnDown1Click(Sender: TObject);
var
  Value: Integer;
begin
  Value := Setting[TBitBtn(Sender).Tag].Position mod 5;

  if Value = 0 then Value := 5;

  Setting[TBitBtn(Sender).Tag].Position := Setting[TBitBtn(Sender).Tag].Position - Value;
end;

procedure TfrmEqualiser.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmEqualiser.FormCreate(Sender: TObject);
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

  {$ifdef ALSA}
  FSupported := GetAlsaEqSupported;
  {$else}
  FSupported := False;
  {$endif}

  if FSupported then
  begin
    ReadEqSettings;

    for i := 0 to 4 do
    begin
      Setting[i].OnChange := @tbarSetting1Change;
    end;
  end;
end;

procedure TfrmEqualiser.btnResetClick(Sender: TObject);
var
  i: Integer;
begin
  for i := 0 to 4 do
  begin
    setting[i].Position := 50;
  end;
end;

procedure TfrmEqualiser.tbarSetting1Change(Sender: TObject);
begin
  WriteEqSettings(TTrackBar(Sender).Tag);
end;

function TfrmEqualiser.GetEqChannel(Channel: integer): integer;
var
  Output, Command, Value: string;
  i: Integer;
begin
  Value := '';
  Command := 'bash -c "amixer -D equal get ''' + EqChannelName[Channel]
    + ''' | grep "%" | sed ''s/\(^[a-z0-9 :]\+\[\|\[\|\]\)//gi'' | head -n 1 | sed ''s/\%//g''"';

  if RunCommand(Command, Output) then
  begin
    for i := 1 to Length(Output) do
    begin
      if (Output[i] >= '0') and (Output[i] <= '9') then
        Value := Value + Output[i];
    end;
    Result := StrToIntDef(Value, 50);
  end
  else Result := 50;
end;

// Test if ALSA equaliser filter is installed
function TfrmEqualiser.GetAlsaEqSupported(): boolean;
var
  Output, Command, Value: string;
  i: Integer;
begin
  Value := '';
  Command := 'bash -c "amixer -D equal get ''' + EqChannelName[9]
    + ''' | grep "%" | sed ''s/\(^[a-z0-9 :]\+\[\|\[\|\]\)//gi'' | head -n 1 | sed ''s/\%//g''"';

  if RunCommand(Command, Output) then
  begin
    Result := Length(Output) > 0
  end
  else Result := False;
end;

procedure TfrmEqualiser.SetEqChannel(Channel, Value: integer);
var
  Output, Command: string;
begin
  Command := 'amixer -D equal set ''' + EqChannelName[Channel]
    + ''' ' + IntToStr(Value);
  RunCommand(Command, Output);
end;

procedure TfrmEqualiser.ReadEqSettings;
var
  i: integer;
begin
  for i := 0 to 9 do
    EqSetting[i] := GetEqChannel(i);

  Setting[0].Position := (EqSetting[0] + EqSetting[1] + 3) div 2;
  Setting[1].Position := (EqSetting[2] + EqSetting[3] + 3) div 2;
  Setting[2].Position := (EqSetting[4] + EqSetting[5] + 3) div 2;
  Setting[3].Position := (EqSetting[6] + EqSetting[7] + 3) div 2;
  Setting[4].Position := (EqSetting[8] + EqSetting[9] + 3) div 2;
end;

procedure TfrmEqualiser.WriteEqSettings(Channel: integer);
var
  i: Integer;
begin
  for i := (2 * Channel) to 1 + (2 * Channel) do
    SetEqChannel(i, Setting[Channel].Position);
end;

end.

