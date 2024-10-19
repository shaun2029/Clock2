//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit Reminders;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, IniFiles, LCLProc, Buttons, Calendar;

type

  TReminderKind = (rkOnce, rkWeekly, rkMonthly, rkYearly, rkQuaterly);

  TReminder = record
    Date: TDateTime;
    Detail: string;
    Kind: TReminderKind;
    WarningDay: boolean;
    WarningTwoDays: boolean;
    WarningWeek: boolean;
  end;

  TReminders = array of TReminder;

  { TfrmReminders }

  TfrmReminders = class(TForm)
    btnOk: TBitBtn;
    btnCancel: TBitBtn;
    Calendar: TCalendar;
    cgrpWarning: TCheckGroup;
    GroupBox1: TGroupBox;
    mmoDetail: TMemo;
    rgrpKind: TRadioGroup;
    procedure btnOkClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
  private
    FEditing: boolean;
    FReminders: TReminders;
    FFilename: string;
    procedure SortReminders(Reminders: TReminders); overload;

    { private declarations }
  public
    { public declarations }

    function GetCurrentReminders: TReminders;
    procedure ReadReminders;
    procedure WriteReminders;
    procedure SortReminders; overload;

    procedure AddReminder(Rem: TReminder);
    procedure DeleteReminder(Index: integer);
    procedure PopulateList(RemList: TStrings); overload;
    procedure PopulateList(Reminders: TReminders; RemList: TStrings); overload;
    procedure DisplayReminder(Index: Integer);
    procedure AddCurrentReminder;
    procedure UpdateWithCurrentReminder(Index: Integer);
    procedure RefreshReminders;
  published
    property Editing: boolean read FEditing write FEditing;
    property Filename: string read FFilename;
  end;

var
  frmReminders: TfrmReminders;

implementation

{$R *.lfm}

{ TfrmReminders }

procedure TfrmReminders.btnOkClick(Sender: TObject);
begin
  if not FEditing then AddCurrentReminder;
  ModalResult := mrOk;
end;

procedure TfrmReminders.FormCreate(Sender: TObject);
begin
  FFilename := ChangeFileExt(GetAppConfigFile(False), '_reminders.cfg');
  ReadReminders;
  FEditing := False;
  Calendar.DateTime := Now;
end;

procedure TfrmReminders.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
  begin
    Self.ModalResult := mrCancel;
  end
  else if Key = #13 then
  begin
    if not FEditing then AddCurrentReminder;
    ModalResult := mrOk;
  end;
end;

procedure TfrmReminders.ReadReminders;
var
  Ini: TIniFile;
  i: integer;
  Section: string;
begin
  SetLength(FReminders, 0);

  try
    Ini := TIniFile.Create(FFilename);
    i := 1;

    while Ini.ReadBool('Reminders' + IntToStr(i), 'Enabled', False) do
    begin
      SetLength(FReminders, i);

      Section := 'Reminders' + IntToStr(i);

      FReminders[i - 1].Date := Ini.ReadDate(Section, 'Date', 0);
      FReminders[i - 1].Detail := Ini.ReadString(Section, 'Detail', '');
      FReminders[i - 1].Kind := TReminderKind(Ini.ReadInteger(Section,'Kind', 0));
      FReminders[i - 1].WarningDay := Ini.ReadBool(Section, 'WarningDay', False);
      FReminders[i - 1].WarningTwoDays := Ini.ReadBool(Section, 'WarningTwoDays', False);
      FReminders[i - 1].WarningWeek := Ini.ReadBool(Section, 'WarningWeek', False);

      Inc(i);
    end;

    SortReminders;
    Ini.Free;
  except
    on E: Exception do
       DebugLn('EXCEPTION: TfrmReminders.ReadReminders IniFile - ' + E.ClassName + #13#10 + E.Message);
  end;
end;

procedure TfrmReminders.WriteReminders;
var
  Ini: TIniFile;
  i: integer;
  Section: string;
begin
  try
    Ini := TIniFile.Create(FFilename);
    Ini.CacheUpdates := True;

    for i := 1 to Length(FReminders) do
    begin
      Section := 'Reminders' + IntToStr(i);

      Ini.WriteBool(Section, 'Enabled', True);
      Ini.WriteDate(Section, 'Date', FReminders[i - 1].Date);
      Ini.WriteString(Section, 'Detail', FReminders[i - 1].Detail);
      Ini.WriteInteger(Section,'Kind', Ord(FReminders[i - 1].Kind));
      Ini.WriteBool(Section, 'WarningDay', FReminders[i - 1].WarningDay);
      Ini.WriteBool(Section, 'WarningTwoDays', FReminders[i - 1].WarningTwoDays);
      Ini.WriteBool(Section, 'WarningWeek', FReminders[i - 1].WarningWeek);
    end;

    // Close off reminders
    Section := 'Reminders' + IntToStr(Length(FReminders) + 1);
    Ini.WriteBool(Section, 'Enabled', False);

    Ini.UpdateFile;
  except
    on E: Exception do
       DebugLn('EXCEPTION: TfrmReminders.WriteReminders IniFile - ' + E.ClassName + #13#10 + E.Message);
  end;

  Ini.Free;
end;

procedure TfrmReminders.AddReminder(Rem: TReminder);
begin
  SetLength(FReminders, Length(FReminders) + 1);
  FReminders[Length(FReminders) - 1] := Rem;
  WriteReminders;
end;

procedure TfrmReminders.DeleteReminder(Index: integer);
var
  i: Integer;
begin
  // Out of bounds
  if (Index < 0) or (Index > Length(FReminders) - 1) then Exit;

  for i := Index to Length(FReminders) - 2 do
  begin
    FReminders[i] := FReminders[i+1];
  end;

  SetLength(FReminders, Length(FReminders) - 1);
  WriteReminders;
end;

procedure TfrmReminders.SortReminders(Reminders: TReminders);
var
  i, j: Integer;
  Temp: TReminder;
begin
  Temp.Date := 0;

  for j := 0 to Length(Reminders) - 1 do
  begin
    for i := 0 to Length(Reminders) - 2 do
    begin
      if Reminders[i].Date > Reminders[i+1].Date then
      begin
        Temp := Reminders[i];
        Reminders[i] := Reminders[i+1];
        Reminders[i+1] := Temp;
      end;
    end;
  end;
end;

procedure TfrmReminders.SortReminders;
begin
  SortReminders(FReminders);
end;

function TfrmReminders.GetCurrentReminders: TReminders;
var
  i, r, PastReminders: Integer;
  CurrDate: TDateTime;
  Found: boolean;
begin
  SetLength(Result, 0);

  SortReminders(FReminders);

  // Get event based on current day
  CurrDate := Date;
  PastReminders := 0;

  for i := 0 to Length(FReminders) - 1 do
  begin
    if (CurrDate - 3 <= FReminders[i].Date) and (CurrDate > FReminders[i].Date) then
    begin
      Inc(PastReminders);

      // Event was in the last 3 days
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := FReminders[i];
    end
    else if (CurrDate = FReminders[i].Date) then
    begin
      // Event is today
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := FReminders[i];
    end
    else if (CurrDate < FReminders[i].Date) then
    begin
      // Event is tomorrow
      if (CurrDate + 1 = FReminders[i].Date) then
      begin
        if FReminders[i].WarningDay then
        begin
          SetLength(Result, Length(Result) + 1);
          Result[Length(Result) - 1] := FReminders[i];
        end;
      end
      else if (CurrDate + 2 = FReminders[i].Date) then
      begin
        // Event in two days
        if FReminders[i].WarningTwoDays then
        begin
          SetLength(Result, Length(Result) + 1);
          Result[Length(Result) - 1] := FReminders[i];
        end;
      end
      else if (CurrDate <= FReminders[i].Date) and (CurrDate + 7 >= FReminders[i].Date) then
      begin
        // Event in a week
        if FReminders[i].WarningWeek then
        begin
          SetLength(Result, Length(Result) + 1);
          Result[Length(Result) - 1] := FReminders[i];
        end;
      end;
    end;
  end;

  // Fill min of 10 + PastReminders reminders
  for i := 0 to Length(FReminders) - 1 do
  begin
    if (Length(Result) >= 10 + PastReminders) then
      Break;

    if (CurrDate <= FReminders[i].Date) then
    begin
      // Is this reminder in the list
      Found := false;
      for r := 0 to Length(Result) - 1 do
      begin
        if (FReminders[i].date = Result[r].Date) and (FReminders[i].Detail = Result[r].Detail) then
        begin
          Found := true;
          Break;
        end;
      end;

      if (Not Found) then
      begin
        SetLength(Result, Length(Result) + 1);
        Result[Length(Result) - 1] := FReminders[i];
      end;
    end
  end;
end;

procedure TfrmReminders.PopulateList(Reminders: TReminders; RemList: TStrings);
var
  Year, Month, Day: word;
  i: Integer;
  CurrDate: Double;
  Mark: String;
begin
  CurrDate := Date;

  for i := 0 to Length(Reminders) - 1 do
  begin
    // Mark old reminders
    if CurrDate > Reminders[i].Date then
      Mark := ' - '
    else Mark := '';

    DecodeDate(Reminders[i].Date, Year, Month, Day);
    RemList.Add(Format('%s%.2d/%.2d/%.2d %s%s', [Mark, Day, Month, Year, Reminders[i].Detail, Mark]));
  end;
end;

procedure TfrmReminders.PopulateList(RemList: TStrings);
begin
  PopulateList(FReminders, RemList);
end;

procedure TfrmReminders.AddCurrentReminder;
var
  Rem: TReminder;
begin
  Rem.Date := Calendar.DateTime;
  Rem.Detail := mmoDetail.Text;
  Rem.Kind := TReminderKind(rgrpKind.ItemIndex);
  Rem.WarningDay := cgrpWarning.Checked[0];
  Rem.WarningTwoDays := cgrpWarning.Checked[1];
  Rem.WarningWeek := cgrpWarning.Checked[2];

  AddReminder(Rem);
end;

procedure TfrmReminders.UpdateWithCurrentReminder(Index: Integer);
var
  Rem: TReminder;
begin
  // Out of bounds
  if (Index < 0) or (Index > Length(FReminders) - 1) then Exit;

  Rem.Date := Calendar.DateTime;
  Rem.Detail := mmoDetail.Text;
  Rem.Kind := TReminderKind(rgrpKind.ItemIndex);
  Rem.WarningDay := cgrpWarning.Checked[0];
  Rem.WarningTwoDays := cgrpWarning.Checked[1];
  Rem.WarningWeek := cgrpWarning.Checked[2];

  FReminders[Index] := Rem;
  WriteReminders;
end;

procedure TfrmReminders.RefreshReminders;
var
  CurrDate: Double;
  Year: word;
  Month: word;
  Day: word;
  i: Integer;
  Calculating: boolean;
begin
  CurrDate := Date;

  repeat
    Calculating := false;
    for i := Length(FReminders) - 1 downto 0 do
    begin
      // If a reminder is more that 3 days old refresh or dispose of it.
      if CurrDate >= FReminders[i].Date + 3 then
      begin
        case FReminders[i].Kind of
          rkOnce: DeleteReminder(i);
          rkWeekly:
            begin
              FReminders[i].Date := FReminders[i].Date + (((Trunc(CurrDate) - Trunc(FReminders[i].Date)) + 6) div 7) * 7;
              Calculating := true;
            end;
          rkMonthly:
            begin
              // Add one month to the reminder
              DecodeDate(FReminders[i].Date, Year, Month, Day);
              IncAMonth(Year, Month, Day, 1);
              FReminders[i].Date := EncodeDate(Year, Month, Day);
              Calculating := true;
            end;
          rkQuaterly:
            begin
              // Add four months to the reminder
              DecodeDate(FReminders[i].Date, Year, Month, Day);
              IncAMonth(Year, Month, Day, 4);
              FReminders[i].Date := EncodeDate(Year, Month, Day);
              Calculating := true;
            end;
          rkYearly:
            begin
              // Add one year to the reminder
              DecodeDate(FReminders[i].Date, Year, Month, Day);
              IncAMonth(Year, Month, Day, 12);
              FReminders[i].Date := EncodeDate(Year, Month, Day);
              Calculating := true;
            end;
        end;
      end;
    end;
  until not Calculating;
end;

procedure TfrmReminders.DisplayReminder(Index: Integer);
var
  Rem: TReminder;
begin
  // Out of bounds
  if (Index < 0) or (Index > Length(FReminders) - 1) then Exit;

  Rem := FReminders[Index];

  Calendar.DateTime := Rem.Date;
  mmoDetail.Text := Rem.Detail;
  rgrpKind.ItemIndex := Ord(Rem.Kind);
  cgrpWarning.Checked[0] := Rem.WarningDay;
  cgrpWarning.Checked[1] := Rem.WarningTwoDays;
  cgrpWarning.Checked[2] := Rem.WarningWeek;
end;

end.

