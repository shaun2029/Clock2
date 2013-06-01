{ The date and time is wrong if the app starts before the WiFi
  has updated the date/time on the Joggler. This is usually by a
  factor of the timezone.

  If the app is then closed and reopened the problem goes away.

  To fix tis a process is used to get the date and time using the
  Linux date command. }

unit DateTime;

{$mode delphi}

interface

uses
  Classes, SysUtils, Process;

const
  UPDATE_INTERVAL = 1; // In miunutes max 59

type

  { TLinuxDateTime }

  TLinuxDateTime = class
  private
    FLastUpdate: TDateTime;
    FOffset: TDateTime;
    function Execute(Command: string): string;
  public
    function GetLocalTime: TDateTime;

    constructor Create;
  end;

implementation

{ TLinuxDateTime }

function TLinuxDateTime.GetLocalTime: TDateTime;
var
  DateStr: string;
  Day, Month, Year: word;
  Hour, Min, Sec: word;
begin
  if Now > FLastUpdate + EncodeTime(0, UPDATE_INTERVAL, 0, 0) then
  begin
    DateStr := Execute('date ''+%C%y%m%d%H%M%S'''); // outputs: 20130601174354

    FLastUpdate := Now;

    if DateStr <> '' then
    begin
      Year := StrToInt(Copy(DateStr, 1, 4));
      Month := StrToInt(Copy(DateStr, 5, 2));
      Day := StrToInt(Copy(DateStr, 7, 2));
      Hour := StrToInt(Copy(DateStr, 9, 2));
      Min := StrToInt(Copy(DateStr, 11, 2));
      Sec := StrToInt(Copy(DateStr, 13, 2));

      FOffset := FLastUpdate - EncodeDate(Year, Month, Day) - EncodeTime(Hour, Min, Sec, 0);
    end;
  end;

  Result := Now - FOffset;
end;

function TLinuxDateTime.Execute(Command: string): string;
var
 AProcess: TProcess;
 ReadSize: integer;
 Buffer: PChar;
begin
  // Now we will create the TProcess object, and
  // assign it to the var AProcess.
  AProcess := TProcess.Create(nil);

  // Tell the new AProcess what the command to execute is.
  // Let's use the FreePascal compiler
  AProcess.CommandLine := Command;

  // We will define an option for when the program
  // is run. This option will make sure that our program
  // does not continue until the program we will launch
  // has stopped running.                vvvvvvvvvvvvvv
  AProcess.Options := AProcess.Options + [poWaitOnExit, poUsePipes];

  // Now that AProcess knows what the commandline is
  // we will run it.
  AProcess.Execute;

  ReadSize := AProcess.Output.NumBytesAvailable;

  if (ReadSize > 0) then
  begin
    SetLength(Result, ReadSize);
    Buffer := @Result[1];
    AProcess.Output.Read(Buffer[0], ReadSize);
  end
  else Result := '';

  // This is not reached until ppc386 stops running.
  AProcess.Free;
end;

constructor TLinuxDateTime.Create;
begin
  FLastUpdate := Now;
  FOffset := 0;
end;

end.

