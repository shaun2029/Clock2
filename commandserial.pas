//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit commandserial;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Process, DateUtils, WebControl, LCLProc,

  // synapse
  blcksock, fpjson, jsonparser;

type
  { TCOMSerialThread }

  TCOMSerialThread = class(TThread)
  private
    FRadioStation: integer;
    FSerialDevice: string;
    FSensorAddress, FSensorPort: string;
    FTemperature: single;
    FHumidity: single;
    FTempTime: TDateTime;
    FHumidityTime: TDateTime;
    FSensorDataTime: TDateTime;

    function GetCommandArg(Command, s: string): string;
    procedure DumpExceptionCallStack;
    function GetHumidity: single;
    function GetHumidityValid: boolean;
    function GetRadioStation: integer;
    function GetCommand: TRemoteCommand;
    function GetTemperature: single;
    function GetTemperatureValid: boolean;
    procedure ReadSerialCommands(Filename: string);
    function GetSensorData(var Temperature, Humidity: single): boolean;
  protected
    FCommand: TRemoteCommand;
    FCritical: TCriticalSection;

    procedure Execute; override;
  public
    constructor Create(SerialDevice, SensorAddress, SensorPort: string);
    destructor Destroy; override;

    procedure Lock();
    procedure Unlock();
  published
    property Command: TRemoteCommand read GetCommand;
    property Temperature: single read GetTemperature;
    property TemperatureValid: boolean read GetTemperatureValid;
    property Humidity: single read GetHumidity;
    property HumidityValid: boolean read GetHumidityValid;
    property RadioStation: integer read GetRadioStation;
  end;

  TCOMSerial = class
  private
    FCOMSerialThread: TCOMSerialThread;
    function GetHumidity: single;
    function GetHumidityValid: boolean;
    function GetRadioStation: integer;
    function GetTemperature: single;
    function GetTemperatureValid: boolean;
    function GetCommand: TRemoteCommand;
  public
    constructor Create(SerialDevice, SensorAddress, SensorPort: string);
    destructor Destroy; override;
  published
    property Command: TRemoteCommand read GetCommand;
    property Temperature: single read GetTemperature;
    property TemperatureValid: boolean read GetTemperatureValid;
    property Humidity: single read GetHumidity;
    property HumidityValid: boolean read GetHumidityValid;
    property RadioStation: integer read GetRadioStation;
  end;

implementation

procedure Log(Message: string);
begin
  DebugLn('TCOMSerial: ' + Message);
end;

{ TCOMSerial }
function TCOMSerial.GetTemperature: single;
begin
  FCOMSerialThread.Lock;
  Result := FCOMSerialThread.Temperature;
  FCOMSerialThread.Unlock;
end;

function TCOMSerial.GetTemperatureValid: boolean;
begin
  FCOMSerialThread.Lock;
  Result := FCOMSerialThread.TemperatureValid;
  FCOMSerialThread.Unlock;
end;

function TCOMSerial.GetHumidity: single;
begin
  FCOMSerialThread.Lock;
  Result := FCOMSerialThread.Humidity;
  FCOMSerialThread.Unlock;
end;

function TCOMSerial.GetHumidityValid: boolean;
begin
  FCOMSerialThread.Lock;
  Result := FCOMSerialThread.HumidityValid;
  FCOMSerialThread.Unlock;
end;

function TCOMSerial.GetRadioStation: integer;
begin
  FCOMSerialThread.Lock;
  Result := FCOMSerialThread.RadioStation;
  FCOMSerialThread.Unlock;
end;

function TCOMSerial.GetCommand: TRemoteCommand;
begin
  FCOMSerialThread.Lock;
  Result := FCOMSerialThread.GetCommand;
  FCOMSerialThread.Unlock;
end;

constructor TCOMSerial.Create(SerialDevice, SensorAddress, SensorPort: string);
begin
  inherited Create;

  FCOMSerialThread := TCOMSerialThread.Create(SerialDevice, SensorAddress, SensorPort);
end;

destructor TCOMSerial.Destroy;
begin
  FCOMSerialThread.Terminate;
  FCOMSerialThread.WaitFor;
  FCOMSerialThread.Free;

  inherited Destroy;
end;


{ TCOMSerialThread }


var
     DumpFatalException: Exception;


procedure TCOMSerialThread.DumpExceptionCallStack;
var
  I: Integer;
  Frames: PPointer;
  Report: string;
  E: Exception;
  EFile: text;
  Filename: String;
begin
  E := DumpFatalException;

  Report := 'TCOMSerialThread Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);

  Filename := GetUserDir + 'Clock2-CrashDump-' + DateTimeToStr(Now) + '.txt';
  Assign(EFile, Filename);
  Rewrite(EFile);
  Writeln(EFile, Report);
  Close(EFile);

  Writeln(Report);
end;

procedure TCOMSerialThread.Execute;
begin
  FSensorDataTime := 0;
  FTempTime := 0;
  FHumidityTime := 0;

  if FileExists(FSerialDevice) then
  begin
    Log('Using Serial Device: ' + FSerialDevice);
  end
  else
  begin
      Log('WARNING: Serial device not found ' + FSerialDevice);
  end;

  try
    repeat
      if FileExists(FSerialDevice) then
      begin
        ReadSerialCommands(FSerialDevice);
      end;

      if (Abs(SecondsBetween(Now, FSensorDataTime)) > 10) then
      begin
        FSensorDataTime := Now;

        if (FSensorAddress <> '') or (FSensorPort <> '') then
        begin
          if GetSensorData(FTemperature, FHumidity) then
          begin
            FTempTime := Now;
            FHumidityTime := Now;
          end;
        end;
      end
      else
      begin
        Sleep(500);
      end;
    until Terminated;
  except
    on E: Exception do
    begin
      DumpFatalException := E;
      Synchronize(@DumpExceptionCallStack);

      Log('Command Serial: Fatal Exception!');
      Log('Fatal Exception: ' + E.Message);
      Log('Command Serial: Set error state!');
      while not Terminated do Sleep(100);
    end;
  end;
end;

function TCOMSerialThread.GetRadioStation: integer;
begin
  FCritical.Enter;
  Result := FRadioStation;
  FCritical.Leave;
end;

function TCOMSerialThread.GetCommand: TRemoteCommand;
begin
  FCritical.Enter;
  Result := FCommand;
  FCommand := rcomNone;
  FCritical.Leave;
end;

function TCOMSerialThread.GetTemperature: single;
begin
  FCritical.Enter;
  Result := FTemperature;
  FCritical.Leave;
end;

function TCOMSerialThread.GetTemperatureValid: boolean;
begin
  FCritical.Enter;
  Result := (Abs(SecondsBetween(Now, FTempTime)) < 60);
  FCritical.Leave;
end;

function TCOMSerialThread.GetHumidity: single;
begin
  FCritical.Enter;
  Result := FHumidity;
  FCritical.Leave;
end;

function TCOMSerialThread.GetHumidityValid: boolean;
begin
  FCritical.Enter;
  Result := (Abs(SecondsBetween(Now, FHumidityTime)) < 60);
  FCritical.Leave;
end;

constructor TCOMSerialThread.Create(SerialDevice, SensorAddress, SensorPort: string);
begin
  inherited Create(False);

  FCritical := TCriticalSection.Create;
  FSerialDevice := SerialDevice;
  FSensorAddress := SensorAddress;
  FSensorPort := SensorPort;
end;

destructor TCOMSerialThread.Destroy;
begin
  inherited Destroy;

  FCritical.Free;
end;

procedure TCOMSerialThread.Lock();
begin
  FCritical.Enter;
end;

procedure TCOMSerialThread.Unlock();
begin
  FCritical.Leave;
end;

{
 Serial device is arduino connected to IR receiver and LM35 temprature sensor.

IR Commands and codes:

  Next
  Received SONY: 8DC
  Code Length:12

  Prev
  Received SONY: DC
  Code Length:12

  Play/Pause
  Received SONY: 59C
  Code Length:12

  Radio 0
  Received SONY: 6BC
  Code Length:12

  Radio 1
  Received SONY: EBC
  Code Length:12

  Radio 3
  Received SONY: 5BC
  Code Length:12

  Sleep
  Received SONY: E3C
  Code Length:12

  Vol Up
  Received SONY: 39C
  Code Length:12

  Vol Down
  Received SONY: D9C
  Code Length:12
}

function TComSerialThread.GetCommandArg(Command, s: string): string;
begin
  Result := '';

  if (Pos(Command, s) > 0) then
  begin
    s := Copy(s, Pos(Command, s), Length(s));

    if (Pos(#13, s) > 0) or (Pos(#10, s) > 0) then
    begin
      if (Pos(#13, s) > 0) then
        s := Copy(s, 1, Pos(#13, s)-1);

      if (Pos(#10, s) > 0) then
        s := Copy(s, 1, Pos(#10, s)-1);

      Result := Trim(StringReplace(s, Command, '', [rfIgnoreCase]));
    end;
  end;
end;

procedure TCOMSerialThread.ReadSerialCommands(Filename: string);
var
  s: string;
begin
  // Embed the file handling in a try/except block to handle errors gracefully
  try
    RunCommand('timeout 1 cat ' + Filename, s);

    if (Length(s) > 1) then
    begin
      if (Pos('TEMPERATURE:', s) > 0) then
      begin
        FCritical.Enter;
        try
          FTemperature := StrToFloat(GetCommandArg('TEMPERATURE:', s));
          FTempTime := Now;
          Log('Serial Command: TEMPERATURE ' + GetCommandArg('TEMPERATURE:', s));
        finally
        end;
        FCritical.Leave;
      end;

      if (Pos('HUMIDITY:', s) > 0) then
      begin
        FCritical.Enter;
        try
          FHumidity := StrToFloat(GetCommandArg('HUMIDITY:', s));
          FHumidityTime := Now;
          Log('Serial Command: HUMIDITY ' + GetCommandArg('HUMIDITY:', s));
        finally
        end;
        FCritical.Leave;
      end;
    end
    else
    begin
      if (Length(s) > 1) then
      begin
         SetLength(s, Pos(#10, s) - 1);
      end
      else s := '';

      if (Length(s) > 0) then
      begin
        Log('Serial Command: ' + s);

        // Next
        if (s = 'Received SONY: 8DC')
          or (s = 'Gesture:Right') then
        begin
          FCritical.Enter;
          FCommand := rcomNext;
          FCritical.Leave;
        end
        else if (s = 'Received SONY: DC')
          or (s = 'Gesture:Left') then
        begin
          FCritical.Enter;
          FCommand := rcomPrevious;
          FCritical.Leave;
        end
        else if (s = 'Received SONY: 59C')
          or (s = 'Gesture:Pause')
          or (s = 'Gesture:Wave') then
        begin
          FCritical.Enter;
          FCommand := rcomPause;
          FCritical.Leave;
        end
        else if (s = 'Received SONY: 39C') then
        begin
          FCritical.Enter;
          FCommand := rcomVolumeUp;
          FCritical.Leave;
        end
        else if (s = 'Received SONY: D9C') then
        begin
          FCritical.Enter;
          FCommand := rcomVolumeDown;
          FCritical.Leave;
        end
        else if (s = 'Gesture:VolUp') then
        begin
          FCritical.Enter;
          FCommand := rcomVolumeUp;
          FCritical.Leave;
        end
        else if (s = 'Gesture:VolDown') then
        begin
          FCritical.Enter;
          FCommand := rcomVolumeDown;
          FCritical.Leave;
        end
        else if (s = 'Received SONY: EBC') then
        begin
          FCritical.Enter;
          FRadioStation := 0;
          FCommand := rcomSetRadioStation;
          FCritical.Leave;
        end
        else if (s = 'Received SONY: 6BC') then
        begin
          FCritical.Enter;
          FRadioStation := 1;
          FCommand := rcomSetRadioStation;
          FCritical.Leave;
        end
        else if (s = 'Received SONY: 5BC') then
        begin
          FCritical.Enter;
          FRadioStation := 2;
          FCommand := rcomSetRadioStation;
          FCritical.Leave;
        end
        else if (s = 'Received SONY: E3C') then
        begin
          FCritical.Enter;
          FCommand := rcomSleep;
          FCritical.Leave;
        end
        else if (Pos('Temp: ', s) = 1) then
        begin
          FCritical.Enter;
          try
            FTempTime := Now;
            FTemperature := StrToFloat(GetCommandArg('Temp:', s));
          finally
          end;
          FCritical.Leave;
        end;
      end;
    end;
  except
  end;
end;

function TCOMSerialThread.GetSensorData(var Temperature, Humidity: single): boolean;
var
  jData : TJSONData;
  jObject : TJSONObject;
  Output: string;
begin
  Result := False;

  if RunCommand('curl -m 5 ' + FSensorAddress + ':' + FSensorPort + '/status' , Output) then
  begin
    Result := ((Pos('"temperature": ', Output) > 0) and (Pos('"humidity": ', Output) > 0));

    if Result then
    begin
      jData := GetJSON(Output);
      jObject := jData as TJSONObject;
      Temperature := jObject.Get('temperature');
      Humidity := jObject.Get('humidity');
    end;
  end;
end;

end.


