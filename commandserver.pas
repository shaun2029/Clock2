//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit commandserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs, Process, WebControl, DateUtils,

  // synapse
  blcksock;

type
  TComErrorState = (cesOK, cesSocketError, cesException, cesBindError);

  { TCOMServerThread }

  TCOMServerThread = class(TThread)
  private
    FPort: integer;
    FPlaying, FRadioStations, FReminders: string;
    FRadioStation: integer;
    FIRRadioStation: integer;
    FErrorState: TComErrorState;
    FWebControl: TSimpleWebControl;
    FSerialDevice: string;
    FTemperature: single;
    FTempTime: TDateTime;

    procedure AttendConnection(Socket: TTCPBlockSocket);
    procedure DumpExceptionCallStack;
    function GetRadioStation: integer;
    function GetCommand: TRemoteCommand;
    function GetTemperature: single;
    function GetTemperatureValid: boolean;
    procedure ReadSerialCommands(Filename: string);
    procedure SetHeatingBoost(AValue: integer);
    procedure SetHostName(AValue: string);
    procedure SetPlaying(const AValue: string);
    procedure SetRadioStations(const AValue: string);
    procedure SetReminders(const AValue: string);
  protected
    FCommand: TRemoteCommand;
    FCritical: TCriticalSection;

    procedure Execute; override;
  public
    constructor Create(Port: integer; SerialDevice: string);
    destructor Destroy; override;

    procedure Lock();
    procedure Unlock();
  published
    property Playing: string write SetPlaying;
    property RadioStation: integer read GetRadioStation;
    property RadioStations: string write SetRadioStations;
    property Reminders: string write SetReminders;
    property HostName: string write SetHostName;
    property HeatingBoost: integer write SetHeatingBoost;
    property Command: TRemoteCommand read GetCommand;
    property Error: TComErrorState read FErrorState;
    property Temperature: single read GetTemperature;
    property TemperatureValid: boolean read GetTemperatureValid;
  end;

  TCOMServer = class
  private
    FCOMServerThread: TCOMServerThread;
    function GetErrorState: TComErrorState;
    function GetRadioStation: integer;
    function GetTemperature: single;
    function GetTemperatureValid: boolean;
    procedure SetHeatingBoost(AValue: integer);
    procedure SetHostName(AValue: string);
    procedure SetPlaying(const AValue: string);
    function GetCommand: TRemoteCommand;
    procedure SetRadioStations(AValue: string);
    procedure SetReminders(AValue: string);
  public
    constructor Create(Port: integer; SerialDevice: string);
    destructor Destroy; override;
  published
    property Playing: string write SetPlaying;
    property RadioStation: integer read GetRadioStation;
    property Command: TRemoteCommand read GetCommand;
    property RadioStations: string write SetRadioStations;
    property Reminders: string write SetReminders;
    property HostName: string write SetHostName;
    property Error: TComErrorState read GetErrorState;
    property HeatingBoost: integer write SetHeatingBoost;
    property Temperature: single read GetTemperature;
    property TemperatureValid: boolean read GetTemperatureValid;
  end;

implementation

procedure Log(Message: string);
begin
  WriteLn(Message);
end;

{ TCOMServer }
function TCOMServer.GetErrorState: TComErrorState;
begin
  FCOMServerThread.Lock;
  Result := FCOMServerThread.Error;
  FCOMServerThread.Unlock;
end;

procedure TCOMServer.SetPlaying(const AValue: string);
begin
  FCOMServerThread.Lock;
  FCOMServerThread.Playing := AValue;
  FCOMServerThread.Unlock;
end;

function TCOMServer.GetRadioStation: integer;
begin
  FCOMServerThread.Lock;
  Result := FCOMServerThread.RadioStation;
  FCOMServerThread.Unlock;
end;

function TCOMServer.GetTemperature: single;
begin
  FCOMServerThread.Lock;
  Result := FCOMServerThread.Temperature;
  FCOMServerThread.Unlock;
end;

function TCOMServer.GetTemperatureValid: boolean;
begin
  FCOMServerThread.Lock;
  Result := FCOMServerThread.TemperatureValid;
  FCOMServerThread.Unlock;
end;

procedure TCOMServer.SetHeatingBoost(AValue: integer);
begin
  FCOMServerThread.Lock;
  FCOMServerThread.HeatingBoost := AValue;
  FCOMServerThread.Unlock;
end;

procedure TCOMServer.SetHostName(AValue: string);
begin
  FCOMServerThread.Lock;
  FCOMServerThread.HostName := AValue;
  FCOMServerThread.Unlock;
end;

function TCOMServer.GetCommand: TRemoteCommand;
begin
  FCOMServerThread.Lock;
  Result := FCOMServerThread.GetCommand;
  FCOMServerThread.Unlock;
end;

procedure TCOMServer.SetRadioStations(AValue: string);
begin
  FCOMServerThread.Lock;
  FCOMServerThread.RadioStations := AValue;
  FCOMServerThread.Unlock;
end;

procedure TCOMServer.SetReminders(AValue: string);
begin
  FCOMServerThread.Lock;
  FCOMServerThread.Reminders := AValue;
  FCOMServerThread.Unlock;
end;

constructor TCOMServer.Create(Port: integer; SerialDevice: string);
begin
  inherited Create;

  FCOMServerThread := TCOMServerThread.Create(Port, SerialDevice);
end;

destructor TCOMServer.Destroy;
begin
  FCOMServerThread.Terminate;
  FCOMServerThread.WaitFor;
  FCOMServerThread.Free;

  inherited Destroy;
end;


{ TCOMServerThread }


var
     DumpFatalException: Exception;


procedure TCOMServerThread.DumpExceptionCallStack;
var
  I: Integer;
  Frames: PPointer;
  Report: string;
  E: Exception;
  EFile: text;
  Filename: String;
begin
  E := DumpFatalException;

  Report := 'TCOMServerThread Program exception! ' + LineEnding +
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

procedure TCOMServerThread.SetPlaying(const AValue: string);
begin
  FCritical.Enter;
  FPlaying := AValue;
  FCritical.Leave;

  if Assigned(FWebControl) then
    FWebControl.Playing := AValue;
end;

procedure TCOMServerThread.SetRadioStations(const AValue: string);
begin
  FCritical.Enter;
  FRadioStations := AValue;
  FCritical.Leave;
end;

procedure TCOMServerThread.SetReminders(const AValue: string);
begin
  FCritical.Enter;
  FReminders := AValue;
  FCritical.Leave;
end;

procedure TCOMServerThread.Execute;
var
  ListenerSocket, ConnectionSocket: TTCPBlockSocket;
  WebControl: TSimpleWebControl;
begin
  FIRRadioStation := 0;

  try
    ListenerSocket := TTCPBlockSocket.Create;
    ConnectionSocket := TTCPBlockSocket.Create;

    ListenerSocket.CreateSocket;
    ListenerSocket.SetLinger(true,10);
    ListenerSocket.Bind('0.0.0.0',IntToStr(FPort));
    ListenerSocket.Listen;
    ListenerSocket.ConvertLineEnd := True;

    if ListenerSocket.LastError <> 0 then
    begin
      Log(Format('Command Server: Bind failed with error code %d', [ListenerSocket.LastError]));
      FErrorState := cesBindError;
      while not Terminated do Sleep(100);
    end
    else
    begin
      repeat
        FCritical.Enter;
        FWebControl.Temperature := FTemperature;
        FWebControl.TempTime := FTempTime;
        FCritical.Leave;
        FWebControl.ProccessConnections();

        if FileExists(FSerialDevice) then
        begin
          ReadSerialCommands(FSerialDevice);
        end;

        if ListenerSocket.CanRead(200) then
        begin
          ConnectionSocket.Socket := ListenerSocket.accept;
          ConnectionSocket.ConvertLineEnd := True;
          //WriteLn('Attending Connection. Error code (0=Success): ', ConnectionSocket.lasterror);
          FCritical.Enter;
          AttendConnection(ConnectionSocket);
          FCritical.Leave;
          ConnectionSocket.CloseSocket;
        end
        else if ListenerSocket.LastError <> 0 then
        begin
          FErrorState := cesSocketError;
          Log(Format('Command Server: RecvPacket failed with error code %d', [ListenerSocket.LastError]));
          Log(Format('Command Server: Set error state!', [ListenerSocket.LastError]));
          while not Terminated do Sleep(100);
        end;
      until Terminated;
    end;

    ListenerSocket.CloseSocket;
    ListenerSocket.Free;
    ConnectionSocket.Free;
  except
    on E: Exception do
    begin
      DumpFatalException := E;
      Synchronize(@DumpExceptionCallStack);

      FErrorState := cesException;
      Log('Command Server: Fatal Exception!');
      Log('Fatal Exception: ' + E.Message);
      Log('Command Server: Set error state!');
      while not Terminated do Sleep(100);
    end;
  end;
end;

{ Linux request:
  exec 3<>/dev/tcp/127.0.0.1/44558; echo -e "CLOCK:TEMPRATURE\r\n" >&3; cat <&3

  Responce:
  27.81999969
  :OK
}
procedure TCOMServerThread.AttendConnection(Socket: TTCPBlockSocket);
var
  Buffer: string;
  LastError: integer;
begin
  // wait one second for new packet
  Buffer := Socket.RecvString(1000);
  LastError := Socket.LastError;

  if LastError = 0 then
  begin
    if Buffer = 'CLOCK:NEXT' then
    begin
      FCritical.Enter;
      FCommand := rcomNext;
      FCritical.Leave;
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:PREVIOUS' then
    begin
      FCritical.Enter;
      FCommand := rcomPrevious;
      FCritical.Leave;
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:MUSIC' then
    begin
      FCritical.Enter;
      FCommand := rcomMusic;
      FCritical.Leave;
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:SLEEP' then
    begin
      FCritical.Enter;
      FCommand := rcomSleep;
      FCritical.Leave;
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:MEDITATION' then
    begin
      FCritical.Enter;
      FCommand := rcomMeditation;
      FCritical.Leave;
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:DISPLAY:TOGGLE' then
    begin
      FCritical.Enter;
      FCommand := rcomDisplayToggle;
      FCritical.Leave;
      Socket.SendString(':OK' + #10);
    end
    else if Pos('CLOCK:RADIO', Buffer) > 0 then
    begin
      FCritical.Enter;
      FCommand := rcomRadio;
      FCritical.Leave;
      Socket.SendString(':OK' + #10);
    end
    else if Pos('CLOCK:SET:RADIOSTATION:', Buffer) > 0 then
    begin
      Buffer := Copy(Buffer, Length('CLOCK:SET:RADIOSTATION:') + 1, Length(Buffer));
      Buffer := StringReplace(Buffer, #10, '', [rfReplaceAll]);
      Buffer := StringReplace(Buffer, #13, '', [rfReplaceAll]);
      FCritical.Enter;
      FRadioStation := StrToIntDef(Buffer, 0);
      FCommand := rcomSetRadioStation;
      FCritical.Leave;
      Socket.SendString(':OK' + #10);
    end
    else if Pos('CLOCK:GET:REMINDERS', Buffer) > 0 then
    begin
      FCritical.Enter;
      Socket.SendString(FReminders + #10);
      FCritical.Leave;
      Socket.SendString(':OK' + #10);
    end
    else if Pos('CLOCK:GET:RADIOSTATIONS', Buffer) > 0 then
    begin
      FCritical.Enter;
      Socket.SendString(FRadioStations + #10);
      FCritical.Leave;
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:PAUSE' then
    begin
      FCritical.Enter;
      FCommand := rcomPause;
      FCritical.Leave;
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:VOLUP' then
    begin
      FCritical.Enter;
      FCommand := rcomVolumeUp;
      FCritical.Leave;
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:VOLDOWN' then
    begin
      FCritical.Enter;
      FCommand := rcomVolumeDown;
      FCritical.Leave;
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:PLAYING' then
    begin
      FCritical.Enter;
      Socket.SendString(FPlaying + #10);
      FCritical.Leave;
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:TEMPRATURE' then
    begin
      if FileExists(FSerialDevice) then
      begin
        FCritical.Enter;
        Socket.SendString(FloatToStr(FTemperature) + #10);
        FCritical.Leave;
        Socket.SendString(':OK' + #10);
      end
      else Socket.SendString(':BAD' + #10);
    end
    else if Buffer = 'CLOCK:FAVORITE' then
    begin
      FCritical.Enter;
      FCommand := rcomFavorite;
      FCritical.Leave;
      Socket.SendString(':OK' + #10);
    end
    else Socket.SendString(':BAD' + #10);
  end;
end;

function TCOMServerThread.GetRadioStation: integer;
begin
  FCritical.Enter;
  Result := FRadioStation;
  FCritical.Leave;
end;

function TCOMServerThread.GetCommand: TRemoteCommand;
begin
  FCritical.Enter;
  Result := FCommand;
  FCommand := rcomNone;
  FCritical.Leave;

  if (Result = rcomNone) then
    Result := FWebControl.Command;
end;

function TCOMServerThread.GetTemperature: single;
begin
  FCritical.Enter;
  Result := FTemperature;
  FCritical.Leave;
end;

function TCOMServerThread.GetTemperatureValid: boolean;
begin
  FCritical.Enter;
  Result := (Abs(SecondsBetween(Now, FTempTime)) < 60);
  FCritical.Leave;
end;

procedure TCOMServerThread.SetHostName(AValue: string);
begin
  FCritical.Enter;
  FWebControl.HostName := AValue;
  FCritical.Leave;
end;

constructor TCOMServerThread.Create(Port: integer; SerialDevice: string);
begin
  inherited Create(False);

  FErrorState := cesOk;
  FCritical := TCriticalSection.Create;
  FCommand := rcomNone;
  FPlaying := '--------';
  FRadioStations := '';
  FReminders := '';
  FPort := Port;
  FSerialDevice := SerialDevice;

  FWebControl := TSimpleWebControl.Create;
end;

destructor TCOMServerThread.Destroy;
begin
  inherited Destroy;

  FWebControl.Free;
  FCritical.Free;
end;

procedure TCOMServerThread.Lock();
begin
  FCritical.Enter;
end;

procedure TCOMServerThread.Unlock();
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

procedure TCOMServerThread.ReadSerialCommands(Filename: string);
var
  s, s2: string;
begin
  // Embed the file handling in a try/except block to handle errors gracefully
  try
    RunCommand('timeout 0.1 cat ' + Filename, s);
    if (Length(s) > 1) then
    begin
       SetLength(s, Pos(#10, s) - 1);
    end
    else s := '';

    if (Length(s) > 0) then
    begin
      write(s);

      // Next
      if (s = 'Received SONY: 8DC') then
      begin
        FCritical.Enter;
        FCommand := rcomNext;
        FCritical.Leave;
      end
      else if (s = 'Received SONY: DC') then
      begin
        FCritical.Enter;
        FCommand := rcomPrevious;
        FCritical.Leave;
      end
      else if (s = 'Received SONY: 59C') then
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
          FTemperature := StrToFloat(StringReplace(s, 'Temp: ', '', [rfIgnoreCase]));
        finally
        end;
        FCritical.Leave;
      end
      else if (Pos('TEMPERATURE:', s) = 1) then
      begin
        FCritical.Enter;
        try
          FTempTime := Now;
          FTemperature := StrToFloat(StringReplace(s, 'TEMPERATURE:', '', [rfIgnoreCase]));
        finally
        end;
        FCritical.Leave;
      end;
    end;
  except
  end;
end;

procedure TCOMServerThread.SetHeatingBoost(AValue: integer);
begin
  FCritical.Enter;
  FWebControl.HeatingBoost := AValue;
  FCritical.Leave;
end;

end.


