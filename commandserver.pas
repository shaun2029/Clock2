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

    procedure AttendConnection(Socket: TTCPBlockSocket);
    procedure DumpExceptionCallStack;
    function GetRadioStation: integer;
    function GetCommand: TRemoteCommand;
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
    constructor Create(Port: integer);
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
  end;

  TCOMServer = class
  private
    FCOMServerThread: TCOMServerThread;
    function GetErrorState: TComErrorState;
    function GetRadioStation: integer;
    procedure SetHostName(AValue: string);
    procedure SetPlaying(const AValue: string);
    function GetCommand: TRemoteCommand;
    procedure SetRadioStations(AValue: string);
    procedure SetReminders(AValue: string);
  public
    constructor Create(Port: integer);
    destructor Destroy; override;
  published
    property Playing: string write SetPlaying;
    property RadioStation: integer read GetRadioStation;
    property Command: TRemoteCommand read GetCommand;
    property RadioStations: string write SetRadioStations;
    property Reminders: string write SetReminders;
    property HostName: string write SetHostName;
    property Error: TComErrorState read GetErrorState;
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

constructor TCOMServer.Create(Port: integer);
begin
  inherited Create;

  FCOMServerThread := TCOMServerThread.Create(Port);
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
        FWebControl.ProccessConnections();

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
  Buffer := Socket.RecvString(100);
  LastError := Socket.LastError;

  //Log('CommandServer AddendConnection: ...');

  if LastError = 0 then
  begin
    //Log('CommandServer Received: ' + Buffer);

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
    else if Buffer = 'CLOCK:FAVORITE' then
    begin
      FCritical.Enter;
      FCommand := rcomFavorite;
      FCritical.Leave;
      Socket.SendString(':OK' + #10);
    end
    else Socket.SendString(':BAD' + #10);
  end
  else
  begin
    Log('Command Server Socket Error: ' + IntToStr(LastError));
  end;

  //Log('CommandServer AddendConnection: end');
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

procedure TCOMServerThread.SetHostName(AValue: string);
begin
  FCritical.Enter;
  FWebControl.HostName := AValue;
  FCritical.Leave;
end;

constructor TCOMServerThread.Create(Port: integer);
begin
  inherited Create(False);

  FErrorState := cesOk;
  FCritical := TCriticalSection.Create;
  FCommand := rcomNone;
  FPlaying := '--------';
  FRadioStations := '';
  FReminders := '';
  FPort := Port;

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

procedure TCOMServerThread.SetHeatingBoost(AValue: integer);
begin
  FCritical.Enter;
  FWebControl.HeatingBoost := AValue;
  FCritical.Leave;
end;

end.


