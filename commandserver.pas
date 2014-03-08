//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit commandserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LCLProc, SyncObjs,

  // synapse
  blcksock;

type

  TRemoteCommand = (rcomNone, rcomNext, rcomMusic, rcomSleep, rcomMeditation, rcomPause, rcomVolumeUp,
    rcomVolumeDown, rcomDisplayToggle, rcomRadioToggle);

  { TCOMServerThread }

  TCOMServerThread = class(TThread)
  private
    FOnCommand: TThreadMethod;
    FPort: integer;
    FPlaying: string;
    FWeatherReport: string;
    FWeatherReports: array [0..4] of string;
    FImageURLs: array [0..4] of string;

    procedure AttendConnection(Socket: TTCPBlockSocket);
    procedure Log(Message: string);
    procedure SetPlaying(const AValue: string);
  protected
    FCommand: TRemoteCommand;
    FCritical: TCriticalSection;

    procedure Execute; override;
  public
    function GetCommnd: TRemoteCommand;

    procedure SetImageURLs(URLs: array of string);

    constructor Create(Port: integer);
    destructor Destroy; override;
  published
    property Playing: string write SetPlaying;
    property OnCommand: TThreadMethod read FOnCommand write FOnCommand;
  end;

  TCOMServer = class
  private
    FCOMServerThread: TCOMServerThread;
    procedure SetOnCommand(AValue: TThreadMethod);
    procedure SetPlaying(const AValue: string);
  public
    function GetCommand: TRemoteCommand;
    procedure SetImageURLs(URLs: array of string);

    constructor Create(Port: integer);
    destructor Destroy; override;
  published
    property Playing: string write SetPlaying;
    property OnCommand: TThreadMethod write SetOnCommand;
  end;

implementation

{ TCOMServer }

procedure TCOMServer.SetPlaying(const AValue: string);
begin
  FCOMServerThread.Playing := AValue;
end;

procedure TCOMServer.SetOnCommand(AValue: TThreadMethod);
begin
  FCOMServerThread.OnCommand := AValue;
end;

function TCOMServer.GetCommand: TRemoteCommand;
begin
  Result := FCOMServerThread.GetCommnd;
end;

procedure TCOMServer.SetImageURLs(URLs: array of string);
begin
  FCOMServerThread.SetImageURLs(URLs);
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

procedure TCOMServerThread.SetPlaying(const AValue: string);
begin
  FCritical.Enter;
  FPlaying := AValue;
  FCritical.Leave;
end;

procedure TCOMServerThread.SetImageURLs(URLs: array of string);
var
  i: Integer;
begin
  FCritical.Enter;

  for i := 0 to High(URLs) do
  begin
    if i <= High(FImageURLs) then
      FImageURLs[i] := URLs[i];
  end;

  FCritical.Leave;
end;

procedure TCOMServerThread.Execute;
var
  ListenerSocket, ConnectionSocket: TTCPBlockSocket;
  Buffer: string;
begin
  ListenerSocket := TTCPBlockSocket.Create;
  ConnectionSocket := TTCPBlockSocket.Create;

  ListenerSocket.CreateSocket;
  ListenerSocket.SetLinger(true,10);
  ListenerSocket.Bind('0.0.0.0',IntToStr(FPort));
  ListenerSocket.Listen;
  ListenerSocket.ConvertLineEnd := True;

  if ListenerSocket.LastError = 0 then
  repeat
    if ListenerSocket.canread(1000) then
    begin
      ConnectionSocket.Socket := ListenerSocket.accept;
      ConnectionSocket.ConvertLineEnd := True;
      //WriteLn('Attending Connection. Error code (0=Success): ', ConnectionSocket.lasterror);
      AttendConnection(ConnectionSocket);
      ConnectionSocket.CloseSocket;
    end;
  until Terminated;

  ListenerSocket.Free;
  ConnectionSocket.Free;
end;

procedure TCOMServerThread.AttendConnection(Socket: TTCPBlockSocket);
var
  Buffer: string;
  LastError: integer;
begin
  // wait one second for new packet
  Buffer := Socket.RecvString(15000);
  LastError := Socket.LastError;

  if LastError = 0 then
  begin
    if Buffer = 'CLOCK:NEXT' then
    begin
      FCritical.Enter;
      FCommand := rcomNext;
      FCritical.Leave;
      if Assigned(FOnCommand) then Self.Synchronize(FOnCommand);
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:MUSIC' then
    begin
      FCritical.Enter;
      FCommand := rcomMusic;
      FCritical.Leave;
      if Assigned(FOnCommand) then Self.Synchronize(FOnCommand);
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:SLEEP' then
    begin
      FCritical.Enter;
      FCommand := rcomSleep;
      FCritical.Leave;
      if Assigned(FOnCommand) then Self.Synchronize(FOnCommand);
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:MEDITATION' then
    begin
      FCritical.Enter;
      FCommand := rcomMeditation;
      FCritical.Leave;
      if Assigned(FOnCommand) then Self.Synchronize(FOnCommand);
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:DISPLAY:TOGGLE' then
    begin
      FCritical.Enter;
      FCommand := rcomDisplayToggle;
      FCritical.Leave;
      if Assigned(FOnCommand) then Self.Synchronize(FOnCommand);
      Socket.SendString(':OK' + #10);
    end
    else if Pos('CLOCK:RADIO:', Buffer) > 0 then
    begin
      FCritical.Enter;
      FCommand := rcomRadioToggle;
      FCritical.Leave;
      if Assigned(FOnCommand) then Self.Synchronize(FOnCommand);
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:PAUSE' then
    begin
      FCritical.Enter;
      FCommand := rcomPause;
      FCritical.Leave;
      if Assigned(FOnCommand) then Self.Synchronize(FOnCommand);
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:VOLUP' then
    begin
      FCritical.Enter;
      FCommand := rcomVolumeUp;
      FCritical.Leave;
      if Assigned(FOnCommand) then Self.Synchronize(FOnCommand);
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:VOLDOWN' then
    begin
      FCritical.Enter;
      FCommand := rcomVolumeDown;
      FCritical.Leave;
      if Assigned(FOnCommand) then Self.Synchronize(FOnCommand);
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:PLAYING' then
    begin
      FCritical.Enter;
      Socket.SendString(FPlaying + #10);
      FCritical.Leave;
      Socket.SendString(':OK' + #10);
    end;
  end;
end;

function TCOMServerThread.GetCommnd: TRemoteCommand;
begin
  FCritical.Enter;
  Result := FCommand;
  FCommand := rcomNone;
  FCritical.Leave;
end;

constructor TCOMServerThread.Create(Port: integer);
begin
  inherited Create(False);

  FCritical := TCriticalSection.Create;
  FCommand := rcomNone;
  FPlaying := '--------';
  FWeatherReport := '';
  FPort := Port;
  FOnCommand := nil;
end;

destructor TCOMServerThread.Destroy;
begin
  FCritical.Free;

  inherited Destroy;
end;

procedure TCOMServerThread.Log(Message: string);
begin
  DebugLn(Self.ClassName + #9#9 + Message);
end;

end.


