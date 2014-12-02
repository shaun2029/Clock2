//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit commandserver;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, SyncObjs,

  // synapse
  blcksock;

type

  TRemoteCommand = (rcomNone, rcomNext, rcomMusic, rcomSleep, rcomMeditation, rcomPause, rcomVolumeUp,
    rcomVolumeDown, rcomDisplayToggle, rcomSetRadioStation, rcomRadio, rcomFavorite);

  { TCOMServerThread }

  TCOMServerThread = class(TThread)
  private
    FOnCommand: TThreadMethod;
    FPort: integer;
    FPlaying, FRadioStations: string;
    FRadioStation: integer;

    procedure AttendConnection(Socket: TTCPBlockSocket);
    function GetRadioStation: integer;
    function GetCommand: TRemoteCommand;
    procedure SetPlaying(const AValue: string);
    procedure SetRadioStations(const AValue: string);
  protected
    FCommand: TRemoteCommand;
    FCritical: TCriticalSection;

    procedure Execute; override;
  public
    constructor Create(Port: integer);
    destructor Destroy; override;
  published
    property Playing: string write SetPlaying;
    property OnCommand: TThreadMethod read FOnCommand write FOnCommand;
    property RadioStation: integer read GetRadioStation;
    property RadioStations: string write SetRadioStations;
    property Command: TRemoteCommand read GetCommand;
  end;

  TCOMServer = class
  private
    FCOMServerThread: TCOMServerThread;
    function GetRadioStation: integer;
    procedure SetOnCommand(AValue: TThreadMethod);
    procedure SetPlaying(const AValue: string);
    function GetCommand: TRemoteCommand;
    procedure SetRadioStations(AValue: string);
  public
    constructor Create(Port: integer);
    destructor Destroy; override;
  published
    property Playing: string write SetPlaying;
    property OnCommand: TThreadMethod write SetOnCommand;
    property RadioStation: integer read GetRadioStation;
    property Command: TRemoteCommand read GetCommand;
    property RadioStations: string write SetRadioStations;
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

function TCOMServer.GetRadioStation: integer;
begin
  Result := FCOMServerThread.RadioStation;
end;

function TCOMServer.GetCommand: TRemoteCommand;
begin
  Result := FCOMServerThread.GetCommand;
end;

procedure TCOMServer.SetRadioStations(AValue: string);
begin
  FCOMServerThread.RadioStations := AValue;
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

procedure TCOMServerThread.SetRadioStations(const AValue: string);
begin
  FCritical.Enter;
  FRadioStations := AValue;
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

  ListenerSocket.CloseSocket;
  ListenerSocket.Free;
  ConnectionSocket.Free;
end;

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
    else if Pos('CLOCK:RADIO', Buffer) > 0 then
    begin
      FCritical.Enter;
      FCommand := rcomRadio;
      FCritical.Leave;
      if Assigned(FOnCommand) then Self.Synchronize(FOnCommand);
      Socket.SendString(':OK' + #10);
    end
    else if Pos('CLOCK:SET:RADIOSTATION:', Buffer) > 0 then
    begin
      FCritical.Enter;
      Buffer := Copy(Buffer, Length('CLOCK:SET:RADIOSTATION:') + 1, Length(Buffer));
      Buffer := StringReplace(Buffer, #10, '', [rfReplaceAll]);
      Buffer := StringReplace(Buffer, #13, '', [rfReplaceAll]);
      FRadioStation := StrToIntDef(Buffer, 0);
      FCommand := rcomSetRadioStation;
      FCritical.Leave;
      if Assigned(FOnCommand) then Self.Synchronize(FOnCommand);
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
    end
    else if Buffer = 'CLOCK:FAVORITE' then
    begin
      FCritical.Enter;
      FCommand := rcomFavorite;
      FCritical.Leave;
      if Assigned(FOnCommand) then Self.Synchronize(FOnCommand);
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
end;

constructor TCOMServerThread.Create(Port: integer);
begin
  inherited Create(False);

  FCritical := TCriticalSection.Create;
  FCommand := rcomNone;
  FPlaying := '--------';
  FRadioStations := '';
  FPort := Port;
  FOnCommand := nil;
end;

destructor TCOMServerThread.Destroy;
begin
  FCritical.Free;

  inherited Destroy;
end;

end.


