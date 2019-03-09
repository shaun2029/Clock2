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

  TRemoteCommand = (rcomNone, rcomNext, rcomPrevious, rcomMusic, rcomSleep, rcomMeditation, rcomPause, rcomVolumeUp,
    rcomVolumeDown, rcomDisplayToggle, rcomSetRadioStation, rcomRadio, rcomFavorite);

  TComErrorState = (cesOK, cesSocketError, cesException, cesBindError);

  { TCOMServerThread }

  TCOMServerThread = class(TThread)
  private
    FOnCommand: TThreadMethod;
    FPort: integer;
    FPlaying, FRadioStations, FReminders: string;
    FRadioStation: integer;
    FErrorState: TComErrorState;

    procedure AttendConnection(Socket: TTCPBlockSocket);
    function GetRadioStation: integer;
    function GetCommand: TRemoteCommand;
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
  published
    property Playing: string write SetPlaying;
    property OnCommand: TThreadMethod read FOnCommand write FOnCommand;
    property RadioStation: integer read GetRadioStation;
    property RadioStations: string write SetRadioStations;
    property Reminders: string write SetReminders;
    property Command: TRemoteCommand read GetCommand;
    property Error: TComErrorState read FErrorState;
  end;

  TCOMServer = class
  private
    FCOMServerThread: TCOMServerThread;
    function GetErrorState: TComErrorState;
    function GetRadioStation: integer;
    procedure SetOnCommand(AValue: TThreadMethod);
    procedure SetPlaying(const AValue: string);
    function GetCommand: TRemoteCommand;
    procedure SetRadioStations(AValue: string);
    procedure SetReminders(AValue: string);
  public
    constructor Create(Port: integer);
    destructor Destroy; override;
  published
    property Playing: string write SetPlaying;
    property OnCommand: TThreadMethod write SetOnCommand;
    property RadioStation: integer read GetRadioStation;
    property Command: TRemoteCommand read GetCommand;
    property RadioStations: string write SetRadioStations;
    property Reminders: string write SetReminders;
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
  Result := FCOMServerThread.Error;
end;

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

procedure TCOMServer.SetReminders(AValue: string);
begin
  FCOMServerThread.Reminders := AValue;
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

procedure TCOMServerThread.SetReminders(const AValue: string);
begin
  FCritical.Enter;
  FReminders := AValue;
  FCritical.Leave;
end;

procedure TCOMServerThread.Execute;
var
  ListenerSocket, ConnectionSocket: TTCPBlockSocket;
begin
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
    on E: exception do
    begin
      FErrorState := cesException;
      Log('Command Server: Fatal Exception!');
      Log('Fatal Exception: ' + E.Message);
      Log('Command Server: Set error state!');
      while not Terminated do Sleep(100);
    end;
  end;
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
      FCommand := rcomNext;
      if Assigned(FOnCommand) then FOnCommand;
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:PREVIOUS' then
    begin
      FCommand := rcomPrevious;
      if Assigned(FOnCommand) then FOnCommand;
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:MUSIC' then
    begin
      FCommand := rcomMusic;
      if Assigned(FOnCommand) then FOnCommand;
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:SLEEP' then
    begin
      FCommand := rcomSleep;
      if Assigned(FOnCommand) then FOnCommand;
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:MEDITATION' then
    begin
      FCommand := rcomMeditation;
      if Assigned(FOnCommand) then FOnCommand;
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:DISPLAY:TOGGLE' then
    begin
      FCommand := rcomDisplayToggle;
      if Assigned(FOnCommand) then FOnCommand;
      Socket.SendString(':OK' + #10);
    end
    else if Pos('CLOCK:RADIO', Buffer) > 0 then
    begin
      FCommand := rcomRadio;
      if Assigned(FOnCommand) then FOnCommand;
      Socket.SendString(':OK' + #10);
    end
    else if Pos('CLOCK:SET:RADIOSTATION:', Buffer) > 0 then
    begin
      Buffer := Copy(Buffer, Length('CLOCK:SET:RADIOSTATION:') + 1, Length(Buffer));
      Buffer := StringReplace(Buffer, #10, '', [rfReplaceAll]);
      Buffer := StringReplace(Buffer, #13, '', [rfReplaceAll]);
      FRadioStation := StrToIntDef(Buffer, 0);
      FCommand := rcomSetRadioStation;
      if Assigned(FOnCommand) then FOnCommand;
      Socket.SendString(':OK' + #10);
    end
    else if Pos('CLOCK:GET:REMINDERS', Buffer) > 0 then
    begin
      Socket.SendString(FReminders + #10);
      Socket.SendString(':OK' + #10);
    end
    else if Pos('CLOCK:GET:RADIOSTATIONS', Buffer) > 0 then
    begin
      Socket.SendString(FRadioStations + #10);
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:PAUSE' then
    begin
      FCommand := rcomPause;
      if Assigned(FOnCommand) then FOnCommand;
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:VOLUP' then
    begin
      FCommand := rcomVolumeUp;
      if Assigned(FOnCommand) then FOnCommand;
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:VOLDOWN' then
    begin
      FCommand := rcomVolumeDown;
      if Assigned(FOnCommand) then FOnCommand;
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:PLAYING' then
    begin
      Socket.SendString(FPlaying + #10);
      Socket.SendString(':OK' + #10);
    end
    else if Buffer = 'CLOCK:FAVORITE' then
    begin
      FCommand := rcomFavorite;
      if Assigned(FOnCommand) then FOnCommand;
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

  FErrorState := cesOk;
  FCritical := TCriticalSection.Create;
  FCommand := rcomNone;
  FPlaying := '--------';
  FRadioStations := '';
  FReminders := '';
  FPort := Port;
  FOnCommand := nil;
end;

destructor TCOMServerThread.Destroy;
begin
  FCritical.Free;

  inherited Destroy;
end;

end.


