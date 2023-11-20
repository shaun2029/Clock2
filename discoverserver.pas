unit DiscoverServer;

//
// Copyright 2014 Shaun Simpson
// shauns2029@gmail.com
//

{$mode Delphi}
{$DEFINE DEBUG}

interface

uses
  Classes, SysUtils, SyncObjs, LCLProc,
  // synapse
  blcksock, synsock;

type
  TDiscoverErrorState = (desOK, desSocketError, desException, desBindError);

  { TUDPServerThread }

  { TDiscoverServerThread }

  TDiscoverServerThread = class(TThread)
  private
    FErrorState: TDiscoverErrorState;
    function GetErrorState: TDiscoverErrorState;
  protected
    FSocket: TUDPBlockSocket;
    FPort: Integer;
    FClockName: string;
    FCritical: TCriticalSection;

    procedure Execute; override;
  public
    constructor Create(Port: Integer; ClockName: string);
    destructor Destroy; override;
    property Error: TDiscoverErrorState read GetErrorState;
  end;

  { TDiscoverServer }

  TDiscoverServer = class
  private
    FDiscoverServerThread: TDiscoverServerThread;
    function GetErrorState: TDiscoverErrorState;
  public
    constructor Create(Port: Integer; ClockName: string);
    destructor Destroy; override;
  published
    property Error: TDiscoverErrorState read GetErrorState;
  end;

implementation

procedure Log(Message: string);
begin
  DebugLn('TDiscoverServer: ' + Message);
end;

{ TDiscoverServer }

function TDiscoverServer.GetErrorState: TDiscoverErrorState;
begin
  Result := FDiscoverServerThread.Error;
end;

constructor TDiscoverServer.Create(Port: Integer; ClockName: string);
begin
  FDiscoverServerThread := TDiscoverServerThread.Create(Port, ClockName);
end;

destructor TDiscoverServer.Destroy;
begin
  FDiscoverServerThread.Terminate;
  FDiscoverServerThread.WaitFor;
  FDiscoverServerThread.Free;

  inherited Destroy;
end;

{ TDiscoverServerThread }

function TDiscoverServerThread.GetErrorState: TDiscoverErrorState;
begin
  FCritical.Enter;
  Result := FErrorState;
  FCritical.Leave;
end;

procedure TDiscoverServerThread.Execute;
var
  Buffer: string;
  PakNo: integer;
begin
  {$IFDEF DEBUG} Log('Discover Server: Running ...'); {$ENDIF}

  try
    {$IFDEF DEBUG} Log('Discover Server: Bind ...'); {$ENDIF}
    FSocket.Bind('0.0.0.0', IntToStr(FPort));
    {$IFDEF DEBUG} Log('Discover Server: Bind ... DONE'); {$ENDIF}

    if FSocket.LastError <> 0 then
    begin
      Log(Format('Discover Server: Bind failed with error code %d', [FSocket.LastError]));

      FCritical.Enter;
      FErrorState := desBindError;
      FCritical.Leave;

      while not Terminated do Sleep(100);
    end
    else
    begin
      while not Terminated do
      begin
        // wait for new packet
        //{$IFDEF DEBUG} Log('Discover Server: RecvPacket ...'); {$ENDIF}
        Buffer := FSocket.RecvPacket(1000);

        if FSocket.LastError = 0 then
        begin
          {$IFDEF DEBUG} Log('Discover Server: Recieved "' + buffer + '"'); {$ENDIF}

          if Buffer = 'REQUEST:CLOCKNAME' then
          begin
            {$IFDEF DEBUG} Log('Discover Server: Received REQUEST:CLOCKNAME ...'); {$ENDIF}

            // Send packet clock name
            Buffer := FClockName + #0;
            FSocket.SendString('CLOCKNAME:' +  Buffer);
            {$IFDEF DEBUG} Log('Discover Server: Sent "' + Buffer + '"'); {$ENDIF}
          end
          else if FSocket.LastError <> WSAETIMEDOUT then
          begin
            FCritical.Enter;
            FErrorState := desSocketError;
            FCritical.Leave;

            Log(Format('Discover Server: RecvPacket failed with error code %d', [FSocket.LastError]));
            Log(Format('Discover Server: Set error state!', [FSocket.LastError]));
            while not Terminated do Sleep(100);
          end;
        end
        else
        begin
          //{$IFDEF DEBUG} Log('Discover Server: RecvPacket ... DONE'); {$ENDIF}
        end;
      end;
      FSocket.CloseSocket;
    end;
  except
    on E: exception do
    begin
      FCritical.Enter;
      FErrorState := desException;
      FCritical.Leave;

      Log('Discover Server: Fatal Exception!');
      Log('Fatal Exception: ' + E.Message);
      Log('Discover Server: Set error state!');
      while not Terminated do Sleep(100);
    end;
  end;

  {$IFDEF DEBUG} Log('Discover Server: Stopped ...'); {$ENDIF}
end;

constructor TDiscoverServerThread.Create(Port: Integer; ClockName: string);
begin
  inherited Create(False);
  FCritical := TCriticalSection.Create;

  FErrorState := desOk;
  FSocket := TUDPBlockSocket.Create;
  FSocket.ConvertLineEnd := True;
  FSocket.EnableReuse(True);
  FSocket.EnableBroadcast(True);

  FPort := Port;
  FClockName := ClockName;
  Randomize;
end;

destructor TDiscoverServerThread.Destroy;
begin
  FSocket.Free;
  FCritical.Free;

  inherited Destroy;
end;

end.


