unit DiscoverServer;

//
// Copyright 2014 Shaun Simpson
// shauns2029@gmail.com
//

{$mode Delphi}
//{$DEFINE DEBUG}

interface

uses
  Classes, SysUtils,
  // synapse
  blcksock, synsock;

type
  TErrorState = (esOK, esSocketError, esException);

  { TUDPServerThread }

  { TDiscoverServerThread }

  TDiscoverServerThread = class(TThread)
  private
    FErrorState: TErrorState;
  protected
    FSocket: TUDPBlockSocket;
    FPort: Integer;
    FClockName: string;

    procedure Execute; override;
  public
    constructor Create(Port: Integer; ClockName: string);
    destructor Destroy; override;
    property Error: TErrorState read FErrorState;
  end;

  { TDiscoverServer }

  TDiscoverServer = class
  private
    FDiscoverServerThread: TDiscoverServerThread;
    function GetErrorState: TErrorState;
  public
    constructor Create(Port: Integer; ClockName: string);
    destructor Destroy; override;
  published
    property Error: TErrorState read GetErrorState;
  end;

implementation

procedure Log(Message: string);
begin
  WriteLn(Message);
end;

{ TDiscoverServer }

function TDiscoverServer.GetErrorState: TErrorState;
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

procedure TDiscoverServerThread.Execute;
var
  Buffer: string;
  DataBuff: string;
  Pos: integer;
  Total: integer;
  PakNo: integer;
begin
  {$IFDEF DEBUG} Log('Discover Server: Running ...'); {$ENDIF}

  try
    FSocket.Bind('0.0.0.0', IntToStr(FPort));

    if FSocket.LastError <> 0 then
    begin
      Log(Format('Discover Server: Bind failed with error code %d', [FSocket.LastError]));
      while not Terminated do Sleep(1000);
    end
    else
    begin
      while not Terminated do
      begin
        // wait two second for new packet
        Buffer := FSocket.RecvPacket(2000);

        if FSocket.LastError = 0 then
        begin
          {$IFDEF DEBUG} Log('Discover Server: Received packet ...'); {$ENDIF}
          {$IFDEF DEBUG} Log('Discover Server: "' + buffer + '"'); {$ENDIF}

          if Buffer = 'REQUEST:CLOCKNAME' then
          begin
            {$IFDEF DEBUG} Log('Discover Server: Received REQUEST:CLOCKNAME ...'); {$ENDIF}

            // Send packet clock name
            Buffer := FClockName + #0#0#0#0#0#0#0#0#0#0;
            FSocket.SendString('CLOCKNAME:' +  Buffer);

            {$IFDEF DEBUG} Log('Discover Server: Sent "' + Buffer + '"'); {$ENDIF}
          end
          else if FSocket.LastError <> WSAETIMEDOUT then
          begin
            Log(Format('Discover Server: RecvPacket failed with error code %d', [FSocket.LastError]));
            Log(Format('Discover Server: Set error state!', [FSocket.LastError]));
            while not Terminated do Sleep(100);
          end;
        end;
      end;
    end;
  except
    on E: exception do
    begin
      Log('Discover Server: Fatal Exception!');
      Log('Fatal Exception: ' + E.Message);
      Log('Discover Server: Set error state!');
      while not Terminated do Sleep(100);
    end;
  end;

  FSocket.CloseSocket;

  {$IFDEF DEBUG} Log('Discover Server: Stopped ...'); {$ENDIF}
end;

constructor TDiscoverServerThread.Create(Port: Integer; ClockName: string);
begin
  inherited Create(False);

  FErrorState := esOk;
  FSocket := TUDPBlockSocket.Create;

  FPort := Port;
  FClockName := ClockName;
end;

destructor TDiscoverServerThread.Destroy;
begin
  if Assigned(FSocket) then
  begin
    FSocket.CloseSocket;
    FreeAndNil(FSocket);
  end;

  inherited Destroy;
end;

end.


