unit DiscoverServer;

//
// Copyright 2014 Shaun Simpson
// shauns2029@gmail.com
//

{$mode objfpc}{$H+}
//{$DEFINE DEBUG}

interface

uses
  Classes, SysUtils,
  // synapse
  blcksock, synsock;

type

  { TUDPServerThread }

  TDiscoverServerThread = class(TThread)
  protected
    FSocket: TUDPBlockSocket;
    FPort: Integer;
    FClockName: string;

    procedure Execute; override;
  public
    constructor Create(Port: Integer; ClockName: string);
    destructor Destroy; override;
  end;

  { TDiscoverServer }

  TDiscoverServer = class
  private
    FDiscoverServerThread: TDiscoverServerThread;
  public
    constructor Create(Port: Integer; ClockName: string);
    destructor Destroy; override;
  end;

implementation

procedure Log(Message: string);
begin
  WriteLn(Message);
end;

{ TDiscoverServer }

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
      try
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
              FSocket.SendString('CLOCKNAME:' + FClockName + #0);

              {$IFDEF DEBUG} Log('Discover Server: Sent "' + FClockName + '"'); {$ENDIF}
            end
            else if FSocket.LastError <> WSAETIMEDOUT then
            begin
              Log(Format('Discover Server: RecvPacket failed with error code %d', [FSocket.LastError]));
              Log('Discover Server: Rebinding socket ...');

              FSocket.CloseSocket;
              FSocket.Bind('0.0.0.0', IntToStr(FPort));

              if FSocket.LastError <> 0 then
              begin
                Log(Format('Discover Server: Bind failed with error code %d', [FSocket.LastError]));
                while not Terminated do Sleep(1000);
              end;

              Log('Discover Server: Restarted.');
            end;
          end;
        end;
      finally
      end;
    end;
  except
    on E: exception do
    begin
      Log('Discover Server: Fatal Exception!');
      Log('Fatal Exception: ' + E.Message);
    end;
  end;

  if Assigned(FSocket) then
    FSocket.CloseSocket;

  {$IFDEF DEBUG} Log('Discover Server: Stopped ...'); {$ENDIF}
end;

constructor TDiscoverServerThread.Create(Port: Integer; ClockName: string);
begin
  inherited Create(False);

  FSocket := TUDPBlockSocket.Create;

  FPort := Port;
  FClockName := ClockName;
end;

destructor TDiscoverServerThread.Destroy;
begin
  if Assigned(FSocket) then
  begin
    FSocket.CloseSocket;
    FSocket.Free;
  end;

  inherited Destroy;
end;

end.


