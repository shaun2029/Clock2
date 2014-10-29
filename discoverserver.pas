unit DiscoverServer;

//
// Copyright 2014 Shaun Simpson
// shauns2029@gmail.com
//

{$mode objfpc}{$H+}
// {$DEFINE DEBUG}

interface

uses
  Classes, SysUtils,
  // synapse
  blcksock;

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
      Log(Format('Bind failed with error code %d', [FSocket.LastError]));
      while not Terminated do Sleep(100);
    end
    else
    begin
      try
        while not Terminated do
        begin
          // wait one second for new packet
          Buffer := FSocket.RecvPacket(1000);

          if FSocket.LastError = 0 then
          begin
            if Buffer = 'REQUEST:CLOCKNAME' then
            begin
              {$IFDEF DEBUG} Log('Discover Server: Received REQUEST:CLOCKNAME ...'); {$ENDIF}

              // Send packet with reminder total
              FSocket.SendString('CLOCKNAME:'+ FClockName);

              {$IFDEF DEBUG} Log('Discover Server: Sent REMINDERS ...'); {$ENDIF}
            end;
          end;
        end;
      finally
        FSocket.CloseSocket;
      end;
    end;
  finally
  end;

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
  FSocket.CloseSocket;
  FSocket.Free;

  inherited Destroy;
end;

end.


