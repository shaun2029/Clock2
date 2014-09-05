//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit udpserver;

{$mode objfpc}{$H+}
// {$DEFINE DEBUG}

interface

uses
  Classes, SysUtils, LCLProc, SyncObjs,

  // synapse
  blcksock;

type

  { TUDPServerThread }

  TUDPServerThread = class(TThread)
  protected
    FSocket: TUDPBlockSocket;
    FData: TStringList;

    procedure Execute; override;
  public
    Critical: TCriticalSection;

    constructor Create(Data: string);
    destructor Destroy; override;

  published
    property Data: TStringList read FData;
  end;

  TUDPServer = class
  private
    FUDPServerThread: TUDPServerThread;

    procedure Stop;
    procedure Start(Data: string);

    function GetRunning: Boolean;
  public
    constructor Create;

    procedure Send(Data: string);
    property Running: Boolean read GetRunning;
  end;

implementation

procedure Log(Message: string);
begin
  DebugLn(Message);
end;

{ TUDPServer }

function TUDPServer.GetRunning: Boolean;
begin
  Result := FUDPServerThread <> nil;
end;

constructor TUDPServer.Create;
begin
  FUDPServerThread := nil;
end;

procedure TUDPServer.Send(Data: string);
begin
  if FUDPServerThread = nil then
    Start(Data)
  else
  begin
    FUDPServerThread.Critical.Enter;
    FUDPServerThread.Data.Text := Data;
    FUDPServerThread.Critical.Leave;
  end;
end;

procedure TUDPServer.Start(Data: string);
begin
  FUDPServerThread := TUDPServerThread.Create(Data);
end;

procedure TUDPServer.Stop;
begin
  if FUDPServerThread <> nil then
  begin
    FUDPServerThread.Terminate;
    FUDPServerThread.WaitFor;
    FreeAndNil(FUDPServerThread);
  end;
end;

{ TUDPServerThread }

procedure TUDPServerThread.Execute;
var
  Buffer: string;
  DataBuff: string;
  Pos: integer;
  Total: integer;
  PakNo: integer;
begin
  {$IFDEF DEBUG} Log('UPDSRV: Running ...'); {$ENDIF}

  try
    FSocket.Bind('0.0.0.0', '44559');

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
            if Buffer = 'REQUEST REMINDERS' then
            begin
              {$IFDEF DEBUG} Log('UPDSRV: Received REQUEST REMINDERS ...'); {$ENDIF}

              Critical.Enter;
              Total := FData.Count;
              DataBuff := FData.Text;
              Critical.Leave;

              // Send packet with reminder total
              FSocket.SendString('REMINDERS:' + IntToStr(Total));

              Pos := 1;
              PakNo := 1;

			        // Send the reminders in packets of <= 512 bytes
              while Pos < Length(DataBuff) do
              begin
                Buffer := 'REMPAK:' + IntToStr(PakNo) + ';' + Copy(DataBuff, Pos, 500);

                FSocket.SendString(Buffer);
                Inc(Pos, 500);
                Inc(PakNo);
              end;

              {$IFDEF DEBUG} Log('UPDSRV: Sent REMINDERS ...'); {$ENDIF}
            end;
          end;
        end;
      finally
        FSocket.CloseSocket;
      end;
    end;
  finally
  end;

  {$IFDEF DEBUG} Log('UPDSRV: Stopped ...'); {$ENDIF}
end;

constructor TUDPServerThread.Create(Data: string);
begin
  inherited Create(False);

  FSocket := TUDPBlockSocket.Create;

  Critical := TCriticalSection.Create;
  FData := TStringList.Create;
  FData.Text := Data;
end;

destructor TUDPServerThread.Destroy;
begin
  FData.Free;
  Critical.Free;
  FSocket.CloseSocket;
  FSocket.Free;

  inherited Destroy;
end;

end.


