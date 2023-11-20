//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//

unit udpclient;

{$mode objfpc}{$H+}

interface

uses
  {$IFDEF WINDOWS}Windows,{$ENDIF}Classes, SysUtils, DateUtils, LCLProc,

  // synapse
  blcksock;

const
  cReceiveTimeout = 2000;
  cBatchSize = 100;

type
  TPacket = record
    No: integer;
	  Data: string;
  end;

  TPackets = array of TPacket;
  
  { TUDPClient }

  TUDPClient = class
  private
    FSocket: TUDPBlockSocket;
    function AssemblePackets(Packets: TPackets): string;
    procedure SetPacketData(var Packet: TPacket);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Disconnect;
    function Connect(const Address, Port: string): Boolean;
    function RequestReminders(Address, Port: string; out Reminders: string): boolean;
  end;

const
  MAX_REMINDER_PACKETS = 254;

implementation

{ TUDPClient }

constructor TUDPClient.Create;
begin
  FSocket := TUDPBlockSocket.Create;
end;

destructor TUDPClient.Destroy;
begin
  FreeAndNil(FSocket);

  inherited Destroy;
end;

procedure TUDPClient.Disconnect;
begin
  FSocket.CloseSocket;
end;

function TUDPClient.Connect(const Address, Port: string): Boolean;
begin
  FSocket.Connect(Address, Port);
  Result := FSocket.LastError = 0;
end;

function TUDPClient.RequestReminders(Address, Port: string; out Reminders: string): boolean;
var
  Buffer: string;
  Data: TStringList;
  RemTotal: integer;
  Packets: TPackets;
  PacketsRead: integer;
begin
  Result := False;
  Reminders := '';
  Data := TStringList.Create;
  RemTotal := -1;
  PacketsRead := 0;
  SetLength(Packets, 0);

  WriteLn('RequestReminders: ...');

  FSocket.CloseSocket;
  FSocket.EnableBroadcast(Address = '255.255.255.255');
  FSocket.Connect(Address, Port); // Default port 44559;
  FSocket.SendString('REQUEST REMINDERS');

  if FSocket.LastError = 0 then
  begin
    repeat
	    Buffer := FSocket.RecvPacket(cReceiveTimeout);
      if (FSocket.LastError <> 0) then
      begin
        if PacketsRead > 0 then
          WriteLn('RequestReminders: ERROR - Failed to get all reminder packets');
        Break;
      end;

      Inc(PacketsRead);
      if (PacketsRead > MAX_REMINDER_PACKETS) then
      begin
        WriteLn('RequestReminders: ERROR - Too many reminder packets');
        Break;
      end;

      Debugln(FSocket.SocksIP);

      if (FSocket.LastError = 0) then
      begin
        if Pos('REMINDERS:', Buffer) = 1 then
        begin
          Buffer := Copy(Trim(Buffer), 11, Length(Buffer));
          RemTotal := StrToIntDef(Buffer, -1);
        end
        else
        begin
          SetLength(Packets, Length(Packets) + 1);
	        Packets[Length(Packets) - 1].No := -1;
          Packets[Length(Packets) - 1].Data := Buffer;
        end;
   
        // Check if we have all the data
        if RemTotal > -1 then
        begin
          Data.Text := AssemblePackets(Packets);

  		    if Data.Count = RemTotal then
		      begin
		        Result := True;
			      Reminders := Data.Text;
		      end;
	      end
	    end;
	  until Result;
  end;

  WriteLn('RequestReminders: ... DONE');

  Data.Free;
end;

function TUDPClient.AssemblePackets(Packets: TPackets): string;
var
  Packet: TPacket;
  Count: integer;
  ValidPackets: TPackets;
  i: Integer;
  j: Integer;
begin
  Result := '';
  SetLength(ValidPackets, 0);

  Count := Length(Packets);

  for i := 0 to Count - 1 do
  begin
    SetPacketData(Packets[i]);
  
    if Packets[i].No > -1 then
    begin
      SetLength(ValidPackets, Length(ValidPackets) + 1);
      ValidPackets[Length(ValidPackets) - 1] := Packets[i];
    end;
  end;

  Count := Length(ValidPackets);

  // Sort packets
  for i := 0 to Count - 1 do
  begin
    for j := 0 to Count - 2 do
    begin
      if ValidPackets[j].No > ValidPackets[j+1].No then
      begin
        Packet := ValidPackets[j];
        ValidPackets[j] := ValidPackets[j + 1];
        ValidPackets[j + 1] := Packet;
      end;
    end;
  end;

  for i := 0 to Count - 1 do
  begin
    Result := Result + ValidPackets[i].Data;
  end;
end;

procedure TUDPClient.SetPacketData(var Packet: TPacket);
var
  Id: string;
begin
  if Packet.No <= -1 then
  begin
    if (Pos('REMPAK:', Packet.Data) = 1) and (Pos(';', Packet.Data) > 8) then
    begin
	    Id := Copy(Packet.Data, 8, Pos(';', Packet.Data) - 8);
      Packet.No := StrToIntDef(Id, -1);
      Packet.Data := Copy(Packet.Data, Pos(';', Packet.Data) + 1, Length(Packet.Data));
    end;
  end;	
end;

end.


