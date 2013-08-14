unit ConnectionHealth;

{$mode delphi}

interface

uses
  Classes, SysUtils, BlckSock, PingSend, Process;

type

  { TConnectionHealth }

  TConnectionHealth = class
  private
    FAddress: string;
    FAutoReset: boolean;
    FHost: string;
    FResetCommand: string;
    procedure Execute(Command: string);
  public
    constructor Create;
    destructor Destroy; override;

    function TestConnection: boolean;
    procedure ResetWifi;
  published
    property Host: string read FHost write FHost;
    property ResetCommand: string read FResetCommand write FResetCommand;
    property AutoReset: boolean read FAutoReset write FAutoReset;
  end;

implementation

{ TConnectionHealth }

constructor TConnectionHealth.Create;
begin
  inherited Create;

  FResetCommand := '';
  AutoReset := False;
end;

destructor TConnectionHealth.Destroy;
begin
  inherited Destroy;
end;

function TConnectionHealth.TestConnection: boolean;
var
  Socket: TTCPBlockSocket;
  Buffer: string;
  Error: integer;
begin
  Socket := TTCPBlockSocket.Create;
  Socket.CreateSocket;
  Socket.TTL := 128;
  Socket.Bind('0.0.0.0',cAnyPort);
  Socket.Connect(FHost, '0');

  Error := Socket.LastError;
  Socket.Free;

  Result  := (Error = 0) or (Error = 111);

  if not Result and FAutoReset then
  begin
    ResetWifi;
  end;
end;

procedure TConnectionHealth.ResetWifi;
begin
  Execute(FResetCommand);
end;

procedure TConnectionHealth.Execute(Command: string);
var
 AProcess: TProcess;
 Timeout: TDateTime;
begin
 // Now we will create the TProcess object, and
 // assign it to the var AProcess.
 AProcess := TProcess.Create(nil);

 // Tell the new AProcess what the command to execute is.
 // Let's use the FreePascal compiler
 AProcess.CommandLine := Command;

 // We will define an option for when the program
 // is run.
 AProcess.Options := AProcess.Options;

 // Now that AProcess knows what the commandline is
 // we will run it.
 AProcess.Execute;

 // 30 second timeout
 Timeout := Now + EncodeTime(0, 0, 30, 0);

 repeat
       Sleep(500);
 until (Now > Timeout) or not AProcess.Running;

 // If the process is still running terminate it.
 if AProcess.Running then
   AProcess.Terminate(1);

 AProcess.Free;
end;


end.

