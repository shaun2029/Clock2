// Code to support older Lazarus/FPC
unit process_legacy;


{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, process;

function RunCommandIndir(const curdir:string;const exename:string;const commands:array of string;var outputstring:string;var exitstatus:integer):integer;
function RunCommandIndir(const curdir:string;const exename:string;const commands:array of string;var outputstring:string):boolean;
function RunCommandInDir(const curdir,cmdline:string;var outputstring:string):boolean; deprecated;

function RunCommand(const exename:string;const commands:array of string;var outputstring:string):boolean;
function RunCommand(const cmdline:string;var outputstring:string):boolean; deprecated;

Const
  READ_BYTES = 65536; // not too small to avoid fragmentation when reading large files.

implementation

// helperfunction that does the bulk of the work.
function internalRuncommand(p:TProcess;var outputstring:string;var exitstatus:integer):integer;
var
    numbytes,bytesread : integer;
begin
  result:=-1;
  try
    try
    p.Options :=  [poUsePipes];
    bytesread:=0;
    p.Execute;
    while p.Running do
      begin
        Setlength(outputstring,BytesRead + READ_BYTES);
        NumBytes := p.Output.Read(outputstring[1+bytesread], READ_BYTES);
        if NumBytes > 0 then
          Inc(BytesRead, NumBytes)
        else
          Sleep(100);
      end;
    repeat
      Setlength(outputstring,BytesRead + READ_BYTES);
      NumBytes := p.Output.Read(outputstring[1+bytesread], READ_BYTES);
      if NumBytes > 0 then
        Inc(BytesRead, NumBytes);
    until NumBytes <= 0;
    setlength(outputstring,BytesRead);
    exitstatus:=p.exitstatus;
    result:=0; // we came to here, document that.
    except
      on e : Exception do
         begin
           result:=1;
           setlength(outputstring,BytesRead);
         end;
     end;
  finally
    p.free;
    end;
end;

function RunCommandIndir(const curdir:string;const exename:string;const commands:array of string;var outputstring:string;var exitstatus:integer):integer;
Var
    p : TProcess;
    i : integer;
begin
  p:=TProcess.create(nil);
  p.Executable:=exename;
  if curdir<>'' then
    p.CurrentDirectory:=curdir;
  if high(commands)>=0 then
   for i:=low(commands) to high(commands) do
     p.Parameters.add(commands[i]);
  result:=internalruncommand(p,outputstring,exitstatus);
end;

function RunCommandInDir(const curdir,cmdline:string;var outputstring:string):boolean; deprecated;
Var
    p : TProcess;
    exitstatus : integer;
begin
  p:=TProcess.create(nil);
  p.commandline:=cmdline;
  if curdir<>'' then
    p.CurrentDirectory:=curdir;
  result:=internalruncommand(p,outputstring,exitstatus)=0;
  if exitstatus<>0 then result:=false;
end;

function RunCommandIndir(const curdir:string;const exename:string;const commands:array of string;var outputstring:string):boolean;
Var
    p : TProcess;
    i,
    exitstatus : integer;
begin
  p:=TProcess.create(nil);
  p.Executable:=exename;
  if curdir<>'' then
    p.CurrentDirectory:=curdir;
  if high(commands)>=0 then
   for i:=low(commands) to high(commands) do
     p.Parameters.add(commands[i]);
  result:=internalruncommand(p,outputstring,exitstatus)=0;
  if exitstatus<>0 then result:=false;
end;

function RunCommand(const cmdline:string;var outputstring:string):boolean; deprecated;
Var
    p : TProcess;
    exitstatus : integer;
begin
  p:=TProcess.create(nil);
  p.commandline:=cmdline;
  result:=internalruncommand(p,outputstring,exitstatus)=0;
  if exitstatus<>0 then result:=false;
end;

function RunCommand(const exename:string;const commands:array of string;var outputstring:string):boolean;
Var
    p : TProcess;
    i,
    exitstatus : integer;
begin
  p:=TProcess.create(nil);
  p.Executable:=exename;
  if high(commands)>=0 then
   for i:=low(commands) to high(commands) do
     p.Parameters.add(commands[i]);
  result:=internalruncommand(p,outputstring,exitstatus)=0;
  if exitstatus<>0 then result:=false;
end;

end.

