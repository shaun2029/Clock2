unit mpd;

{$mode delphi}

interface

uses
  Classes, SysUtils, Process, process_legacy;

  function MPDCommand(const Host, Port, Command: string): boolean; overload;
  function MPDCommand(const Host, Port, Command: string;
    var Output: string): boolean; overload;
  function MPDParseURL(const FStreamURL: string; out Host, Port: string): string;
  function MPDGetTrackTitle(const Host, Port: string): string;

implementation

function MPDParseURL(const FStreamURL: string; out Host, Port: string): string;
var
  SPos, EPos: integer;
begin
  SPos := Pos('MPD_HOST=', UpperCase(FStreamURL));

  if SPos > 0 then
  begin
    Host := Copy(FStreamURL, SPos + 9, Length(FStreamURL));
    EPos := Pos(' ', Host);
    if EPos > 0 then
       Host := Copy(Host, 1, EPos - 1);
  end
  else Host := '';

  SPos := Pos('MPD_PORT=', UpperCase(FStreamURL));

  if SPos > 0 then
  begin
    Port := Copy(FStreamURL, SPos + 9, Length(FStreamURL));
    EPos := Pos(' ', Port);
    if EPos > 0 then
       Port := Copy(Port, 1, EPos - 1);
  end
  else Port := '';

  EPos := Pos('MPD_HOST=', FStreamURL);

  if EPos > 0 then
  begin
    Result := Trim(Copy(FStreamURL, 1, EPos - 1));
  end
  else Result := Trim(FStreamURL);
end;

function MPDCommand(const Host, Port, Command: string): boolean;
var
  Output: string;
  CommandLine: string;
begin
  CommandLine := '';

  if Host <> '' then
    CommandLine := CommandLine + ' MPD_HOST=' + Host;

  if Port <> '' then
    CommandLine := CommandLine + ' MPD_PORT=' + Port;

  CommandLine := CommandLine + ' mpc';

  if Command <> '' then
    CommandLine := CommandLine + ' ' + Command;

  Result := RunCommand('bash -c "' + CommandLine + '"', Output);
end;

function MPDCommand(const Host, Port, Command: string;
  var Output: string): boolean;
var
  CommandLine: string;
  Commands: array of string;
  Process: TProcess;
  Len: Integer;
begin
  Result := True;
  CommandLine := '';
  Output := '';

  Process := TProcess.Create(nil);
  Process.CommandLine := '';

  if Host <> '' then
    Process.Environment.Add('MPD_HOST=' + Host);

  if Port <> '' then
    Process.Environment.Add('MPD_PORT=' + Port);

  CommandLine := 'mpc';

  if Command <> '' then
     Process.Parameters.Add(Command);

  Process.CommandLine := CommandLine;
  Process.Options := [poStderrToOutPut, poUsePipes, poWaitOnExit];
  try
  Process.Execute;
  if (Process.Output.NumBytesAvailable > 0) then
  begin
    Len := Process.Output.NumBytesAvailable;
    SetLength(Output, Len);
    Process.Output.Read(PChar(@Output[1])^, Len);
  end;
  finally
    Result := False;
  end;
  Process.Free;
end;

function MPDGetTrackTitle(const Host, Port: string): string;
var
  Title: string;
  Len: integer;
begin
  Title := '';
  MPDCommand(Host, Port, '', Title);

  Len := Pos(LineEnding, Title);
  if (Len > 1) then
     Title := Copy(Title, 1, Len - 1);

  Result := Title;
end;

end.

