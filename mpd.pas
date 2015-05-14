unit mpd;

{$mode delphi}

interface

uses
  Classes, SysUtils, Process;

  function MPDCommand(const Host, Port, Command: string): boolean;
  function MPDParseURL(const FStreamURL: string; out Host, Port: string): string;

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

  EPos := Pos(' ', FStreamURL);

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


end.

