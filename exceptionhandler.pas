unit ExceptionHandler;

{$mode delphi}

interface

uses
  Classes, SysUtils;

procedure DumpExceptionCallStack(E: Exception);

implementation

procedure DumpExceptionCallStack(E: Exception);
var
  I: Integer;
  Frames: PPointer;
  Report: string;
  EFile: text;
  Filename: String;
begin
  Report := 'Program exception! ' + LineEnding +
    'Stacktrace:' + LineEnding + LineEnding;
  if E <> nil then begin
    Report := Report + 'Exception class: ' + E.ClassName + LineEnding +
    'Message: ' + E.Message + LineEnding;
  end;
  Report := Report + BackTraceStrFunc(ExceptAddr);
  Frames := ExceptFrames;
  for I := 0 to ExceptFrameCount - 1 do
    Report := Report + LineEnding + BackTraceStrFunc(Frames[I]);

  Filename := GetUserDir + 'Clock2-CrashDump-' + DateTimeToStr(Now) + '.txt';
  Assign(EFile, Filename);
  Rewrite(EFile);
  Writeln(EFile, Report);
  Close(EFile);

  writeln(Report);
  Halt; // End of program execution
end;

end.

