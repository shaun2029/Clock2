unit webcontrol;

{$mode delphi}

interface

uses
  Classes, blcksock, sockets, Synautil, SysUtils, SyncObjs;

type
  { TSimpleWebControl }
  TRemoteCommand = (rcomNone, rcomNext, rcomPrevious, rcomMusic, rcomSleep, rcomMeditation, rcomPause, rcomVolumeUp,
    rcomVolumeDown, rcomDisplayToggle, rcomSetRadioStation, rcomRadio, rcomFavorite, rcomChannelUp, rcomChannelDown);

  TSimpleWebControl = class
    private
      FCommand: TRemoteCommand;
      FCritical: TCriticalSection;
      ListenerSocket, ConnectionSocket: TTCPBlockSocket;
      FPlaying: string;
      FHostName: string;
      FTemprature: single;

      procedure AttendConnection(ASocket: TTCPBlockSocket);
      procedure CreateFile(Filename: string);
      function GetCommand: TRemoteCommand;
      procedure SetHostName(AValue: string);
      procedure SetPlaying(const AValue: string);
    public
      constructor Create;
      destructor Destroy; override;

      procedure ProccessConnections;

      procedure Lock();
      procedure Unlock();
    published
      property Command: TRemoteCommand read GetCommand;
      property Playing: string write SetPlaying;
      property HostName: string write SetHostName;
      property Temprature: single write FTemprature;
  end;

implementation

constructor TSimpleWebControl.Create;
var
  err: Integer;
begin
  FPlaying := '';
  FHostName := '';
  FCritical := TCriticalSection.Create;

  ListenerSocket := TTCPBlockSocket.Create;
  ConnectionSocket := TTCPBlockSocket.Create;

  ListenerSocket.CreateSocket;
  ListenerSocket.SetLinger(True, 10);
  ListenerSocket.Bind('0.0.0.0','8080');

  if (ListenerSocket.LastError) <> 0 then
  begin
    ListenerSocket.Bind('0.0.0.0','80');

    if (ListenerSocket.LastError) = 0 then
      WriteLn('Started HTTP control on port 8080')
    else
      WriteLn('Error: Failed to start HTTP control on ports 80 and 8080');
  end
  else
  begin
     WriteLn('Started HTTP control on port 80');
  end;

  ListenerSocket.listen;
end;

destructor TSimpleWebControl.Destroy;
begin
  ListenerSocket.Free;
  ConnectionSocket.Free;

  inherited Destroy;
end;

procedure TSimpleWebControl.ProccessConnections;
begin
  if ListenerSocket.canread(0) then
  begin
    ConnectionSocket.Socket := ListenerSocket.accept;
    //WriteLn('Attending Connection. Error code (0=Success): ', ConnectionSocket.lasterror);
    AttendConnection(ConnectionSocket);
    ConnectionSocket.CloseSocket;
  end;
end;

procedure TSimpleWebControl.Lock;
begin
  FCritical.Enter;
end;

procedure TSimpleWebControl.Unlock;
begin
  FCritical.Leave;
end;

function UppercaseFirstChar(s: String): String;
var
  ch, rest: String;
begin
  ch := Copy(s, 1, 1);
  rest := Copy(s, Length(ch)+1, MaxInt);
  Result := Uppercase(ch) + rest
end;

{@@
  Attends a connection. Reads the headers and gives an
  appropriate response
}
procedure TSimpleWebControl.AttendConnection(ASocket: TTCPBlockSocket);
var
  timeout: integer;
  s: string;
  method, uri, protocol: string;
  OutputDataString, HostName: string;
  ResultCode: integer;
  i: Integer;
  Command: TRemoteCommand;
begin
  timeout := 12000;

//  WriteLn('Received headers+document from browser:');

  //read request line
  s := ASocket.RecvString(timeout);
//  WriteLn(s);
  method := fetch(s, ' ');
  uri := fetch(s, ' ');
  protocol := fetch(s, ' ');
{
  //read request headers
  repeat
    s := ASocket.RecvString(Timeout);
    WriteLn(s);
  until s = '';
}
  // Now write the document to the output stream

  Command := rcomNone;

  OutputDataString := '';

  if (uri = '/playing?') or (uri = '/playing') then
  begin
    OutputDataString :=
        FPlaying;
    // Write the document back to the browser
    ASocket.SendString(OutputDataString);
  end
  else if (uri = '/next?') or (uri = '/next') then
  begin
    Command := rcomNext;
  end
  else if (uri = '/pause?') or (uri = '/pause') then
  begin
    Command := rcomPause;
  end
  else if (uri = '/previous?') or (uri = '/previous') then
  begin
    Command := rcomPrevious;
  end
  else if (uri = '/volup?') or (uri = '/volup') then
  begin
    Command := rcomVolumeUp;
  end
  else if (uri = '/voldown?') or (uri = '/voldown') then
  begin
    Command := rcomVolumeDown;
  end
  else if (uri = '/chandown?') or (uri = '/chandown') then
  begin
    Command := rcomChannelDown;
  end
  else if (uri = '/chanup?') or (uri = '/chanup') then
  begin
    Command := rcomChannelUp;
  end;

  if (Command <> rcomNone) then
  begin
    Lock();
    FCommand := Command;
    Unlock();

    while true do
    begin
      Sleep(100);
      Lock();
      Command := FCommand;
      Unlock();

      if (Command = rcomNone) then
        Break;
    end;

    // Write the document back to the browser
    ASocket.SendString('OK' + CRLF);

  end
  else if (OutputDataString = '') then
  begin
    Lock();
    HostName := FHostName;
    Unlock();

    // Write the output document to the stream
    OutputDataString :=
      '<!DOCTYPE HTML>' + CRLF
      + '<html>' + CRLF
      + '<head>' + CRLF
      + '    <title>Clock2 ' + UppercaseFirstChar(HostName) + '</title>' + CRLF
      + '    <meta http-equiv="Content-Type" content="text/html; charset=utf-8" />' + CRLF
      + '    <meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0" />' + CRLF
      + '</head>' + CRLF
      + '<body>' + CRLF
      + '<font size="6">' + CRLF
      + '<h3>Clock2 ' + UppercaseFirstChar(HostName) +'</h3>' + CRLF
      + '<p id="display" >Updating...</p>' + CRLF
      + '</font>' + CRLF
      + '<button id="previous" style="padding: 15px 25px;"><font size="6"><|</font></button>' + CRLF
      + '<button id="pause" style="padding: 15px 25px;"><font size="6">||</font></button>' + CRLF
      + '<button id="next" style="padding: 15px 25px;"><font size="6">|></font></button>' + CRLF
      + '<button id="voldown" style="padding: 15px 25px;"><font size="6">-</font></button>' + CRLF
      + '<button id="volup" style="padding: 15px 25px;"><font size="6">+</font></button>' + CRLF
      + '<br>' + CRLF
      + '<br>' + CRLF
      + '<button id="chandown" style="padding: 15px 10px;"><font size="6">Channel -</font></button>' + CRLF
      + '<button id="chanup" style="padding: 15px 10px;"><font size="6">Channel +</font></button>' + CRLF
      + '</table>' + CRLF
      + '<br>' + CRLF
      + '<br>' + CRLF
      + 'TEMPERATURE:' + FormatFloat('0.0', FTemprature) + CRLF
      + '<script>' + CRLF
      + 'var HttpClient = function() {' + CRLF
      + '    this.get = function(aUrl, aCallback) {' + CRLF
      + '        var anHttpRequest = new XMLHttpRequest();' + CRLF
      + '        anHttpRequest.onreadystatechange = function() {' + CRLF
      + '            if (anHttpRequest.readyState == 4 && anHttpRequest.status == 200)' + CRLF
      + '                aCallback(anHttpRequest.responseText);' + CRLF
      + '        }' + CRLF
      + '' + CRLF
      + '        anHttpRequest.open( "GET", aUrl, true );' + CRLF
      + '        anHttpRequest.send( null );' + CRLF
      + '    }' + CRLF
      + '}' + CRLF
      + '' + CRLF
      + 'function updatePlaying() {' + CRLF
      + '    var client = new HttpClient();' + CRLF
      + '    client.get(''/playing'', function(response) {' + CRLF
      + '        document.getElementById("display").innerHTML = response;' + CRLF
      + '    });' + CRLF
      + '' + CRLF
      + '    setTimeout(updatePlaying, 2000);' + CRLF
      + '}' + CRLF
      + '' + CRLF
      + 'setTimeout(updatePlaying,2000);' + CRLF
      + '' + CRLF
      + 'var buttons = document.getElementsByTagName("button");' + CRLF
      + 'var buttonsCount = buttons.length;' + CRLF
      + 'for (var i = 0; i <= buttonsCount; i += 1) {' + CRLF
      + '    buttons[i].onclick = function(e) {' + CRLF
      + '        var client = new HttpClient();' + CRLF
      + '        client.get(''/'' + this.id, function(response) {' + CRLF
      + '        });' + CRLF
      + '    };' + CRLF
      + '}' + CRLF
      + '</script>' + CRLF
      + '</body>' + CRLF
      + '</html>' + CRLF;

    // Write the headers back to the client
    ASocket.SendString('HTTP/1.0 200' + CRLF);
    ASocket.SendString('Content-type: Text/Html' + CRLF);
    ASocket.SendString('Content-length: ' + IntTostr(Length(OutputDataString)) + CRLF);
    ASocket.SendString('Connection: close' + CRLF);
    ASocket.SendString('Date: ' + Rfc822DateTime(now) + CRLF);
    ASocket.SendString('Server: Servidor do Felipe usando Synapse' + CRLF);
    ASocket.SendString('' + CRLF);

    // Write the document back to the browser
    ASocket.SendString(OutputDataString);
  end;
end;

procedure TSimpleWebControl.CreateFile(Filename: string);
var
  FS: TFileStream = nil;
  sOut: string;
  i: Integer;
  Flags: Word;
begin
  try
    if not FileExists(Filename) then
    begin
      Flags := fmOpenReadWrite or fmCreate;
      FS := TFileStream.Create(Filename, Flags);
    end;
  finally
    if Assigned(FS) then
      FS.Free;
  end;
end;

function TSimpleWebControl.GetCommand: TRemoteCommand;
begin
  Lock();
  Result := FCommand;
  FCommand := rcomNone;
  Unlock();
end;

procedure TSimpleWebControl.SetHostName(AValue: string);
begin
  Lock();
  FHostName := AValue;
  Unlock();
end;

procedure TSimpleWebControl.SetPlaying(const AValue: string);
begin
  Lock();
  FPlaying := AValue;
  Unlock();
end;


end.

