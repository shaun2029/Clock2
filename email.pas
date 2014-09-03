
unit Email;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lSMTP, lNet, lNetSSL, lNetComponents;

type

  { TForm1 }

  { TEmail }

  TEmail = class
  private
    { private declarations }
    SSL: TLSSLSessionComponent;
    SMTP: TLSMTPClientComponent;
    FFrom, FRecipients, FSubject, FMsg: string;

    procedure Authenticate;
    procedure SMTPConnect(aSocket: TLSocket);
    procedure SMTPError(const msg: string; aSocket: TLSocket);
    procedure SMTPFailure(aSocket: TLSocket; const aStatus: TLSMTPStatus);
    procedure SMTPReceive(aSocket: TLSocket);
    procedure SMTPSuccess(aSocket: TLSocket; const aStatus: TLSMTPStatus);
    procedure SSLSSLConnect(aSocket: TLSocket);
  public
    { public declarations }
    Error: string;
    Success: Boolean;
    function Send(From, Recipients, Subject, Msg: string): boolean;
  end;

implementation

{ TEmail }

function TEmail.Send(From, Recipients, Subject, Msg: string): boolean;
begin
  Success := False;
  Error := '';
  FFrom := From;
  FRecipients := Recipients;
  FSubject := Subject;
  FMsg := Msg;

  SMTP := TLSMTPClientComponent.Create(nil);
  SSL := TLSSLSessionComponent.Create(nil);
  SMTP.Session := SSL;
  SSL.SSLActive := True;
  SSL.OnSSLConnect := @SSLSSLConnect;
  SMTP.OnReceive:= @SMTPReceive;
  SMTP.OnConnect:= @SMTPConnect;
  SMTP.OnError := @SMTPError;
  SMTP.OnFailure := @SMTPFailure;
  SMTP.OnSuccess := @SMTPSuccess;
  Result := SMTP.Connect('smtp.gmail.com', 465);
  //SMTP.StartTLS;
end;

procedure TEmail.SMTPFailure(aSocket: TLSocket;
  const aStatus: TLSMTPStatus);
begin
  case aStatus of
    ssCon,
    ssEhlo: Authenticate;
    ssData: begin
              SMTP.Rset;
            end;
    ssQuit: begin
              SMTP.Disconnect;
            end;
  end;
end;

procedure TEmail.SMTPError(const msg: string; aSocket: TLSocket);
begin
  Error := msg;
end;

procedure TEmail.SMTPSuccess(aSocket: TLSocket; const aStatus: TLSMTPStatus);
begin
case aStatus of
  ssCon : begin
            if SMTP.HasFeature('EHLO') then // check for EHLO support
              SMTP.Ehlo('smtp.gmail.com')
            else
              SMTP.Helo('smtp.gmail.com');
          end;
  ssEhlo: Authenticate;
  ssAuthLogin,
  ssAuthPlain : Authenticate;
  ssData: Success := True;
  ssQuit: begin
            SMTP.Disconnect;
          end;
end;
end;

procedure TEmail.Authenticate;
begin
  if SMTP.HasFeature('AUTH LOGIN') then // use login if possible
    SMTP.AuthLogin('clock2utility', 'jtQdjd1v17648977frwnbmFs')
  else if SMTP.HasFeature('AUTH PLAIN') then // fall back to plain if possible
    SMTP.AuthPlain('clock2utility', 'jtQdjd1v17648977frwnbmFs');
end;

procedure TEmail.SMTPConnect(aSocket: TLSocket);
begin
  SMTP.SendMail(FFrom, FRecipients, FSubject, FMsg);
end;

procedure TEmail.SMTPReceive(aSocket: TLSocket);
var
  s, st: string;
begin
  if SMTP.GetMessage(s) > 0 then
  begin
    st := StringReplace(s, #13, '', [rfReplaceAll]);
    st := StringReplace(st, #10, '', [rfReplaceAll]);
    Error := st;
  end;
end;

procedure TEmail.SSLSSLConnect(aSocket: TLSocket);
begin
  SMTP.Ehlo;
end;


end.

