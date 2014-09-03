//
// Copyright 2012 Shaun Simpson
// shauns2029@gmail.com
//
// Email sending via Gmail SMTP
// Uses lNet components

unit Email;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, lSMTP, lNet, lNetSSL, lNetComponents;

type

  TAppProcessMessage = procedure of object;
  TSMTPState = (smConnecting, smConnected, smAuthenticating, smAuthenticated, smSuccess, smError);

  { TEmail }

  TEmail = class
  private
    { private declarations }
    SSL: TLSSLSessionComponent;
    SMTP: TLSMTPClientComponent;
    FFrom, FRecipients, FSubject, FMsg: string;
    FState: TSMTPState;
    FProcessMessages: TAppProcessMessage;
    FSMTPAccount: string;
    FSMTPPassword: string;

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

    constructor Create(SMTPAccount, SMTPPassword: string; ProcessMessages: TAppProcessMessage);

    function Send(From, Recipients, Subject, Msg: string): boolean;
  end;

implementation

{ TEmail }

function TEmail.Send(From, Recipients, Subject, Msg: string): boolean;
var
  Timeout: TDateTime;
begin
  Result := False;
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
  FState := smConnecting;
  Result := SMTP.Connect('smtp.gmail.com', 465);

  Timeout := Now + EncodeTime(0, 0, 30, 0);

  while (Timeout > Now) do
  begin
    Sleep(100);
    FProcessMessages;
    if (FState = smError) or (FState = smSuccess) then
      break;
  end;

  Result := (FState = smSuccess);

  SMTP.Free;
  SSL.Free;
end;

procedure TEmail.SMTPFailure(aSocket: TLSocket;
  const aStatus: TLSMTPStatus);
begin
  if (FState = smAuthenticating) or (aStatus = ssAuthPlain) or (aStatus = ssAuthLogin) then
    Error := 'Authentication error for account "' + FSMTPAccount + '".';

  case aStatus of
    ssCon,
    ssEhlo,
    ssData: begin
              SMTP.Rset;
            end;
    ssQuit: begin
              SMTP.Disconnect;
            end;
    ssRcpt: begin
              if Error = '' then
                Error := 'Recipient address error "' + FRecipients + '".';
            end;
  end;

  if Error = '' then
    Error := 'Unknown error.';

  FState := smError
end;

procedure TEmail.SMTPError(const msg: string; aSocket: TLSocket);
begin
  Error := msg;
  FState := smError;
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
    ssEhlo:
      begin
        Authenticate;
      end;
    ssData: begin
              if FState = smAuthenticated then
                FState := smSuccess;
            end;

    ssQuit: begin
              SMTP.Disconnect;
            end;
  end;
end;

procedure TEmail.Authenticate;
begin
  if FState = smConnected then
  begin
    if SMTP.HasFeature('AUTH LOGIN') then // use login if possible
      SMTP.AuthLogin(FSMTPAccount, FSMTPPassword)
    else if SMTP.HasFeature('AUTH PLAIN') then // fall back to plain if possible
      SMTP.AuthPlain(FSMTPAccount, FSMTPPassword);
    FState := smAuthenticating;
  end;
end;

procedure TEmail.SMTPConnect(aSocket: TLSocket);
begin
//  SMTP.StartTLS;
  SMTP.SendMail(FFrom, FRecipients, FSubject, FMsg);
  FState := smConnected;
end;

procedure TEmail.SMTPReceive(aSocket: TLSocket);
var
  s, st: string;
begin
  if SMTP.GetMessage(s) > 0 then
  begin
    st := StringReplace(s, #13, '', [rfReplaceAll]);
    st := StringReplace(st, #10, '', [rfReplaceAll]);

    if (FState = smAuthenticating) and (Pos ('235', st) = 1) then
      FState := smAuthenticated;
  end;
end;

procedure TEmail.SSLSSLConnect(aSocket: TLSocket);
begin
  SMTP.Ehlo;
end;

constructor TEmail.Create(SMTPAccount, SMTPPassword: string;
  ProcessMessages: TAppProcessMessage);
begin
  FSMTPAccount := SMTPAccount;
  FSMTPPassword := SMTPPassword;
  FProcessMessages := ProcessMessages;
end;


end.

