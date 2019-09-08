//
// Copyright 2014 Shaun Simpson
// shauns2029@gmail.com
//
{
Vol+    VOLUMEUP    29
Vol-    VOLDOWN     14
Next    NEXTSONG    10
Music   PLAY        31
Radio   RIGHT       1
Pause   PAUSE       15
}

unit SignalHandler;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, BaseUnix, WebControl;

type
  TSigCallback = procedure(Command: TRemoteCommand) of object;

var
  OnSignal: TSigCallback = nil;
  { TSigHandler }
  { vars for daemonizing }
  aIO,
  aOld,
  aTerm,
  aUsr1,
  aUnused,
  aAlarm,
  aHup : pSigActionRec;
  zerosigs : sigset_t;

procedure InitSignalHandlers;

implementation

procedure DoSig(sig : cint);cdecl;
begin
  case sig of
    SIGUSR1: if Assigned(OnSignal) then OnSignal(rcomNext);       // Signal 10
    SIGUNUSED: if Assigned(OnSignal) then OnSignal(rcomMusic);    // Signal 31
      SIGHUP: if Assigned(OnSignal) then OnSignal(rcomRadio);     // Signal  1
    SIGTERM: if Assigned(OnSignal) then OnSignal(rcomPause);      // Signal 15
    SIGIO: if Assigned(OnSignal) then OnSignal(rcomVolumeUp);     // Signal 29
    SIGALRM: if Assigned(OnSignal) then OnSignal(rcomVolumeDown); // Signal 14
  end;
end;

{ TSigHandler }

procedure InitSignalHandlers;
begin
//  if Assigned(OnSignal) then
  begin
    { setup the signal handlers }
    new(aOld);

    new(aIO);
    aIO^.sa_handler := SigactionHandler(@DoSig);
    aIO^.sa_mask := zerosigs;
    aIO^.sa_flags := 0;
    aIO^.sa_restorer := nil;

    new(aUsr1);
    aUsr1^.sa_handler := SigactionHandler(@DoSig);
    aUsr1^.sa_mask := zerosigs;
    aUsr1^.sa_flags := 0;
    aUsr1^.sa_restorer := nil;

    new(aUnused);
    aUnused^.sa_handler := SigactionHandler(@DoSig);
    aUnused^.sa_mask := zerosigs;
    aUnused^.sa_flags := 0;
    aUnused^.sa_restorer := nil;

    new(aHup);
    aHup^.sa_handler := SigactionHandler(@DoSig);
    aHup^.sa_mask := zerosigs;
    aHup^.sa_flags := 0;
    aHup^.sa_restorer := nil;

    new(aAlarm);
    aAlarm^.sa_handler := SigactionHandler(@DoSig);
    aAlarm^.sa_mask := zerosigs;
    aAlarm^.sa_flags := 0;
    aAlarm^.sa_restorer := nil;

    new(aTerm);
    aTerm^.sa_handler := SigactionHandler(@DoSig);
    aTerm^.sa_mask := zerosigs;
    aTerm^.sa_flags := 0;
    aTerm^.sa_restorer := nil;

    fpSigAction(SIGIO,aIO,aOld);
    fpSigAction(SIGUSR1,aUsr1,aOld);
    fpSigAction(SIGUNUSED,aUnused,aOld);
    fpSigAction(SIGHUP,aHup,aOld);
    fpSigAction(SIGALRM,aAlarm,aOld);
    fpSigAction(SIGTERM,aTerm,aOld);
  end;
end;

end.

