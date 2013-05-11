unit WaitForMedia;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls, ComCtrls;

type

  { TfrmWaitForMedia }

  TfrmWaitForMedia = class(TForm)
    lbWaitForMedia: TLabel;
    pbarTimeout: TProgressBar;
    tmrWait: TTimer;
    procedure FormShow(Sender: TObject);
    procedure tmrWaitTimer(Sender: TObject);
  private
    FPath: string;
    FTimeout: TDateTime;
    { private declarations }
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Path: string; Timeout: TDateTime); overload;
  end;

var
  frmWaitForMedia: TfrmWaitForMedia;

implementation

{ TfrmWaitForMedia }

procedure TfrmWaitForMedia.tmrWaitTimer(Sender: TObject);
begin
  if DirectoryExists(FPath) then ModalResult := mrOk
  else if Now > FTimeout then ModalResult := mrCancel;
end;

procedure TfrmWaitForMedia.FormShow(Sender: TObject);
begin
  if DirectoryExists(FPath) then ModalResult := mrOk
  else tmrWait.Enabled := True;
end;

constructor TfrmWaitForMedia.Create(TheOwner: TComponent; Path: string;
  Timeout: TDateTime);
begin
  inherited Create(TheOwner);

  FTimeout := Timeout + Now;
  FPath := Path;

  lbWaitForMedia.Caption := 'Waiting for media to become avaliable.'
    + LineEnding + LineEnding + FPath;
end;

initialization
  {$I waitformedia.lrs}

end.

