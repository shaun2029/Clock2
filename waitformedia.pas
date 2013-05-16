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
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormShow(Sender: TObject);
    procedure tmrWaitTimer(Sender: TObject);
  private
    FPath: string;
    FTimeout: TDateTime;
    FDirectoryExists: boolean;
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
  if DirectoryExists(FPath) then
  begin
    FDirectoryExists := True;
    Close;
  end
  else if Now > FTimeout then Close;
end;

procedure TfrmWaitForMedia.FormShow(Sender: TObject);
begin
  if DirectoryExists(FPath) then
  begin
    FDirectoryExists := True;
    Close;
  end
  else tmrWait.Enabled := True;
end;

procedure TfrmWaitForMedia.FormClose(Sender: TObject;
  var CloseAction: TCloseAction);
begin
  if FDirectoryExists then ModalResult := mrOk
  else ModalResult := mrCancel;
end;

constructor TfrmWaitForMedia.Create(TheOwner: TComponent; Path: string;
  Timeout: TDateTime);
begin
  inherited Create(TheOwner);

  FDirectoryExists := False;

  FTimeout := Timeout + Now;
  FPath := Path;

  lbWaitForMedia.Caption := 'Waiting for media to become avaliable.'
    + LineEnding + LineEnding + FPath;
end;

initialization
  {$I waitformedia.lrs}

end.

