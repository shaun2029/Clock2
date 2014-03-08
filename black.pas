unit black;

{$mode delphi}

interface

uses
  {$IFDEF LCLGTK2}gtk2, gdk2, glib2,{$ENDIF}
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs;

type

  { TfrmBlack }

  TfrmBlack = class(TForm)
    procedure FormActivate(Sender: TObject);
    procedure FormClick(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  frmBlack: TfrmBlack;

implementation

{ TfrmBlack }

procedure TfrmBlack.FormActivate(Sender: TObject);
begin
  gdk_window_fullscreen(PGtkWidget(Handle)^.window);
end;

procedure TfrmBlack.FormClick(Sender: TObject);
begin
  Close;
end;

initialization
  {$I black.lrs}

end.

