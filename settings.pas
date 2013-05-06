unit settings;

{$mode objfpc}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Spin;

type

  { TfrmSettings }

  TfrmSettings = class(TForm)
    Button1: TButton;
    edtPicturePath: TEdit;
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    seDelay: TSpinEdit;
    SelectDirectoryDialog1: TSelectDirectoryDialog;
    procedure Button1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmSettings: TfrmSettings;

implementation

{ TfrmSettings }

procedure TfrmSettings.Button1Click(Sender: TObject);
begin
  SelectDirectoryDialog1.FileName := edtPicturePath.Text;
  if SelectDirectoryDialog1.Execute then
  begin
    edtPicturePath.Text := SelectDirectoryDialog1.FileName;
  end;
end;

initialization
  {$I settings.lrs}

end.

