unit RadioStations;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, Buttons;

type

  { TfrmRadioStations }

  TfrmRadioStations = class(TForm)
    btnHelp: TBitBtn;
    btnSave: TBitBtn;
    btnCancel: TBitBtn;
    Defaults: TBitBtn;
    mmoRadioStations: TMemo;
    mmoRadioDefaults: TMemo;
    procedure btnHelpClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure DefaultsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure mmoRadioStationsChange(Sender: TObject);
  private
    { private declarations }
    FConfigFile: string;
    FChanged: boolean;
  public
    { public declarations }
  published
    property ConfigFile: string read FConfigFile write FConfigFile;
    property Changed: boolean read FChanged write FChanged;
  end;

var
  frmRadioStations: TfrmRadioStations;

implementation

{ TfrmRadioStations }

procedure TfrmRadioStations.DefaultsClick(Sender: TObject);
begin
  mmoRadioStations.Lines.Text := mmoRadioDefaults.Lines.Text;
end;

procedure TfrmRadioStations.FormCreate(Sender: TObject);
begin
  FChanged := True;
end;

procedure TfrmRadioStations.mmoRadioStationsChange(Sender: TObject);
begin
  FChanged := True;
end;

procedure TfrmRadioStations.btnHelpClick(Sender: TObject);
begin
  ShowMessage('An internet radio station entry consists of two lines.' + LineEnding
    + 'The first line consists of the station title and the second the URL.'
    + LineEnding + 'Appending "MPD_HOST=" and "MPD_PORT=" will enable MPD control.');
end;

procedure TfrmRadioStations.btnSaveClick(Sender: TObject);
begin
  try
     mmoRadioStations.Lines.SaveToFile(FConfigFile);
  except
    on E : Exception do
      MessageDlg(E.Message,mtError, [mbOK], 0);
  end;
end;

initialization
  {$I radiostations.lrs}

end.

