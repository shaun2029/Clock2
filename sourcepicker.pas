unit SourcePicker;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

type

  TSourceOption = record
    Title: string;
    Resource: string;
  end;

  TSourceArray = array of TSourceOption;

  { TfrmSourcePicker }

  TfrmSourcePicker = class(TForm)
    imgMusic: TImage;
    imgMusic1: TImage;
    imgMusic10: TImage;
    imgMusic11: TImage;
    imgMusic12: TImage;
    imgMusic13: TImage;
    imgMusic14: TImage;
    imgMusic2: TImage;
    imgMusic3: TImage;
    imgMusic4: TImage;
    imgMusic5: TImage;
    imgMusic6: TImage;
    imgMusic7: TImage;
    imgMusic8: TImage;
    imgCancel: TImage;
    imgMusic9: TImage;
    lbMusic: TLabel;
    lbMusic1: TLabel;
    lbMusic10: TLabel;
    lbMusic11: TLabel;
    lbMusic12: TLabel;
    lbMusic13: TLabel;
    lbMusic14: TLabel;
    lbMusic2: TLabel;
    lbMusic3: TLabel;
    lbMusic4: TLabel;
    lbMusic5: TLabel;
    lbMusic6: TLabel;
    lbMusic7: TLabel;
    lbMusic8: TLabel;
    lbCancel: TLabel;
    lbMusic9: TLabel;
    procedure lbCancelClick(Sender: TObject);
    procedure lbMusicClick(Sender: TObject);
  private
    { private declarations }
    FItemIndex: integer;
  public
    { public declarations }
    constructor Create(TheOwner: TComponent; Sources: TSourceArray);
  published
    property ItemIndex: integer read FItemIndex;
  end;

var
  frmSourcePicker: TfrmSourcePicker;

implementation

{ TfrmSourcePicker }

procedure TfrmSourcePicker.lbCancelClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmSourcePicker.lbMusicClick(Sender: TObject);
begin
  FItemIndex := TControl(Sender).Tag;
  ModalResult := mrOk;
end;

constructor TfrmSourcePicker.Create(TheOwner: TComponent; Sources: TSourceArray);
var
  Buttons: array [0..14] of TImage;
  Labels: array [0..14] of TLabel;
  i: integer;
begin
  inherited Create(TheOwner);

  FItemIndex := -1;

  Buttons[0] := imgMusic;
  Buttons[1] := imgMusic1;
  Buttons[2] := imgMusic2;
  Buttons[3] := imgMusic3;
  Buttons[4] := imgMusic4;
  Buttons[5] := imgMusic5;
  Buttons[6] := imgMusic6;
  Buttons[7] := imgMusic7;
  Buttons[8] := imgMusic8;
  Buttons[9] := imgMusic9;
  Buttons[10] := imgMusic10;
  Buttons[11] := imgMusic11;
  Buttons[12] := imgMusic12;
  Buttons[13] := imgMusic13;
  Buttons[14] := imgMusic14;

  Labels[0] := lbMusic;
  Labels[1] := lbMusic1;
  Labels[2] := lbMusic2;
  Labels[3] := lbMusic3;
  Labels[4] := lbMusic4;
  Labels[5] := lbMusic5;
  Labels[6] := lbMusic6;
  Labels[7] := lbMusic7;
  Labels[8] := lbMusic8;
  Labels[9] := lbMusic9;
  Labels[10] := lbMusic10;
  Labels[11] := lbMusic11;
  Labels[12] := lbMusic12;
  Labels[13] := lbMusic13;
  Labels[14] := lbMusic14;

  for i := 0 to 14 do
  begin
    if i > High(Sources) then break;

    Buttons[i].Tag := i;
    Buttons[i].OnClick := lbMusicClick;
    Labels[i].Tag := i;
    Labels[i].OnClick := lbMusicClick;
    Labels[i].Caption := Sources[i].Title;

//    if Labels[i].Canvas.TextExtent(Labels[i].Caption).cx < Labels[i].Width then
//      Labels[i].Top := Labels[i].Top + (Labels[i].Canvas.TextExtent('X').cy div 2);

    Buttons[i].Visible := True;
    Labels[i].Visible := True;
  end;
end;


initialization
  {$I sourcepicker.lrs}

end.

