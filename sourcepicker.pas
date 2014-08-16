unit SourcePicker;

{$mode delphi}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls, ExtCtrls;

const
  BUTTONCOUNT = 12;

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
    imgNext: TImage;
    imgPrevious: TImage;
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
    lbNext: TLabel;
    lbPrevious: TLabel;
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
    procedure lbNextClick(Sender: TObject);
    procedure lbPreviousClick(Sender: TObject);
  private
    { private declarations }
    FItemIndex: integer;
    FPage, FPageCount: integer;
    FSources: TSourceArray;
    FButtons: array [0..BUTTONCOUNT-1] of TImage;
    FLabels: array [0..BUTTONCOUNT-1] of TLabel;

    procedure PopulateSelections(Page: integer; Sources: TSourceArray);
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

procedure TfrmSourcePicker.lbNextClick(Sender: TObject);
begin
  Inc(FPage);
  PopulateSelections(FPage, FSources);
end;

procedure TfrmSourcePicker.lbPreviousClick(Sender: TObject);
begin
  Dec(FPage);
  PopulateSelections(FPage, FSources);
end;

procedure TfrmSourcePicker.PopulateSelections(Page: integer; Sources: TSourceArray);
var
  i, Offset: integer;
begin
  if Page < 1 then
  begin
    lbPrevious.Visible := False;
    imgPrevious.Visible := False;
  end
  else
  begin
    lbPrevious.Visible := True;
    imgPrevious.Visible := True;
  end;

  if Page >= FPageCount -1 then
  begin
    lbNext.Visible := False;
    imgNext.Visible := False;
  end
  else
  begin
    lbNext.Visible := True;
    imgNext.Visible := True;
  end;

  FItemIndex := -1;

  Offset := BUTTONCOUNT * Page;

  for i := 0 to BUTTONCOUNT - 1 do
  begin
    if i + Offset > High(Sources) then
    begin
      FButtons[i].Visible := False;
      FLabels[i].Visible := False;
    end
    else
    begin
      FButtons[i].Tag := i + Offset;
      FButtons[i].OnClick := lbMusicClick;
      FLabels[i].Tag := i + Offset;
      FLabels[i].OnClick := lbMusicClick;
      FLabels[i].Caption := Sources[i+ Offset].Title;

      FButtons[i].Visible := True;
      FLabels[i].Visible := True;
    end;
  end;
end;


constructor TfrmSourcePicker.Create(TheOwner: TComponent; Sources: TSourceArray);
var
  i: integer;
begin
  inherited Create(TheOwner);

  FItemIndex := -1;

  FPage := 0;
  FPageCount := (High(Sources) div BUTTONCOUNT) + 1;

  FSources := Sources;

  FButtons[0] := imgMusic;
  FButtons[1] := imgMusic1;
  FButtons[2] := imgMusic2;
  FButtons[3] := imgMusic3;
  FButtons[4] := imgMusic4;
  FButtons[5] := imgMusic5;
  FButtons[6] := imgMusic6;
  FButtons[7] := imgMusic7;
  FButtons[8] := imgMusic8;
  FButtons[9] := imgMusic9;
  FButtons[10] := imgMusic10;
  FButtons[11] := imgMusic11;

  FLabels[0] := lbMusic;
  FLabels[1] := lbMusic1;
  FLabels[2] := lbMusic2;
  FLabels[3] := lbMusic3;
  FLabels[4] := lbMusic4;
  FLabels[5] := lbMusic5;
  FLabels[6] := lbMusic6;
  FLabels[7] := lbMusic7;
  FLabels[8] := lbMusic8;
  FLabels[9] := lbMusic9;
  FLabels[10] := lbMusic10;
  FLabels[11] := lbMusic11;

  PopulateSelections(FPage, FSources);
end;


initialization
  {$I sourcepicker.lrs}

end.

