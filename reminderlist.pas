unit ReminderList;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, Reminders;

type

  { TfrmReminderList }

  TfrmReminderList = class(TForm)
    btnDelete: TBitBtn;
    btnEdit: TBitBtn;
    btnAdd: TBitBtn;
    btnOk: TBitBtn;
    lbxReminders: TListBox;
    procedure btnAddClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEditClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyPress(Sender: TObject; var Key: char);
    procedure FormShow(Sender: TObject);
    procedure lbxRemindersDblClick(Sender: TObject);
  private
    FCanEdit: boolean;
    procedure AddReminder;
    procedure DeleteReminder(Index: integer);
    procedure EditReminder(Index: integer);
    procedure SetCanEdit(const AValue: boolean);
    { private declarations }
  public
    { public declarations }
    FReminders: TfrmReminders;

    property CanEdit: boolean read FCanEdit write SetCanEdit;
  end;

var
  frmReminderList: TfrmReminderList;

implementation

{$R *.lfm}

{ TfrmReminderList }

procedure TfrmReminderList.FormKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #27 then
  begin
    Close
  end
  else if FCanEdit then
  begin
    if (Key = 'd') or (Key = 'D') then
    begin
      DeleteReminder(lbxReminders.ItemIndex);
      Key := #0;
    end
    else if (Key = #13) or (Key = 'e') or (Key = 'E') then
    begin
      EditReminder(lbxReminders.ItemIndex);
      Key := #0;
    end
    else if (Key = 'a') or (Key = 'A') then
    begin
      AddReminder;
      Key := #0;
    end;
  end
  else if (Key = 'd') or (Key = 'D') or (Key = 'e') or (Key = 'E')
   or (Key = 'e') or (Key = 'A') or (Key = #13) then
  begin
    ShowMessage('You can not edit this list when reminders are fetched from ' +
     'the server.' + LineEnding + 'Change the setting "Fetch reminders from server" by using the Settings dialog.');
  end;
end;

procedure TfrmReminderList.EditReminder(Index: integer);
begin
  FReminders.DisplayReminder(Index);

  FReminders.Editing := True;

  if FReminders.ShowModal = mrOk then
  begin
    FReminders.UpdateWithCurrentReminder(Index);

    lbxReminders.Clear;
    FReminders.PopulateList(lbxReminders.Items);

    if Index < lbxReminders.Count - 1 then
      lbxReminders.ItemIndex := Index
    else if lbxReminders.Count > 0 then
      lbxReminders.ItemIndex := lbxReminders.Count - 1;
  end;

  FReminders.Editing := False;
end;

procedure TfrmReminderList.AddReminder;
begin
  frmReminders.ShowModal;

  lbxReminders.Clear;
  FReminders.SortReminders;
  FReminders.PopulateList(lbxReminders.Items);
end;

procedure TfrmReminderList.DeleteReminder(Index: integer);
begin
  FReminders.DeleteReminder(lbxReminders.ItemIndex);
  lbxReminders.Clear;
  FReminders.PopulateList(lbxReminders.Items);

  if Index < lbxReminders.Count - 1 then
    lbxReminders.ItemIndex := Index
  else if lbxReminders.Count > 0 then
    lbxReminders.ItemIndex := lbxReminders.Count - 1;
end;


procedure TfrmReminderList.FormCreate(Sender: TObject);
begin
  FCanEdit := False;
end;

procedure TfrmReminderList.btnAddClick(Sender: TObject);
begin
  AddReminder;
end;

procedure TfrmReminderList.btnDeleteClick(Sender: TObject);
begin
  DeleteReminder(lbxReminders.ItemIndex);
end;

procedure TfrmReminderList.btnEditClick(Sender: TObject);
begin
  EditReminder(lbxReminders.ItemIndex);
end;

procedure TfrmReminderList.FormShow(Sender: TObject);
begin
  lbxReminders.Clear;
  FReminders.SortReminders;
  FReminders.PopulateList(lbxReminders.Items);
  if lbxReminders.Count > 0 then lbxReminders.ItemIndex := 0;
end;

procedure TfrmReminderList.lbxRemindersDblClick(Sender: TObject);
begin
  EditReminder(lbxReminders.ItemIndex);
end;

procedure TfrmReminderList.SetCanEdit(const AValue: boolean);
begin
  if FCanEdit=AValue then Exit;
  FCanEdit := AValue;
end;

end.

