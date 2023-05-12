unit UFrame_Question;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, ExtCtrls, StdCtrls, ComCtrls, Dialogs,
  UThread, USetting, Clipbrd, Menus, ButtonPanel, EditBtn, Buttons, RTTICtrls;

type

  { TFrame1 }
  TFrame1 = class(TFrame)
    btnAsk: TButton;
    btnClipBoard: TButton;
    chkAutoCopy: TCheckBox;
    Label1: TLabel;
    Label2: TLabel;
    MenuItem1: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    mmoQuestion: TMemo;
    mmoAnswer: TMemo;
    pnlQuestion: TPanel;
    pnlAnswer: TPanel;
    pnlBottom: TPanel;
    pnlTop: TPanel;
    pnlCenter: TPanel;
    pnlMain: TPanel;
    PopupClear: TPopupMenu;
    ProgressBar: TProgressBar;
    btnClear: TSpeedButton;
    Splitter1: TSplitter;
    procedure btnAskClick(Sender: TObject);
    procedure btnClearClick(Sender: TObject);
    procedure btnClipBoardClick(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure MenuItem3Click(Sender: TObject);
    procedure mmoQuestionKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
  private
    procedure ShowAnswer(AAnswer: string; AAnimated: Boolean; ADone: Boolean); // This is an event that will be called inworker Thread to update UI.
  public
    FAnswer: TStringList;
    procedure CallThread(AQuestion: String);
  end;

implementation

{$R *.lfm}

{ TFrame1 }

procedure TFrame1.btnAskClick(Sender: TObject);
begin
  if TSingletonSettingObj.Instance.ApiKey.IsEmpty then
  begin
    ShowMessage('You need an API key, please fill the setting parameters in setting form.');
    Frm_Setting := TFrm_Setting.Create(Application);
    try
      Frm_Setting.ChangeFocus := True;
      Frm_Setting.ShowModal;
    finally
      Frm_Setting.Free;
    end;

    Exit;
  end;

  if mmoQuestion.Lines.Text.Trim.IsEmpty then
  begin
    ShowMessage('You need to type a question first!');
    if mmoQuestion.CanFocus then
      mmoQuestion.SetFocus;

    Exit;
  end;
  mmoAnswer.Lines.Clear;
  CallThread(mmoQuestion.Lines.Text);
  ProgressBar.Visible := True;
end;

procedure TFrame1.btnClearClick(Sender: TObject);
begin
  PopupClear.PopUp;
end;

procedure TFrame1.btnClipBoardClick(Sender: TObject);
begin
  Clipboard.SetTextBuf(PChar(mmoAnswer.Lines.Text));
end;

procedure TFrame1.MenuItem1Click(Sender: TObject);
begin
  mmoQuestion.Clear;
end;

procedure TFrame1.MenuItem2Click(Sender: TObject);
begin
  mmoAnswer.Clear;
end;

procedure TFrame1.MenuItem3Click(Sender: TObject);
begin
  mmoQuestion.Clear;
  mmoAnswer.Clear;
end;

procedure TFrame1.mmoQuestionKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift = [ssCtrl]) and (Ord(Key) = 13) then
    btnAsk.Click;
end;

procedure TFrame1.ShowAnswer(AAnswer: string; AAnimated: Boolean; ADone: Boolean);
begin
  if AAnimated then
  begin
    if AAnswer = #13 then
      mmoAnswer.Lines.Add('')
    else
      mmoAnswer.Lines[Pred(mmoAnswer.Lines.Count)] := mmoAnswer.Lines[Pred(mmoAnswer.Lines.Count)] + AAnswer
  end
  else
    mmoAnswer.Lines.Add(AAnswer);

  ProgressBar.Visible := not ADone;
  if (ADone) and (chkAutoCopy.Checked) then
    btnClipBoard.Click;
end;

procedure TFrame1.CallThread(AQuestion: String);
var
  LvChatGPTTrd: TExecutorTrd;
  LvSetting: TSingletonSettingObj;
begin
  LvSetting := TSingletonSettingObj.Instance;
  LvChatGPTTrd := TExecutorTrd.Create(LvSetting.ApiKey, LvSetting.Model, AQuestion, LvSetting.URL, LvSetting.MaxToken,
                                      LvSetting.Temperature, LvSetting.AnimatedLetters, LvSetting.TimeOut);
  LvChatGPTTrd.OnShowAnswer := @ShowAnswer;
  LvChatGPTTrd.Start;
end;

end.

