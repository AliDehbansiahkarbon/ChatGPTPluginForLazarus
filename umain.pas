unit Umain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazLoggerBase, FileUtil, Forms, Controls, Graphics,
  Dialogs, LCLType, Spin, StdCtrls, Buttons, ExtCtrls, IDECommands,
  IDEWindowIntf, LazIDEIntf, SrcEditorIntf, MenuIntf, UQuestion, USetting;

var
  Frm_Question: TFrm_Question;
  Frm_QuestionCreator: TIDEWindowCreator; // set by Register procedure

procedure ShowQuestionForm(Sender: TObject);
procedure ShowSettingForm(Sender: TObject);
procedure Register;

implementation

resourcestring
  SCaption = 'ASK';

const
  FORM_NAME='Frm_Question';   //should be distinct of design time form.Name

procedure ShowQuestionForm(Sender: TObject);
begin
  IDEWindowCreators.ShowForm(Frm_QuestionCreator.FormName, True);
end;

procedure CreateFrm_Question(Sender: TObject; aFormName: string; var AForm: TCustomForm; DoDisableAutoSizing: boolean);
begin
  if CompareText(aFormName, FORM_NAME) <> 0 then
  begin
    DebugLn(['ERROR: There is already a form with this name']);
    Exit;
  end;

  IDEWindowCreators.CreateForm(AForm, TFrm_Question, DoDisableAutoSizing, LazarusIDE.OwningComponent);
  AForm.Name := aFormName;
  AForm.Caption := 'ChatGPT';
  Frm_Question := AForm as TFrm_Question;
end;

procedure ShowSettingForm(Sender: TObject);
var
  LvSettingFrm: TFrm_Setting;
begin
  LvSettingFrm := TFrm_Setting.Create(Application);
  try
    TSingletonSettingObj.Instance.ReadConfig;

    with TSingletonSettingObj.Instance do
    begin
      LvSettingFrm.edt_Url.Text := URL;
      LvSettingFrm.edt_ApiKey.Text := ApiKey;
      LvSettingFrm.cbbModel.Text := Model;
      LvSettingFrm.edt_MaxToken.Text := IntToStr(MaxToken);
      LvSettingFrm.edt_Temperature.Text := IntToStr(Temperature);
      LvSettingFrm.edtTimeout.Text := IntToStr(TimeOut);
      LvSettingFrm.chk_AnimatedLetters.Checked := AnimatedLetters;
    end;

    LvSettingFrm.ShowModal;
  finally
    LvSettingFrm.Free;
  end;
end;

procedure Register;
var
  LvCommandCategory: TIDECommandCategory;
  LvIDECommand, LvIDESettingCommand: TIDECommand;
  LvMenuItemCaption: string;
  LvMenuSection, LvChatGPTHeaderMenu: TIDEMenuSection;
begin
  LvMenuItemCaption := SCaption;

  // Find shortcut category
  LvCommandCategory := IDECommandList.FindCategoryByName(CommandCategoryCodeTools);

  // Register shortcut
  LvIDECommand := RegisterIDECommand(LvCommandCategory, 'ChatGPTCommandShrtCt',
    LvMenuItemCaption, IDEShortCut(VK_C, [ssShift, ssAlt, ssCtrl]), // <- set here your default shortcut
    CleanIDEShortCut, nil, @ShowQuestionForm);

  // Register menu section
  LvMenuSection := RegisterIDEMenuSection(itmCustomTools, 'ChatGPTSec');

  // Register menu item in tools menu and under our created section.
  LvChatGPTHeaderMenu := RegisterIDESubMenu(LvMenuSection, 'ChatPGTSubMenu', 'ChatGPT', nil, nil);
  RegisterIDEMenuCommand(LvChatGPTHeaderMenu, 'ChatGPTCommandShrtCt', LvMenuItemCaption, nil, nil, LvIDECommand);


  LvIDESettingCommand := RegisterIDECommand(LvCommandCategory, 'ChatGPTCommandShrtCt',
    'Setting', IDEShortCut(VK_S, [ssShift, ssAlt, ssCtrl]), // <- set here your default shortcut
    CleanIDEShortCut, nil, @ShowSettingForm);

  RegisterIDEMenuCommand(LvChatGPTHeaderMenu, 'ChatGPTSettingShrtCt', 'Setting', nil, nil, LvIDESettingCommand);

  // register dockable Window
  Frm_QuestionCreator := IDEWindowCreators.Add(FORM_NAME, @CreateFrm_Question,
  nil, '100', '100', '420', '620'  // default place at left=100, top=100, right=300, bottom=300
    // you can also define percentage values of screen or relative positions, see wiki
    );

  TSingletonSettingObj.Instance.ReadConfig;
end;


end.

