// ***********************************************************************
// ***********************************************************************
// mxMarkEdit 1.x
// Author and copyright: Massimo Nardello, Modena (Italy) 2024.
// Free software released under GPL licence version 3 or later.

// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version. You can read the version 3
// of the Licence in http://www.gnu.org/licenses/gpl-3.0.txt
// or in the file Licence.txt included in the files of the
// source code of this software.

// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
// GNU General Public License for more details.
// ***********************************************************************
// ***********************************************************************

unit Unit1;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, CocoaAll,
  CocoaTextEdits, CocoaUtils, Clipbrd, Menus, StdCtrls, Grids, ExtCtrls,
  DefaultTranslator, translate, IniFiles, LazUTF8, LazUTF16, FileUtil,
  LazFileUtils, Unix, Types, DateUtils;

type

  { TfmMain }

  TfmMain = class(TForm)
    bvList: TBevel;
    cbFilter: TComboBox;
    dbText: TMemo;
    lbDateTime: TLabel;
    lbChars: TLabel;
    miEditTasks: TMenuItem;
    miEditShowCurrent: TMenuItem;
    miEditFindDuplicate: TMenuItem;
    miEditHideList: TMenuItem;
    miEditLink: TMenuItem;
    miEditDisableForm: TMenuItem;
    miEditDisSpell: TMenuItem;
    miSepLastFiles: TMenuItem;
    miFileOpenLast1: TMenuItem;
    miFileOpenLast2: TMenuItem;
    miFileOpenLast3: TMenuItem;
    miFileOpenLast4: TMenuItem;
    miToolsOpenWin: TMenuItem;
    odLink: TOpenDialog;
    pnTitTodo: TPanel;
    Sep3: TMenuItem;
    Sep5: TMenuItem;
    Sep6: TMenuItem;
    miToolsTrans3: TMenuItem;
    miToolsTrans2: TMenuItem;
    miToolsTrans1: TMenuItem;
    miToolsTransparency: TMenuItem;
    Sep4: TMenuItem;
    miToolsOptions: TMenuItem;
    miToolsPandoc: TMenuItem;
    miCopyright: TMenuItem;
    miHelp: TMenuItem;
    miTools: TMenuItem;
    miEditFind: TMenuItem;
    odOpen: TOpenDialog;
    sdSave: TSaveDialog;
    Sep1: TMenuItem;
    miEditCut: TMenuItem;
    miEditCopy: TMenuItem;
    miEditPaste: TMenuItem;
    miEditSelectAll: TMenuItem;
    miEdit: TMenuItem;
    miFileNew: TMenuItem;
    miFileOpen: TMenuItem;
    miFileSave: TMenuItem;
    miFileSaveAs: TMenuItem;
    mmMenu: TMainMenu;
    miFile: TMenuItem;
    pnBottom: TPanel;
    Sep2: TMenuItem;
    sgTitles: TStringGrid;
    spTitles: TSplitter;
    tmDateTime: TTimer;
    procedure cbFilterChange(Sender: TObject);
    procedure dbTextChange(Sender: TObject);
    procedure dbTextClick(Sender: TObject);
    procedure dbTextKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure dbTextKeyPress(Sender: TObject; var Key: char);
    procedure dbTextKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure miEditFindDuplicateClick(Sender: TObject);
    procedure miEditHideListClick(Sender: TObject);
    procedure miEditLinkClick(Sender: TObject);
    procedure miEditDisableFormClick(Sender: TObject);
    procedure miEditDisSpellClick(Sender: TObject);
    procedure miCopyrightClick(Sender: TObject);
    procedure miEditCopyClick(Sender: TObject);
    procedure miEditCutClick(Sender: TObject);
    procedure miEditFindClick(Sender: TObject);
    procedure miEditPasteClick(Sender: TObject);
    procedure miEditSelectAllClick(Sender: TObject);
    procedure miEditShowCurrentClick(Sender: TObject);
    procedure miEditTasksClick(Sender: TObject);
    procedure miFileNewClick(Sender: TObject);
    procedure miFileOpenClick(Sender: TObject);
    procedure miFileOpenLast1Click(Sender: TObject);
    procedure miFileOpenLast2Click(Sender: TObject);
    procedure miFileOpenLast3Click(Sender: TObject);
    procedure miFileOpenLast4Click(Sender: TObject);
    procedure miFileSaveAsClick(Sender: TObject);
    procedure miFileSaveClick(Sender: TObject);
    procedure miToolsOpenWinClick(Sender: TObject);
    procedure miToolsOptionsClick(Sender: TObject);
    procedure miToolsPandocClick(Sender: TObject);
    procedure miToolsTrans1Click(Sender: TObject);
    procedure miToolsTrans2Click(Sender: TObject);
    procedure miToolsTrans3Click(Sender: TObject);
    procedure sgTitlesClick(Sender: TObject);
    procedure sgTitlesDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure sgTitlesGetCellHint(Sender: TObject; ACol, ARow: integer;
      var HintText: string);
    procedure sgTitlesPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
    procedure tmDateTimeTimer(Sender: TObject);
  private
    procedure CreateBackup;
    procedure CreateYAML;
    function GetDict(txt: NSTextStorage; textOffset: integer): NSDictionary;
    function GetPara(txt: NSTextStorage; textOffset: integer;
      isReadOnly, useDefault: boolean): NSParagraphStyle;
    function GetWritePara(txt: NSTextStorage;
      textOffset: integer): NSMutableParagraphStyle;
    procedure LabelFileNameChars;
    procedure MoveToPos;
    procedure RenumberList;
    function SaveFile: boolean;
    procedure UpdateLastFile;
  public
    procedure ShowCurrentTitleTodo;
    procedure FormatListTitleTodo;
    function UTF8CocoaPos(const SearchForText, SearchInText: string;
      StartPos: SizeInt = 1): PtrInt;
    function FindFont(FamilyName: string; iStyle: smallint): NSFontDescriptor;

  end;

var
  fmMain: TfmMain;
  myHomeDir, myConfigFile: string;
  iTitleTodoRowHeight: Integer = 28;
  clTitle1: TColor = clBlack;
  clTitle2: TColor = clBlack;
  clTitle3: TColor = clBlack;
  clRepetition: TColor = $005766EA;
  clFootnote: TColor = clSilver;
  clLink: TColor = clSilver;
  clCode: TColor = clSilver;
  clHighlightList: TColor = clDkGray;
  clHighlightText: TColor = clGreen;
  stFontMono: string = 'Menlo';
  iFontMonoSize: smallint = 18;
  stFileName: string = '';
  iBookmarkPos: Integer = 0;
  LastDatabase1, LastDatabase2, LastDatabase3, LastDatabase4: string;
  LastPosDatabase1, LastPosDatabase2, LastPosDatabase3, LastPosDatabase4: Integer;
  TopIndexDatabase1, TopIndexDatabase2, TopIndexDatabase3, TopIndexDatabase4: Integer;
  blFileSaved: boolean = True;
  blDisableFormatting: boolean = False;
  pandocPath: string = '/usr/local/bin/';
  pandocOptions: string = '+footnotes+inline_notes';
  pandocTemplate: string = 'word-template.docx';
  pandocOutput: string = '.docx';

resourcestring

  msg001 = 'Characters:';
  msg002 = 'Discard text changes?';
  msg003 = 'It was not possible to save the current file.';
  msg004 = 'It was not possible to load the selected file.';
  msg005 = 'Create a new document?';
  msg006 = 'It was not possible to load the last used file.';
  msg007 = 'The file is not available.';
  msg008 = 'The current document has no name.';
  msg009 = 'It was not possible to create the backup file.';
  msg010 = 'Find the repeated words in all the current document?';
  dlg001 = 'Markdown files|*.md|All files|*';
  dlg002 = 'Save Markdown file';
  dlg003 = 'Open Markdown file';
  dlg004 = 'All files|*';
  dlg005 = 'Open file';
  lb001 = 'All the headings';
  lb002 = 'Headings 1 - 5';
  lb003 = 'Headings 1 - 4';
  lb004 = 'Headings 1 - 3';
  lb005 = 'Headings 1 - 2';
  lb006 = 'Headings 1';
  dateformat = 'en';

implementation

uses copyright, unit2, unit3, unit4;

  {$R *.lfm}

  { TfmMain }

  // *****************************************************
  // ************ Procedures of the main form ************
  // *****************************************************

procedure TfmMain.FormCreate(Sender: TObject);
var
  MyIni: TIniFile;
begin
  if LowerCase(UTF8Copy(NSStringToString(
    NSLocale.preferredLanguages.objectAtIndex(0)), 1, 2)) = 'it' then
  begin
    translate.TranslateTo('mxmarkedit.it');
  end;
  if IsAppDark = True then
  begin
    dbText.Font.Color := clWhite;
    sgTitles.Font.Color := clWhite;
    lbChars.Font.Color := clSilver;
    lbDateTime.Font.Color := clSilver;
    clTitle1 := clWhite;
    clTitle2 := clWhite;
    clTitle3 := clWhite;
    clFootnote := clSilver;
    clLink := clSilver;
    clCode := clSilver;
    clHighlightList := $005E5E5E;
    clRepetition := $005766EA;
    clHighlightText := $00445D31;
  end
  else
  begin
    dbText.Font.Color := clBlack;
    sgTitles.Font.Color := clBlack;
    dbText.Color := clWhite;
    sgTitles.Color := clWhite;
    fmMain.Color := clWhite;
    spTitles.Color := clForm;
    pnBottom.Color := clForm;
    lbChars.Font.Color := clDkGray;
    lbDateTime.Font.Color := clDkGray;
    clTitle1 := clBlack;
    clTitle2 := clBlack;
    clTitle3 := clBlack;
    clFootnote := clSilver;
    clLink := clSilver;
    clCode := clSilver;
    clHighlightList := $00EBEBEB;
    clRepetition := clRed;
    clHighlightText := $0079FBD4;
  end;
  sgTitles.FocusRectVisible := False;
  lbChars.Caption := msg001 + ' 0';
  sdSave.Filter := dlg001;
  sdSave.Title := dlg002;
  odOpen.Filter := dlg001;
  odOpen.Title := dlg003;
  odLink.Filter := dlg004;
  odLink.Title := dlg005;
  cbFilter.Items.Clear;
  cbFilter.Items.Add(lb006);
  cbFilter.Items.Add(lb005);
  cbFilter.Items.Add(lb004);
  cbFilter.Items.Add(lb003);
  cbFilter.Items.Add(lb002);
  cbFilter.Items.Add(lb001);
  cbFilter.ItemIndex := 5;
  CreateYAML;
  myHomeDir := GetUserDir + 'Library/Preferences/';
  myConfigFile := 'mxmarkedit';
  if DirectoryExists(myHomeDir) = False then
  begin
    CreateDirUTF8(myHomeDir);
  end;
  if FileExistsUTF8(myHomeDir + myConfigFile) then
  begin
    try
      MyIni := TIniFile.Create(myHomeDir + myConfigFile);
      if MyIni.ReadString('mxpanmark', 'maximize', '') = 'true' then
      begin
        fmMain.WindowState := wsMaximized;
      end
      else
      begin
        fmMain.Top := MyIni.ReadInteger('mxmarkedit', 'top', 0);
        fmMain.Left := MyIni.ReadInteger('mxmarkedit', 'left', 0);
        if MyIni.ReadInteger('mxmarkedit', 'width', 0) > 100 then
          fmMain.Width := MyIni.ReadInteger('mxmarkedit', 'width', 0)
        else
          fmMain.Width := 1000;
        if MyIni.ReadInteger('mxmarkedit', 'heigth', 0) > 100 then
          fmMain.Height := MyIni.ReadInteger('mxmarkedit', 'heigth', 0)
        else
          fmMain.Height := 600;
      end;
      dbText.Font.Name := MyIni.ReadString('mxmarkedit', 'fontname', 'Avenir Next');
      dbText.Font.Size := MyIni.ReadInteger('mxmarkedit', 'fontsize', 22);
      dbText.Font.Color := StringToColor(MyIni.ReadString('mxmarkedit',
        'fontcolor', ColorToString(dbText.Font.Color)));
      stFontMono := MyIni.ReadString('mxmarkedit', 'fontmononame', 'Menlo');
      iFontMonoSize := MyIni.ReadInteger('mxmarkedit', 'fontmonosize', 20);
      clTitle1 := StringToColor(MyIni.ReadString('mxmarkedit', 'title1',
        'clTitle1'));
      clTitle2 := StringToColor(MyIni.ReadString('mxmarkedit', 'title2', 'clTitle2'));
      clTitle3 := StringToColor(MyIni.ReadString('mxmarkedit', 'title3', 'clTitle3'));
      clFootnote := StringToColor(MyIni.ReadString('mxmarkedit',
        'footnote', 'clFootnote'));
      clLink := StringToColor(MyIni.ReadString('mxmarkedit',
        'link', 'clLink'));
      clCode := StringToColor(MyIni.ReadString('mxmarkedit',
        'code', 'clCode'));
      pnTitTodo.Width := MyIni.ReadInteger('mxmarkedit', 'titlewidth', 400);
      stFileName := MyIni.ReadString('mxmarkedit', 'filename', '');
      pandocOptions := MyIni.ReadString('mxmarkedit', 'panoption',
        '+footnotes+inline_notes');
      pandocTemplate := MyIni.ReadString('mxmarkedit', 'pantemplate',
        'word-template.docx');
      pandocOutput := MyIni.ReadString('mxmarkedit', 'panoutputput', '.docx');
      pandocPath := MyIni.ReadString('mxmarkedit', 'panpath', '/usr/local/bin/');
      if MyIni.ReadString('mxmarkedit', 'lastfile1', '') <> '' then
      begin
        LastDatabase1 := MyIni.ReadString('mxmarkedit', 'lastfile1', '');
        miFileOpenLast1.Caption := ExtractFileNameOnly(LastDatabase1);
        miFileOpenLast1.Visible := True;
      end
      else
      begin
        miFileOpenLast1.Visible := False;
      end;
      if MyIni.ReadString('mxmarkedit', 'lastfile2', '') <> '' then
      begin
        LastDatabase2 := MyIni.ReadString('mxmarkedit', 'lastfile2', '');
        miFileOpenLast2.Caption := ExtractFileNameOnly(LastDatabase2);
        miFileOpenLast2.Visible := True;
      end
      else
      begin
        miFileOpenLast2.Visible := False;
      end;
      if MyIni.ReadString('mxmarkedit', 'lastfile3', '') <> '' then
      begin
        LastDatabase3 := MyIni.ReadString('mxmarkedit', 'lastfile3', '');
        miFileOpenLast3.Caption := ExtractFileNameOnly(LastDatabase3);
        miFileOpenLast3.Visible := True;
      end
      else
      begin
        miFileOpenLast3.Visible := False;
      end;
      if MyIni.ReadString('mxmarkedit', 'lastfile4', '') <> '' then
      begin
        LastDatabase4 := MyIni.ReadString('mxmarkedit', 'lastfile4', '');
        miFileOpenLast4.Caption := ExtractFileNameOnly(LastDatabase4);
        miFileOpenLast4.Visible := True;
      end
      else
      begin
        miFileOpenLast4.Visible := False;
      end;
      if ((miFileOpenLast1.Visible = False) and
        (miFileOpenLast2.Visible = False) and (miFileOpenLast3.Visible = False) and
        (miFileOpenLast4.Visible = False)) then
      begin
        miSepLastFiles.Visible := False;
      end
      else
      begin
        miSepLastFiles.Visible := True;
      end;
      LastPosDatabase1 := MyIni.ReadInteger('mxmarkedit', 'lastposdatabase1', 0);
      LastPosDatabase2 := MyIni.ReadInteger('mxmarkedit', 'lastposdatabase2', 0);
      LastPosDatabase3 := MyIni.ReadInteger('mxmarkedit', 'lastposdatabase3', 0);
      LastPosDatabase4 := MyIni.ReadInteger('mxmarkedit', 'lastposdatabase4', 0);
      TopIndexDatabase1 := MyIni.ReadInteger('mxmarkedit', 'topindexdatabase1', 0);
      TopIndexDatabase2 := MyIni.ReadInteger('mxmarkedit', 'topindexdatabase2', 0);
      TopIndexDatabase3 := MyIni.ReadInteger('mxmarkedit', 'topindexdatabase3', 0);
      TopIndexDatabase4 := MyIni.ReadInteger('mxmarkedit', 'topindexdatabase4', 0);
    finally
      MyIni.Free;
    end;
  end;
  TCocoaTextView(NSScrollView(dbText.Handle).documentView).
    setContinuousSpellCheckingEnabled(True);
  TCocoaTextView(NSScrollView(dbText.Handle).documentView).
    setGrammarCheckingEnabled(False);
  TCocoaTextView(NSScrollView(dbText.Handle).documentView).
    setFocusRingType(1);
  TCocoaTextView(NSScrollView(dbText.Handle).documentView).
    setAutomaticQuoteSubstitutionEnabled(True);
  TCocoaTextView(NSScrollView(dbText.Handle).documentView).
    setSmartInsertDeleteEnabled(True);
  TCocoaTextView(NSScrollView(dbText.Handle).documentView).
    setAutomaticLinkDetectionEnabled(True);
  TCocoaTextView(NSScrollView(dbText.Handle).documentView).
    setImportsGraphics(False);
  TCocoaTextView(NSScrollView(dbText.Handle).documentView).
    setRichText(True);
  TCocoaTextView(NSScrollView(dbText.Handle).documentView).
    textContainer.setLineFragmentPadding(50);
  // To avoid messing text on formatting
  TCocoaTextView(NSScrollView(dbText.Handle).documentView).
    layoutManager.setAllowsNonContiguousLayout(False);
  // Open file from paramater on console
  if ParamStrUTF8(1) <> '' then
  begin
    if FileExistsUTF8(ParamStrUTF8(1)) = True then
    try
      stFileName := ParamStrUTF8(1);
      dbText.Lines.LoadFromFile(stFileName);
      iBookmarkPos := 0;
      MoveToPos;
      LabelFileNameChars;
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        checkTextInDocument(nil);
      UpdateLastFile;
      ShowCurrentTitleTodo;
    except
      MessageDlg(msg004, mtWarning, [mbOK], 0);
    end;
  end
  else
  if stFileName <> '' then
  begin
    if FileExistsUTF8(stFileName) then
    try
      odOpen.FileName := stFileName;
      dbText.Lines.LoadFromFile(stFileName);
      iBookmarkPos := 0;
      MoveToPos;
      LabelFileNameChars;
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        checkTextInDocument(nil);
      UpdateLastFile;
      ShowCurrentTitleTodo;
    except
      MessageDlg(msg006, mtWarning, [mbOK], 0);
    end
    else
    begin
      stFileName := '';
    end;
  end;
end;

procedure TfmMain.FormDropFiles(Sender: TObject; const FileNames: array of string);
begin
  // Open file from file association
  if FileExistsUTF8(FileNames[0]) = True then
  try
    stFileName := FileNames[0];
    dbText.Lines.LoadFromFile(stFileName);
    iBookmarkPos := 0;
    MoveToPos;
    LabelFileNameChars;
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      checkTextInDocument(nil);
    UpdateLastFile;
    ShowCurrentTitleTodo;
  except
    MessageDlg(msg004, mtWarning, [mbOK], 0);
  end;
end;

procedure TfmMain.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  rng: NSRange;
begin
  if ((key = 187) and (Shift = [ssMeta])) then
  begin
    if dbText.Font.Size < 128 then
    begin
      dbText.Font.Size := dbText.Font.Size + 1;
      FormatListTitleTodo;
      rng := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        textStorage.string_.paragraphRangeForRange(TCocoaTextView(
        NSScrollView(fmMain.dbText.Handle).documentView).selectedRange);
      Application.ProcessMessages;
      TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        scrollRangeToVisible(rng);
    end;
    key := 0;
  end
  else
  if ((key = 189) and (Shift = [ssMeta])) then
  begin
    if dbText.Font.Size > 6 then
    begin
      dbText.Font.Size := dbText.Font.Size - 1;
      FormatListTitleTodo;
      rng := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        textStorage.string_.paragraphRangeForRange(TCocoaTextView(
        NSScrollView(fmMain.dbText.Handle).documentView).selectedRange);
      Application.ProcessMessages;
      TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        scrollRangeToVisible(rng);
    end;
    key := 0;
  end
  else
  if ((key = 187) and (Shift = [ssMeta, ssCtrl])) then
  begin
    if iFontMonoSize < 128 then
    begin
      iFontMonoSize := iFontMonoSize + 1;
    end;
    FormatListTitleTodo;
    rng := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      textStorage.string_.paragraphRangeForRange(TCocoaTextView(
      NSScrollView(fmMain.dbText.Handle).documentView).selectedRange);
    Application.ProcessMessages;
    TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      scrollRangeToVisible(rng);
    key := 0;
  end
  else
  if ((key = 189) and (Shift = [ssMeta, ssCtrl])) then
  begin
    if iFontMonoSize > 6 then
    begin
      iFontMonoSize := iFontMonoSize - 1;
    end;
    FormatListTitleTodo;
    rng := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      textStorage.string_.paragraphRangeForRange(TCocoaTextView(
      NSScrollView(fmMain.dbText.Handle).documentView).selectedRange);
    Application.ProcessMessages;
    TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      scrollRangeToVisible(rng);
    key := 0;
  end;
end;

procedure TfmMain.miEditDisSpellClick(Sender: TObject);
begin
  if miEditDisSpell.Checked = False then
  begin
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      setContinuousSpellCheckingEnabled(False);
    miEditDisSpell.Checked := True;
  end
  else
  begin
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      setContinuousSpellCheckingEnabled(True);
    miEditDisSpell.Checked := False;
  end;
end;

procedure TfmMain.FormActivate(Sender: TObject);
var
  rng: NSRange;
begin
  LabelFileNameChars;
  FormatListTitleTodo;
  // scrollRangeToVisible in MoveToPos doesn't work OnCreate
  rng.location := dbText.SelStart;
  rng.length := 1;
  TCocoaTextView(NSScrollView(dbText.Handle).documentView).
    scrollRangeToVisible(rng);
  dbText.SetFocus;
end;

procedure TfmMain.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  MyIni: TIniFile;
begin
  if SaveFile = False then
  begin
    Abort;
  end;
  try
    MyIni := TIniFile.Create(myHomeDir + myConfigFile);
    if fmMain.WindowState = wsMaximized then
    begin
      MyIni.WriteString('mxmarkedit', 'maximize', 'true');
    end
    else
    begin
      MyIni.WriteString('mxmarkedit', 'maximize', 'false');
      MyIni.WriteInteger('mxmarkedit', 'top', fmMain.Top);
      MyIni.WriteInteger('mxmarkedit', 'left', fmMain.Left);
      MyIni.WriteInteger('mxmarkedit', 'width', fmMain.Width);
      MyIni.WriteInteger('mxmarkedit', 'heigth', fmMain.Height);
    end;
    MyIni.WriteString('mxmarkedit', 'fontname', dbText.Font.Name);
    MyIni.WriteInteger('mxmarkedit', 'fontsize', dbText.Font.Size);
    MyIni.WriteString('mxmarkedit', 'fontcolor', ColorToString(dbText.Font.Color));
    MyIni.WriteString('mxmarkedit', 'fontmononame', stFontMono);
    MyIni.WriteInteger('mxmarkedit', 'fontmonosize', iFontMonoSize);
    MyIni.WriteString('mxmarkedit', 'title1', ColorToString(clTitle1));
    MyIni.WriteString('mxmarkedit', 'title2', ColorToString(clTitle2));
    MyIni.WriteString('mxmarkedit', 'title3', ColorToString(clTitle3));
    MyIni.WriteString('mxmarkedit', 'footnote', ColorToString(clFootnote));
    MyIni.WriteString('mxmarkedit', 'link', ColorToString(clLink));
    MyIni.WriteString('mxmarkedit', 'code', ColorToString(clCode));
    MyIni.WriteInteger('mxmarkedit', 'titlewidth', pnTitTodo.Width);
    MyIni.WriteString('mxmarkedit', 'filename', stFileName);
    MyIni.WriteString('mxmarkedit', 'pantemplate', pandocTemplate);
    MyIni.WriteString('mxmarkedit', 'panoutputput', pandocOutput);
    MyIni.WriteString('mxmarkedit', 'panpath', pandocPath);
    if LastDatabase1 <> '' then
    begin
      MyIni.WriteString('mxmarkedit', 'lastfile1', LastDatabase1);
    end;
    if LastDatabase2 <> '' then
    begin
      MyIni.WriteString('mxmarkedit', 'lastfile2', LastDatabase2);
    end;
    if LastDatabase3 <> '' then
    begin
      MyIni.WriteString('mxmarkedit', 'lastfile3', LastDatabase3);
    end;
    if LastDatabase4 <> '' then
    begin
      MyIni.WriteString('mxmarkedit', 'lastfile4', LastDatabase4);
    end;
    MyIni.WriteInteger('mxmarkedit', 'lastposdatabase1', LastPosDatabase1);
    MyIni.WriteInteger('mxmarkedit', 'lastposdatabase2', LastPosDatabase2);
    MyIni.WriteInteger('mxmarkedit', 'lastposdatabase3', LastPosDatabase3);
    MyIni.WriteInteger('mxmarkedit', 'lastposdatabase4', LastPosDatabase4);
    MyIni.WriteInteger('mxmarkedit', 'topindexdatabase1', TopIndexDatabase1);
    MyIni.WriteInteger('mxmarkedit', 'topindexdatabase2', TopIndexDatabase2);
    MyIni.WriteInteger('mxmarkedit', 'topindexdatabase3', TopIndexDatabase3);
    MyIni.WriteInteger('mxmarkedit', 'topindexdatabase4', TopIndexDatabase4);
  finally
    MyIni.Free;
  end;
  CreateBackup;
end;

procedure TfmMain.dbTextChange(Sender: TObject);
begin
  FormatListTitleTodo;
  LabelFileNameChars;
  blFileSaved := False;
end;

procedure TfmMain.cbFilterChange(Sender: TObject);
begin
  FormatListTitleTodo;
  dbText.SetFocus;
end;

procedure TfmMain.dbTextClick(Sender: TObject);
begin
  FormatListTitleTodo;
  LabelFileNameChars
end;

procedure TfmMain.dbTextKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  stClip: string;
  i, iPos: integer;
  rngStart, rngEnd: NSRange;
  blCode: boolean;
  stText: WideString;
  myDate: TDate;
begin
  if ((key = 8) and (Shift = [ssMeta, ssShift])) then
  begin
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      selectParagraph(nil);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      Delete(nil);
    key := 0;
  end
  else
  if ((key = Ord('D')) and (Shift = [ssMeta])) then
  begin
    if dateformat = 'en' then
    begin
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        insertText(NSStringUtf8(FormatDateTime('dddd mmmm dd yyyy', Date())));
    end
    else
    begin
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        insertText(NSStringUtf8(FormatDateTime('dddd dd mmmm yyyy', Date())));
    end;
    key := 0;
  end
  else
  if ((key = Ord('D')) and (Shift = [ssMeta, ssShift])) then
  begin
    if dateformat = 'en' then
    begin
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        insertText(NSStringUtf8(FormatDateTime('dddd mmmm dd yyyy, hh.mm', Now())));
    end
    else
    begin
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        insertText(NSStringUtf8(FormatDateTime('dddd dd mmmm yyyy, hh.mm', Now())));
    end;
    key := 0;
  end
  else
  if ((key = Ord('G')) and (Shift = [ssMeta])) then
  begin
    fmSearch.bnNextClick(nil);
    key := 0;
  end
  else
  if ((key = 38) and (Shift = [ssAlt, ssMeta])) then
  begin
    if dbText.CaretPos.y > 0 then
    begin
      if dbText.CaretPos.y >= dbText.Lines.Count - 1 then
      begin
        iPos := dbText.SelStart;
        dbText.Lines.Add('');
        dbText.SelStart := iPos;
      end;
      stClip := Clipboard.AsText;
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        selectParagraph(nil);
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        copy_(nil);
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        Delete(nil);
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        moveToBeginningOfParagraph(nil);
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        moveBackward(nil);
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        moveToBeginningOfParagraph(nil);
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        paste(nil);
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        moveBackward(nil);
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        moveToBeginningOfParagraph(nil);
      Clipboard.AsText := stClip;
      if TryStrToInt(UTF8Copy(dbText.Lines[dbText.CaretPos.Y],
        1, UTF8Pos('. ', dbText.Lines[dbText.CaretPos.Y]) - 1), i) = True then
      begin
        RenumberList;
        FormatListTitleTodo;
      end;
    end;
    key := 0;
  end
  else
  if ((key = 40) and (Shift = [ssAlt, ssMeta])) then
  begin
    if dbText.CaretPos.y < dbText.Lines.Count - 1 then
    begin
      stClip := Clipboard.AsText;
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        selectParagraph(nil);
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        copy_(nil);
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        Delete(nil);
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        moveToEndOfParagraph(nil);
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        moveForward(nil);
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        paste(nil);
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        moveBackward(nil);
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        moveToBeginningOfParagraph(nil);
      Clipboard.AsText := stClip;
      if TryStrToInt(UTF8Copy(dbText.Lines[dbText.CaretPos.Y - 1],
        1, UTF8Pos('. ', dbText.Lines[dbText.CaretPos.Y - 1]) - 1), i) = True then
      begin
        RenumberList;
        FormatListTitleTodo;
      end;
    end;
    key := 0;
  end
  else
  if ((key = 38) and (Shift = [ssCtrl, ssMeta])) then
  begin
    key := 0;
    stText := WideString(dbText.Text);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      moveToBeginningOfParagraph(nil);
    iPos := dbText.SelStart;
    blCode := False;
    while iPos > -1 do
    begin
      if ((stText[iPos] = '`') and (stText[iPos + 1] = '`') and
        (stText[iPos + 2] = '`') and ((stText[iPos - 1] = LineEnding) or
        (iPos = 1))) then
      begin
        blCode := not blCode;
      end
      else
      if ((stText[iPos] = '#') and ((stText[iPos - 1] = LineEnding) or
        (iPos = 1)) and (blCode = False)) then
      begin
        dbText.SelStart := iPos;
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).
          moveToBeginningOfParagraph(nil);
        Break;
      end;
      Dec(iPos);
    end;
  end
  else
  if ((key = 40) and (Shift = [ssCtrl, ssMeta])) then
  begin
    key := 0;
    stText := WideString(dbText.Text);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      moveToEndOfParagraph(nil);
    iPos := dbText.SelStart;
    blCode := False;
    while iPos <= Length(stText) do
    begin
      if ((stText[iPos] = '`') and (stText[iPos + 1] = '`') and
        (stText[iPos + 2] = '`') and ((stText[iPos - 1] = LineEnding) or
        (iPos = 1))) then
      begin
        blCode := not blCode;
      end
      else
      if ((stText[iPos] = '#') and (stText[iPos - 1] = LineEnding) and
        (blCode = False)) then
      begin
        dbText.SelStart := iPos;
        Break;
      end;
      Inc(iPos);
    end;
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      moveToBeginningOfParagraph(nil);
  end
  else
  if ((key = Ord('B')) and (Shift = [ssMeta])) then
  begin
    if UTF8Copy(dbText.Text, dbText.SelStart, 1) = LineEnding then
    begin
      key := 0;
      Exit;
    end;
    if dbText.SelLength = 0 then
    begin
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        selectWord(nil);
    end;
    rngStart := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      selectedRange;
    rngEnd.location := rngStart.location + rngStart.length + 2;
    rngEnd.length := 0;
    rngStart.length := 0;
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      setSelectedRange(rngStart);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      insertText(NSStringUtf8('**'));
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      setSelectedRange(rngEnd);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      insertText(NSStringUtf8('**'));
    key := 0;
  end
  else
  if ((key = Ord('I')) and (Shift = [ssMeta])) then
  begin
    if UTF8Copy(dbText.Text, dbText.SelStart, 1) = LineEnding then
    begin
      key := 0;
      Exit;
    end;
    if dbText.SelLength = 0 then
    begin
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        selectWord(nil);
    end;
    rngStart := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      selectedRange;
    rngEnd.location := rngStart.location + rngStart.length + 1;
    rngEnd.length := 0;
    rngStart.length := 0;
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      setSelectedRange(rngStart);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      insertText(NSStringUtf8('*'));
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      setSelectedRange(rngEnd);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      insertText(NSStringUtf8('*'));
    key := 0;
  end
  else
  if ((key = Ord('T')) and ((Shift = [ssMeta]) or
    (Shift = [ssAlt, ssMeta]))) then
  begin
    if ((Copy(dbText.Lines[dbText.CaretPos.Y], 1, 6) = '- [ ] ') or
      ((TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      selectedRange.location = TCocoaTextView(
      NSScrollView(fmMain.dbText.Handle).documentView).textStorage.length) and
      (Copy(dbText.Lines[dbText.CaretPos.Y - 1], 1, 6) = '- [ ] '))) then
    begin
      rngStart := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        textStorage.string_.paragraphRangeForRange(TCocoaTextView(
        NSScrollView(fmMain.dbText.Handle).documentView).selectedRange);
      rngStart.length := 6;
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        insertText_replacementRange(NSStringUtf8('- [X] '), rngStart);
    end
    else
    if ((Copy(dbText.Lines[dbText.CaretPos.Y], 1, 6) = '- [X] ') or
      ((TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      selectedRange.location = TCocoaTextView(
      NSScrollView(fmMain.dbText.Handle).documentView).textStorage.length) and
      (Copy(dbText.Lines[dbText.CaretPos.Y - 1], 1, 6) = '- [X] ')) or
      (Copy(dbText.Lines[dbText.CaretPos.Y], 1, 6) = '- [x] ') or
      ((TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      selectedRange.location = TCocoaTextView(
      NSScrollView(fmMain.dbText.Handle).documentView).textStorage.length) and
      (Copy(dbText.Lines[dbText.CaretPos.Y - 1], 1, 6) = '- [x] '))) then
    begin
      rngStart := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        textStorage.string_.paragraphRangeForRange(TCocoaTextView(
        NSScrollView(fmMain.dbText.Handle).documentView).selectedRange);
      rngStart.length := 6;
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        insertText_replacementRange(NSStringUtf8('- [ ] '), rngStart);
    end
    else
    begin
      rngStart := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        textStorage.string_.paragraphRangeForRange(TCocoaTextView(
        NSScrollView(fmMain.dbText.Handle).documentView).selectedRange);
      rngStart.Length := 0;
      if Shift = [ssAlt, ssMeta] then
      begin
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).
          insertText_replacementRange(NSStringUtf8('- [ ] '), rngStart);
      end
      else
      begin
        myDate := Date;
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).
          insertText_replacementRange(NSStringUtf8('- [ ] ' +
          FormatDateTime('yyyy-mm-dd', IncWeek(myDate, 1)) + ' • '), rngStart);
      end;
    end;
    key := 0;
  end
  else
  if ((key = Ord('J')) and (Shift = [ssMeta, ssShift])) then
  begin
    iBookmarkPos := dbText.SelStart;
    key := 0;
  end
  else
  if ((key = Ord('J')) and (Shift = [ssMeta])) then
  begin
    if UTF8Length(dbText.Text) > iBookmarkPos then
    begin
      dbText.SelStart := iBookmarkPos;
    end;
    key := 0;
  end
  else
  if ((key = Ord('U')) and (Shift = [ssMeta])) then
  begin
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      uppercaseWord(nil);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      moveForward(nil);
    key := 0;
  end
  else
  if ((key = Ord('U')) and (Shift = [ssMeta, ssAlt, ssShift])) then
  begin
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      lowercaseWord(nil);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      moveForward(nil);
    key := 0;
  end
  else
  if ((key = Ord('U')) and (Shift = [ssMeta, ssAlt])) then
  begin
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      capitalizeWord(nil);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      moveForward(nil);
    key := 0;
  end
  else
  if ((key = Ord('E')) and (Shift = [ssMeta])) then
  begin
    if miEditDisableForm.Checked = True then
    begin
      miEditDisableFormClick(nil);
    end;
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      moveToEndOfParagraph(nil);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      moveForward(nil);
    while (((dbText.Lines[dbText.CaretPos.Y] = '') or
      (dbText.Lines[dbText.CaretPos.Y] = '---') or
      (Copy(dbText.Lines[dbText.CaretPos.Y], 1, 2) = '# ') or
      (Copy(dbText.Lines[dbText.CaretPos.Y], 1, 3) = '## ') or
      (Copy(dbText.Lines[dbText.CaretPos.Y], 1, 4) = '### ') or
      (Copy(dbText.Lines[dbText.CaretPos.Y], 1, 5) = '#### ') or
      (Copy(dbText.Lines[dbText.CaretPos.Y], 1, 6) = '##### ') or
      (Copy(dbText.Lines[dbText.CaretPos.Y], 1, 7) = '###### '))
      and (dbText.CaretPos.Y < dbText.Lines.Count)) do
    begin
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        moveForward(nil);
    end;
    FormatListTitleTodo;
    rngStart := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      textStorage.string_.paragraphRangeForRange(TCocoaTextView(
      NSScrollView(fmMain.dbText.Handle).documentView).selectedRange);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
      addAttribute_value_range(NSBackgroundColorAttributeName,
      ColorToNSColor(clHighlightText), rngStart);
    TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      scrollRangeToVisible(rngStart);
    ShowCurrentTitleTodo;
    key := 0;
  end
  else
  if ((key = Ord('1')) and (Shift = [ssAlt, ssMeta])) then
  begin
   cbFilter.ItemIndex := 0;
   FormatListTitleTodo;
   dbText.SetFocus;
  end
  else
  if ((key = Ord('2')) and (Shift = [ssAlt, ssMeta])) then
  begin
   cbFilter.ItemIndex := 1;
   FormatListTitleTodo;
   dbText.SetFocus;
  end
  else
  if ((key = Ord('3')) and (Shift = [ssAlt, ssMeta])) then
  begin
   cbFilter.ItemIndex := 2;
   FormatListTitleTodo;
   dbText.SetFocus;
  end
  else
  if ((key = Ord('4')) and (Shift = [ssAlt, ssMeta])) then
  begin
   cbFilter.ItemIndex := 3;
   FormatListTitleTodo;
   dbText.SetFocus;
  end
  else
  if ((key = Ord('5')) and (Shift = [ssAlt, ssMeta])) then
  begin
   cbFilter.ItemIndex := 4;
   FormatListTitleTodo;
   dbText.SetFocus;
  end
  else
  if ((key = Ord('6')) and (Shift = [ssAlt, ssMeta])) then
  begin
   cbFilter.ItemIndex := 5;
   FormatListTitleTodo;
   dbText.SetFocus;
  end;
end;

procedure TfmMain.dbTextKeyPress(Sender: TObject; var Key: char);
var
  i: integer;
  myDate: TDate;
  fs: TFormatSettings;
begin
  if key = #13 then
  begin
    if ((dbText.Lines[dbText.CaretPos.Y - 1] = '- [ ] ') or
      (dbText.Lines[dbText.CaretPos.Y - 1] = '- [X] ') or
      (dbText.Lines[dbText.CaretPos.Y - 1] = '- [x] ') or
      (dbText.Lines[dbText.CaretPos.Y - 1] = '+ ') or
      (dbText.Lines[dbText.CaretPos.Y - 1] = '- ') or
      (dbText.Lines[dbText.CaretPos.Y - 1] = '+ ') or
      (dbText.Lines[dbText.CaretPos.Y - 1] = '* ') or
      ((TryStrToInt(UTF8Copy(dbText.Lines[dbText.CaretPos.Y - 1],
      1, UTF8Pos('. ', dbText.Lines[dbText.CaretPos.Y - 1]) - 1), i) = True) and
      (UTF8Pos('. ', dbText.Lines[dbText.CaretPos.Y - 1]) > 1) and
      (UTF8Length(dbText.Lines[dbText.CaretPos.Y - 1]) =
      UTF8Pos('. ', dbText.Lines[dbText.CaretPos.Y - 1]) + 1))) then
    begin
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        deleteBackward(nil);
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        deleteToBeginningOfParagraph(nil);
    end
    else
    begin
      fs := DefaultFormatSettings;
      fs.DateSeparator := '-';
      fs.ShortDateFormat := 'yyyy/mm/dd';
      if UTF8Copy(dbText.Lines[dbText.CaretPos.Y - 1], 1, 6) = '- [ ] ' then
      begin
        if TryStrToDate(UTF8Copy(dbText.Lines[dbText.CaretPos.Y - 1], 7, 10),
          myDate, fs) = True then
        begin
          TCocoaTextView(NSScrollView(dbText.Handle).documentView).
            insertText(NSStringUtf8('- [ ] ' +
            UTF8Copy(dbText.Lines[dbText.CaretPos.Y - 1], 7, 13)));
        end
        else
        begin
          TCocoaTextView(NSScrollView(dbText.Handle).documentView).
            insertText(NSStringUtf8('- [ ] '));
        end;
      end
      else
      if UTF8Copy(dbText.Lines[dbText.CaretPos.Y - 1], 1, 6) = '- [X] ' then
      begin
        if TryStrToDate(UTF8Copy(dbText.Lines[dbText.CaretPos.Y - 1], 7, 10),
          myDate, fs) = True then
        begin
          TCocoaTextView(NSScrollView(dbText.Handle).documentView).
            insertText(NSStringUtf8('- [X] ' +
            UTF8Copy(dbText.Lines[dbText.CaretPos.Y - 1], 7, 13)));
        end
        else
        begin
          TCocoaTextView(NSScrollView(dbText.Handle).documentView).
            insertText(NSStringUtf8('- [X] '));
        end;
      end
      else
      if UTF8Copy(dbText.Lines[dbText.CaretPos.Y - 1], 1, 6) = '- [x] ' then
      begin
        if TryStrToDate(UTF8Copy(dbText.Lines[dbText.CaretPos.Y - 1], 7, 13),
          myDate, fs) = True then
        begin
          TCocoaTextView(NSScrollView(dbText.Handle).documentView).
            insertText(NSStringUtf8('- [x] ' +
            UTF8Copy(dbText.Lines[dbText.CaretPos.Y - 1], 7, 10)));
        end
        else
        begin
          TCocoaTextView(NSScrollView(dbText.Handle).documentView).
            insertText(NSStringUtf8('- [x] '));
        end;
      end
      else
      if UTF8Copy(dbText.Lines[dbText.CaretPos.Y - 1], 1, 2) = '* ' then
      begin
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).
          insertText(NSStringUtf8('* '));
      end
      else
      if UTF8Copy(dbText.Lines[dbText.CaretPos.Y - 1], 1, 2) = '+ ' then
      begin
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).
          insertText(NSStringUtf8('+ '));
      end
      else
      if UTF8Copy(dbText.Lines[dbText.CaretPos.Y - 1], 1, 2) = '- ' then
      begin
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).
          insertText(NSStringUtf8('- '));
      end
      else
      if TryStrToInt(UTF8Copy(dbText.Lines[dbText.CaretPos.Y - 1],
        1, UTF8Pos('. ', dbText.Lines[dbText.CaretPos.Y - 1]) - 1), i) = True then
      begin
        Inc(i);
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).
          insertText(NSStringUtf8(IntToStr(i) + '. '));
        RenumberList;
        FormatListTitleTodo;
      end;
    end;
  end;
end;

procedure TfmMain.dbTextKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if ((key > 36) and (key < 41)) then
  begin
    FormatListTitleTodo;
    LabelFileNameChars;
  end;
end;

procedure TfmMain.sgTitlesClick(Sender: TObject);
var
  i, iLen, iHeader: integer;
  stText: WideString = '';
  rng: NSRange;
  blCode: boolean = False;
begin
  stText := WideString(dbText.Text);
  iLen := Length(stText);
  i := 1;
  iHeader := 0;
  while i <= iLen do
  begin
    if ((stText[i] = '`') and (stText[i + 1] = '`') and
      (stText[i + 2] = '`') and ((stText[i - 1] = LineEnding) or
      (i = 1))) then
    begin
      if blCode = False then
      begin
        blCode := True;
      end
      else
      begin
        blCode := False;
      end;
    end
    else
    if (((stText[i] = '#') or ((stText[i] = '-') and (stText[i + 1] = ' ') and
      (stText[i + 2] = '[') and (stText[i + 4] = ']'))) and
      ((i = 1) or (stText[i - 1] = LineEnding))) then
    begin
      if blCode = False then
      begin
        Inc(iHeader);
      end;
      if iHeader = sgTitles.Row + 1 then
      begin
        dbText.SelStart := i - 1;
        Application.ProcessMessages;
        rng := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
          textStorage.string_.paragraphRangeForRange(TCocoaTextView(
          NSScrollView(fmMain.dbText.Handle).documentView).selectedRange);
        TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
          showFindIndicatorForRange(rng);
        dbText.SelStart := i - 1;
        dbText.SetFocus;
        FormatListTitleTodo;
        Break;
      end;
    end;
    Inc(i);
  end;
end;

procedure TfmMain.sgTitlesDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
begin
  if ((Pos('  ☑ ', sgTitles.Cells[aCol, aRow]) > 0) or
    (Pos('  □ ', sgTitles.Cells[aCol, aRow]) > 0)) then
  begin
    sgTitles.canvas.Font.Color := dbText.Font.Color;
  end
  else
  if Copy(sgTitles.Cells[aCol, aRow], 1, 9) = '         ' then
  begin
    sgTitles.canvas.Font.Color := clTitle3;
  end
  else
  if Copy(sgTitles.Cells[aCol, aRow], 1, 6) = '      ' then
  begin
    sgTitles.canvas.Font.Color := clTitle2;
  end
  else
  if Copy(sgTitles.Cells[aCol, aRow], 1, 3) = '   ' then
  begin
    sgTitles.canvas.Font.Color := clTitle1;
  end;
  sgTitles.Canvas.TextOut(aRect.Left + 3, aRect.Top + 5,
    sgTitles.Cells[aCol, aRow]);
end;

procedure TfmMain.sgTitlesGetCellHint(Sender: TObject; ACol, ARow: integer;
  var HintText: string);
begin
  if sgTitles.Canvas.TextWidth(sgTitles.Cells[ACol, ARow]) > sgTitles.Width - 20 then
  begin
    HintText := sgTitles.Cells[ACol, ARow];
  end;
end;

procedure TfmMain.sgTitlesPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
  aState: TGridDrawState);
begin
  if sgTitles.Cells[1, aRow] = ' ' then
  begin
    sgTitles.Canvas.Brush.Color := clHighlightList;
  end;
end;

procedure TfmMain.tmDateTimeTimer(Sender: TObject);
begin
  if dateformat = 'en' then
  begin
    lbDateTime.Caption := FormatDateTime('dddd mmmm dd yyyy • hh.mm', Now());
  end
  else
  begin
    lbDateTime.Caption := FormatDateTime('dddd dd mmmm yyyy • hh.mm', Now());
  end;
end;

// *******************************************************
// *************** Menu procedures **************
// *******************************************************

procedure TfmMain.miFileNewClick(Sender: TObject);
begin
  if SaveFile = False then
  begin
    Exit;
  end;
  CreateBackup;
  stFileName := '';
  CreateYAML;
end;

procedure TfmMain.miFileOpenClick(Sender: TObject);
begin
  if SaveFile = False then
  begin
    Exit;
  end;
  CreateBackup;
  if odOpen.Execute then
  try
    stFileName := odOpen.FileName;
    dbText.Lines.LoadFromFile(stFileName);
    iBookmarkPos := 0;
    MoveToPos;
    LabelFileNameChars;
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      checkTextInDocument(nil);
    UpdateLastFile;
    ShowCurrentTitleTodo;
  except
    MessageDlg(msg004, mtWarning, [mbOK], 0);
  end;
end;

procedure TfmMain.miFileSaveClick(Sender: TObject);
var
  myList: TStringList;
begin
  if stFileName <> '' then
  try
    try
      myList := TStringList.Create;
      myList.Text := dbText.Text;
      myList.SaveToFile(stFileName);
    finally
      myList.Free;
    end;
    blFileSaved := False;
  except
    MessageDlg(msg003, mtWarning, [mbOK], 0);
  end
  else
  begin
    miFileSaveAsClick(nil);
  end;
end;

procedure TfmMain.miFileSaveAsClick(Sender: TObject);
var
  myList: TStringList;
begin
  if sdSave.Execute then
  try
    stFileName := sdSave.FileName;
    try
      myList := TStringList.Create;
      myList.Text := dbText.Text;
      myList.SaveToFile(stFileName);
      UpdateLastFile;
    finally
      myList.Free;
    end;
    LabelFileNameChars;
    blFileSaved := False;
  except
    MessageDlg(msg003, mtWarning, [mbOK], 0);
  end;
end;

procedure TfmMain.miFileOpenLast1Click(Sender: TObject);
begin
  if SaveFile = False then
  begin
    Exit;
  end;
  CreateBackup;
  if FileExistsUTF8(LastDatabase1) then
  try
    stFileName := LastDatabase1;
    dbText.Lines.LoadFromFile(stFileName);
    iBookmarkPos := 0;
    MoveToPos;
    LabelFileNameChars;
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      checkTextInDocument(nil);
    UpdateLastFile;
    ShowCurrentTitleTodo;
  except
    MessageDlg(msg004, mtWarning, [mbOK], 0);
  end
  else
  begin
    MessageDlg(msg007, mtWarning, [mbOK], 0);
  end;
end;

procedure TfmMain.miFileOpenLast2Click(Sender: TObject);
begin
  if SaveFile = False then
  begin
    Exit;
  end;
  CreateBackup;
  if FileExistsUTF8(LastDatabase2) then
  try
    stFileName := LastDatabase2;
    dbText.Lines.LoadFromFile(stFileName);
    iBookmarkPos := 0;
    MoveToPos;
    LabelFileNameChars;
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      checkTextInDocument(nil);
    UpdateLastFile;
  except
    MessageDlg(msg004, mtWarning, [mbOK], 0);
  end
  else
  begin
    MessageDlg(msg007, mtWarning, [mbOK], 0);
  end;
end;

procedure TfmMain.miFileOpenLast3Click(Sender: TObject);
begin
  if SaveFile = False then
  begin
    Exit;
  end;
  CreateBackup;
  if FileExistsUTF8(LastDatabase3) then
  try
    stFileName := LastDatabase3;
    dbText.Lines.LoadFromFile(stFileName);
    iBookmarkPos := 0;
    MoveToPos;
    LabelFileNameChars;
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      checkTextInDocument(nil);
    UpdateLastFile;
  except
    MessageDlg(msg004, mtWarning, [mbOK], 0);
  end
  else
  begin
    MessageDlg(msg007, mtWarning, [mbOK], 0);
  end;
end;

procedure TfmMain.miFileOpenLast4Click(Sender: TObject);
begin
  if SaveFile = False then
  begin
    Exit;
  end;
  CreateBackup;
  if FileExistsUTF8(LastDatabase4) then
  try
    stFileName := LastDatabase4;
    dbText.Lines.LoadFromFile(stFileName);
    iBookmarkPos := 0;
    MoveToPos;
    LabelFileNameChars;
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      checkTextInDocument(nil);
    UpdateLastFile;
  except
    MessageDlg(msg004, mtWarning, [mbOK], 0);
  end
  else
  begin
    MessageDlg(msg007, mtWarning, [mbOK], 0);
  end;
end;

procedure TfmMain.miEditCopyClick(Sender: TObject);
begin
  TCocoaTextView(NSScrollView(dbText.Handle).documentView).
    copy_(nil);
end;

procedure TfmMain.miEditCutClick(Sender: TObject);
begin
  TCocoaTextView(NSScrollView(dbText.Handle).documentView).
    cut(nil);
end;

procedure TfmMain.miEditPasteClick(Sender: TObject);
begin
  TCocoaTextView(NSScrollView(dbText.Handle).documentView).
    pasteAsPlainText(nil);
  TCocoaTextView(NSScrollView(dbText.Handle).documentView).
    checkTextInDocument(nil);
end;

procedure TfmMain.miEditSelectAllClick(Sender: TObject);
begin
  dbText.SelectAll;
end;

procedure TfmMain.miEditFindClick(Sender: TObject);
begin
  fmSearch.Show;
end;

procedure TfmMain.miEditLinkClick(Sender: TObject);
var
  stLink: String;
begin
  if odLink.Execute then
  begin
    stLink := odLink.FileName;
    stLink := 'file://' + StringReplace(stLink, ' ', '%20', [rfReplaceAll]);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      insertText(NSStringUtf8(stLink));
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      checkTextInDocument(nil);
  end;
end;

procedure TfmMain.miEditFindDuplicateClick(Sender: TObject);
var
  rng: NSRange;
  iLen, i, iSelStart: Integer;
  slList1, slList2: TStringList;
  stWord: NSAttributedString;
begin
  if dbText.SelLength = 0 then
  begin
    if MessageDlg(msg010, mtConfirmation, [mbOK, mbCancel], 0) = mrCancel then
    begin
      Exit;
    end;
  end;
  try
    Screen.Cursor := crHourGlass;
    slList1 := TStringList.Create;
    slList2 := TStringList.Create;
    i := 0;
    iSelStart := -1;
    if dbText.SelLength = 0 then
    begin
      rng.location := 0;
      rng.length := 1;
      iLen := Length(WideString(dbText.Text));
    end
    else
    begin
      rng.location := dbText.SelStart;
      rng.length := 1;
      iLen := dbText.SelStart + dbText.SelLength;
      iSelStart := dbText.SelStart;
    end;
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      setSelectedRange(rng);
    if iSelStart = -1 then
    begin
      dbText.SelStart := 0;
      // To clear the selection of the title
      FormatListTitleTodo;
    end;
    while rng.location + rng.length < iLen - 1 do
    begin
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        moveWordForward(nil);
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        selectWord(nil);
      rng := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).
        documentView).selectedRange;
      if ((UTF8Copy(dbText.Text, rng.location, 2) = '. ') or
        (UTF8Copy(dbText.Text, rng.location, 2) = '.' + LineEnding)) then
      begin
        slList1.Text := slList2.Text;
        slList2.Clear;
      end;
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        moveBackward(nil);
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        moveBackward(nil);
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        selectWord(nil);
      rng := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).
        documentView).selectedRange;
      stWord := TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        attributedSubstringFromRange(rng);
      slList2.Add(NSStringToString(stWord.string_));
      if slList1.IndexOf(NSStringToString(stWord.string_)) > -1 then
      begin
        TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
          setTextColor_range(ColorToNSColor(clRepetition), rng);
      end;
      Inc(i);
      if i > 3000 then
      begin
        dbText.SelLength := 0;
        Application.ProcessMessages;
        i := 0;
      end;
    end;
    if iSelStart = -1 then
    begin
      dbText.SelStart := 0;
    end
    else
    begin
      dbText.SelStart := iSelStart;
    end;
    dbText.SelLength := 0;
  finally
    Screen.Cursor := crDefault;
    slList1.Free;
    slList2.Free;
  end;
end;

procedure TfmMain.miEditShowCurrentClick(Sender: TObject);
begin
  ShowCurrentTitleTodo;
end;

procedure TfmMain.miEditTasksClick(Sender: TObject);
begin
  fmTasks.ShowModal;
end;

procedure TfmMain.miEditHideListClick(Sender: TObject);
begin
  if pnTitTodo.Visible = True then
  begin
    pnTitTodo.Visible := False;
    spTitles.Visible := False;
    miEditHideList.Checked := True;
    FormatListTitleTodo;
  end
  else
  begin
    pnTitTodo.Visible := True;
    spTitles.Visible := True;
    miEditHideList.Checked := False;
    FormatListTitleTodo;
    ShowCurrentTitleTodo;
  end;
end;

procedure TfmMain.miEditDisableFormClick(Sender: TObject);
var
  iLen, iPos: integer;
  stText: WideString = '';
  myFont: NSFont;
  fd: NSFontDescriptor;
  rng: NSRange;
begin
  miEditDisableForm.Checked := not miEditDisableForm.Checked;
  iPos := dbText.SelStart;
  if miEditDisableForm.Checked = True then
  begin
    blDisableFormatting := True;
    stText := WideString(dbText.Text);
    iLen := Length(stText);
    rng.location := 0;
    rng.length := iLen;
    TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      setTextColor_range(ColorToNSColor(dbText.Font.Color), rng);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
      applyFontTraits_range(NSUnboldFontMask, rng);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
      applyFontTraits_range(NSUnitalicFontMask, rng);
    fd := FindFont(dbText.Font.Name, 0);
    myFont := NSFont.fontWithDescriptor_size(fd, -dbText.font.Height);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
      addAttribute_value_range(NSFontAttributeName, myFont, rng);
  end
  else
  begin
    blDisableFormatting := False;
    FormatListTitleTodo;
  end;
  Application.ProcessMessages;
  dbText.SelStart := iPos;
end;

procedure TfmMain.miToolsPandocClick(Sender: TObject);
var
  stArgument, stOutput: string;
begin
  if stFileName = '' then
  begin
    MessageDlg(msg008, mtWarning, [mbOK], 0);
    Exit;
  end
  else
  if FileExistsUTF8(stFileName) = False then
  begin
    MessageDlg(msg007, mtWarning, [mbOK], 0);
    Exit;
  end;
  if SaveFile = False then
  begin
    Exit;
  end;
  stOutput := ExtractFileNameWithoutExt(stFileName) + pandocOutput;
  if FileExistsUTF8(pandocTemplate) then
  begin
    stArgument := pandocPath + 'pandoc ' + '--from markdown' +
      pandocOptions + ' -s "' + stFileName + '" -o "' + stOutput +
      '" --reference-doc "' + pandocTemplate + '" && open "' + stOutput + '"';
  end
  else
  begin
    stArgument := pandocPath + 'pandoc ' + '--from markdown' +
      pandocOptions + ' -s "' + stFileName + '" -o "' + stOutput +
      '" && open "' + stOutput + '"';
  end;
  try
    Screen.Cursor := crHourGlass;
    Application.ProcessMessages;
    Unix.fpSystem(stArgument);
  finally
    Screen.Cursor := crDefault;
  end;
end;

procedure TfmMain.miToolsOpenWinClick(Sender: TObject);
begin
  // FileExist doesn't work on app directory
  Unix.fpSystem('open -n /Applications/mxMarkEdit.app');
end;

procedure TfmMain.miToolsTrans1Click(Sender: TObject);
begin
  fmMain.AlphaBlendValue := 255;
  miToolsTrans1.Checked := True;
end;

procedure TfmMain.miToolsTrans2Click(Sender: TObject);
begin
  fmMain.AlphaBlendValue := 210;
  miToolsTrans2.Checked := True;
end;

procedure TfmMain.miToolsTrans3Click(Sender: TObject);
begin
  fmMain.AlphaBlendValue := 170;
  miToolsTrans3.Checked := True;
end;

procedure TfmMain.miToolsOptionsClick(Sender: TObject);
begin
  fmOptions.ShowModal;
end;

procedure TfmMain.miCopyrightClick(Sender: TObject);
begin
  fmCopyright.ShowModal;
end;


// *******************************************************
// *************** Procedures of formatting **************
// *******************************************************

procedure TfmMain.FormatListTitleTodo;
var
  i, iLen, iPos, iTopRow, iLevel: integer;
  blHeading, blPosInHeading, blBoldItalics, blItalics, blBold, blMono,
  blQuote, blStartLinesQuote, blFootnote, blLink: boolean;
  iStartHeading, iStartBoldItalics, iStartItalics, iStartBold,
  iStartMono, iStartQuote, iStartLinesQuote, iStartFootnote, iStartLink: integer;
  stText: WideString = '';
  stTitle: WideString = '';
  stSpaces: string = '';
  fd: NSFontDescriptor;
  myFont, quoteFont, monoFont, miniFont: NSFont;
  rng: NSRange;
begin
  if ((dbText.Text = '') or ((blDisableFormatting = True) and
    (pnTitTodo.Visible = False))) then
  begin
    Exit;
  end;
  iTopRow := sgTitles.TopRow;
  blHeading := False;
  blBoldItalics := False;
  blItalics := False;
  blBold := False;
  blMono := False;
  blStartLinesQuote := False;
  blQuote := False;
  blFootnote := False;
  blLink := False;
  blPosInHeading := False;
  iStartHeading := -1;
  iStartBoldItalics := -1;
  iStartItalics := -1;
  iStartBold := -1;
  iStartMono := -1;
  iStartQuote := -1;
  iStartLinesQuote := -1;
  iStartFootnote := -1;
  iStartLink := -1;
  iLevel := -1;
  stText := WideString(dbText.Text);
  iLen := Length(stText);
  rng.location := 0;
  rng.length := iLen;
  if blDisableFormatting = False then
  begin
    TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      setTextColor_range(ColorToNSColor(dbText.Font.Color), rng);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
      applyFontTraits_range(NSUnboldFontMask, rng);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
      applyFontTraits_range(NSUnitalicFontMask, rng);
    fd := FindFont(dbText.Font.Name, 0);
    myFont := NSFont.fontWithDescriptor_size(fd, -dbText.font.Height);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
      addAttribute_value_range(NSFontAttributeName, myFont, rng);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
      removeAttribute_range(NSBackgroundColorAttributeName, rng);
    quoteFont := NSFont.fontWithDescriptor_size(fd, -dbText.font.Height - 4);
    miniFont := NSFont.fontWithDescriptor_size(fd, 1);
    fd := FindFont(stFontMono, 0);
    monoFont := NSFont.fontWithDescriptor_size(fd, iFontMonoSize);
  end;
  sgTitles.Clear;

  // Markdown headings
  if blDisableFormatting = False then
  begin
    if dbText.Lines[0] = '---' then
    begin
      rng.location := 0;
      rng.length := 3;
      TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        setTextColor_range(ColorToNSColor(clTitle1), rng);
      iPos := 4;
      for i := 1 to dbText.Lines.Count - 1 do
      begin
        if i = 10 then
        begin
          Break;
        end;
        if dbText.Lines[i] = '---' then
        begin
          rng.location := iPos;
          rng.length := 3;
          TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
            setTextColor_range(ColorToNSColor(clTitle1), rng);
          Break;
        end;
        rng.location := iPos;
        rng.length := UTF8CocoaPos(':', dbText.Lines[i]);
        TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
          setTextColor_range(ColorToNSColor(clTitle1), rng);
        iPos := iPos + StrToNSString(dbText.Lines[i], True).length +
          UTF8Length(LineEnding);
      end;
    end;
  end;

  // Text
  i := 1;
  while i <= iLen do
  begin
    if i = dbText.SelStart + 2 then
    begin
      if blHeading = False then
      begin
        if sgTitles.RowCount > 0 then
        begin
          sgTitles.Cells[1, sgTitles.RowCount - 1] := ' ';
          sgTitles.RowHeights[sgTitles.RowCount - 1] := iTitleTodoRowHeight;
        end;
      end
      else
      begin
        blPosInHeading := True;
      end;
    end;
    // Headings and titles
    if ((stText[i] = '#') and ((i = 1) or (stText[i - 1] = LineEnding))) then
    begin
      if ((Copy(stText, i, 2) = '# ') or (Copy(stText, i, 3) = '## ') or
        (Copy(stText, i, 4) = '### ') or (Copy(stText, i, 5) = '#### ') or
        (Copy(stText, i, 6) = '##### ') or (Copy(stText, i, 7) = '###### ')) then
      begin
        if blStartLinesQuote = False then
        begin
          blHeading := True;
          iStartHeading := i;
        end;
      end;
    end
    else
    if ((stText[i] = '-') and ((i = 1) or (stText[i - 1] = LineEnding))) then
    begin
      if ((Copy(stText, i, 6) = '- [ ] ') or (Copy(stText, i, 6) = '- [x] ') or
        (Copy(stText, i, 6) = '- [X] ')) then
      begin
        if blStartLinesQuote = False then
        begin
          blHeading := True;
        end;
      end;
    end
    else
    if ((stText[i] = '>') and (stText[i + 1] = ' ') and
      ((i = 1) or (stText[i - 1] = LineEnding))) then
    begin
      blQuote := True;
      iStartQuote := i;
    end;
    if blHeading = True then
    begin
      stTitle := stTitle + stText[i];
    end;
    if stText[i] = LineEnding then
    begin
      blBoldItalics := False;
      blItalics := False;
      blBold := False;
      blMono := False;
      blFootnote := False;
      blLink := False;
    end;
    if ((stText[i] = LineEnding) or (i = iLen)) then
    begin
      if blHeading = True then
      begin
        blHeading := False;
        if blDisableFormatting = False then
        begin
          if Copy(stTitle, 1, 2) = '# ' then
          begin
            rng.location := iStartHeading - 1;
            rng.length := 1;
            TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
              addAttribute_value_range(NSFontAttributeName, miniFont, rng);
            rng.location := iStartHeading;
            rng.length := i - iStartHeading;
            TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
              setTextColor_range(ColorToNSColor(clTitle1), rng);
            TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
              applyFontTraits_range(NSBoldFontMask, rng);
          end
          else if Copy(stTitle, 1, 3) = '## ' then
          begin
            rng.location := iStartHeading - 1;
            rng.length := 2;
            TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
              addAttribute_value_range(NSFontAttributeName, miniFont, rng);
            rng.location := iStartHeading + 1;
            rng.length := i - iStartHeading - 1;
            TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
              setTextColor_range(ColorToNSColor(clTitle2), rng);
            TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
              applyFontTraits_range(NSItalicFontMask, rng);
          end
          else if Copy(stTitle, 1, 4) = '### ' then
          begin
            rng.location := iStartHeading - 1;
            rng.length := 3;
            TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
              addAttribute_value_range(NSFontAttributeName, miniFont, rng);
            rng.location := iStartHeading + 2;
            rng.length := i - iStartHeading - 2;
            TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
              setTextColor_range(ColorToNSColor(clTitle3), rng);
            TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
              applyFontTraits_range(NSItalicFontMask, rng);
          end
          else if Copy(stTitle, 1, 5) = '#### ' then
          begin
            rng.location := iStartHeading - 1;
            rng.length := 4;
            TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
              addAttribute_value_range(NSFontAttributeName, miniFont, rng);
            rng.location := iStartHeading + 3;
            rng.length := i - iStartHeading - 3;
            TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
              setTextColor_range(ColorToNSColor(clTitle3), rng);
            TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
              applyFontTraits_range(NSItalicFontMask, rng);
          end
          else if Copy(stTitle, 1, 6) = '##### ' then
          begin
            rng.location := iStartHeading - 1;
            rng.length := 5;
            TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
              addAttribute_value_range(NSFontAttributeName, miniFont, rng);
            rng.location := iStartHeading + 4;
            rng.length := i - iStartHeading - 4;
            TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
              setTextColor_range(ColorToNSColor(clTitle3), rng);
            TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
              applyFontTraits_range(NSItalicFontMask, rng);
          end
          else if Copy(stTitle, 1, 7) = '###### ' then
          begin
            rng.location := iStartHeading - 1;
            rng.length := 6;
            TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
              addAttribute_value_range(NSFontAttributeName, miniFont, rng);
            rng.location := iStartHeading + 5;
            rng.length := i - iStartHeading - 5;
            TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
              setTextColor_range(ColorToNSColor(clTitle3), rng);
            TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
              applyFontTraits_range(NSItalicFontMask, rng);
          end;
        end;
        iLevel := 0;
        if Copy(stTitle, 1, 6) = '- [ ] ' then
        begin
          stTitle := stSpaces + '  □ ' + Copy(stTitle, 7, Length(stTitle));
        end
        else
        if ((Copy(stTitle, 1, 6) = '- [X] ') or
          (Copy(stTitle, 1, 6) = '- [x] ')) then
        begin
          stTitle := stSpaces + '  ☑ ' + Copy(stTitle, 7, Length(stTitle));
        end
        else
        begin
          stSpaces := '';
          iLevel := 0;
          if Copy(stTitle, 1, 2) = '# ' then
          begin
            stTitle := '   ' + Copy(stTitle, 3, Length(stTitle));
            stSpaces := stSpaces + '    ';
            iLevel := 1;
          end
          else
          if Copy(stTitle, 1, 3) = '## ' then
          begin
            stTitle := '      ' + Copy(stTitle, 4, Length(stTitle));
            stSpaces := stSpaces + '       ';
            iLevel := 2;
          end
          else
          if Copy(stTitle, 1, 4) = '### ' then
          begin
            stTitle := '         ' + Copy(stTitle, 5, Length(stTitle));
            stSpaces := stSpaces + '          ';
            iLevel := 3;
          end
          else
          if Copy(stTitle, 1, 5) = '#### ' then
          begin
            stTitle := '            ' + Copy(stTitle, 6, Length(stTitle));
            stSpaces := stSpaces + '             ';
            iLevel := 4;
          end
          else
          if Copy(stTitle, 1, 6) = '##### ' then
          begin
            stTitle := '               ' + Copy(stTitle, 7, Length(stTitle));
            stSpaces := stSpaces + '                ';
            iLevel := 5;
          end
          else
          if Copy(stTitle, 1, 7) = '###### ' then
          begin
            stTitle := '                  ' + Copy(stTitle, 8, Length(stTitle));
            stSpaces := stSpaces + '                   ';
            iLevel := 6;
          end;
        end;
        sgTitles.RowCount := sgTitles.RowCount + 1;
        sgTitles.Cells[0, sgTitles.RowCount - 1] := string(stTitle);
        if blPosInHeading = True then
        begin
          sgTitles.Cells[1, sgTitles.RowCount - 1] := ' ';
          blPosInHeading := False;
        end;
        if ((iLevel <= cbFilter.ItemIndex + 1) or
          (sgTitles.Cells[1, sgTitles.RowCount - 1] = ' ')) then
        begin
          sgTitles.RowHeights[sgTitles.RowCount - 1] := iTitleTodoRowHeight;
        end
        else
        begin
          sgTitles.RowHeights[sgTitles.RowCount - 1] := 0;
         end;
        stTitle := '';
      end
      else
      // Quote
      if blQuote = True then
      begin
        if blDisableFormatting = False then
        begin
          blQuote := False;
          rng.location := iStartQuote - 1;
          rng.length := i - iStartQuote + 1;
          TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
          addAttribute_value_range(NSFontAttributeName, quoteFont, rng);
          rng.location := iStartQuote - 1;
          rng.length := 1;
          TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
            addAttribute_value_range(NSFontAttributeName, miniFont, rng);
        end;
      end;
    end;
    if blDisableFormatting = True then
    begin
      Inc(i);
      Continue;
    end;
    // Bold Italics
    if (((stText[i] = '*') and (stText[i + 1] = '*') and (stText[i + 2] = '*')) or
      ((stText[i] = '_') and (stText[i + 1] = '_') and (stText[i + 2] = '_'))) then
    begin
      if ((blBoldItalics = False) and (blMono = False) and
        (stText[i + 3] <> ' ')) then
      begin
        blBoldItalics := True;
        iStartBoldItalics := i;
      end
      else
      if ((blBoldItalics = True) and (stText[i - 1] <> ' ')) then
      begin
        blBoldItalics := False;
        rng.location := iStartBoldItalics + 2;
        rng.length := i - iStartBoldItalics - 3;
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
          applyFontTraits_range(NSBoldFontMask, rng);
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
          applyFontTraits_range(NSItalicFontMask, rng);
        rng.location := iStartBoldItalics - 1;
        rng.length := 3;
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
          addAttribute_value_range(NSFontAttributeName, miniFont, rng);
        rng.location := i - 1;
        rng.length := 3;
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
          addAttribute_value_range(NSFontAttributeName, miniFont, rng);
      end;
    end
    // Bold
    else if (((stText[i] = '*') and (stText[i + 1] = '*')) or
      ((stText[i] = '_') and (stText[i + 1] = '_'))) then
    begin
      if ((blBold = False) and (blMono = False) and (stText[i + 2] <> ' ')) then
      begin
        blBold := True;
        iStartBold := i;
      end
      else
      if ((blBold = True) and (stText[i - 1] <> ' ')) then
      begin
        blBold := False;
        rng.location := iStartBold + 1;
        rng.length := i - iStartBold - 2;
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
          applyFontTraits_range(NSBoldFontMask, rng);
        rng.location := iStartBold - 1;
        rng.length := 2;
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
          addAttribute_value_range(NSFontAttributeName, miniFont, rng);
        rng.location := i - 1;
        rng.length := 2;
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
          addAttribute_value_range(NSFontAttributeName, miniFont, rng);
      end;
    end
    // Italics
    else if (((stText[i] = '*') and (stText[i - 1] <> '*')) or
      ((stText[i] = '_') and (stText[i - 1] <> '_'))) then
    begin
      if ((blItalics = False) and (blMono = False) and
        (stText[i + 1] <> ' ')) then
      begin
        blItalics := True;
        iStartItalics := i;
      end
      else
      if ((blItalics = True) and (stText[i - 1] <> ' ')) then
      begin
        blItalics := False;
        rng.location := iStartItalics - 1;
        rng.length := i - iStartItalics + 1;
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
          applyFontTraits_range(NSItalicFontMask, rng);
        rng.location := iStartItalics - 1;
        rng.length := 1;
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
          addAttribute_value_range(NSFontAttributeName, miniFont, rng);
        rng.location := i - 1;
        rng.length := 1;
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
          addAttribute_value_range(NSFontAttributeName, miniFont, rng);
      end;
    end
    else
    // Lines of code
    if ((stText[i] = '`') and (stText[i + 1] = '`')
      and (stText[i + 2] = '`') and (blStartLinesQuote = False) and
      ((i = 1) or (stText[i - 1] = LineEnding))) then
    begin
      blStartLinesQuote := True;
      blBoldItalics := False;
      blItalics := False;
      blBold := False;
      blMono := False;
      blFootnote := False;
      blLink := False;
      blHeading := False;
      iStartLinesQuote := i;
    end
    else
    if ((stText[i] = '`') and (stText[i + 1] = '`')
      and (stText[i + 2] = '`') and (blStartLinesQuote = True)) then
    begin
      blStartLinesQuote := False;
      rng.location := iStartLinesQuote - 1;
      rng.length := i - iStartLinesQuote + 1;
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
        addAttribute_value_range(NSFontAttributeName, monoFont, rng);
      TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        setTextColor_range(ColorToNSColor(clCode), rng);
    end
    else
    // Mono
    if stText[i] = '`' then
    begin
      if ((blMono = False) and (blStartLinesQuote = False) and
        (stText[i + 1] <> ' ')) then
      begin
        blMono := True;
        iStartMono := i;
      end
      else
      if ((blMono = True) and (stText[i - 1] <> ' ')) then
      begin
        blMono := False;
        rng.location := iStartMono - 1;
        rng.length := i - iStartMono + 1;
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
          addAttribute_value_range(NSFontAttributeName, monoFont, rng);
        TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
          setTextColor_range(ColorToNSColor(clCode), rng);
        if i > iStartMono + 1 then
        begin
          rng.location := iStartMono - 1;
          rng.length := 1;
          TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
            addAttribute_value_range(NSFontAttributeName, miniFont, rng);
          rng.location := i - 1;
          rng.length := 1;
          TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
            addAttribute_value_range(NSFontAttributeName, miniFont, rng);
        end;
      end;
    end
    // Footnote
    else if ((stText[i] = '^') and (stText[i + 1] = '[')) then
    begin
      if blFootnote = False then
      begin
        blFootnote := True;
        iStartFootnote := i;
      end;
    end
    else if ((stText[i] = ']') and (blFootnote = True)) then
    begin
      blFootnote := False;
      rng.location := iStartFootnote - 1;
      rng.length := i - iStartFootnote + 1;
      TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        setTextColor_range(ColorToNSColor(clFootnote), rng);
    end
    // Link
    else if stText[i] = '[' then
    begin
      if blLink = False then
      begin
        blLink := True;
        if stText[i - 1] = '!' then
        begin
          iStartLink := i - 1;
        end
        else
        begin
          iStartLink := i;
        end;
      end;
    end
    else if ((stText[i] = ']') and (stText[i + 1] = '(') and
      (blLink = True)) then
    begin
      blLink := False;
      rng.location := iStartLink - 1;
      rng.length := i - iStartLink + 1;
      TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        setTextColor_range(ColorToNSColor(clLink), rng);
    end;
    Inc(i);
  end;
  if dbText.SelStart > iLen - 2 then
  begin
    if sgTitles.RowCount > 0 then
    begin
      sgTitles.Cells[1, sgTitles.RowCount - 1] := ' ';
    end;
  end;
  sgTitles.TopRow := iTopRow;
end;

procedure TfmMain.LabelFileNameChars;
var
  iLength, iPos: Integer;
begin
  iLength := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
    textStorage.characters.count;
  iPos := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
    selectedRange.location;
  if iLength > 0 then
    begin
    if stFileName <> '' then
    begin
      lbChars.Caption := ExtractFileDir(stFileName) + '/  •  ' +
        ExtractFileName(stFileName) + '  •  ' + msg001 + ' ' +
        FormatFloat('#,##0', iLength) + ' (' +
        FormatFloat('#0', iPos / iLength * 100) + '%)'
    end
    else
    begin
      lbChars.Caption := msg001 + ' ' +
      FormatFloat('#,##0', iLength) + ' (' +
      FormatFloat('#0', iPos / iLength * 100) + '%)'
    end;
  end;
end;

procedure TfmMain.UpdateLastFile;
begin
  if stFileName = LastDatabase2 then
  begin
    LastDatabase2 := LastDatabase1;
    LastPosDatabase2 := LastPosDatabase1;
    TopIndexDatabase2 := TopIndexDatabase1;
    LastDatabase1 := stFileName;
  end
  else if stFileName = LastDatabase3 then
  begin
    LastDatabase3 := LastDatabase2;
    LastDatabase2 := LastDatabase1;
    LastPosDatabase3 := LastPosDatabase2;
    LastPosDatabase2 := LastPosDatabase1;
    TopIndexDatabase3 := TopIndexDatabase2;
    TopIndexDatabase2 := TopIndexDatabase1;
    LastDatabase1 := stFileName;
  end
  else if stFileName <> LastDatabase1 then
  begin
    LastDatabase4 := LastDatabase3;
    LastDatabase3 := LastDatabase2;
    LastDatabase2 := LastDatabase1;
    LastPosDatabase4 := LastPosDatabase3;
    LastPosDatabase3 := LastPosDatabase2;
    LastPosDatabase2 := LastPosDatabase1;
    TopIndexDatabase4 := TopIndexDatabase3;
    TopIndexDatabase3 := TopIndexDatabase2;
    TopIndexDatabase2 := TopIndexDatabase1;
    LastDatabase1 := stFileName;
  end;
  if LastDatabase1 <> '' then
  begin
    miFileOpenLast1.Caption := ExtractFileName(LastDatabase1);
    miFileOpenLast1.Visible := True;
    miSepLastFiles.Visible := True;
  end;
  if LastDatabase2 <> '' then
  begin
    miFileOpenLast2.Caption := ExtractFileName(LastDatabase2);
    miFileOpenLast2.Visible := True;
    miSepLastFiles.Visible := True;
  end;
  if LastDatabase3 <> '' then
  begin
    miFileOpenLast3.Caption := ExtractFileName(LastDatabase3);
    miFileOpenLast3.Visible := True;
    miSepLastFiles.Visible := True;
  end;
  if LastDatabase4 <> '' then
  begin
    miFileOpenLast4.Caption := ExtractFileName(LastDatabase4);
    miFileOpenLast4.Visible := True;
    miSepLastFiles.Visible := True;
  end;
end;

function TfmMain.SaveFile: boolean;
var
  myList: TStringList;
begin
  Result := True;
  if ((dbText.Text <> '') and (blFileSaved = False)) then
  begin
    if stFileName <> '' then
    try
      try
        myList := TStringList.Create;
        myList.Text := dbText.Text;
        myList.SaveToFile(stFileName);
        if stFileName = LastDatabase1 then
        begin
          LastPosDatabase1 := dbText.SelStart;
          TopIndexDatabase1 := sgTitles.TopRow;
        end
        else
        if stFileName = LastDatabase2 then
        begin
          LastPosDatabase2 := dbText.SelStart;
          TopIndexDatabase2 := sgTitles.TopRow;
        end
        else
        if stFileName = LastDatabase3 then
        begin
          LastPosDatabase3 := dbText.SelStart;
          TopIndexDatabase3 := sgTitles.TopRow;
        end
        else
        if stFileName = LastDatabase4 then
        begin
          LastPosDatabase4 := dbText.SelStart;
          TopIndexDatabase4 := sgTitles.TopRow;
        end;
      finally
        myList.Free;
      end;
      blFileSaved := True;
    except
      MessageDlg(msg003, mtWarning, [mbOK], 0);
      Result := False;
    end
    else
    begin
      if MessageDlg(msg002, mtConfirmation, [mbOK, mbCancel], 0) = mrCancel then
      begin
        Result := False;
      end;
    end;
  end;
end;

procedure TfmMain.MoveToPos;
var
  rng: NSRange;
begin
  if ((stFileName = LastDatabase1) and (LastPosDatabase1 > -1) and
    (LastPosDatabase1 < Length(dbText.Text))) then
  begin
    dbText.SelStart := LastPosDatabase1;
    sgTitles.TopRow := TopIndexDatabase1;
  end
  else
  if ((stFileName = LastDatabase2) and (LastPosDatabase2 > -1) and
    (LastPosDatabase2 < Length(dbText.Text))) then
  begin
    dbText.SelStart := LastPosDatabase2;
    sgTitles.TopRow := TopIndexDatabase2;
  end
  else
  if ((stFileName = LastDatabase3) and (LastPosDatabase3 > -1) and
    (LastPosDatabase3 < Length(dbText.Text))) then
  begin
    dbText.SelStart := LastPosDatabase3;
    sgTitles.TopRow := TopIndexDatabase3;
  end
  else
  if ((stFileName = LastDatabase4) and (LastPosDatabase4 > -1) and
    (LastPosDatabase4 < Length(dbText.Text))) then
  begin
    dbText.SelStart := LastPosDatabase4;
    sgTitles.TopRow := TopIndexDatabase4;
  end
  else
  begin
    dbText.SelStart := 0;
  end;
  rng.location := dbText.SelStart;
  rng.length := 1;
  FormatListTitleTodo;
  TCocoaTextView(NSScrollView(dbText.Handle).documentView).
    scrollRangeToVisible(rng);
end;

procedure TfmMain.RenumberList;
var
  i, iStart, iEnd, iPos, iNum, iTest: integer;
  rng: NSRange;
  stText: WideString;
begin
  if dbText.Text = '' then
  begin
    Exit;
  end;
  iPos := dbText.SelStart;
  stText := WideString(dbText.Text);
  iStart := iPos - 1;
  while ((iStart >= 0) and (iStart < Length(stText) - 3)) do
  begin
    if (((stText[iStart] = LineEnding) or (iStart = 0)) and (stText[iStart + 1] =
      LineEnding)) then
    begin
      Break;
    end;
    Dec(iStart);
  end;
  Inc(iStart);
  iEnd := iPos + 1;
  while (iEnd < Length(stText) - 5) do
  begin
    if ((stText[iEnd] = LineEnding) and (stText[iEnd + 1] = LineEnding)) then
    begin
      Break;
    end;
    Inc(iEnd);
  end;
  iNum := 1;
  for i := iStart to iEnd do
  begin
    if ((i >= iEnd) or (iEnd > Length(stText) - 5)) then
    begin
      Continue;
    end;
    if ((stText[i] = LineEnding) or (i = 0)) then
    begin
      if ((TryStrToInt(string(stText[i + 1]), iTest) = True) and
        (stText[i + 2] = '.') and (stText[i + 3] = ' ')) then
      begin
        rng.location := i;
        rng.length := 1;
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).
          insertText_replacementRange(NSStringUtf8(IntToStr(iNum)), rng);
        iEnd := iEnd + UTF8Length(IntToStr(iNum)) - 1;
        if iNum > 9 then
        begin
          Insert(' ', stText, i + 1);
        end;
        if iPos > i + 1 then
        begin
          iPos := iPos + UTF8Length(IntToStr(iNum)) - 1;
        end;
        Inc(iNum);
      end
      else
      if ((TryStrToInt(string(stText[i + 1]) + string(stText[i + 2]), iTest) =
        True) and (stText[i + 3] = '.') and (stText[i + 4] = ' ')) then
      begin
        rng.location := i;
        rng.length := 2;
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).
          insertText_replacementRange(NSStringUtf8(IntToStr(iNum)), rng);
        iEnd := iEnd + UTF8Length(IntToStr(iNum)) - 2;
        if iNum < 10 then
        begin
          Delete(stText, i + 2, 1);
        end;
        if iPos > i + 1 then
        begin
          iPos := iPos + UTF8Length(IntToStr(iNum)) - 2;
        end;
        Inc(iNum);
      end
      else
      begin
        iNum := 1;
      end;
    end;
  end;
  dbText.SelStart := iPos;
end;

procedure TfmMain.ShowCurrentTitleTodo;
var
  i: Integer;
begin
  if ((pnTitTodo.Visible = True) and (sgTitles.RowCount > 0)) then
  begin
    for i := 0 to sgTitles.RowCount - 1 do
    begin
      if sgTitles.Cells[1, i] = ' ' then
      begin
        sgTitles.TopRow := i;
        Break;
      end;
    end;
  end;
end;

procedure TfmMain.CreateYAML;
begin
  dbText.Clear;
  sgTitles.Clear;
  dbText.Lines.Add('---');
  dbText.Lines.Add('title: ');
  dbText.Lines.Add('author: ');
  if dateformat = 'en' then
  begin
    dbText.Lines.Add('date: ' + FormatDateTime('mmmm dd yyyy', Date()));
  end
  else
  begin
    dbText.Lines.Add('date: ' + FormatDateTime('dd mmmm yyyy', Date()));
  end;
  dbText.Lines.Add('abstract: ');
  dbText.Lines.Add('---');
  dbText.SelStart := 11;
end;

procedure TfmMain.CreateBackup;
begin
  if stFileName <> '' then
  begin
    if FileExistsUTF8(stFileName) = True then
    try
      if FileExistsUTF8(ExtractFileNameWithoutExt(stFileName) +
        '.bak') = True then
      begin
        DeleteFileUTF8(ExtractFileNameWithoutExt(stFileName) + '.bak');
      end;
      CopyFile(stFileName, ExtractFileNameWithoutExt(stFileName) + '.bak');
    except
      MessageDlg(msg009, mtWarning, [mbOK], 0);
    end;
  end;
end;

// *******************************************************
// ************ Procedures of font management ************
// *******************************************************

function TfmMain.UTF8CocoaPos(const SearchForText, SearchInText: string;
  StartPos: SizeInt = 1): PtrInt;
var
  iPos: integer = 0;
  stText: string;
begin
  // since rangeOfString_options_range doesn't work...
  stText := NSStringToString(StrToNSString(SearchInText,
    True).substringFromIndex(StartPos - 1));
  iPos := StrToNSString(stText, True).rangeOfString(
    StrToNSString(SearchForText)).location;
  if ((iPos < 0) or (iPos > stText.length - 1)) then
  begin
    Result := 0;
  end
  else
  begin
    Result := iPos + StartPos;
  end;
end;

function TfmMain.GetPara(txt: NSTextStorage; textOffset: integer;
  isReadOnly, useDefault: boolean): NSParagraphStyle;
var
  dict: NSDictionary;
  op: NSParagraphStyle;
begin
  Result := nil;
  if not Assigned(txt) then
    Exit;
  dict := GetDict(txt, textOffset);
  op := nil;
  if Assigned(dict) then
    op := NSParagraphStyle(dict.objectForKey(NSParagraphStyleAttributeName));
  if not Assigned(op) then
  begin
    if not useDefault then
      Exit;
    op := NSParagraphStyle.defaultParagraphStyle;
  end;
  if isReadOnly then
    Result := op
  else
    Result := op.mutableCopyWithZone(nil);
end;

function TfmMain.GetWritePara(txt: NSTextStorage;
  textOffset: integer): NSMutableParagraphStyle;
begin
  Result := NSMutableParagraphStyle(GetPara(txt, textOffset, False, True));
end;

function TfmMain.GetDict(txt: NSTextStorage; textOffset: integer): NSDictionary;
begin
  if textOffset >= txt.string_.length then
  begin
    textOffset := txt.string_.length - 1;
  end;
  Result := txt.attributesAtIndex_effectiveRange(textOffset, nil);
end;

function TfmMain.FindFont(FamilyName: string; iStyle: smallint): NSFontDescriptor;
var
  fd: NSFontDescriptor;
  fdd: NSFontDescriptor;
  trt: NSFontSymbolicTraits;
  ns: NSString;
begin
  trt := 0;
  ns := NSStringUtf8(FamilyName);
  if iStyle = 1 then
  begin
    trt := trt or NSFontItalicTrait;
  end
  else
  if iStyle = 2 then
  begin
    trt := trt or NSFontBoldTrait;
  end
  else
  if iStyle = 3 then
  begin
    trt := trt or NSFontBoldTrait or NSFontItalicTrait;
  end;
  fd := NSFontDescriptor(NSFontDescriptor.alloc).initWithFontAttributes(nil);
  try
    fd := fd.fontDescriptorWithFamily(ns);
    fd := fd.fontDescriptorWithSymbolicTraits(trt);
    fdd := fd.matchingFontDescriptorWithMandatoryKeys(nil);
    Result := fdd;
  finally
    ns.Release;
  end;
end;

end.
