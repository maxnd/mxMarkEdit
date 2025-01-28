//***********************************************************************
// ***********************************************************************
// mxMarkEdit 1.x
// Author and copyright: Massimo Nardello, Modena (Italy) 2024 - 2025.
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
  DefaultTranslator, translate, IniFiles, LazUTF8, FileUtil,
  LazFileUtils, Unix, Types, DateUtils, LCLType;

type

  { TfmMain }

  TfmMain = class(TForm)
    cbFilter: TComboBox;
    dbText: TMemo;
    lbDateTime: TLabel;
    lbChars: TLabel;
    lbFindGrid: TLabel;
    miToolsOptmize: TMenuItem;
    miFilesSearch: TMenuItem;
    miEditWords: TMenuItem;
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
    pnBackground: TPanel;
    pnFindGrid: TPanel;
    pnGrid: TPanel;
    pnTitTodo: TPanel;
    Sep2a: TMenuItem;
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
    Sep7: TMenuItem;
    sgTitles: TStringGrid;
    sgTable: TStringGrid;
    Shortcuts: TMenuItem;
    spTable: TSplitter;
    spTitles: TSplitter;
    tmDateTime: TTimer;
    edFindGrid: TEdit;
    procedure cbFilterChange(Sender: TObject);
    procedure dbTextChange(Sender: TObject);
    procedure dbTextClick(Sender: TObject);
    procedure dbTextKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure dbTextKeyPress(Sender: TObject; var Key: char);
    procedure dbTextKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure edFindGridKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDropFiles(Sender: TObject; const FileNames: array of string);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
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
    procedure miEditWordsClick(Sender: TObject);
    procedure miFileNewClick(Sender: TObject);
    procedure miFileOpenClick(Sender: TObject);
    procedure miFileOpenLast1Click(Sender: TObject);
    procedure miFileOpenLast2Click(Sender: TObject);
    procedure miFileOpenLast3Click(Sender: TObject);
    procedure miFileOpenLast4Click(Sender: TObject);
    procedure miFileSaveAsClick(Sender: TObject);
    procedure miFileSaveClick(Sender: TObject);
    procedure miFilesSearchClick(Sender: TObject);
    procedure miToolsOpenWinClick(Sender: TObject);
    procedure miToolsOptionsClick(Sender: TObject);
    procedure miToolsOptmizeClick(Sender: TObject);
    procedure miToolsPandocClick(Sender: TObject);
    procedure miToolsTrans1Click(Sender: TObject);
    procedure miToolsTrans2Click(Sender: TObject);
    procedure miToolsTrans3Click(Sender: TObject);
    procedure sgTableDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure sgTableEditingDone(Sender: TObject);
    procedure sgTableKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure sgTableKeyPress(Sender: TObject; var Key: char);
    procedure sgTablePrepareCanvas(Sender: TObject; aCol, aRow: integer;
      aState: TGridDrawState);
    procedure sgTableSetEditText(Sender: TObject; ACol, ARow: Integer;
      const Value: string);
    procedure sgTitlesClick(Sender: TObject);
    procedure sgTitlesDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
    procedure sgTitlesGetCellHint(Sender: TObject; ACol, ARow: integer;
      var HintText: string);
    procedure sgTitlesPrepareCanvas(Sender: TObject; aCol, aRow: integer;
      aState: TGridDrawState);
    procedure ShortcutsClick(Sender: TObject);
    procedure tmDateTimeTimer(Sender: TObject);
  private
    procedure CreateBackup;
    procedure CreateYAML;
    procedure DeactForm(stFileName: String);
    procedure FindInGrid;
    function GetDict(txt: NSTextStorage; textOffset: integer): NSDictionary;
    function GetHeaderLevel(stHeader: String): Integer;
    function GetPara(txt: NSTextStorage; textOffset: integer;
      isReadOnly, useDefault: boolean): NSParagraphStyle;
    function GetWritePara(txt: NSTextStorage;
      textOffset: integer): NSMutableParagraphStyle;
    procedure RenumberFootnotes;
    procedure RenumberList;
    function SaveFile: boolean;
    procedure SelectInsertFootnote;
    procedure CutZone;
    procedure SetTable;
  public
    procedure UpdateLastFile;
    procedure LabelFileNameChars;
    procedure MoveToPos;
    procedure ShowCurrentTitleTodo;
    procedure FormatListTitleTodo;
    function UTF8CocoaPos(const SearchForText, SearchInText: string;
      StartPos: SizeInt = 1): PtrInt;
    function FindFont(FamilyName: string; iStyle: smallint): NSFontDescriptor;

  end;

var
  fmMain: TfmMain;
  myHomeDir, myConfigFile: string;
  iTitleTodoRowHeight: integer = 28;
  clTitle1: TColor = clBlack;
  clTitle2: TColor = clBlack;
  clTitle3: TColor = clBlack;
  clRepetition: TColor = $005766EA;
  clFootnote: TColor = clSilver;
  clLink: TColor = clSilver;
  clCode: TColor = clSilver;
  clHighlightList: TColor = clDkGray;
  clQuote: TColor = clSilver;
  clTodo: TColor = clBlack;
  clInsertionPoint: TColor;
  clFontContrast: TColor;
  clFontFade: Tcolor;
  stFontMono: string = 'Menlo';
  iFontMonoSize: smallint = 18;
  stFileName: string = '';
  iBookmarkPos: integer = 0;
  iDelay: integer = 7;
  iLineSpacing: double = 1.0;
  LastDatabase1, LastDatabase2, LastDatabase3, LastDatabase4: string;
  LastPosDatabase1, LastPosDatabase2, LastPosDatabase3, LastPosDatabase4: integer;
  TopIndexDatabase1, TopIndexDatabase2, TopIndexDatabase3, TopIndexDatabase4: integer;
  ColDatabase1, ColDatabase2, ColDatabase3, ColDatabase4: integer;
  RowDatabase1, RowDatabase2, RowDatabase3, RowDatabase4: integer;
  ColWidthDatabase1, ColWidthDatabase2, ColWidthDatabase3, ColWidthDatabase4: String;
  blFileSaved: boolean = True;
  blFileMod: boolean = False;
  blTableSaved: boolean = True;
  blTableMod: boolean = False;
  blHideTitleTodo: boolean = False;
  blDisableFormatting: boolean = False;
  blIsPresenting: boolean = False;
  iMaxSize: Integer = 250000;
  stTableLoaded: String = ' && .csv ';
  csTableRowCount: Integer = 10000;
  blTextOnChange: boolean = False;
  stGridLoaded: String = '';
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
  msg010 = 'Delete the current column of the current table?';
  msg011 = 'It''s not possible to create a footnote reference at the beginning of a paragraph.';
  msg012 = 'It''s not possible to insert a new row since the last one contains some data.';
  msg013 = 'Delete the current row?';
  msg014 = 'Sort the content of current column of the current table?';
  msg015 = 'Delete the content of the selected cells?';
  msg016 = 'Insert a new column in the current table?';
  msg017 = 'This functionality must be called within a heading and not at the end of the text.';
  msg018 = 'Cut in the clipboard all the text under the current heading?';
  msg019 = 'The table is not delimited at the bottom; add a fictional title ' +
    'after its last row in the first left column.';
  msg020 = 'Create in a new file a version of the current presentation ' +
    'optimised for mxMarkEdit?';
  msg021 = 'It was not possible to save the optimised file.';
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
  lb007 = 'Tables names';
  lb008 = 'Tables';
  lb009 = 'Sum:';
  lb010 = 'Maximum value:';
  lb011 = 'Minimum value:';
  lb012 = 'Average:';
  lb013 = 'Count:';
  dateformat = 'en';

implementation

uses copyright, unit2, unit3, unit4, unit5, unit6, unit7;

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
    sgTable.FixedGridLineColor := $005E5E5E;
    sgTable.GridLineColor := $005E5E5E;
    sgTable.Color := $00282A2B;
    sgTable.FixedColor := $00282A2B;
    sgTable.FocusColor := clSilver;
    sgTable.Editor.Color := $00282A2B;
    sgTable.SelectedColor := $00454545;
    lbChars.Font.Color := clSilver;
    lbFindGrid.Font.Color := clSilver;
    edFindGrid.Font.Color := clSilver;
    lbDateTime.Font.Color := clSilver;
    clTitle1 := clWhite;
    clTitle2 := clWhite;
    clTitle3 := clWhite;
    clFootnote := clSilver;
    clLink := clSilver;
    clCode := clSilver;
    clQuote := $00404040;
    clHighlightList := $005E5E5E;
    clRepetition := $005766EA;
    clTodo := clWhite;
    clFontContrast := clWhite;
    clFontFade := $00797979;
  end
  else
  begin
    dbText.Font.Color := clBlack;
    sgTitles.Font.Color := clBlack;
    dbText.Color := clWhite;
    sgTitles.Color := clWhite;
    sgTable.FixedGridLineColor := clSilver;
    sgTable.GridLineColor := clSilver;
    sgTable.Color := clWhite;
    sgTable.Editor.Color := clWhite;
    sgTable.FixedColor := clWhite;
    sgTable.BorderStyle := bsNone;
    sgTable.FocusColor := clGray;
    sgTable.SelectedColor := clSilver;
    fmMain.Color := clWhite;
    spTitles.Color := clForm;
    pnBottom.Color := clForm;
    lbChars.Font.Color := clDkGray;
    lbFindGrid.Font.Color := clDkGray;
    edFindGrid.Font.Color := clDkGray;
    lbDateTime.Font.Color := clDkGray;
    clTitle1 := clBlack;
    clTitle2 := clBlack;
    clTitle3 := clBlack;
    clFootnote := clDkGray;
    clLink := clDkGray;
    clCode := clDkGray;
    clQuote := $00EFEFEF;
    clHighlightList := $00EBEBEB;
    clRepetition := clRed;
    clTodo := clBlack;
    clFontContrast := clBlack;
    clFontFade := clSilver;
  end;
  clInsertionPoint := NSColorToColorRef(TCocoaTextView(NSScrollView(dbText.Handle).
    documentView).insertionPointColor);
  sgTitles.FocusRectVisible := False;
  sgTable.FocusRectVisible := False;
  sgTable.TitleFont.Style := [fsBold];
  pnGrid.Height := 1;
  SetTable;
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
      dbText.Font.Size := MyIni.ReadInteger('mxmarkedit', 'fontsize', 16);
      dbText.Font.Color := StringToColor(MyIni.ReadString('mxmarkedit',
        'fontcolor', ColorToString(dbText.Font.Color)));
      stFontMono := MyIni.ReadString('mxmarkedit', 'fontmononame', 'Menlo');
      iFontMonoSize := MyIni.ReadInteger('mxmarkedit', 'fontmonosize', 18);
      clTitle1 := StringToColor(MyIni.ReadString('mxmarkedit', 'title1',
        'clTitle1'));
      clTitle2 := StringToColor(MyIni.ReadString('mxmarkedit', 'title2', 'clTitle2'));
      clTitle3 := StringToColor(MyIni.ReadString('mxmarkedit', 'title3', 'clTitle3'));
      clFootnote := StringToColor(MyIni.ReadString('mxmarkedit',
        'footnote', 'clFootnote'));
      clLink := StringToColor(MyIni.ReadString('mxmarkedit', 'link', 'clLink'));
      clCode := StringToColor(MyIni.ReadString('mxmarkedit', 'code', 'clCode'));
      clTodo := StringToColor(MyIni.ReadString('mxmarkedit', 'todo', 'clTodo'));
      pnTitTodo.Width := MyIni.ReadInteger('mxmarkedit', 'titlewidth', 400);
      stFileName := MyIni.ReadString('mxmarkedit', 'filename', '');
      iDelay := MyIni.ReadInteger('mxmarkedit', 'delay', 7);
      iLineSpacing := MyIni.ReadFloat('mxmarkedit', 'linespacing', 1.0);
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
      ColDatabase1 := MyIni.ReadInteger('mxmarkedit', 'coldatabase1', 1);
      ColDatabase2 := MyIni.ReadInteger('mxmarkedit', 'coldatabase2', 1);
      ColDatabase3 := MyIni.ReadInteger('mxmarkedit', 'coldatabase3', 1);
      ColDatabase4 := MyIni.ReadInteger('mxmarkedit', 'coldatabase4', 1);
      RowDatabase1 := MyIni.ReadInteger('mxmarkedit', 'rowdatabase1', 1);
      RowDatabase2 := MyIni.ReadInteger('mxmarkedit', 'rowdatabase2', 1);
      RowDatabase3 := MyIni.ReadInteger('mxmarkedit', 'rowdatabase3', 1);
      RowDatabase4 := MyIni.ReadInteger('mxmarkedit', 'rowdatabase4', 1);
      ColWidthDatabase1 := MyIni.ReadString('mxmarkedit', 'colwidthdatabase1', '');
      ColWidthDatabase2 := MyIni.ReadString('mxmarkedit', 'colwidthdatabase2', '');
      ColWidthDatabase3 := MyIni.ReadString('mxmarkedit', 'colwidthdatabase3', '');
      ColWidthDatabase4 := MyIni.ReadString('mxmarkedit', 'colwidthdatabase4', '');
      blHideTitleTodo := MyIni.ReadBool('mxmarkedit', 'blhidetodo', False);
      iMaxSize := MyIni.ReadInteger('mxmarkedit', 'maxsize', 250000);
    finally
      MyIni.Free;
    end;
  end;
  if blDisableFormatting = True then
  begin
    miEditDisableFormClick(nil);
  end;
  if blHideTitleTodo = True then
  begin
    miEditHideListClick(nil);
  end;
  sgTable.Font.Color := dbText.Font.Color;
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
    if ParamStrUTF8(1) = '-' then
    begin
      stFileName := '';
      CreateYAML;
      LabelFileNameChars;
    end
    else
    if FileExistsUTF8(ParamStrUTF8(1)) = True then
    try
      stFileName := ParamStrUTF8(1);
      DeactForm(stFileName);
      dbText.Lines.LoadFromFile(stFileName);
      if FileExistsUTF8(ExtractFileNameWithoutExt(stFileName) + '.csv') then
      begin
        sgTable.LoadFromCSVFile(ExtractFileNameWithoutExt(stFileName) + '.csv',
          #9, False);
        sgTable.RowCount := csTableRowCount;
        stGridLoaded := stTableLoaded;
      end
      else
      begin
        stGridLoaded := '';
      end;
      MoveToPos;
      iBookmarkPos := 0;
      LabelFileNameChars;
      if blDisableFormatting = False then
      begin
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).
          checkTextInDocument(nil);
      end;
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        undoManager.removeAllActions;
      UpdateLastFile;
      ShowCurrentTitleTodo;
      blFileMod := False;
      blTableMod := False;
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
      DeactForm(stFileName);
      dbText.Lines.LoadFromFile(stFileName);
      if FileExistsUTF8(ExtractFileNameWithoutExt(stFileName) + '.csv') then
      begin
        sgTable.LoadFromCSVFile(ExtractFileNameWithoutExt(stFileName) + '.csv',
          #9, False);
        sgTable.RowCount := csTableRowCount;
        stGridLoaded := stTableLoaded;
      end
      else
      begin
        stGridLoaded := '';
      end;
      MoveToPos;
      iBookmarkPos := 0;
      LabelFileNameChars;
      if blDisableFormatting = False then
      begin
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).
          checkTextInDocument(nil);
      end;
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        undoManager.removeAllActions;
      UpdateLastFile;
      ShowCurrentTitleTodo;
      blFileMod := False;
      blTableMod := False;
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
    DeactForm(stFileName);
    dbText.Lines.LoadFromFile(stFileName);
    if FileExistsUTF8(ExtractFileNameWithoutExt(stFileName) + '.csv') then
    begin
      sgTable.LoadFromCSVFile(ExtractFileNameWithoutExt(stFileName) + '.csv',
        #9, False);
      sgTable.RowCount := csTableRowCount;
      stGridLoaded := stTableLoaded;
    end
    else
    begin
      stGridLoaded := '';
    end;
    MoveToPos;
    iBookmarkPos := 0;
    LabelFileNameChars;
    if blDisableFormatting = False then
    begin
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        checkTextInDocument(nil);
    end;
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      undoManager.removeAllActions;
    UpdateLastFile;
    ShowCurrentTitleTodo;
    blFileMod := False;
    blTableMod := False;
  except
    MessageDlg(msg004, mtWarning, [mbOK], 0);
  end;
end;

procedure TfmMain.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  rng: NSRange;
begin
  if ((key = Ord('T')) and (Shift = [ssShift, ssMeta])) then
  begin
    if pnGrid.Height = 1 then
    begin
      pnGrid.Height := 400;
      sgTable.SetFocus;
    end
    else
    begin
      pnGrid.Height := 1;
      dbText.SetFocus;
    end;
    key := 0;
  end
  else
  if ((key = Ord('F')) and (Shift = [ssCtrl, ssMeta])) then
  begin
    if ((pnGrid.Height > 1) and (edFindGrid.Visible = True)) then
    begin
      edFindGrid.SetFocus;
    end;
    key := 0;
  end
  else
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

procedure TfmMain.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ((key = Ord('V')) and (Shift = [ssMeta])) then
  begin
    sgTableEditingDone(nil);
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
    MyIni.WriteString('mxmarkedit', 'todo', ColorToString(clTodo));
    MyIni.WriteInteger('mxmarkedit', 'titlewidth', pnTitTodo.Width);
    MyIni.WriteString('mxmarkedit', 'filename', stFileName);
    MyIni.WriteInteger('mxmarkedit', 'delay', iDelay);
    MyIni.WriteFloat('mxmarkedit', 'linespacing', iLineSpacing);
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
    MyIni.WriteInteger('mxmarkedit', 'coldatabase1', ColDatabase1);
    MyIni.WriteInteger('mxmarkedit', 'coldatabase2', ColDatabase2);
    MyIni.WriteInteger('mxmarkedit', 'coldatabase3', ColDatabase3);
    MyIni.WriteInteger('mxmarkedit', 'coldatabase4', ColDatabase4);
    MyIni.WriteInteger('mxmarkedit', 'rowdatabase1', RowDatabase1);
    MyIni.WriteInteger('mxmarkedit', 'rowdatabase2', RowDatabase2);
    MyIni.WriteInteger('mxmarkedit', 'rowdatabase3', RowDatabase3);
    MyIni.WriteInteger('mxmarkedit', 'rowdatabase4', RowDatabase4);
    MyIni.WriteString('mxmarkedit', 'colwidthdatabase1', ColWidthDatabase1);
    MyIni.WriteString('mxmarkedit', 'colwidthdatabase2', ColWidthDatabase2);
    MyIni.WriteString('mxmarkedit', 'colwidthdatabase3', ColWidthDatabase3);
    MyIni.WriteString('mxmarkedit', 'colwidthdatabase4', ColWidthDatabase4);
    MyIni.WriteBool('mxmarkedit', 'blhidetodo', blHideTitleTodo);
    MyIni.WriteInteger('mxmarkedit', 'maxsize', iMaxSize);
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
  blFileMod := True;
end;

procedure TfmMain.cbFilterChange(Sender: TObject);
begin
  FormatListTitleTodo;
  dbText.SetFocus;
end;

procedure TfmMain.dbTextClick(Sender: TObject);
begin
  blIsPresenting := False;
  cbFilter.Visible := True;
  FormatListTitleTodo;
  LabelFileNameChars;
end;

procedure TfmMain.dbTextKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  stClip: string;
  i, iPos, iNum: integer;
  rngStart, rngEnd: NSRange;
  blCode: boolean;
  stText: WideString;
  myDate: TDate;
begin
  if ((key = 27) and (blIsPresenting = True)) then
  begin
    blIsPresenting := False;
    cbFilter.Visible := True;
    FormatListTitleTodo;
  end
  else
  if (((key = Ord('E')) and (Shift = [ssMeta]) or
  ((key = 40) and (Shift = [ssMeta]) and (blIsPresenting = True)))) then
  begin
    if miEditDisableForm.Checked = True then
    begin
      miEditDisableFormClick(nil);
    end;
    if miEditDisSpell.Checked = False then
    begin
      miEditDisSpellClick(nil);
    end;
    key := 0;
    blIsPresenting := True;
    cbFilter.Visible := False;
    sgTitles.ScrollBars := ssNone;
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      moveToEndOfParagraph(nil);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      moveForward(nil);
    while (((dbText.Lines[dbText.CaretPos.Y] = '') or
        (dbText.Lines[dbText.CaretPos.Y] = '---')) and
        (dbText.CaretPos.Y < dbText.Lines.Count)) do
    begin
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        moveForward(nil);
    end;
    FormatListTitleTodo;
    pnBottom.Height := 0;
    stText := WideString(dbText.Text);
    rngStart.location := 0;
    rngStart.length := Length(stText);
    TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      setTextColor_range(ColorToNSColor(clFontFade), rngStart);
    rngStart := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      textStorage.string_.paragraphRangeForRange(TCocoaTextView(
      NSScrollView(fmMain.dbText.Handle).documentView).selectedRange);
    TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      setTextColor_range(ColorToNSColor(clFontContrast), rngStart);
    // To have the selected paragraph vertically centered
    rngEnd.location := 1;
    rngEnd.length := 1;
    TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      scrollRangeToVisible(rngEnd);
    TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      scrollRangeToVisible(rngStart);
    ShowCurrentTitleTodo;
    if rngStart.length > 1 then
    begin
      dbText.SelStart := dbText.SelStart + 1;
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        setInsertionPointColor(ColorToNSColor(dbText.Color));
    end;
    sgTitles.ScrollBars := ssAutoVertical;
  end
  else
  if (((key = Ord('E')) and (Shift = [ssMeta, ssShift]) or
    ((key = 38) and (Shift = [ssMeta]) and (blIsPresenting = True)))) then
  begin
    if miEditDisableForm.Checked = True then
    begin
      miEditDisableFormClick(nil);
    end;
    if miEditDisSpell.Checked = False then
    begin
      miEditDisSpellClick(nil);
    end;
    key := 0;
    blIsPresenting := True;
    cbFilter.Visible := False;
    sgTitles.ScrollBars := ssNone;
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      moveToBeginningOfParagraph(nil);
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      moveBackward(nil);
    while (((dbText.Lines[dbText.CaretPos.Y] = '') or
        (dbText.Lines[dbText.CaretPos.Y] = '---')) and
        (dbText.CaretPos.Y > 0)) do
    begin
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        moveBackward(nil);
    end;
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      moveToBeginningOfParagraph(nil);
    if dbText.Lines[dbText.CaretPos.Y] = '---' then
    begin
      Exit;
    end;
    FormatListTitleTodo;
    pnBottom.Height := 0;
    stText := WideString(dbText.Text);
    rngStart.location := 0;
    rngStart.length := Length(stText);
    TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      setTextColor_range(ColorToNSColor(clFontFade), rngStart);
    rngStart := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      textStorage.string_.paragraphRangeForRange(TCocoaTextView(
      NSScrollView(fmMain.dbText.Handle).documentView).selectedRange);
    TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      setTextColor_range(ColorToNSColor(clFontContrast), rngStart);
    // To have the selected paragraph vertically centered
    rngEnd.location := 1;
    rngEnd.length := 1;
    TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      scrollRangeToVisible(rngEnd);
    TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      scrollRangeToVisible(rngStart);
    ShowCurrentTitleTodo;
    if rngStart.length > 1 then
    begin
      dbText.SelStart := dbText.SelStart + 1;
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        setInsertionPointColor(ColorToNSColor(dbText.Color));
    end;
    sgTitles.ScrollBars := ssAutoVertical;
  end
  else
  if blIsPresenting = True then
  begin
    key := 0;
  end
  else
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
  if ((key = Ord('G')) and (Shift = [ssMeta, ssShift])) then
  begin
    fmSearch.bnPreviousClick(nil);
    key := 0;
  end
  else
  if ((key = Ord('F')) and (Shift = [ssMeta, ssShift])) then
  begin
    SelectInsertFootnote;
    key := 0;
  end
  else
  if ((key = Ord('X')) and (Shift = [ssMeta, ssShift])) then
  begin
    CutZone;
    key := 0;
  end
  else
  if ((key = 190) and (Shift = [ssAlt, ssMeta])) then
  begin
    i := dbText.CaretPos.Y;
    if UTF8Copy(dbText.Lines[i], 1, 2) = '- ' then
    iNum := 1;
    while i > -1 do
    begin
      if UTF8Copy(dbText.Lines[i], 1, 2) <> '- ' then
      begin
        Break;
      end;
      Dec(i);
    end;
    Inc(i);
    while i < dbText.Lines.Count do
    begin
      if UTF8Copy(dbText.Lines[i], 1, 2) = '- ' then
      begin
        dbText.Lines[i] := IntToStr(iNum) +'. ' +
          UTF8Copy(dbText.Lines[i], 3, UTF8Length(dbText.Lines[i]));
        Inc(iNum);
        Inc(i);
      end
      else
      begin
        Break;
      end;
    end;
  end
  else
  if ((key = 190) and (Shift = [ssCtrl, ssMeta])) then
  begin
    i := dbText.CaretPos.Y;
    if TryStrToInt(UTF8Copy(dbText.Lines[i], 1,
      UTF8Pos('. ', dbText.Lines[i]) - 1), iNum) = True then
    while i > -1 do
    begin
      if TryStrToInt(UTF8Copy(dbText.Lines[i], 1,
        UTF8Pos('. ', dbText.Lines[i]) - 1), iNum) = False then
      begin
        Break;
      end;
      Dec(i);
    end;
    Inc(i);
    while i < dbText.Lines.Count do
    begin
      if TryStrToInt(UTF8Copy(dbText.Lines[i], 1,
        UTF8Pos('. ', dbText.Lines[i]) - 1), iNum) = True then
      begin
        dbText.Lines[i] := '- ' + UTF8Copy(dbText.Lines[i], UTF8Pos('. ',
          dbText.Lines[i]) + 2, UTF8Length(dbText.Lines[i]));
        Inc(i);
      end
      else
      begin
        Break;
      end;
    end;
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
      if TryStrToInt(UTF8Copy(dbText.Lines[dbText.CaretPos.Y], 1,
        UTF8Pos('. ', dbText.Lines[dbText.CaretPos.Y]) - 1), i) = True then
      begin
        RenumberList;
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
  if ((key = Ord('R')) and (Shift = [ssMeta])) then
  begin
    RenumberList;
    key := 0;
  end
  else
  if ((key = Ord('R')) and (Shift = [ssMeta, ssShift])) then
  begin
    RenumberFootnotes;
    key := 0;
  end
  else
  if ((key = Ord('B')) and (Shift = [ssMeta])) then
  begin
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
  if ((key = Ord('T')) and ((Shift = [ssMeta]) or (Shift = [ssAlt, ssMeta]))) then
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
          FormatDateTime('yyyy-mm-dd',
          IncDay(myDate, StrToInt(fmOptions.cbDelay.Text))) + ' • '), rngStart);
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
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      moveForward(nil);
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
  end
  else
  if blIsPresenting = True then
  begin
    if ((key = 38) or (key = 40)) then
    begin
      key := 0;
    end
  end
end;

procedure TfmMain.dbTextKeyPress(Sender: TObject; var Key: char);
var
  i: integer;
  myDate: TDate;
  fs: TFormatSettings;
begin
  if key = #13 then
  begin
    if (((dbText.Lines[dbText.CaretPos.Y - 1] = '- [ ] ') or
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
      UTF8Pos('. ', dbText.Lines[dbText.CaretPos.Y - 1]) + 1))) and
      ((dbText.Lines[dbText.CaretPos.Y] ='') or
      (dbText.CaretPos.Y = dbText.Lines.Count))) then
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
      end;
    end;
    Application.ProcessMessages;
  end;
end;

procedure TfmMain.dbTextKeyUp(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if ((key > 36) and (key < 41) and (blIsPresenting = False)) then
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
  if blIsPresenting = True then
  begin
    dbText.SetFocus;
    Exit;
  end;
  stText := WideString(dbText.Text);
  iLen := Length(stText);
  i := 1;
  iHeader := 0;
  while i <= iLen do
  begin
    if ((stText[i] = '`') and (stText[i + 1] = '`') and
      (stText[i + 2] = '`') and ((stText[i - 1] = LineEnding) or (i = 1))) then
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
  if blIsPresenting = True then
  begin
    sgTitles.canvas.Font.Color := clFontFade;
    sgTitles.Canvas.TextOut(aRect.Left + 3, aRect.Top + 5,
      sgTitles.Cells[aCol, aRow]);
  end
  else
  begin
    if ((Pos('  ☑ ', sgTitles.Cells[aCol, aRow]) > 0) or
      (Pos('  □ ', sgTitles.Cells[aCol, aRow]) > 0)) then
    begin
      sgTitles.canvas.Font.Color := clTodo;
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
end;

procedure TfmMain.sgTitlesGetCellHint(Sender: TObject; ACol, ARow: integer;
  var HintText: string);
begin
  if sgTitles.Canvas.TextWidth(sgTitles.Cells[ACol, ARow]) > sgTitles.Width - 20 then
  begin
    HintText := sgTitles.Cells[ACol, ARow];
  end;
end;

procedure TfmMain.sgTitlesPrepareCanvas(Sender: TObject; aCol, aRow: integer;
  aState: TGridDrawState);
begin
  if sgTitles.Cells[1, aRow] = ' ' then
  begin
    sgTitles.Canvas.Brush.Color := clHighlightList;
  end;
end;

procedure TfmMain.sgTableDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
begin
  sgTable.Canvas.TextOut(aRect.Left + 4, aRect.Top,
    sgTable.Cells[aCol, aRow]);
end;

procedure TfmMain.sgTableEditingDone(Sender: TObject);
var
  dbNum, dbSum, dbMax, dbMin: Double;
  flNum: boolean;
  dbCount, iTop, i: Integer;
begin
  if ((sgTable.Row > 1) and (sgTable.Col > 1)) then
  begin
    dbNum := 0;
    iTop := -1;
    for i := sgTable.Row - 1 downto 1 do
    begin
      if sgTable.Cells[1, i] <> '' then
      begin
        iTop := i + 1;
        Break;
      end;
    end;
    dbSum := 0;
    dbMin := 0;
    dbMax := 0;
    dbCount := 0;
    flNum := False;
    if iTop > - 1 then
    begin
      for i := iTop to sgTable.RowCount - 1 do
      begin
        if sgTable.Cells[1, i] <> '' then
        begin
          Exit;
        end
        else
        if (((sgTable.Cells[sgTable.Col, i] = '------') or
           (sgTable.Cells[sgTable.Col, i] = '---sum')) and
          (i < sgTable.RowCount - 2)) then
        begin
          if flNum = True then
          begin
            sgTable.Cells[sgTable.Col, i + 1] := lb009 + ' ' +
              FormatFloat('0.##', dbSum);
          end;
        end
        else
        if ((sgTable.Cells[sgTable.Col, i] = '---max') and
          (i < sgTable.RowCount - 2)) then
        begin
          if flNum = True then
          begin
            sgTable.Cells[sgTable.Col, i + 1] := lb010 + ' ' +
              FormatFloat('0.##', dbMax);
          end;
        end
        else
        if ((sgTable.Cells[sgTable.Col, i] = '---min') and
          (i < sgTable.RowCount - 2)) then
        begin
          if flNum = True then
          begin
            sgTable.Cells[sgTable.Col, i + 1] := lb011 + ' ' +
              FormatFloat('0.##', dbMin);
          end;
        end
        else
        if ((sgTable.Cells[sgTable.Col, i] = '---avg') and
          (i < sgTable.RowCount - 2)) then
        begin
          if flNum = True then
          begin
            sgTable.Cells[sgTable.Col, i + 1] := lb012 + ' ' +
              FormatFloat('0.##', dbSum / dbCount);
          end;
        end
        else
        if ((sgTable.Cells[sgTable.Col, i] = '---count') and
          (i < sgTable.RowCount - 2)) then
        begin
          if flNum = True then
          begin
            sgTable.Cells[sgTable.Col, i + 1] := lb013 + ' ' +
              FormatFloat('0.##', dbCount);
          end;
        end
        else
        if TryStrToFloat(sgTable.Cells[sgTable.Col, i], dbNum) = True then
        begin
          if flNum = False then
          begin
            dbMin := dbNum;
            dbMax := dbNum;
            flNum := True;
          end;
          dbSum := dbSum + dbNum;
          if dbMax < dbNum then
          begin
            dbMax := dbNum;
          end;
          if dbMin > dbNum then
          begin
            dbMin := dbNum;
          end;
          Inc(dbCount);
        end;
      end;
    end;
  end;
end;

procedure TfmMain.edFindGridKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if ((key = 13) or (((key = Ord('G')) and (Shift = [ssMeta])))) then
  begin
    FindInGrid;
    key := 0;
  end;
end;

procedure TfmMain.sgTableKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  i, x, iTop, iBottom, iRight, iNextTable: Integer;
  iNum: Double;
  stField: String;
  grRect: TGridRect;
begin
  if ((key = 8) and (Shift = [])) then
  begin
    if sgTable.EditorMode = False then
    begin
      if MessageDlg(msg015, mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
      begin
        for i := 0 to sgTable.SelectedRangeCount-1 do
        begin
          grRect := sgTable.SelectedRange[i];
          sgTable.Clean(grRect, [gzNormal]);
        end;
      end;
      key := 0;
    end;
  end
  else
  if ((key = 8) and (Shift = [ssMeta, ssShift])) then
  begin
    if MessageDlg(msg013, mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
    begin
      sgTable.DeleteColRow(False, sgTable.Row);
      sgTable.RowCount := csTableRowCount;
      blTableMod := True;
      stGridLoaded := stTableLoaded;
      LabelFileNameChars;
      blTableSaved := False;
    end;
    key := 0;
  end
  else
  if ((key = 8) and (Shift = [ssMeta, ssShift, ssAlt])) then
  begin
    if sgTable.Col = 1 then
    begin
      key := 0;
      Exit;
    end;
    if MessageDlg(msg010, mtConfirmation, [mbOK, mbCancel], 0) = mrCancel then
    begin
      key := 0;
      Exit;
    end;
    iTop := -1;
    iBottom := -1;
    iRight := -1;
    for i := sgTable.Row downto 1 do
    begin
      if sgTable.Cells[1, i] <> '' then
      begin
        iTop := i;
        Break;
      end;
    end;
    for i := sgTable.Row + 1 to sgTable.RowCount - 1 do
    begin
      if sgTable.Cells[1, i] <> '' then
      begin
        Break;
      end;
      iBottom := i;
    end;
    if ((iTop > -1) and (iBottom > -1)) then
    begin
      for i := sgTable.Col to sgTable.ColCount - 1 do
      begin
        if sgTable.Cells[i, iTop] <> '' then
        begin
          iRight := i + 1;
        end;
      end;
      if ((iRight > sgTable.Col) and (iRight < sgTable.ColCount - 1)) then
      begin
        for i := sgTable.Col to iRight do
        begin
          for x := iTop to iBottom do
          begin
            sgTable.Cells[i, x] := sgTable.Cells[i + 1, x];
          end;
        end;
        blTableMod := True;
        stGridLoaded := stTableLoaded;
        LabelFileNameChars;
        blTableSaved := False;
      end;
    end;
    key := 0;
  end
  else
  if ((key = Ord('G')) and (Shift = [ssMeta])) then
  begin
    FindInGrid;
    key := 0;
  end
  else
  if ((key = Ord('V')) and (Shift = [ssMeta])) then
  begin
    blTableMod := True;
    stGridLoaded := stTableLoaded;
    LabelFileNameChars;
    blTableSaved := False;
  end
  else
  if ((key = Ord('I')) and (Shift = [ssMeta, ssShift, ssAlt])) then
  begin
    if MessageDlg(msg016, mtConfirmation, [mbOK, mbCancel], 0) = mrCancel then
    begin
      key := 0;
      Exit;
    end;
    iTop := -1;
    iBottom := -1;
    iRight := -1;
    for i := sgTable.Row downto 1 do
    begin
      if sgTable.Cells[1, i] <> '' then
      begin
        iTop := i;
        Break;
      end;
    end;
    for i := sgTable.Row + 1 to sgTable.RowCount - 1 do
    begin
      if sgTable.Cells[1, i] <> '' then
      begin
        Break;
      end;
      iBottom := i;
    end;
    if ((iTop > -1) and (iBottom > -1)) then
    begin
      for i := sgTable.Col to sgTable.ColCount - 1 do
      begin
        if sgTable.Cells[i, iTop] <> '' then
        begin
          iRight := i + 1;
        end;
      end;
      if ((iRight > sgTable.Col) and (iRight < sgTable.ColCount - 1)) then
      begin
        for i := iRight downto sgTable.Col do
        begin
          for x := iTop to iBottom do
          begin
            sgTable.Cells[i + 1, x] := sgTable.Cells[i, x];
          end;
        end;
        for x := iTop to iBottom do
        begin
          sgTable.Cells[sgTable.Col, x] := '';
        end;
        blTableMod := True;
        stGridLoaded := stTableLoaded;
        LabelFileNameChars;
        blTableSaved := False;
      end;
    end;
    key := 0;
  end
  else
  if ((key = Ord('I')) and (Shift = [ssMeta, ssShift])) then
  begin
    if sgTable.Row < sgTable.RowCount - 1 then
    begin
      for i := 1 to sgTable.ColCount - 1 do
      begin
        if sgTable.Cells[i, sgTable.RowCount - 1] <> '' then
        begin
          MessageDlg(msg012, mtWarning, [mbOK], 0);
          key := 0;
          Exit;
        end;
      end;
      sgTable.InsertColRow(False, sgTable.Row);
      sgTable.Row := sgTable.Row - 1;
      sgTable.RowCount := csTableRowCount;
      blTableMod := True;
      stGridLoaded := stTableLoaded;
      LabelFileNameChars;
      blTableSaved := False;
    end;
    key := 0;
  end
  else
  if ((key = Ord('S')) and ((Shift = [ssMeta, ssCtrl]) or
    (Shift = [ssMeta, ssCtrl, ssShift]))) then
  begin
    if MessageDlg(msg014, mtConfirmation, [mbOK, mbCancel], 0) = mrOK then
    begin
      iTop := -1;
      iBottom := -1;
      for i := sgTable.Row downto 1 do
      begin
        if sgTable.Cells[1, i] <> '' then
        begin
          iTop := i + 1;
          Break;
        end;
      end;
      for i := sgTable.Row + 1 to sgTable.RowCount - 1 do
      begin
        if sgTable.Cells[1, i] <> '' then
        begin
          iBottom := i - 1;
          Break;
        end
        else
        begin
          for x := 2 to sgTable.ColCount - 1 do
          begin
            if ((sgTable.Cells[x, i] = '------') or
              (sgTable.Cells[x, i] = '---sum') or
              (sgTable.Cells[x, i] = '---avg') or
              (sgTable.Cells[x, i] = '---min') or
              (sgTable.Cells[x, i] = '---max') or
              (sgTable.Cells[x, i] = '---count')) then
              begin
                iBottom := i - 1;
                Break;
              end;
          end;
        end;
        if iBottom > -1 then
        begin
          Break;
        end;
      end;
      if iBottom = -1 then
      begin
        MessageDlg(msg019, mtWarning, [mbOK], 0);
        Exit;
      end;
      if ((iTop > -1) and (iBottom > -1)) then
      begin
        for i := iTop to iBottom do
        begin
          if TryStrToFloat(sgTable.Cells[sgTable.Col, i], iNum) = True then
          begin
            sgTable.Cells[sgTable.Col, i] := FormatFloat('000000000000.####', iNum);
          end;
        end;
        if Shift = [ssMeta, ssCtrl] then
        begin
          sgTable.SortOrder := soAscending;
        end
        else
        begin
          sgTable.SortOrder := soDescending;
        end;
        sgTable.SortColRow(True, sgTable.Col, iTop, iBottom);
        for i := iTop to iBottom do
        begin
          if TryStrToFloat(sgTable.Cells[sgTable.Col, i], iNum) = True then
          begin
            sgTable.Cells[sgTable.Col, i] := FormatFloat('0.####', iNum);
          end;
        end;
      end;
    end;
    blTableMod := True;
    stGridLoaded := stTableLoaded;
    LabelFileNameChars;
    blTableSaved := False;
    key := 0;
  end
  else
  if ((key = 37) and (Shift = [ssMeta])) then
  begin
    sgTable.Col := 1;
    key := 0;
  end
  else
  if ((key = 37) and (Shift = [ssAlt, ssMeta])) then
  begin
    if ((sgTable.Col < 3) or (sgTable.Row = sgTable.RowCount - 1) or
      (sgTable.Col = 1)) then
    begin
      key := 0;
      Exit;
    end;
    iTop := -1;
    iBottom := -1;
    for i := sgTable.Row downto 1 do
    begin
      if sgTable.Cells[1, i] <> '' then
      begin
        iTop := i;
        Break;
      end;
    end;
    for i := sgTable.Row + 1 to sgTable.RowCount - 1 do
    begin
      if sgTable.Cells[1, i] <> '' then
      begin
        Break;
      end;
      iBottom := i - 1;
    end;
    if ((iTop > -1) and (iBottom > -1)) then
    begin
      for i := iTop to iBottom do
      begin
        stField := sgTable.Cells[sgTable.Col, i];
        sgTable.Cells[sgTable.Col, i] := sgTable.Cells[sgTable.Col - 1, i];
        sgTable.Cells[sgTable.Col - 1, i] := stField;
      end;
      sgTable.Col := sgTable.Col - 1;
    end;
    blTableMod := True;
    stGridLoaded := stTableLoaded;
    LabelFileNameChars;
    blTableSaved := False;
    key := 0;
  end
  else
  if ((key = 38) and (Shift = [ssMeta])) then
  begin
    if ((sgTable.Row > 1) and (sgTable.Col = 1)) then
    begin
      for i := sgTable.Row - 1 downto 1 do
      if sgTable.Cells[1, i] <> '' then
      begin
        sgTable.Row := i;
        Break;
      end;
    end
    else
    begin
      sgTable.Row := 1;
    end;
    key := 0;
  end
  else
  if ((key = 38) and (Shift = [ssMeta, ssCtrl])) then
  begin
    if sgTable.Row = 1 then
    begin
      key := 0;
      Exit;
    end;
    iTop := -1;
    iBottom := -1;
    iNextTable := -1;
    for i := sgTable.Row downto 1 do
    begin
      if sgTable.Cells[1, i] <> '' then
      begin
        iTop := i;
        Break;
      end;
    end;
    if i > 1 then
    begin
      for i := i - 1 downto 1 do
      begin
        if sgTable.Cells[1, i] <> '' then
        begin
          iNextTable := i;
          Break;
        end;
      end;
    end;
    for i := sgTable.RowCount - 1 downto 1 do
    begin
      for x := 1 to sgTable.ColCount - 1 do
      begin
        if sgTable.Cells[x, i] <> '' then
        begin
          iBottom := i;
          Break;
        end;
      end;
      if iBottom > - 1 then
      begin
        Break;
      end;
    end;
    for i := sgTable.Row + 1 to iBottom do
    begin
      if sgTable.Cells[1, i] <> '' then
      begin
        Break;
      end;
    end;
    if sgTable.Cells[1, i] <> '' then
    begin
      iBottom := i - 2;
    end
    else
    begin
      iBottom := i;
    end;
    if ((iTop > -1) and (iBottom > -1) and (iNextTable > -1)) then
    begin
      for i := 1 to iBottom - iTop + 2 do
      begin
        sgTable.MoveColRow(False, iBottom + 1, iNextTable);
      end;
      sgTable.Row := iNextTable;
      sgTable.TopRow := iNextTable;
    end;
    blTableMod := True;
    stGridLoaded := stTableLoaded;
    LabelFileNameChars;
    blTableSaved := False;
    key := 0;
  end
  else
  if ((key = 38) and (Shift = [ssAlt, ssMeta])) then
  begin
    if sgTable.Row > 1 then
    begin
      sgTable.MoveColRow(False, sgTable.Row, sgTable.Row - 1);
      blTableMod := True;
      stGridLoaded := stTableLoaded;
      LabelFileNameChars;
      blTableSaved := False;
      key := 0;
    end;
  end
  else
  if ((key = 39) and (Shift = [ssMeta])) then
  begin
    for i := sgTable.ColCount - 1 downto 1 do
    begin
      if sgTable.Cells[i, sgTable.Row] <> '' then
      begin
        Break;
      end;
    end;
    sgTable.Col := i;
    key := 0;
  end
  else
  if ((key = 39) and (Shift = [ssAlt, ssMeta])) then
  begin
    if ((sgTable.Col = sgTable.ColCount - 1) or (sgTable.Col = 1) or
      (sgTable.Row = sgTable.RowCount - 1))then
    begin
      key := 0;
      Exit;
    end;
    iTop := -1;
    iBottom := -1;
    for i := sgTable.Row downto 1 do
    begin
      if sgTable.Cells[1, i] <> '' then
      begin
        iTop := i;
        Break;
      end;
    end;
    for i := sgTable.Row + 1 to sgTable.RowCount - 1 do
    begin
      if sgTable.Cells[1, i] <> '' then
      begin
        Break;
      end;
      iBottom := i - 1;
    end;
    if ((iTop > -1) and (iBottom > -1)) then
    begin
      for i := iTop to iBottom do
      begin
        stField := sgTable.Cells[sgTable.Col, i];
        sgTable.Cells[sgTable.Col, i] := sgTable.Cells[sgTable.Col + 1, i];
        sgTable.Cells[sgTable.Col + 1, i] := stField;
      end;
      sgTable.Col := sgTable.Col + 1;
    end;
    blTableMod := True;
    stGridLoaded := stTableLoaded;
    LabelFileNameChars;
    blTableSaved := False;
    key := 0;
  end
  else
  if ((key = 40) and (Shift = [ssMeta])) then
  begin
    if ((sgTable.Row < sgTable.RowCount - 1) and (sgTable.Col = 1)) then
    begin
      for i := sgTable.Row + 1 to sgTable.RowCount - 1 do
      if sgTable.Cells[1, i] <> '' then
      begin
        sgTable.Row := i;
        Break;
      end;
    end
    else
    begin
      for i := sgTable.RowCount - 1 downto 1 do
      begin
        if sgTable.Cells[sgTable.Col, i] <> '' then
        begin
          Break;
        end;
      end;
      sgTable.Row := i;
    end;
    key := 0;
  end
  else
  if ((key = 40) and (Shift = [ssMeta, ssCtrl])) then
  begin
    if sgTable.Row = sgTable.RowCount -1 then
    begin
      key := 0;
      Exit;
    end;
    iTop := -1;
    iBottom := -1;
    iNextTable := -1;
    for i := sgTable.Row downto 1 do
    begin
      if sgTable.Cells[1, i] <> '' then
      begin
        iTop := i;
        Break;
      end;
    end;
    for i := sgTable.RowCount - 1 downto 1 do
    begin
      for x := 1 to sgTable.ColCount - 1 do
      begin
        if sgTable.Cells[x, i] <> '' then
        begin
          iBottom := i;
          iNextTable := i + 1;
          Break;
        end;
      end;
      if iBottom > - 1 then
      begin
        Break;
      end;
    end;
    for i := sgTable.Row + 1 to iBottom do
    begin
      if sgTable.Cells[1, i] <> '' then
      begin
        Break;
      end;
    end;
    if sgTable.Cells[1, i] <> '' then
    begin
      iBottom := i - 2;
    end
    else
    begin
      iBottom := i;
    end;
    if i < sgTable.RowCount - 1 then
    begin
      for i := i + 1 to sgTable.RowCount - 1 do
      begin
        if sgTable.Cells[1, i] <> '' then
        begin
          iNextTable := i - 1;
          Break;
        end;
      end;
    end;
    if ((iTop > -1) and (iBottom > -1) and (iNextTable > -1)) then
    begin
      for i := 1 to iBottom - iTop + 2 do
      begin
        sgTable.MoveColRow(False, iTop, iNextTable);
      end;
      sgTable.Row := iNextTable - i + 1;
      sgTable.TopRow := iNextTable - i + 1;
    end;
    blTableMod := True;
    stGridLoaded := stTableLoaded;
    LabelFileNameChars;
    blTableSaved := False;
    key := 0;
  end
  else
  if ((key = 40) and (Shift = [ssAlt, ssMeta])) then
  begin
    if sgTable.Row < sgTable.RowCount - 1 then
    begin
      sgTable.MoveColRow(False, sgTable.Row, sgTable.Row + 1);
      blTableMod := True;
      stGridLoaded := stTableLoaded;
      LabelFileNameChars;
      blTableSaved := False;
      key := 0;
    end;
  end;
end;

procedure TfmMain.sgTableKeyPress(Sender: TObject; var Key: char);
var
  i, iTop: Integer;
begin
  if key = #13 then
  begin
    if ((sgTable.Cells[1, sgTable.Row] = '') and
      (sgTable.Col < sgTable.ColCount - 1) and
      (sgTable.Row < sgTable.RowCount - 1) and
      (sgTable.EditorMode = True)) then
    begin
      iTop := -1;
      for i := sgTable.Row downto 1 do
      begin
        if ((sgTable.Cells[1, i] <> '') and
          (sgTable.Cells[sgTable.Col, i] <> '')) then
        begin
          iTop := i;
          Break;
        end;
      end;
      if iTop > -1 then
      begin
        if sgTable.Cells[sgTable.Col + 1, iTop] = '' then
        begin
          sgTable.Col := 2;
          sgTable.Row := sgTable.Row + 1;
          key := #0;
        end;
      end;
    end;
  end;
end;

procedure TfmMain.sgTablePrepareCanvas(Sender: TObject; aCol, aRow: integer;
  aState: TGridDrawState);
begin
  if aRow = 0 then
  begin
    sgTable.Canvas.Font.Style := [fsBold];
    sgTable.Canvas.Font.Color := clTitle1;
  end
  else
  if ((sgTable.Cells[1, aRow] <> '') and (aCol = 1)) then
  begin
    sgTable.Canvas.Font.Style := [fsBold];
    sgTable.Canvas.Font.Color := clTitle2;
  end
  else
  if ((sgTable.Cells[1, aRow] <> '') and (aCol > 1)) then
  begin
    sgTable.Canvas.Font.Style := [fsBold];
    sgTable.Canvas.Font.Color := clTitle3;
  end
  else
  if ((sgTable.Cells[aCol, aRow] = '------') or
    (sgTable.Cells[aCol, aRow] = '---sum') or
    (sgTable.Cells[aCol, aRow] = '---avg') or
    (sgTable.Cells[aCol, aRow] = '---min') or
    (sgTable.Cells[aCol, aRow] = '---max') or
    (sgTable.Cells[aCol, aRow] = '---count')) then
  begin
    sgTable.Canvas.Font.Style := [];
    sgTable.Canvas.Font.Color := clCode;
  end
  else
  begin
    sgTable.Canvas.Font.Style := [];
    sgTable.Canvas.Font.Color := dbText.Font.Color;
  end;
end;

procedure TfmMain.sgTableSetEditText(Sender: TObject; ACol, ARow: Integer;
  const Value: string);
begin
  blTableMod := True;
  stGridLoaded := stTableLoaded;
  LabelFileNameChars;
  blTableSaved := False;
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
var
  i: Integer;
begin
  blIsPresenting := False;
  cbFilter.Visible := True;
  if SaveFile = False then
  begin
    Exit;
  end;
  CreateBackup;
  stFileName := '';
  CreateYAML;
  dbText.SelStart := 11;
  dbText.SetFocus;
  sgTable.RowCount := 1;
  sgTable.RowCount := csTableRowCount;
  for i := 1 to sgTable.ColCount - 1 do
  begin
    sgTable.ColWidths[i] := 280;
  end;
  pnGrid.Height := 1;
end;

procedure TfmMain.miFileOpenClick(Sender: TObject);
begin
  blIsPresenting := False;
  cbFilter.Visible := True;
  if SaveFile = False then
  begin
    Exit;
  end;
  CreateBackup;
  if odOpen.Execute then
  try
    sgTable.RowCount := 1;
    sgTable.RowCount := csTableRowCount;
    stFileName := odOpen.FileName;
    DeactForm(stFileName);
    dbText.Lines.LoadFromFile(stFileName);
    if FileExistsUTF8(ExtractFileNameWithoutExt(stFileName) + '.csv') then
    begin
      sgTable.LoadFromCSVFile(ExtractFileNameWithoutExt(stFileName) + '.csv',
        #9, False);
      sgTable.RowCount := csTableRowCount;
      stGridLoaded := stTableLoaded;
    end
    else
    begin
      stGridLoaded := '';
    end;
    MoveToPos;
    iBookmarkPos := 0;
    LabelFileNameChars;
    if blDisableFormatting = False then
    begin
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        checkTextInDocument(nil);
    end;
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      undoManager.removeAllActions;
    UpdateLastFile;
    ShowCurrentTitleTodo;
    blFileMod := False;
    blTableMod := False;
  except
    MessageDlg(msg004, mtWarning, [mbOK], 0);
  end;
end;

procedure TfmMain.miFilesSearchClick(Sender: TObject);
begin
  fmFiles.ShowModal;
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
      blFileSaved := True;
      UpdateLastFile;
    finally
      myList.Free;
    end;
    if blTableSaved = False then
    begin
      sgTable.SaveToCSVFile(ExtractFileNameWithoutExt(stFileName) + '.csv',
        #9, False);
      blTableSaved := True;
    end;
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
  stOldFile: String;
begin
  blIsPresenting := False;
  cbFilter.Visible := True;
  if ((UTF8Length(dbText.Lines[1]) > 7) and
    (UTF8Copy(dbText.Lines[1], 1, 7) = 'title: ')) then
  begin
    sdSave.FileName := UTF8Copy(dbText.Lines[1], 8, 100) + '.md';
  end;
  if sdSave.Execute then
  try
    stOldFile := stFileName;
    stFileName := sdSave.FileName;
    try
      myList := TStringList.Create;
      myList.Text := dbText.Text;
      myList.SaveToFile(stFileName);
      blFileSaved := True;
      UpdateLastFile;
      LabelFileNameChars;
    finally
      myList.Free;
    end;
    if ((FileExistsUTF8(ExtractFileNameWithoutExt(stOldFile) + '.csv')) or
      (blTableSaved = False))then
    begin
      sgTable.SaveToCSVFile(ExtractFileNameWithoutExt(stFileName) + '.csv',
        #9, False);
      blTableSaved := True;
    end;
  except
    MessageDlg(msg003, mtWarning, [mbOK], 0);
  end;
end;

procedure TfmMain.miFileOpenLast1Click(Sender: TObject);
begin
  blIsPresenting := False;
  cbFilter.Visible := True;
  if SaveFile = False then
  begin
    Exit;
  end;
  CreateBackup;
  if FileExistsUTF8(LastDatabase1) then
  try
    sgTable.RowCount := 1;
    sgTable.RowCount := csTableRowCount;
    stFileName := LastDatabase1;
    DeactForm(stFileName);
    dbText.Lines.LoadFromFile(stFileName);
    if FileExistsUTF8(ExtractFileNameWithoutExt(stFileName) + '.csv') then
    begin
      sgTable.LoadFromCSVFile(ExtractFileNameWithoutExt(stFileName) + '.csv',
        #9, False);
      sgTable.RowCount := csTableRowCount;
      stGridLoaded := stTableLoaded;
    end
    else
    begin
      stGridLoaded := '';
    end;
    MoveToPos;
    iBookmarkPos := 0;
    LabelFileNameChars;
    if blDisableFormatting = False then
    begin
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        checkTextInDocument(nil);
    end;
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      undoManager.removeAllActions;
    UpdateLastFile;
    ShowCurrentTitleTodo;
    blFileMod := False;
    blTableMod := False;
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
  blIsPresenting := False;
  cbFilter.Visible := True;
  if SaveFile = False then
  begin
    Exit;
  end;
  CreateBackup;
  if FileExistsUTF8(LastDatabase2) then
  try
    sgTable.RowCount := 1;
    sgTable.RowCount := csTableRowCount;
    stFileName := LastDatabase2;
    DeactForm(stFileName);
    dbText.Lines.LoadFromFile(stFileName);
    if FileExistsUTF8(ExtractFileNameWithoutExt(stFileName) + '.csv') then
    begin
      sgTable.LoadFromCSVFile(ExtractFileNameWithoutExt(stFileName) + '.csv',
        #9, False);
      sgTable.RowCount := csTableRowCount;
      stGridLoaded := stTableLoaded;
    end
    else
    begin
      stGridLoaded := '';
    end;
    MoveToPos;
    iBookmarkPos := 0;
    LabelFileNameChars;
    if blDisableFormatting = False then
    begin
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        checkTextInDocument(nil);
    end;
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      undoManager.removeAllActions;
    UpdateLastFile;
    blFileMod := False;
    blTableMod := False;
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
  blIsPresenting := False;
  cbFilter.Visible := True;
  if SaveFile = False then
  begin
    Exit;
  end;
  CreateBackup;
  if FileExistsUTF8(LastDatabase3) then
  try
    sgTable.RowCount := 1;
    sgTable.RowCount := csTableRowCount;
    stFileName := LastDatabase3;
    DeactForm(stFileName);
    dbText.Lines.LoadFromFile(stFileName);
    if FileExistsUTF8(ExtractFileNameWithoutExt(stFileName) + '.csv') then
    begin
      sgTable.LoadFromCSVFile(ExtractFileNameWithoutExt(stFileName) + '.csv',
        #9, False);
      sgTable.RowCount := csTableRowCount;
      stGridLoaded := stTableLoaded;
    end
    else
    begin
      stGridLoaded := '';
    end;
    MoveToPos;
    iBookmarkPos := 0;
    LabelFileNameChars;
    if blDisableFormatting = False then
    begin
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        checkTextInDocument(nil);
    end;
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      undoManager.removeAllActions;
    UpdateLastFile;
    blFileMod := False;
    blTableMod := False;
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
  blIsPresenting := False;
  cbFilter.Visible := True;
  if SaveFile = False then
  begin
    Exit;
  end;
  CreateBackup;
  if FileExistsUTF8(LastDatabase4) then
  try
    sgTable.RowCount := 1;
    sgTable.RowCount := csTableRowCount;
    stFileName := LastDatabase4;
    DeactForm(stFileName);
    dbText.Lines.LoadFromFile(stFileName);
    if FileExistsUTF8(ExtractFileNameWithoutExt(stFileName) + '.csv') then
    begin
      sgTable.LoadFromCSVFile(ExtractFileNameWithoutExt(stFileName) + '.csv',
        #9, False);
      sgTable.RowCount := csTableRowCount;
      stGridLoaded := stTableLoaded;
    end
    else
    begin
      stGridLoaded := '';
    end;
    MoveToPos;
    iBookmarkPos := 0;
    LabelFileNameChars;
    if blDisableFormatting = False then
    begin
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        checkTextInDocument(nil);
    end;
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      undoManager.removeAllActions;
    UpdateLastFile;
    blFileMod := False;
    blTableMod := False;
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
  if ((sgTable.Focused = False) and (blIsPresenting = False)) then
  begin
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      copy_(nil);
  end;
end;

procedure TfmMain.miEditCutClick(Sender: TObject);
begin
  if ((sgTable.Focused = False) and (blIsPresenting = False)) then
  begin
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      cut(nil);
  end;
end;

procedure TfmMain.miEditPasteClick(Sender: TObject);
begin
  if ((sgTable.Focused = False) and (blIsPresenting = False)) then
  begin
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      pasteAsPlainText(nil);
    if blDisableFormatting = False then
    begin
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        checkTextInDocument(nil);
    end;
  end;
end;

procedure TfmMain.miEditSelectAllClick(Sender: TObject);
begin
  if ((sgTable.Focused = False) and (blIsPresenting = False)) then
  begin
    dbText.SelectAll;
  end;
end;

procedure TfmMain.miEditFindClick(Sender: TObject);
begin
  if blIsPresenting = True then
  begin
    blIsPresenting := False;
    cbFilter.Visible := True;
    FormatListTitleTodo;
  end;
  fmSearch.Show
end;

procedure TfmMain.miEditLinkClick(Sender: TObject);
var
  stLink: string;
  i: Integer;
begin
  if blIsPresenting = True then
  begin
    blIsPresenting := False;
    cbFilter.Visible := True;
    FormatListTitleTodo;
  end;
  if odLink.Execute then
  begin
    for i := 0 to odLink.Files.Count - 1 do
    begin
      stLink := odLink.Files[i];
      stLink := '[](file://' + StringReplace(stLink, ' ', '%20',
        [rfReplaceAll]) + ')';
      if odLink.Files.Count = 1 then
      begin
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).
          insertText(NSStringUtf8(stLink));
        dbText.SelStart := dbText.SelStart - UTF8Length(stLink) + 1;
      end
      else
      begin
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).
          insertText(NSStringUtf8(stLink + LineEnding));
      end;
    end;
    if blDisableFormatting = False then
    begin
      TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        checkTextInDocument(nil);
    end;
  end;
end;

procedure TfmMain.miEditFindDuplicateClick(Sender: TObject);
var
  rng: NSRange;
  iLen, i, iSelStart: integer;
  slList1, slList2: TStringList;
  stText: WideString;
  stItem: String;
  stSeparators: String = '.,;:-–(){}[]/\''"’‘”“«»?¿!¡ ';
begin
  if blIsPresenting = True then
  begin
    blIsPresenting := False;
    cbFilter.Visible := True;
    FormatListTitleTodo;
  end;
  if miEditDisableForm.Checked = True then
  begin
    miEditDisableFormClick(nil);
  end;
  try
    Screen.Cursor := crHourGlass;
    slList1 := TStringList.Create;
    slList2 := TStringList.Create;
    stText := WideString(dbText.Text);
    iLen := Length(stText);
    i := 1;
    iSelStart := 1;
    while i <= iLen do
    begin
      if (((stText[i] = '.') and ((stText[i + 1] = ' ') or (i = iLen))) or
        (stText[i] = '?') or (stText[i] = '!') or
        (stText[i] = LineEnding)) then
      begin
        slList1.Text := slList2.Text;
        slList2.Clear;
      end;
      if Pos(stText[i], stSeparators) > 0 then
      begin
        if ((slList1.IndexOf(UTF8UpperCase(stItem)) > -1) or
          (slList2.IndexOf(UTF8UpperCase(stItem)) > -1)) then
        begin
          rng.location := iSelStart - 1;
          rng.length := i - iSelStart;
          TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
            setTextColor_range(ColorToNSColor(clRepetition), rng);
        end;
        slList2.Add(UTF8UpperCase(stItem));
        stItem := '';
        iSelStart := i + 1;
      end
      else
      begin
        stItem := stItem + stText[i];
      end;
      Inc(i);
    end;
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
  if blIsPresenting = True then
  begin
    blIsPresenting := False;
    cbFilter.Visible := True;
    FormatListTitleTodo;
  end;
  fmTasks.ShowModal;
end;

procedure TfmMain.miEditWordsClick(Sender: TObject);
begin
  if blIsPresenting = True then
  begin
    blIsPresenting := False;
    cbFilter.Visible := True;
    FormatListTitleTodo;
  end;
  fmWords.ShowModal;
end;

procedure TfmMain.miEditHideListClick(Sender: TObject);
var
  iPos: Integer;
begin
  if blIsPresenting = True then
  begin
    blIsPresenting := False;
    cbFilter.Visible := True;
  end;
  iPos := dbText.SelStart;
  if pnTitTodo.Visible = True then
  begin
    pnTitTodo.Visible := False;
    spTitles.Visible := False;
    miEditHideList.Checked := True;
    blHideTitleTodo := True;
    FormatListTitleTodo;
  end
  else
  begin
    pnTitTodo.Visible := True;
    spTitles.Visible := True;
    miEditHideList.Checked := False;
    blHideTitleTodo := False;
    FormatListTitleTodo;
    ShowCurrentTitleTodo;
  end;
  dbText.SelStart := iPos;
end;

procedure TfmMain.miEditDisableFormClick(Sender: TObject);
var
  iLen, iPos: integer;
  stText: WideString = '';
  myFont: NSFont;
  fd: NSFontDescriptor;
  rng: NSRange;
begin
  if blIsPresenting = True then
  begin
    blIsPresenting := False;
    cbFilter.Visible := True;
    FormatListTitleTodo;
  end;
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
    TCocoaTextView(NSScrollView(dbText.Handle).documentView).textStorage.
      removeAttribute_range(NSBackgroundColorAttributeName, rng);
  end
  else
  begin
    blDisableFormatting := False;
    FormatListTitleTodo;
  end;
  dbText.SelStart := iPos;
end;

procedure TfmMain.miToolsPandocClick(Sender: TObject);
var
  stArgument, stInput, stOutput, stLine, stHeader: string;
  slDocTable: TStringList;
  x, y, n, iLastRow: integer;
begin
  if blIsPresenting = True then
  begin
    blIsPresenting := False;
    cbFilter.Visible := True;
    FormatListTitleTodo;
  end;
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
  if FileExistsUTF8(ExtractFileNameWithoutExt(stFileName) + '.csv') then
  try
    iLastRow := -1;
    for y := sgTable.RowCount - 1 downto 1 do
    begin
      for x := 1 to sgTable.ColCount - 1 do
      begin
        if sgTable.Cells[x, y] <> '' then
        begin
          iLastRow := y;
          Break;
        end;
      end;
      if iLastRow > -1 then
      begin
        Break;
      end;
    end;
    slDocTable := TStringList.Create;
    slDocTable.Text := dbText.Text;
    slDocTable.Add('');
    slDocTable.Add('# ' + lb008);
    slDocTable.Add('');
    y := 1;
    while y <= iLastRow do
    begin
      if sgTable.Cells[1, y] <> '' then
      begin
        slDocTable.Add('## ' + sgTable.Cells[1, y]);
        slDocTable.Add('');
        stLine := '| ';
        stHeader := '|';
        for x := 2 to sgTable.ColCount - 1 do
        begin
          if sgTable.Cells[x, y] = '' then
          begin
            Break;
          end;
          stLine := stLine + sgTable.Cells[x, y] + ' | ';
          stHeader := stHeader + '-----|';
        end;
        slDocTable.Add(stLine);
        slDocTable.Add(stHeader);
        Inc(y);
        while ((sgTable.Cells[1, y] = '') and (y <= iLastRow)) do
        begin
          stLine := '| ';
          for n := 2 to x - 1 do
          begin
            if sgTable.Cells[n, y] <> '' then
            begin
              stLine := stLine + sgTable.Cells[n, y] + ' | ';
            end;
          end;
          if stLine <> '| ' then
          begin
            slDocTable.Add(stLine);
          end;
          Inc(y);
        end;
      end;
    end;
    slDocTable.SaveToFile(ExtractFileNameWithoutExt(stFileName) + '.export');
    stInput := ExtractFileNameWithoutExt(stFileName) + '.export';
  finally
    slDocTable.Free;
  end
  else
  begin
    stInput := stFileName;
  end;
  if FileExistsUTF8(pandocTemplate) then
  begin
    stArgument := pandocPath + 'pandoc ' + '--from markdown' +
      pandocOptions + ' -s "' + stInput + '" -o "' + stOutput +
      '" --reference-doc "' + pandocTemplate + '" && open "' + stOutput + '"';
  end
  else
  begin
    stArgument := pandocPath + 'pandoc ' + '--from markdown' +
      pandocOptions + ' -s "' + stInput + '" -o "' + stOutput +
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

procedure TfmMain.miToolsOptmizeClick(Sender: TObject);
var
  slOrig, slDest: TStringList;
  i: Integer;
  blYAML: boolean = False;
  stHeading2: String = '';
begin
  if blIsPresenting = True then
  begin
    blIsPresenting := False;
    cbFilter.Visible := True;
    FormatListTitleTodo;
  end;
  if dbText.Text = '' then
  begin
    Exit;
  end;
  if MessageDlg(msg020, mtConfirmation, [mbOK, mbCancel], 0) = mrCancel then
  begin
    Exit;
  end;
  if SaveFile = False then
  begin
    Exit;
  end;
  try
    try
      slOrig := TStringList.Create;
      slDest := TStringList.Create;
      slOrig.AddStrings(dbText.Lines);
      if slOrig[0] = '---' then
      begin
        blYAML := True;
      end;
      for i := 0 to slOrig.Count - 1 do
      begin
        if UTF8Copy(slOrig[i], 1, 2) = '- ' then
        begin
          slDest.Add('⦿ ' + UTF8Copy(slOrig[i], 3, UTF8Length(slOrig[i])));
          if i < slOrig.Count - 1 then
          begin
            if UTF8Copy(slOrig[i + 1], 1, 4) <> '  - ' then
            begin
              slDest.Add('');
            end;
          end;
        end
        else
        if UTF8Copy(slOrig[i], 1, 4) = '  - ' then
        begin
          slDest.Add('  ◆ ' + UTF8Copy(slOrig[i], 5, UTF8Length(slOrig[i])));
        end
        else
        if UTF8Copy(slOrig[i], 1, 3) = '## ' then
        begin
          if slOrig[i] <> stHeading2 then
          begin
            slDest.Add(slOrig[i]);
            stHeading2 := slOrig[i];
          end
          else
          begin
            slDest.Add('');
          end
        end
        else
        if ((slOrig[i] = '---') and (i > 0)) then
        begin
          slDest.Add(slOrig[i]);
          blYAML := False;
        end
        else
        begin
          slDest.Add(slOrig[i]);
          if blYAML = False then
          begin
            slDest.Add('');
          end;
        end;
        Application.ProcessMessages;
      end;
      while UTF8Pos(LineEnding + LineEnding + LineEnding, slDest.Text) > 0 do
      begin
        slDest.Text := StringReplace(slDest.Text, LineEnding + LineEnding +
          LineEnding, LineEnding + LineEnding, [rfReplaceAll]);
      end;
      slDest.SaveToFile(ExtractFileNameWithoutExt(stFileName) + ' - mxMarkEdit.md');
    except
      MessageDlg(msg021, mtWarning, [mbOK], 0);
    end;
  finally
    slOrig.Free;
    slDest.Free;
  end;
end;


procedure TfmMain.miToolsOpenWinClick(Sender: TObject);
begin
  // FileExist doesn't work on app directory
  Unix.fpSystem('open -n /Applications/mxMarkEdit.app --args -');
end;

procedure TfmMain.miToolsTrans1Click(Sender: TObject);
begin
  fmMain.AlphaBlendValue := 255;
  miToolsTrans1.Checked := True;
end;

procedure TfmMain.miToolsTrans2Click(Sender: TObject);
begin
  fmMain.AlphaBlendValue := 200;
  miToolsTrans2.Checked := True;
end;

procedure TfmMain.miToolsTrans3Click(Sender: TObject);
begin
  fmMain.AlphaBlendValue := 170;
  miToolsTrans3.Checked := True;
end;

procedure TfmMain.miToolsOptionsClick(Sender: TObject);
begin
  if blIsPresenting = True then
  begin
    blIsPresenting := False;
    cbFilter.Visible := True;
    FormatListTitleTodo;
  end;
  fmOptions.ShowModal;
end;

procedure TfmMain.ShortcutsClick(Sender: TObject);
begin
  if blIsPresenting = True then
  begin
    blIsPresenting := False;
    cbFilter.Visible := True;
  end;
  fmShortcuts.ShowModal;
end;

procedure TfmMain.miCopyrightClick(Sender: TObject);
begin
  if blIsPresenting = True then
  begin
    blIsPresenting := False;
    cbFilter.Visible := True;
  end;
  fmCopyright.ShowModal;
end;


// *******************************************************
// *************** Procedures of formatting **************
// *******************************************************

procedure TfmMain.FormatListTitleTodo;
var
  i, iLen, iPos, iTopRow, iLevel, iIndent, iTab: integer;
  blHeading, blPosInHeading, blBoldItalics, blItalics, blBold, blMono,
  blQuote, blStartLinesQuote, blFootnote, blLink: boolean;
  iStartHeading, iStartBoldItalics, iStartItalics, iStartBold, iStartMono,
  iStartQuote, iStartLinesQuote, iStartFootnote, iStartLink: integer;
  stText: WideString = '';
  stTitle: WideString = '';
  stSpaces: string = '';
  fd: NSFontDescriptor;
  myFont, monoFont, miniFont: NSFont;
  rng: NSRange;
  par: NSMutableParagraphStyle;
  tabs: NSMutableArray;
  tab: NSTextTab;
begin
  TCocoaTextView(NSScrollView(dbText.Handle).documentView).
    setInsertionPointColor(ColorToNSColor(clInsertionPoint));
  if ((blTextOnChange = True) or ((blDisableFormatting = True) and
    (pnTitTodo.Visible = False))) then
  begin
    Exit;
  end;
  if dbText.Text = '' then
  begin
    sgTitles.Clear;
    Exit;
  end;
  pnBottom.Height := 28;
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
  tabs := NSMutableArray.alloc.init;
  iIndent := dbText.Font.Size * 5 - dbText.Font.Size div 3;
  for iTab := 1 to 30 do
  begin
    tab := NSTextTab.alloc.initWithType_location(NSLeftTabStopType, iTab * iIndent);
    Tabs.addObject(tab);
    tab.Release;
  end;
  par := GetWritePara(TCocoaTextView(
    NSScrollView(dbText.Handle).documentView).textStorage, 1);
  par.setLineHeightMultiple(iLineSpacing);
  par.setTabStops(tabs);
  TCocoaTextView(NSScrollView(dbText.Handle).documentView).
    textStorage.addAttribute_value_range(NSParagraphStyleAttributeName,
    par, rng);
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
    miniFont := NSFont.fontWithDescriptor_size(fd, 1);
    fd := FindFont(stFontMono, 0);
    monoFont := NSFont.fontWithDescriptor_size(fd, iFontMonoSize);
  end;
  sgTitles.Clear;

  // Markdown YAML
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
          iStartHeading := i;
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
          rng.location := iStartHeading - 1;
          rng.length := i - iStartHeading + 1;
          TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
            setTextColor_range(ColorToNSColor(clTodo), rng);
          stTitle := stSpaces + '  □ ' + Copy(stTitle, 7, Length(stTitle));
        end
        else
        if ((Copy(stTitle, 1, 6) = '- [X] ') or
          (Copy(stTitle, 1, 6) = '- [x] ')) then
        begin
          rng.location := iStartHeading - 1;
          rng.length := i - iStartHeading + 1;
          TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
            setTextColor_range(ColorToNSColor(clTodo), rng);
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
            addAttribute_value_range(NSBackgroundColorAttributeName,
            ColorToNSColor(clQuote), rng);
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
    if ((stText[i] = '`') and (stText[i + 1] = '`') and
      (stText[i + 2] = '`') and (blStartLinesQuote = False) and
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
    if ((stText[i] = '`') and (stText[i + 1] = '`') and
      (stText[i + 2] = '`') and (blStartLinesQuote = True)) then
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
    else if (((stText[i] = '^') and (stText[i + 1] = '[')) or
      ((stText[i] = '[') and (stText[i + 1] = '^'))) then
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
    else if stText[i] = ']' then
    begin
      if ((stText[i + 1] = '(') and (blLink = True)) then
      begin
        rng.location := iStartLink - 1;
        rng.length := i - iStartLink + 1;
        TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
          setTextColor_range(ColorToNSColor(clLink), rng);
        iStartLink := rng.location + rng.length + 2;
      end
      else
      begin
        blLink := False;
      end;
    end
    else if ((stText[i] = ')') and (blLink = True)) then
    begin
      blLink := False;
      rng.location := iStartLink - 2;
      rng.length := 1;
      TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        setTextColor_range(ColorToNSColor(clLink), rng);
      rng.location := iStartLink - 1;
      rng.length := i - iStartLink;
      TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        setTextColor_range(ColorToNSColor(clCode), rng);
      rng.location := i - 1;
      rng.length := 1;
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

procedure TfmMain.SelectInsertFootnote;
var
  iPos, iOrigPos, iNew: integer;
  rng: NSRange;
  stAttWord: NSAttributedString;
  stWord: string;
begin
  if dbText.Text = '' then
  begin
    Exit;
  end;
  blTextOnChange := True;
  if dbText.SelStart = StrToNSString(dbText.Text, True).length then
  begin
    dbText.Lines.Add('');
    dbText.SelStart := dbText.SelStart - 2;
  end;
  iOrigPos := dbText.SelStart;
  if UTF8Copy(dbText.Lines[dbText.CaretPos.y], 1, 2) = '[^' then
  begin
    iPos := UTF8CocoaPos(']', dbText.Lines[dbText.CaretPos.y]);
    dbText.SelStart := UTF8CocoaPos(UTF8Copy(dbText.Lines[dbText.CaretPos.y],
      1, iPos), dbText.Text) + iPos - 1;
    FormatListTitleTodo;
  end
  else
  begin
    TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      selectWord(nil);
    rng := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).
      documentView).selectedRange;
    stAttWord := TCocoaTextView(NSScrollView(dbText.Handle).documentView).
      attributedSubstringFromRange(rng);
    if UTF8Copy(NSStringToString(stAttWord.string_), 1, 1) = ']' then
    begin
      TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        moveBackward(nil);
      TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        moveBackward(nil);
      TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        selectWord(nil);
      rng := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).
        documentView).selectedRange;
      stAttWord := TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        attributedSubstringFromRange(rng);
    end
    else
    if UTF8Copy(NSStringToString(stAttWord.string_), 1, 1) = '^' then
    begin
      TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        moveForward(nil);
      TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        selectWord(nil);
      rng := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).
        documentView).selectedRange;
      stAttWord := TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        attributedSubstringFromRange(rng);
    end
    else
    if UTF8Copy(NSStringToString(stAttWord.string_), 1, 1) = '[' then
    begin
      TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        moveForward(nil);
      TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        selectWord(nil);
      rng := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).
        documentView).selectedRange;
      stAttWord := TCocoaTextView(NSScrollView(dbText.Handle).documentView).
        attributedSubstringFromRange(rng);
    end;
    stWord := NSStringToString(stAttWord.string_);
    if TryStrToInt(stWord, iNew) = True then
    begin
      if UTF8CocoaPos(LineEnding + '[^' + IntToStr(iNew) + ']:',
        dbText.Text, 1) > 0 then
      begin
        dbText.SelStart := UTF8CocoaPos(LineEnding + '[^' + IntToStr(iNew) +
          ']:', dbText.Text, 1) + UTF8Length(IntToStr(iNew)) + 5;
        dbText.SelLength := 0;
        FormatListTitleTodo;
      end;
    end
    else
    begin
      dbText.SelLength := 0;
      dbText.SelStart := iOrigPos;
      if dbText.CaretPos.X = 0 then
      begin
        MessageDlg(msg011, mtWarning, [mbOK], 0);
      end
      else
      begin
        TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
          insertText(NSStringUtf8('[^0]'));
        dbText.Lines.Add('[^0]: ' + #1);
        RenumberFootnotes;
        dbText.SelStart := UTF8CocoaPos(#1, dbText.Text) - 1;
        TCocoaTextView(NSScrollView(dbText.Handle).documentView).
          deleteForward(nil);
      end;
    end;
  end;
  blTextOnChange := False;
  FormatListTitleTodo;
end;

procedure TfmMain.RenumberFootnotes;
var
  blFootNum, blFootnote: boolean;
  iStartFootNum, iStartFootnote, iNum, iIncNum, i, iLen, iLine, n: integer;
  stNum: string;
  stText: WideString;
  rng: NSRange;
  slNumList, slFootnotes: TStringList;
begin
  if dbText.Text = '' then
  begin
    Exit;
  end;
  blTextOnChange := True;
  blFootNum := False;
  blFootnote := False;
  iStartFootNum := -1;
  iStartFootnote := -1;
  iNum := -1;
  iIncNum := 1;
  stNum := '';
  stText := WideString(dbText.Text);
  iLen := Length(stText);
  rng.location := 0;
  rng.length := iLen;
  i := 1;
  slNumList := TStringList.Create;
  slFootnotes := TStringList.Create;
  try
    while i <= iLen do
    begin
      if ((stText[i] = '^') and (stText[i - 1] = '[') and
        (stText[i - 2] <> LineEnding)) then
      begin
        if blFootNum = False then
        begin
          blFootNum := True;
          iStartFootNum := i;
        end;
      end
      else
      if ((stText[i] = '^') and (stText[i - 1] = '[') and
        (stText[i - 2] = LineEnding)) then
      begin
        if blFootnote = False then
        begin
          blFootnote := True;
          iStartFootnote := i;
        end;
      end
      else
      if ((stText[i] = ']') and (blFootNum = True)) then
      begin
        blFootNum := False;
        rng.location := iStartFootNum;
        rng.length := i - iStartFootNum - 1;
        if TryStrToInt(stNum, iNum) = True then
        begin
          if iIncNum <> iNum then
          begin
            slNumList.Add(stNum);
            TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
              insertText_replacementRange(NSStringUtf8(IntToStr(iIncNum)), rng);
            stText := WideString(dbText.Text);
            iLen := Length(stText);
          end
          else
          begin
            slNumList.Add('');
          end;
          Inc(iIncNum);
        end;
        stNum := '';
      end
      else
      if ((stText[i] = ']') and (stText[i + 1] = ':') and
        (blFootnote = True)) then
      begin
        blFootnote := False;
        rng.location := iStartFootnote;
        rng.length := i - iStartFootnote - 1;
        iNum := slNumList.IndexOf(stNum);
        if iNum > -1 then
        begin
          TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
            insertText_replacementRange(NSStringUtf8(IntToStr(iNum + 1)), rng);
          stText := WideString(dbText.Text);
          iLen := Length(stText);
        end;
        stNum := '';
      end
      else
      if ((blFootNum = True) or (blFootnote = True)) then
      begin
        stNum := stNum + stText[i];
      end;
      Inc(i);
    end;
    for iLine := 0 to dbText.Lines.Count - 1 do
    begin
      if UTF8Copy(dbText.Lines[iLine], 1, 2) = '[^' then
      begin
        if TryStrToInt(UTF8Copy(dbText.Lines[iLine], 3,
          UTF8Pos(']', dbText.Lines[iLine]) - 3), iNum) then
        begin
          Break;
        end;
      end;
    end;
    for i := iLine to dbText.Lines.Count - 1 do
    begin
      if TryStrToInt(UTF8Copy(dbText.Lines[i], 3, UTF8Pos(']', dbText.Lines[i]) -
        3), iNum) then
        slFootnotes.Add(FormatFloat('000000', iNum) + dbText.Lines[i]);
    end;
    slFootnotes.Sort;
    if slFootnotes.Count > 0 then
    begin
      n := 0;
      for i := iLine to dbText.Lines.Count - 1 do
      begin
        dbText.Lines[i] := UTF8Copy(slFootnotes[n], 7,
          UTF8Length(slFootnotes[n]));
        Inc(n);
        if n > slFootnotes.Count - 1 then
        begin
          Break;
        end;
      end;
    end;
  finally
    slNumList.Free;
    slFootnotes.Free;
    blTextOnChange := False;
  end;
  FormatListTitleTodo;
end;

procedure TfmMain.LabelFileNameChars;
var
  iLength, iPos: integer;
  stFilePath: String;
begin
  iLength := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
    textStorage.characters.Count;
  iPos := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
    selectedRange.location;
  if iLength > 0 then
  begin
    if stFileName <> '' then
    begin
      lbChars.Hint := ExtractFileDir(stFileName) + '/  •  ' +
        ExtractFileName(stFileName);
      stFilePath :=  ExtractFileDir(stFileName);
      if Length(stFilePath) > 30 then
      begin
        stFilePath := '...' + Copy(stFilePath, Length(stFilePath) - 30,
          Length(stFilePath));
      end;
      lbChars.Caption := stFilePath + '/  •  ' +
        ExtractFileName(stFileName) + stGridLoaded + '  •  ' + msg001 + ' ' +
        FormatFloat('#,##0', iLength) + ' (' +
        FormatFloat('#0', iPos / iLength * 100) + '%)';
    end
    else
    begin
      lbChars.Caption := msg001 + ' ' + FormatFloat('#,##0', iLength) +
        ' (' + FormatFloat('#0', iPos / iLength * 100) + '%)';
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
    ColDatabase2 := ColDatabase1;
    RowDatabase2 := RowDatabase1;
    ColWidthDatabase2 := ColWidthDatabase1;
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
    ColDatabase3 := ColDatabase2;
    ColDatabase2 := ColDatabase1;
    RowDatabase3 := RowDatabase2;
    RowDatabase2 := RowDatabase1;
    ColWidthDatabase3 := ColWidthDatabase2;
    ColWidthDatabase2 := ColWidthDatabase1;
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
    ColDatabase4 := ColDatabase3;
    ColDatabase3 := ColDatabase2;
    ColDatabase2 := ColDatabase1;
    RowDatabase4 := RowDatabase3;
    RowDatabase3 := RowDatabase2;
    RowDatabase2 := RowDatabase1;
    ColWidthDatabase4 := ColWidthDatabase3;
    ColWidthDatabase3 := ColWidthDatabase2;
    ColWidthDatabase2 := ColWidthDatabase1;
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
  i: Integer;
begin
  Result := True;
  if ((stFileName <> '') and (stFileName = LastDatabase1)) then
  begin
    LastPosDatabase1 := dbText.SelStart;
    TopIndexDatabase1 := sgTitles.TopRow;
    if sgTable.Col > 0 then
    begin
      ColDatabase1 := sgTable.Col;
    end
    else
    begin
      ColDatabase1 := 1;
    end;
    if sgTable.Row > 0 then
    begin
      RowDatabase1 := sgTable.Row;
    end
    else
    begin
      RowDatabase1 := 1;
    end;
    ColWidthDatabase1 := '';
    for i := 1 to sgTable.ColCount - 1 do
    begin
      ColWidthDatabase1 := ColWidthDatabase1 +
        IntToStr(sgTable.ColWidths[i]) + ',';
    end;
  end;
  if ((blFileSaved = False) or (blTableSaved = False)) then
  begin
    if stFileName <> '' then
    try
      try
        myList := TStringList.Create;
        if blFileSaved = False then
        begin
          myList.Text := dbText.Text;
          myList.SaveToFile(stFileName);
          blFileSaved := True;
        end;
        if blTableSaved = False then
        begin
          sgTable.SaveToCSVFile(ExtractFileNameWithoutExt(stFileName) + '.csv',
            #9, False);
          blTableSaved := True;
        end;
      finally
        myList.Free;
      end;
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
  i: Integer;
  slColWidth: TStringList;
begin
  if blDisableFormatting = False then
  begin
    Application.processMessages;
  end;
  if ((stFileName = LastDatabase1) and (LastPosDatabase1 > -1) and
    (LastPosDatabase1 < Length(dbText.Text))) then
  begin
    dbText.SelStart := LastPosDatabase1;
    sgTitles.TopRow := TopIndexDatabase1;
    sgTable.Col := ColDatabase1;
    sgTable.Row := RowDatabase1;
    if ColWidthDatabase1 <> '' then
    try
      try
        slColWidth := TStringList.Create;
        slColWidth.CommaText := ColWidthDatabase1;
        for i := 1 to sgTable.ColCount - 1 do
        begin
          sgTable.ColWidths[i] := StrToInt(slColWidth[i - 1]);
        end;
      finally
        slColWidth.Free;
      end;
    except
    end
    else
    for i := 1 to sgTable.ColCount - 1 do
    begin
      sgTable.ColWidths[i] := 280;
    end;
  end
  else
  if ((stFileName = LastDatabase2) and (LastPosDatabase2 > -1) and
    (LastPosDatabase2 < Length(dbText.Text))) then
  begin
    dbText.SelStart := LastPosDatabase2;
    sgTitles.TopRow := TopIndexDatabase2;
    sgTable.Col := ColDatabase2;
    sgTable.Row := RowDatabase2;
    if ColWidthDatabase2 <> '' then
    try
      try
        slColWidth := TStringList.Create;
        slColWidth.CommaText := ColWidthDatabase2;
        for i := 1 to sgTable.ColCount - 1 do
        begin
          sgTable.ColWidths[i] := StrToInt(slColWidth[i - 1]);
        end;
      finally
        slColWidth.Free;
      end
    except
    end
    else
    for i := 1 to sgTable.ColCount - 1 do
    begin
      sgTable.ColWidths[i] := 280;
    end;
  end
  else
  if ((stFileName = LastDatabase3) and (LastPosDatabase3 > -1) and
    (LastPosDatabase3 < Length(dbText.Text))) then
  begin
    dbText.SelStart := LastPosDatabase3;
    sgTitles.TopRow := TopIndexDatabase3;
    sgTable.Col := ColDatabase3;
    sgTable.Row := RowDatabase3;
    if ColWidthDatabase3 <> '' then
    try
      try
        slColWidth := TStringList.Create;
        slColWidth.CommaText := ColWidthDatabase3;
        for i := 1 to sgTable.ColCount - 1 do
        begin
          sgTable.ColWidths[i] := StrToInt(slColWidth[i - 1]);
        end;
      finally
        slColWidth.Free;
      end;
    except
    end
    else
    for i := 1 to sgTable.ColCount - 1 do
    begin
      sgTable.ColWidths[i] := 280;
    end;
  end
  else
  if ((stFileName = LastDatabase4) and (LastPosDatabase4 > -1) and
    (LastPosDatabase4 < Length(dbText.Text))) then
  begin
    dbText.SelStart := LastPosDatabase4;
    sgTitles.TopRow := TopIndexDatabase4;
    sgTable.Col := ColDatabase4;
    sgTable.Row := RowDatabase4;
    if ColWidthDatabase4 <> '' then
    try
      try
        slColWidth := TStringList.Create;
        slColWidth.CommaText := ColWidthDatabase4;
        for i := 1 to sgTable.ColCount - 1 do
        begin
          sgTable.ColWidths[i] := StrToInt(slColWidth[i - 1]);
        end;
      finally
        slColWidth.Free;
      end;
    except
    end
    else
    for i := 1 to sgTable.ColCount - 1 do
    begin
      sgTable.ColWidths[i] := 280;
    end;
  end
  else
  begin
    dbText.SelStart := 0;
    for i := 1 to sgTable.ColCount - 1 do
    begin
      sgTable.ColWidths[i] := 280;
    end;
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
  blTextOnChange := True;
  iPos := dbText.SelStart;
  stText := WideString(dbText.Text);
  iStart := iPos - 1;
  while ((iStart >= 0) and (iStart < Length(stText) - 3)) do
  begin
    if (((stText[iStart] = LineEnding) or (iStart = 0)) and
      (stText[iStart + 1] = LineEnding)) then
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
  blTextOnChange := False;
  Application.ProcessMessages;
  FormatListTitleTodo;
end;

procedure TfmMain.ShowCurrentTitleTodo;
var
  i: integer;
begin
  if ((pnTitTodo.Visible = True) and (sgTitles.RowCount > 0)) then
  begin
    for i := 0 to sgTitles.RowCount - 1 do
    begin
      if sgTitles.Cells[1, i] = ' ' then
      begin
        if i > 5 then
        begin
          sgTitles.TopRow := i - 5;
        end
        else
        begin
          sgTitles.TopRow := 0;
        end;
        Break;
      end;
    end;
  end;
end;

procedure TfmMain.CutZone;
var
  stText: String;
  iTop, iBottom, iPos, iLevel: Integer;
begin
  iLevel := GetHeaderLevel(dbText.Lines[dbText.CaretPos.Y]);
  if iLevel = 7 then
  begin
    MessageDlg(msg017, mtInformation, [mbOK], 0);
  end
  else
  begin
    if dbText.CaretPos.Y > dbText.Lines.Count - 2 then
    begin
      Exit;
    end;
    if MessageDlg(msg018, mtConfirmation, [mbOK, mbCancel], 0) = mrCancel then
    begin
      Exit;
    end;
    iPos := dbText.SelStart;
    stText := dbText.Lines[dbText.CaretPos.Y] + LineEnding;
    iTop := dbText.CaretPos.Y;
    for iBottom := dbText.CaretPos.Y + 1 to dbText.Lines.Count - 1 do
    begin
      if GetHeaderLevel(dbText.Lines[iBottom]) > iLevel then
      begin
        stText := stText + dbText.Lines[iBottom] + LineEnding;
      end
      else
      begin
        Break;
      end;
    end;
    while iTop < iBottom do
    begin
      dbText.Lines.Delete(iTop);
      Dec(iBottom);
    end;
    Clipboard.AsText := stText;
    dbText.SelStart := iPos;
  end;
end;

function TfmMain.GetHeaderLevel(stHeader: String): Integer;
begin
  Result := 7;
  if Copy(stHeader, 1, 2) = '# ' then Result := 1
  else
  if Copy(stHeader, 1, 3) = '## ' then result := 2
  else
  if Copy(stHeader, 1, 4) = '### ' then Result := 3
  else
  if Copy(stHeader, 1, 5) = '#### ' then result := 4
  else
  if Copy(stHeader, 1, 6) = '##### ' then Result := 5
  else
  if Copy(stHeader, 1, 7) = '###### ' then result := 6;
end;

procedure TfmMain.FindInGrid;
var
  i: integer;
begin
  if ((pnGrid.Height > 1) and (edFindGrid.Text <> '') and
    (sgTable.Row < sgTable.RowCount - 1)) then
  begin
    if sgTable.Col = 1 then
    begin
      for i := sgTable.Row + 1 to sgTable.RowCount - 1 do
      begin
        if UTF8CocoaPos(UTF8UpperString(edFindGrid.Text),
          UTF8UpperString(sgTable.Cells[sgTable.Col, i]), 1) > 0 then
        begin
          sgTable.Row := i;
          sgTable.SetFocus;
          Break;
        end;
      end;
    end
    else
    for i := sgTable.Row + 1 to sgTable.RowCount - 1 do
    begin
      if sgTable.Cells[1, i] <> '' then
      begin
        Break;
      end
      else
      if UTF8CocoaPos(UTF8UpperString(edFindGrid.Text),
        UTF8UpperString(sgTable.Cells[sgTable.Col, i]), 1) > 0 then
      begin
        sgTable.Row := i;
        sgTable.SetFocus;
        Break;
      end;
    end;
  end;
end;

procedure TfmMain.CreateYAML;
begin
  blTextOnChange := True;
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
  blTextOnChange := False;
  FormatListTitleTodo;
end;

procedure TfmMain.DeactForm(stFileName: String);
begin
  if FileSizeUtf8(stFileName) > iMaxSize then
  begin
    if miEditDisableForm.Checked = False then
    begin
      miEditDisableFormClick(nil);
    end;
  end
  else
  begin
    if miEditDisableForm.Checked = True then
    begin
      miEditDisableFormClick(nil);
    end;
  end;
end;

procedure TfmMain.CreateBackup;
begin
  if ((blFileMod = True) and (stFileName <> '')) then
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
  if ((blTableMod = True) and (stFileName <> '')) then
  begin
    if FileExistsUTF8(ExtractFileNameWithoutExt(stFileName) + '.csv') = True then
    try
      if FileExistsUTF8(ExtractFileNameWithoutExt(stFileName) +
        '.cvs.bak') = True then
      begin
        DeleteFileUTF8(ExtractFileNameWithoutExt(stFileName) + '.cvs.bak');
      end;
      CopyFile(ExtractFileNameWithoutExt(stFileName) + '.csv',
        ExtractFileNameWithoutExt(stFileName) + '.cvs.bak');
    except
      MessageDlg(msg009, mtWarning, [mbOK], 0);
    end;
  end;
end;

procedure TfmMain.SetTable;
var
  i: integer;
begin
  sgTable.Cells[1, 0] := lb007;
  for i := 2 to sgTable.ColCount - 1 do
  begin
    if i < 28 then
      sgTable.Cells[i, 0] := Chr(i + 63) + '1'
    else if i < 54 then
      sgTable.Cells[i, 0] := Chr(i + 37) + '2'
    else if i < 80 then
      sgTable.Cells[i, 0] := Chr(i + 11) + '3'
    else if i < 106 then
      sgTable.Cells[i, 0] := Chr(i - 15) + '4';
  end;
  sgTable.ColWidths[0] := 50;
end;

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

// *******************************************************
// ************ Procedures of font management ************
// *******************************************************

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
