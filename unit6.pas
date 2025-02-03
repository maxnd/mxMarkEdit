// ***********************************************************************
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

unit Unit6;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, LazUTF8, translate, Types, CocoaAll, CocoaTextEdits, CocoaUtils,
  Clipbrd, FileUtil, IniFiles, LazFileUtils;

type

  { TfmFiles }

  TfmFiles = class(TForm)
    bnFind: TButton;
    bnFolder: TButton;
    edFolder: TEdit;
    edFind: TEdit;
    lbCount: TLabel;
    lbFolder: TLabel;
    lbFind: TLabel;
    pnTasks: TPanel;
    sdDirOpen: TSelectDirectoryDialog;
    sgFiles: TStringGrid;
    bnOK: TButton;
    procedure bnFolderClick(Sender: TObject);
    procedure bnFindClick(Sender: TObject);
    procedure bnOKClick(Sender: TObject);
    procedure edFindKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgFilesDblClick(Sender: TObject);
    procedure sgFilesKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
  private
    procedure CreateFilesList;
    procedure OpenFile;

  public

  end;

var
  fmFiles: TfmFiles;

resourcestring

  msgfl001 = 'Error in searching in the files.';
  msgfl002 = 'The search has stopped to 5,000 occurrences; try to refine it.';

implementation

uses Unit1;

{$R *.lfm}

{ TfmFiles }

procedure TfmFiles.FormCreate(Sender: TObject);
var
  MyIni: TIniFile;
begin
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
      edFolder.Text := MyIni.ReadString('mxmarkedit', 'searchdir', '');
    finally
      MyIni.Free;
    end;
  end;
  sgFiles.FocusRectVisible := False;
  if IsAppDark = True then
  begin
    sgFiles.SelectedColor := $005E5E5E;
  end
  else
  begin
    sgFiles.SelectedColor := $00EBEBEB;
  end;
end;


procedure TfmFiles.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  MyIni: TIniFile;
begin
  try
    MyIni := TIniFile.Create(myHomeDir + myConfigFile);
    MyIni.WriteString('mxmarkedit', 'searchdir', edFolder.Text);
  finally
    MyIni.Free;
  end;
end;

procedure TfmFiles.bnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TfmFiles.edFindKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // On TEdit, the keyup e keypress events to not manage Return
  if key = 13 then
  begin
    CreateFilesList;
    key := 0;
  end;
end;

procedure TfmFiles.FormActivate(Sender: TObject);
begin
  if edFind.Text = '' then
  begin
    edFind.SetFocus;
  end
  else
  begin
    sgFiles.SetFocus;
  end;
end;

procedure TfmFiles.bnFindClick(Sender: TObject);
begin
  CreateFilesList;
end;

procedure TfmFiles.bnFolderClick(Sender: TObject);
begin
  if sdDirOpen.Execute = True then
  begin
    edFolder.Text := sdDirOpen.FileName;
  end;
end;

procedure TfmFiles.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 27 then
  begin
    Close;
    key := 0;
  end
  else
  if ((Key = Ord('F')) and (Shift = [ssMeta])) then
  begin
    key := 0;
    edFind.SetFocus;
  end;
end;

procedure TfmFiles.sgFilesDblClick(Sender: TObject);
begin
  OpenFile;
end;

procedure TfmFiles.sgFilesKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 13 then
  begin
    OpenFile;
    key := 0;
  end;
end;

procedure TfmFiles.CreateFilesList;
var
  slMarkdownFiles, slContent: TStringList;
  i, iPos, iStart, iRec: Integer;
  stContent, stContentLower, stRow: WideString;
  stFind, stDotIni, stDotEnd: String;
  rng: NSRange;
begin
  sgFiles.RowCount := 1;
  lbCount.Caption := '0';
  if ((edFolder.Text <> '') and (edFind.Text <> '')) then
  begin
    //No need to create slMarkdownFiles; the function does that
    try
      stFind := UTF8StringReplace(edFind.Text, '\n', LineEnding, [rfReplaceAll]);
      stFind := UTF8StringReplace(stFind, '\t', #9, [rfReplaceAll]);
      stFind := UTF8StringReplace(stFind, '\r', #13, [rfReplaceAll]);
      slContent := TStringList.Create;
      slMarkdownFiles := FindAllFiles(edFolder.Text, '*.md;*.csv', True);
      if slMarkdownFiles.Count > 0 then
      try
        Screen.Cursor := crHourGlass;
        for i := 0 to slMarkdownFiles.Count - 1 do
        begin
          slContent.LoadFromFile(slMarkdownFiles[i]);
          stContent := slContent.Text;
          stContentLower := WideLowerCase(slContent.Text);
          iPos := 1;
          iStart := 1;
          iRec := 0;
          while iPos > 0 do
          begin
            iPos := fmMain.UTF8CocoaPos(UTF8LowerCase(stFind),
              stContentLower, iStart);
            if iPos > 0 then
            begin
              Inc(iRec);
              sgFiles.RowCount := sgFiles.RowCount + 1;
              sgFiles.Cells[0, sgFiles.RowCount - 1] :=
                IntToStr(iPos);
              sgFiles.Cells[1, sgFiles.RowCount - 1] :=
                ExtractFileDir(slMarkdownFiles[i]);
              sgFiles.Cells[2, sgFiles.RowCount - 1] :=
                ExtractFileName(slMarkdownFiles[i]);
              if iPos > 19 then
              begin
                rng.location := iPos - 20;
                stDotIni := '...'
              end
              else
              begin
                rng.location := 0;
                stDotIni := ''
              end;
              rng.length := UTF8Length(stFind) + 40;
              stDotEnd := '...';
              if Length(stContent) < rng.location + rng.length then
              begin
                rng.length := Length(stContent) - rng.location;
                stDotEnd := '';
              end;
              stRow := stDotIni + NSStringToString(StrToNSString(stContent,
                True).substringWithRange(rng)) + stDotEnd;
              stRow := StringReplace(stRow, LineEnding, ' ', [rfReplaceAll]);
              stRow := StringReplace(stRow, #9, ' ', [rfReplaceAll]);
              sgFiles.Cells[3, sgFiles.RowCount - 1] := stRow;
              iStart := iPos + 1;
              lbCount.Caption := FormatFloat('#,0', sgFiles.RowCount - 1);
              Application.ProcessMessages;
              if iRec > 100 then
              begin
                Break;
              end;
              if sgFiles.RowCount > 5000 then
              begin
                MessageDlg(msgfl002, mtWarning, [mbOK], 0);
                Exit;
              end;
            end;
          end;
          Application.ProcessMessages;
        end;
        sgFiles.SetFocus;
      except
        MessageDlg(msgfl001, mtWarning, [mbOK], 0);
      end;
    finally
      slMarkdownFiles.Free;
      slContent.Free;
      Screen.Cursor := crDefault;
    end;
  end;
end;

procedure TfmFiles.OpenFile;
var
  stFind, stOpenFileName: String;
  blTable: boolean = False;
  x, y, iPos, iLoc: Integer;
begin
  stOpenFileName := sgFiles.Cells[1, sgFiles.Row] + '/' +
    sgFiles.Cells[2, sgFiles.Row];
  if FilenameExtIs(stOpenFileName, '.csv') then
  begin
    stOpenFileName := ExtractFileNameWithoutExt(stOpenFileName) + '.md';
    blTable := True;
  end;
  if ((sgFiles.RowCount > 1) and (FileExistsUTF8(stOpenFileName))) then
  try
    fmMain.sgTable.RowCount := 1;
    fmMain.sgTable.RowCount := csTableRowCount;
    stFileName := stOpenFileName;
    fmMain.dbText.Lines.LoadFromFile(stFileName);
    if FileExistsUTF8(ExtractFileNameWithoutExt(stFileName) + '.csv') then
    begin
      fmMain.sgTable.LoadFromCSVFile(ExtractFileNameWithoutExt(stFileName) + '.csv',
        #9, False);
      fmMain.sgTable.RowCount := csTableRowCount;
      stGridLoaded := stTableLoaded;
    end
    else
    begin
      stGridLoaded := '';
    end;
    iBookmarkPos := 0;
    fmMain.LabelFileNameChars;
    TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      checkTextInDocument(nil);
    TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      undoManager.removeAllActions;
    fmMain.UpdateLastFile;
    fmMain.ShowCurrentTitleTodo;
    blFileMod := False;
    blTableMod := False;
    if blTable = False then
    begin
      fmMain.dbText.SelStart := StrToInt(sgFiles.Cells[0, sgFiles.Row]) - 1;
      stFind := UTF8StringReplace(edFind.Text, '\n', LineEnding, [rfReplaceAll]);
      stFind := UTF8StringReplace(stFind, '\t', #9, [rfReplaceAll]);
      stFind := UTF8StringReplace(stFind, '\r', #13, [rfReplaceAll]);
      fmMain.dbText.SelLength := UTF8Length(stFind);
      if fmMain.pnGrid.Height > 1 then
      begin
        fmMain.pnGrid.Height := 1;
      end;
    end
    else
    begin
      iLoc := StrToInt(sgFiles.Cells[0, sgFiles.Row]) - 1;
      iPos := 0;
      for y := 1 to fmMain.sgTable.RowCount - 1 do
      begin
        for x := 1 to fmMain.sgTable.ColCount - 1 do
        begin
          if fmMain.sgTable.Cells[x, y] <> '' then
          begin
            iPos := iPos + UTF8Length(fmMain.sgTable.Cells[x, y]);
          end;
          Inc(iPos);
          if iPos > iLoc then
          begin
            if fmMain.pnGrid.Height = 1 then
            begin
              fmMain.pnGrid.Height := 400;
            end;
            fmMain.sgTable.Col := x;
            fmMain.sgTable.Row := y;
            fmMain.sgTable.TopRow := y;
            fmMain.sgTable.LeftCol := x;
            Break;
          end;
        end;
        if iPos > iLoc then
        begin
          Break;
        end;
        Inc(iPos);
      end;
    end;
    Close;
  except
    MessageDlg(msg004, mtWarning, [mbOK], 0);
  end;
end;

end.

