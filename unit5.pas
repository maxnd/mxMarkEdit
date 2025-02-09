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

unit Unit5;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, LazUTF8, translate, CocoaUtils, Clipbrd, IniFiles, LazFileUtils;

type

  { TfmWords }

  TfmWords = class(TForm)
    bnCopy: TButton;
    bnRefresh: TButton;
    bnRemove: TButton;
    lbResults: TLabel;
    lbSkip: TLabel;
    pnWords: TPanel;
    rgSkip: TRadioGroup;
    sgWords: TStringGrid;
    bnOK: TButton;
    meSkip: TMemo;
    procedure bnCopyClick(Sender: TObject);
    procedure bnOKClick(Sender: TObject);
    procedure bnRefreshClick(Sender: TObject);
    procedure bnRemoveClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure meSkipExit(Sender: TObject);
    procedure rgSkipClick(Sender: TObject);
  private
    procedure CreateWordsList;
    procedure SortSkipWords;

  public

  end;

var
  fmWords: TfmWords;

resourcestring

  msgsk001 = 'Words used:';
  msgsk002 = 'Sort by words';
  msgsk003 = 'Sort by recurrences';

implementation

uses Unit1;

{$R *.lfm}

{ TfmTasks }

procedure TfmWords.FormCreate(Sender: TObject);
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
      meSkip.Text := MyIni.ReadString('mxmarkedit', 'skipwords', '');
    finally
      MyIni.Free;
    end;
  end;
  sgWords.FocusRectVisible := False;
  rgSkip.Items[0] := msgsk002;
  rgSkip.Items[1] := msgsk003;
  rgSkip.ItemIndex := 1;
  if IsAppDark = True then
  begin
    sgWords.SelectedColor := $005E5E5E;
  end
  else
  begin
    sgWords.SelectedColor := $00EBEBEB;
    meSkip.Color := $00ECECEB;
  end;
end;

procedure TfmWords.bnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TfmWords.bnRefreshClick(Sender: TObject);
begin
  CreateWordsList;
end;

procedure TfmWords.bnRemoveClick(Sender: TObject);
begin
  if sgWords.RowCount > 1 then
  begin
    if meSkip.Text = '' then
    begin
      meSkip.Text := sgWords.Cells[0, sgWords.Row];
    end
    else
    begin
      meSkip.Text := sgWords.Cells[0, sgWords.Row] + ', ' + meSkip.Text;
    end;
    SortSkipWords;
    sgWords.DeleteRow(sgWords.Row);
  end;
end;

procedure TfmWords.FormActivate(Sender: TObject);
begin
  rgSkip.ItemIndex := 1;
  CreateWordsList;
end;

procedure TfmWords.FormClose(Sender: TObject; var CloseAction: TCloseAction);
var
  MyIni: TIniFile;
begin
  try
    MyIni := TIniFile.Create(myHomeDir + myConfigFile);
    MyIni.WriteString('mxmarkedit', 'skipwords', meSkip.Text);
  finally
    MyIni.Free;
  end;
end;

procedure TfmWords.bnCopyClick(Sender: TObject);
var
  stClip: String = '';
  i: Integer;
begin
  if sgWords.RowCount > 1 then
  begin
    stClip := sgWords.Columns[0].Title.Caption + #9 +
      sgWords.Columns[1].Title.Caption + LineEnding;
    for i := 1 to sgWords.RowCount - 1 do
    begin
      stClip := stClip + sgWords.Cells[0, i] + #9 +
        sgWords.Cells[1, i] + LineEnding;
    end;
  end;
  Clipboard.AsText := stClip;
end;

procedure TfmWords.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 27 then
  begin
    key := 0;
    Close
  end;
end;

procedure TfmWords.meSkipExit(Sender: TObject);
begin
  SortSkipWords
end;

procedure TfmWords.rgSkipClick(Sender: TObject);
var
  i: Integer;
begin
  if rgSkip.ItemIndex = 0 then
  begin
    sgWords.SortOrder := soAscending;
    sgWords.SortColRow(True, 0);
    sgWords.Row := 1;
  end
  else
  if rgSkip.ItemIndex = 1 then
  begin
    for i := 1 to sgWords.RowCount - 1 do
    begin
      sgWords.Cells[1, i] := FormatFloat('0000000',
        StrToInt(sgWords.Cells[1, i]));
    end;
    sgWords.SortOrder := soDescending;
    sgWords.SortColRow(True, 1);
    for i := 1 to sgWords.RowCount - 1 do
    begin
      sgWords.Cells[1, i] := FormatFloat('0',
      StrToInt(sgWords.Cells[1, i]));
    end;
  end;
end;

procedure TfmWords.CreateWordsList;
var
  iPosWord, i, n, iLen: integer;
  slListWords, slListRec, slListSkip: TStringList;
  stItem, stText: WideString;
  stSkip, stWord: String;
  stSeparators: String = '.,;:+=&^-–(){}[]/\''"’‘”“«»?¿!¡ ' + #13 + #9 + LineEnding;
  stNumbers: String = '1234567890';
begin
  try
    Screen.Cursor := crHourGlass;
    slListWords := TStringList.Create;
    slListRec := TStringList.Create;
    slListSkip := TStringList.Create;
    stText := WideString(fmMain.dbText.Text + ' ');
    iLen := Length(stText);
    stItem := '';
    if fmMain.dbText.Lines[0] = '---' then
    begin
      i := Pos('---', stText, 4) + 3;
    end
    else
    begin
      i := 1;
    end;
    if meSkip.Text <> '' then
    begin
      stSkip := meSkip.Text;
      slListSkip.Sorted := True;
      slListSkip.Duplicates := dupIgnore;
      stWord := '';
      for n := 1 to Length(stSkip) + 1 do
      begin
        if ((stSkip[n] = ',') or (n = Length(stSkip) + 1)) then
        begin
          stWord := Trim(stWord);
          if stWord <> '' then
          begin
            slListSkip.Add(stWord);
          end;
          stWord := '';
        end
        else
        begin
          stWord := stWord + stSkip[n];
        end;
      end;
    end;
    while i <= iLen do
    begin
      if Pos(stText[i], stSeparators) > 0 then
      begin
        if slListSkip.IndexOf(UTF8LowerCase(stItem)) > -1 then
        begin
          stItem := '';
          Continue;
        end
        else
        begin
          if stItem <> '' then
          begin
            iPosWord := slListWords.IndexOf(UTF8LowerCase(stItem));
            if iPosWord > -1 then
            begin
              slListRec[iPosWord] := IntToStr(StrToInt(slListRec[iPosWord]) + 1);
            end
            else
            begin
              slListWords.Add(UTF8LowerCase(stItem));
              slListRec.Add('1');
             end;
            stItem := '';
          end;
        end;
      end
      else
      begin
        if ((stText[i] <> '*') and (stText[i] <> '#') and (stText[i] <> '_')
          and (stText[i] <> '`') and (Pos(stText[i], stNumbers) < 1)) then
        begin
          stItem := stItem + stText[i];
        end;
      end;
      Inc(i);
    end;
    if slListWords.Count > 0 then
    begin
      sgWords.RowCount := slListWords.Count + 1;
      for i := 0 to slListWords.Count - 1 do
      begin
        sgWords.Cells[0, i + 1] := slListWords[i];
        sgWords.Cells[1, i + 1] := FormatFloat('0000000',
          StrToInt(slListRec[i]));
      end;
      sgWords.SortOrder := soDescending;
      sgWords.SortColRow(True, 1);
      for i := 1 to sgWords.RowCount - 1 do
      begin
        sgWords.Cells[1, i] := FormatFloat('0',
          StrToInt(sgWords.Cells[1, i]));
      end;
      sgWords.Row := 1;
    end
    else
    begin
      sgWords.RowCount := 1;
    end;
    lbResults.Caption := msgsk001 + ' ' + IntToStr(sgWords.RowCount - 1);
    sgWords.SetFocus;
    Screen.Cursor := crDefault;
  finally
    Screen.Cursor := crDefault;
    slListWords.Free;
    slListRec.Free;
    slListSkip.Free;
  end;
end;

procedure TfmWords.SortSkipWords;
var
  slListSkip: TStringList;
  stSkip, stWord: String;
  i: Integer;
begin
  if meSkip.Text = '' then
  begin
    Exit;
  end;
  try
    slListSkip := TStringList.Create;
    stSkip := meSkip.Text;
    slListSkip.Sorted := True;
    slListSkip.Duplicates := dupIgnore;
    stWord := '';
    for i := 1 to Length(stSkip) + 1 do
    begin
      if ((stSkip[i] = ',') or (i = Length(stSkip) + 1)) then
      begin
        stWord := Trim(stWord);
        if stWord <> '' then
        begin
          slListSkip.Add(stWord);
        end;
        stWord := '';
      end
      else
      begin
        stWord := stWord + stSkip[i];
      end;
    end;
    if slListSkip.Count > 0 then
    begin
      stSkip := '';
      for i := 0 to slListSkip.Count - 1 do
      begin
        stSkip := stSkip + slListSkip[i];
        if i < slListSkip.Count - 1 then
        begin
          stSkip := stSkip + ', ';
        end;
      end;
      meSkip.Text := stSkip;
    end;
  finally
    slListSkip.Free;
  end;
end;

end.

