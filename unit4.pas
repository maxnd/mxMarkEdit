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

unit Unit4;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, LazUTF8, translate, Types, CocoaAll, CocoaTextEdits, CocoaUtils,
  Clipbrd;

type

  { TfmTasks }

  TfmTasks = class(TForm)
    bnCopy: TButton;
    bnToggle: TButton;
    cbHide: TCheckBox;
    lbTot: TLabel;
    pnTasks: TPanel;
    sgTasks: TStringGrid;
    bnOK: TButton;
    procedure bnCopyClick(Sender: TObject);
    procedure bnOKClick(Sender: TObject);
    procedure bnToggleClick(Sender: TObject);
    procedure cbHideClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure sgTasksDblClick(Sender: TObject);
    procedure sgTasksDrawCell(Sender: TObject; aCol, aRow: integer;
      aRect: TRect; aState: TGridDrawState);
  private
    procedure CreateList;
  public

  end;

var
  fmTasks: TfmTasks;
  fs: TFormatSettings;
  clProperRed: TColor = clRed;

resourcestring

  tsk001 = 'day';
  tsk002 = 'days';
  tsk003 = 'Todo items:';

implementation

uses Unit1;

  {$R *.lfm}

  { TfmTasks }

procedure TfmTasks.FormCreate(Sender: TObject);
begin
  sgTasks.FocusRectVisible := False;
  fs := DefaultFormatSettings;
  fs.DateSeparator := '-';
  fs.ShortDateFormat := 'yyyy/mm/dd';
  if IsAppDark = True then
  begin
    clProperRed := $005662FF;
    sgTasks.SelectedColor := $005E5E5E;
  end
  else
  begin
    clProperRed := clRed;
    sgTasks.SelectedColor := $00EBEBEB;
  end;
end;

procedure TfmTasks.FormActivate(Sender: TObject);
begin
  CreateList;
end;

procedure TfmTasks.bnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TfmTasks.bnToggleClick(Sender: TObject);
var
  rng: NSRange;
begin
  if sgTasks.RowCount > 1 then
  begin
    if sgTasks.Cells[1, sgTasks.Row] = '0' then
    begin
      sgTasks.Cells[1, sgTasks.Row] := '1';
      fmMain.dbText.SelStart := StrToInt(sgTasks.Cells[0, sgTasks.Row]);
      Application.ProcessMessages;
      rng := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        selectedRange;
      rng.location := rng.location + 3;
      rng.length := 1;
      TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        insertText_replacementRange(NSStringUtf8('X'), rng);
      fmMain.FormatListTitleTodo;
      if cbHide.Checked = True then
      begin
        CreateList;
      end;
    end
    else
    begin
      sgTasks.Cells[1, sgTasks.Row] := '0';
      Application.ProcessMessages;
      rng := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        selectedRange;
      rng.location := rng.location + 3;
      rng.length := 1;
      TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        insertText_replacementRange(NSStringUtf8(' '), rng);
      fmMain.FormatListTitleTodo;
    end;
    sgTasks.Refresh;
  end;
end;

procedure TfmTasks.bnCopyClick(Sender: TObject);
var
  stClip: string = '';
  i: integer;
begin
  if sgTasks.RowCount > 1 then
  begin
    stClip := sgTasks.Columns[1].Title.Caption + #9 +
      sgTasks.Columns[2].Title.Caption + #9 +
      sgTasks.Columns[3].Title.Caption + #9 +
      sgTasks.Columns[4].Title.Caption + LineEnding;
    for i := 1 to sgTasks.RowCount - 1 do
    begin
      if sgTasks.Cells[1, i] = '1' then
      begin
        stClip := stClip + '●'#9;
      end
      else
      begin
        stClip := stClip + '○'#9;
      end;
      if sgTasks.Cells[2, i] <> '' then
      begin
        stClip := stClip + UTF8Copy(sgTasks.Cells[2, i], 1, 10) + #9;
      end;
      stClip := stClip + sgTasks.Cells[3, i] + #9 +
        sgTasks.Cells[4, i] + LineEnding;
    end;
  end;
  Clipboard.AsText := stClip;
end;

procedure TfmTasks.cbHideClick(Sender: TObject);
begin
  CreateList;
end;

procedure TfmTasks.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
var
  i: integer;
  rng: NSRange;
begin
  if key = 13 then
  begin
    if sgTasks.RowCount > 1 then
    begin
      if TryStrToInt(sgTasks.Cells[0, sgTasks.Row], i) = True then
      begin
        fmMain.dbText.SelStart := i;
        Application.ProcessMessages;
        rng := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
          textStorage.string_.paragraphRangeForRange(TCocoaTextView(
          NSScrollView(fmMain.dbText.Handle).documentView).selectedRange);
        TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
          showFindIndicatorForRange(rng);
      end;
    end;
    Close;
  end
  else
  if key = 27 then
  begin
    Close;
  end
  else
  if ((key = Ord('T')) and (Shift = [ssMeta])) then
  begin
    bnToggleClick(nil);
    key := 0;
  end
  else
  if ((key = Ord('C')) and (Shift = [ssMeta])) then
  begin
    bnCopyClick(nil);
    key := 0;
  end
  else
  if ((key = Ord('H')) and (Shift = [ssMeta, ssAlt])) then
  begin
    cbHide.Checked := not cbHide.Checked;
    if cbHide.Checked = True then
    begin
      CreateList;
    end;
    key := 0;
  end;
end;

procedure TfmTasks.sgTasksDblClick(Sender: TObject);
var
  i: integer;
  rng: NSRange;
begin
  if sgTasks.RowCount > 1 then
  begin
    if TryStrToInt(sgTasks.Cells[0, sgTasks.Row], i) = True then
    begin
      fmMain.dbText.SelStart := i;
      Application.ProcessMessages;
      rng := TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        textStorage.string_.paragraphRangeForRange(TCocoaTextView(
        NSScrollView(fmMain.dbText.Handle).documentView).selectedRange);
      TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
        showFindIndicatorForRange(rng);
    end;
    Close;
  end;
end;

procedure TfmTasks.sgTasksDrawCell(Sender: TObject; aCol, aRow: integer;
  aRect: TRect; aState: TGridDrawState);
var
  myDate: TDate;
begin
  if aCol = 1 then
  begin
    if sgTasks.Cells[1, aRow] = '1' then
    begin
      sgTasks.Canvas.TextOut(aRect.Left + 3, aRect.Top + 3, '  ●    ');
    end
    else
    if sgTasks.Cells[1, aRow] = '0' then
    begin
      sgTasks.Canvas.TextOut(aRect.Left + 3, aRect.Top + 3, '  ○    ');
    end;
  end
  else
  if aCol = 2 then
  begin
    if TryStrToDate(UTF8Copy(sgTasks.Cells[2, aRow], 1,
      Pos(' ', sgTasks.Cells[2, aRow])), myDate, fs) = True then
    begin
      if ((myDate <= Date) and (sgTasks.Cells[1, aRow] = '0')) then
      begin
        sgTasks.Canvas.Font.Color := clProperRed;
      end
      else
      begin
        sgTasks.Canvas.Font.Color := clDefault;
      end;
      sgTasks.Canvas.TextOut(aRect.Left + 3, aRect.Top + 3,
        sgTasks.Cells[aCol, aRow]);
    end;
  end;
end;

procedure TfmTasks.CreateList;
var
  i, iLength: integer;
  dtDeadline: TDate;
  stDays, stHeading: string;
  slTodo: TStringList;
begin
  sgTasks.RowCount := 1;
  iLength := 0;
  try
    Cursor := crHourGlass;
    slTodo := TStringList.Create;
    slTodo.AddStrings(fmMain.dbText.Lines);
    for i := 0 to slTodo.Count - 1 do
    begin
      if UTF8Copy(slTodo[i], 1, 1) = '#' then
      begin
        if fmMain.GetHeaderLevel(slTodo[i]) < 7 then
        begin
          stHeading := UTF8Copy(slTodo[i], UTF8Pos(' ', slTodo[i]) + 1,
            UTF8Length(slTodo[i]));
        end;
      end
      else
      if UTF8Copy(slTodo[i], 1, 6) = '- [ ] ' then
      begin
        sgTasks.RowCount := sgTasks.RowCount + 1;
        sgTasks.Cells[0, sgTasks.RowCount - 1] := IntToStr(iLength);
        if TryStrToDate(UTF8Copy(slTodo[i], 7, 10), dtDeadline, fs) = True then
        begin
          stDays := FloatToStr(dtDeadline - Date);
          if ((stDays = '1') or (stDays = '-1')) then
          begin
            stDays := stDays + ' ' + tsk001;
          end
          else
          begin
            stDays := stDays + ' ' + tsk002;
          end;
          sgTasks.Cells[2, sgTasks.RowCount - 1] :=
            UTF8Copy(slTodo[i], 7, 10) + ' (' + stDays + ')';
          sgTasks.Cells[3, sgTasks.RowCount - 1] := stHeading;
          sgTasks.Cells[4, sgTasks.RowCount - 1] :=
            UTF8Copy(slTodo[i], 20, UTF8Length(slTodo[i]));
        end
        else
        begin
          sgTasks.Cells[4, sgTasks.RowCount - 1] :=
            UTF8Copy(slTodo[i], 7, UTF8Length(slTodo[i]));
        end;
        sgTasks.Cells[1, sgTasks.RowCount - 1] := '0';
      end
      else
      if (((UTF8Copy(slTodo[i], 1, 6) = '- [X] ') or
        (UTF8Copy(slTodo[i], 1, 6) = '- [x] ')) and (cbHide.Checked = False)) then
      begin
        sgTasks.RowCount := sgTasks.RowCount + 1;
        sgTasks.Cells[0, sgTasks.RowCount - 1] := IntToStr(iLength);
        if TryStrToDate(UTF8Copy(slTodo[i], 7, 10), dtDeadline, fs) = True then
        begin
          stDays := FloatToStr(dtDeadline - Date);
          if ((stDays = '1') or (stDays = '-1')) then
          begin
            stDays := stDays + ' ' + tsk001;
          end
          else
          begin
            stDays := stDays + ' ' + tsk002;
          end;
          sgTasks.Cells[2, sgTasks.RowCount - 1] :=
            UTF8Copy(fmMain.dbText.Lines[i], 7, 10) + ' (' + stDays + ')';
          sgTasks.Cells[3, sgTasks.RowCount - 1] := stHeading;
          sgTasks.Cells[4, sgTasks.RowCount - 1] :=
            UTF8Copy(slTodo[i], 20, UTF8Length(slTodo[i]));
        end
        else
        begin
          sgTasks.Cells[4, sgTasks.RowCount - 1] :=
            UTF8Copy(slTodo[i], 7, UTF8Length(slTodo[i]));
        end;
        sgTasks.Cells[1, sgTasks.RowCount - 1] := '1';
      end;
      iLength := iLength + StrToNSString(slTodo[i], True).length + 1;
    end;
  finally
    slTodo.Free;
    Cursor := crDefault;
  end;
  if sgTasks.RowCount > 1 then
  begin
    sgTasks.SortColRow(True, 2);
    sgTasks.Row := 1;
  end;
  lbTot.Caption := tsk003 + ' ' + FormatFloat('#,0', sgTasks.RowCount - 1);
end;

end.
