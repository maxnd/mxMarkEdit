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

unit Unit7;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, LazUTF8, translate, Types, CocoaAll, CocoaTextEdits, CocoaUtils;

type

  { TfmShortcuts }

  TfmShortcuts = class(TForm)
    pnBottom: TPanel;
    bnOK: TButton;
    sgShortcuts: TStringGrid;
    procedure bnOKClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure sgShortcutsPrepareCanvas(Sender: TObject; aCol, aRow: Integer;
      aState: TGridDrawState);
  private
    procedure AddText;

  public

  end;

var
  fmShortcuts: TfmShortcuts;

resourcestring

  srcut001 = ' In the main form';
  srcut002 = 'Undo the last action in the document (not in the grid).';
  srcut003 = 'Redo the last action in the document (not in the grid).';
  srcut004 = 'Select one of the six filter options above the title and todo list.';
  srcut005 = 'Change the size of the normal font.';
  srcut006 = 'Change the size of the mono font, used for code.';
  srcut007 = 'Delete the current paragraph.';
  srcut008 = 'Insert the current date.';
  srcut009 = 'Insert the current date and time.';
  srcut010 = 'Format the selected text in bold, or the current word if no text is selected.';
  srcut011 = 'Format the selected text in italics, or the current word if no text is selected.';
  srcut012 = 'If the cursor is inside a numbered list, renumber the list.';
  srcut013 = 'Renumber all the possible footnotes references if they have been manually changed.';
  srcut014 = 'Find in the document the next recurrence of the text specified ' +
    'in the search form, even if it’s closed.';
  srcut015 = 'Find in the document the previous recurrence of the text specified ' +
    'in the search form, even if it’s closed.';
  srcut016 = 'Create a todo item with a deadline whose delay in days is specified ' +
     'in the options, or set it as done or to be done.';
  srcut017 = 'Create a todo item without a deadline, or set it as done or to be done.';
  srcut018 = 'Move up the current paragraph.';
  srcut019 = 'Move down the current paragraph.';
  srcut020 = 'Select the previous heading.';
  srcut021 = 'Select the next heading.';
  srcut022 = 'Transform a list with dashes (-) into a numbered list.';
  srcut023 = 'Transform a numbered list into a with dashes (-).';
  srcut024 = 'Set the current position of the cursor in the bookmark.';
  srcut025 = 'Move the cursor to the position already set in the bookmark.';
  srcut026 = 'Show the following paragraph with a green background, skipping the ' +
    'empty lines; this is useful for presentations.';
  srcut027 = 'Make uppercase the current word.';
  srcut028 = 'Make lowercase the current word.';
  srcut029 = 'Capitalize the current word.';
  srcut030 = 'Show the tables grid.';
  srcut031 = 'If the cursor is within a heading, cut this heading and all the ' +
    'text that is under it, included possible headings of lower levels.';
  srcut032 = 'Within a footnote reference in the document, move the ' +
    'cursor to the corresponding footnote.';
  srcut033 = 'Within a footnote, move the cursor to the corresponding footnote ' +
    'reference in the document.';
  srcut034 = 'In other positions, create a new footnote reference and a new' +
    'footnote, both properly automatically numbered.';
  srcut035 = ' In the tables grid';
  srcut036 = 'Insert a new row.';
  srcut037 = 'Delete the content of the selected cell or cells, after confirmation.';
  srcut038 = 'Delete the current row, after confirmation.';
  srcut039 = 'Search the text typed in the Find field starting from the current position ' +
      'and just in the current column of the current table.';
  srcut040 = 'Move up the current row.';
  srcut041 = 'Move down the current row.';
  srcut042 = 'Add a new column in the current table, after confirmation.';
  srcut043 = 'Delete the current column in the current table, after confirmation, ' +
    'if it’s not the first one, containing the tables names.';
  srcut044 = 'Move left the current column just of the current table ' +
    'with its content.';
  srcut045 = 'Move right the current column just of the current table' +
    'with its content.';
  srcut046 = 'Move the current table, with all its field, before the previous one.';
  srcut047 = 'Move the current table, with all its field, after the next one.';
  srcut048 = 'In the tables names column, select the previous table title, ' +
    'while in the other columns move to the top of the grid.';
  srcut049 = 'In the tables names column, select the following table title, while in ' +
    'the  other columns move to the last edited row of the current column.';
  srcut050 = 'Move to the first column of the grid.';
  srcut051 = 'Move to the last right edited column of the current row.';
  srcut052 = 'Sort the content of the current column in the current table, ' +
    'after confirmation, stopping before the formula if it''s present in any column.';
  srcut053 = 'Move the cursor in the search field, if it’s visible.';
  srcut054 = 'Copy the content of the selected cells in the clipboard.';
  srcut055 = 'Paste the content of the clipboard in the current (and following) cells.';
  srcut056 = 'Undo the last changes while the editor of a cell in the grid is still active.';
  srcut057 = ' In the todo form';
  srcut058 = 'Hide the todo items already done.';
  srcut059 = 'Toggle the state of the selected todo item from to be done to done, and vice versa.';
  srcut060 = 'Close the form.';
  srcut061 = ' In the search files form';
  srcut062 = 'Move the cursor in the Find field.';
  srcut063 = 'Close the form.';
  srcut064 = ' In the words recurrence form';
  srcut065 = 'Close the form.';


implementation

uses Unit1;

{$R *.lfm}

{ TfmShortcuts }

procedure TfmShortcuts.FormCreate(Sender: TObject);
begin
  sgShortcuts.FocusRectVisible := False;
  sgShortcuts.SelectedColor := sgShortcuts.Color;
  AddText;
end;

procedure TfmShortcuts.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 27 then
  begin
    key := 0;
    Close;
  end;
end;

procedure TfmShortcuts.sgShortcutsPrepareCanvas(Sender: TObject; aCol,
  aRow: Integer; aState: TGridDrawState);
begin
  if ((aRow > 0) and (sgShortcuts.Cells[0, aRow] <> '') and
    (sgShortcuts.Cells[1, aRow] = '')) then
  begin
    sgShortcuts.Canvas.Brush.Color := clHighlightList;
  end;
end;

procedure TfmShortcuts.bnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TfmShortcuts.AddText;
var
  i: Integer;
begin
  sgShortcuts.RowCount := 77;
  sgShortcuts.RowHeights[0];
  i := 1;
  // Main form
  sgShortcuts.Cells[0, i] := srcut001;
  Inc(i);
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Z';
  sgShortcuts.Cells[1, i] := srcut002;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Shift + Y';
  sgShortcuts.Cells[1, i] := srcut003;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Opt + 1-6';
  sgShortcuts.Cells[1, i] := srcut004;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + + or -';
  sgShortcuts.Cells[1, i] := srcut005;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Ctrl + + or -';
  sgShortcuts.Cells[1, i] := srcut006;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Shift + Backspace';
  sgShortcuts.Cells[1, i] := srcut007;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + D';
  sgShortcuts.Cells[1, i] := srcut008;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Shift + D';
  sgShortcuts.Cells[1, i] := srcut009;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + B';
  sgShortcuts.Cells[1, i] := srcut010;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + I';
  sgShortcuts.Cells[1, i] := srcut011;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + R';
  sgShortcuts.Cells[1, i] := srcut012;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Shift + R';
  sgShortcuts.Cells[1, i] := srcut013;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + G';
  sgShortcuts.Cells[1, i] := srcut014;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Shift + G';
  sgShortcuts.Cells[1, i] := srcut015;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + T';
  sgShortcuts.Cells[1, i] := srcut016;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Opt + T';
  sgShortcuts.Cells[1, i] := srcut017;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Opt + Arrow up';
  sgShortcuts.Cells[1, i] := srcut018;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Opt + Arrow down';
  sgShortcuts.Cells[1, i] := srcut019;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Ctrl + Arrow up';
  sgShortcuts.Cells[1, i] := srcut020;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Ctrl + Arrow down';
  sgShortcuts.Cells[1, i] := srcut021;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Opt + .';
  sgShortcuts.Cells[1, i] := srcut022;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Ctrl + .';
  sgShortcuts.Cells[1, i] := srcut023;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Shift + J';
  sgShortcuts.Cells[1, i] := srcut024;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + J';
  sgShortcuts.Cells[1, i] := srcut025;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + E';
  sgShortcuts.Cells[1, i] := srcut026;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + U';
  sgShortcuts.Cells[1, i] := srcut027;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Opt + Shift + U';
  sgShortcuts.Cells[1, i] := srcut028;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Opt + U';
  sgShortcuts.Cells[1, i] := srcut029;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Shift + T';
  sgShortcuts.Cells[1, i] := srcut030;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Shift + X';
  sgShortcuts.Cells[1, i] := srcut031;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Shift + F';
  sgShortcuts.Cells[1, i] := srcut032;
  Inc(i);
  sgShortcuts.Cells[1, i] := srcut033;
  Inc(i);
  sgShortcuts.Cells[1, i] := srcut034;
  Inc(i);
  Inc(i);
  // Table
  sgShortcuts.Cells[0, i] := srcut035;
  Inc(i);
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Shift + I';
  sgShortcuts.Cells[1, i] := srcut036;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Backspace';
  sgShortcuts.Cells[1, i] := srcut037;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Shift + Backspace';
  sgShortcuts.Cells[1, i] := srcut038;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + G';
  sgShortcuts.Cells[1, i] := srcut039;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Opt + Arrow up';
  sgShortcuts.Cells[1, i] := srcut040;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Opt + Arrow down';
  sgShortcuts.Cells[1, i] := srcut041;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Opt + Shift + I';
  sgShortcuts.Cells[1, i] := srcut042;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Opt + Shift + Backspace';
  sgShortcuts.Cells[1, i] := srcut043;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Opt + Arrow left';
  sgShortcuts.Cells[1, i] := srcut044;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Opt + Arrow right';
  sgShortcuts.Cells[1, i] := srcut045;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Ctrl + Arrow up';
  sgShortcuts.Cells[1, i] := srcut046;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Ctrl + Arrow down';
  sgShortcuts.Cells[1, i] := srcut047;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Arrow up';
  sgShortcuts.Cells[1, i] := srcut048;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Arrow down';
  sgShortcuts.Cells[1, i] := srcut049;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Arrow left';
  sgShortcuts.Cells[1, i] := srcut050;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Arrow right';
  sgShortcuts.Cells[1, i] := srcut051;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Ctrl + S';
  sgShortcuts.Cells[1, i] := srcut052;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Ctrl + F';
  sgShortcuts.Cells[1, i] := srcut053;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + C';
  sgShortcuts.Cells[1, i] := srcut054;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + V';
  sgShortcuts.Cells[1, i] := srcut055;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Esc';
  sgShortcuts.Cells[1, i] := srcut056;
  Inc(i);
  Inc(i);
  sgShortcuts.Cells[0, i] := srcut057;
  Inc(i);
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Opt + H';
  sgShortcuts.Cells[1, i] := srcut058;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + T';
  sgShortcuts.Cells[1, i] := srcut059;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Esc';
  sgShortcuts.Cells[1, i] := srcut060;
  Inc(i);
  Inc(i);
  //
  sgShortcuts.Cells[0, i] := srcut061;
  Inc(i);
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Meta + Shift + F';
  sgShortcuts.Cells[1, i] := srcut062;
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Esc';
  sgShortcuts.Cells[1, i] := srcut063;
  Inc(i);
  Inc(i);
  sgShortcuts.Cells[0, i] := srcut064;
  Inc(i);
  Inc(i);
  sgShortcuts.Cells[0, i] := 'Esc';
  sgShortcuts.Cells[1, i] := srcut065;
end;

end.

