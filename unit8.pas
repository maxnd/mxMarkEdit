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

unit Unit8;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, Grids, ExtCtrls,
  StdCtrls, LazUTF8, translate, CocoaAll, CocoaTextEdits, CocoaUtils;

type

  { TfmEditor }

  TfmEditor = class(TForm)
    bnLeft: TButton;
    bnUp: TButton;
    bnDown: TButton;
    bnRight: TButton;
    dbEditor: TMemo;
    pnTasks: TPanel;
    bnOK: TButton;
    procedure bnDownClick(Sender: TObject);
    procedure bnLeftClick(Sender: TObject);
    procedure bnOKClick(Sender: TObject);
    procedure bnRightClick(Sender: TObject);
    procedure bnUpClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private
    procedure LoadCell;
    procedure SaveCell;

  public

  end;

var
  fmEditor: TfmEditor;

resourcestring

  lbedit001 = 'Cell';

implementation

uses Unit1;

{$R *.lfm}

{ TfmEditor }

procedure TfmEditor.FormCreate(Sender: TObject);
begin
  if IsAppDark = True then
  begin
    dbEditor.Font.Color := clWhite;
  end
  else
  begin
    dbEditor.Font.Color := clBlack;
    dbEditor.Color := clWhite;
  end;
end;

procedure TfmEditor.FormActivate(Sender: TObject);
begin
  TCocoaTextView(NSScrollView(dbEditor.Handle).documentView).
    setContinuousSpellCheckingEnabled(True);
  TCocoaTextView(NSScrollView(dbEditor.Handle).documentView).
    setGrammarCheckingEnabled(False);
  TCocoaTextView(NSScrollView(dbEditor.Handle).documentView).
    setFocusRingType(1);
  TCocoaTextView(NSScrollView(dbEditor.Handle).documentView).
    setAutomaticQuoteSubstitutionEnabled(True);
  TCocoaTextView(NSScrollView(dbEditor.Handle).documentView).
    setSmartInsertDeleteEnabled(True);
  TCocoaTextView(NSScrollView(dbEditor.Handle).documentView).
    setAutomaticLinkDetectionEnabled(True);
  TCocoaTextView(NSScrollView(dbEditor.Handle).documentView).
    setImportsGraphics(False);
  TCocoaTextView(NSScrollView(dbEditor.Handle).documentView).
    setRichText(False);
  TCocoaTextView(NSScrollView(dbEditor.Handle).documentView).
    textContainer.setLineFragmentPadding(20);
  TCocoaTextView(NSScrollView(dbEditor.Handle).documentView).
    checkTextInDocument(nil);
  TCocoaTextView(NSScrollView(dbEditor.Handle).documentView).
    undoManager.removeAllActions;
  dbEditor.Font.Name := fmMain.dbText.Font.Name;
end;

procedure TfmEditor.FormShow(Sender: TObject);
begin
  LoadCell;
end;

procedure TfmEditor.FormHide(Sender: TObject);
begin
  SaveCell;
end;

procedure TfmEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if ((key = 37) and (Shift = [ssMeta, ssAlt])) then
  begin
    bnLeftClick(nil);
    key := 0;
  end
  else
  if ((key = 38) and (Shift = [ssMeta, ssAlt])) then
  begin
    bnUpClick(nil);
    key := 0;
  end
  else
  if ((key = 39) and (Shift = [ssMeta, ssAlt])) then
  begin
    bnRightClick(nil);
    key := 0;
  end
  else
  if ((key = 40) and (Shift = [ssMeta, ssAlt])) then
  begin
    bnDownClick(nil);
    key := 0;
  end
  else
  if key = 27 then
  begin
    Close;
    key := 0;
  end;
end;

procedure TfmEditor.bnOKClick(Sender: TObject);
begin
  Close;
end;

procedure TfmEditor.bnLeftClick(Sender: TObject);
begin
  if fmMain.sgTable.Col > 1 then
  begin
    SaveCell;
    fmMain.sgTable.Col := fmMain.sgTable.Col - 1;
    LoadCell;
  end;
end;

procedure TfmEditor.bnUpClick(Sender: TObject);
begin
  if fmMain.sgTable.Row > 1 then
  begin
    SaveCell;
    fmMain.sgTable.Row := fmMain.sgTable.Row - 1;
    LoadCell;
  end;
end;

procedure TfmEditor.bnDownClick(Sender: TObject);
begin
  if fmMain.sgTable.Row < fmMain.sgTable.RowCount - 1 then
  begin
    SaveCell;
    fmMain.sgTable.Row := fmMain.sgTable.Row + 1;
    LoadCell;
  end;
end;

procedure TfmEditor.bnRightClick(Sender: TObject);
begin
  if fmMain.sgTable.Col < fmMain.sgTable.ColCount - 1 then
  begin
    SaveCell;
    fmMain.sgTable.Col := fmMain.sgTable.Col + 1;
    LoadCell;
  end;
end;

procedure TfmEditor.LoadCell;
begin
  dbEditor.Text := fmMain.sgTable.Cells[fmMain.sgTable.Col, fmMain.sgTable.Row];
  dbEditor.SelStart := 0;
  fmEditor.Caption := lbedit001 + ' ' + IntToStr(fmMain.sgTable.Row) + ' / ' +
    fmMain.sgTable.Cells[fmMain.sgTable.Col, 0];
end;

procedure TfmEditor.SaveCell;
var
  stText: String;
begin
  stText := dbEditor.Text;
  stText := UTF8StringReplace(stText, #9, ' ', [rfReplaceAll]);
  fmMain.sgTable.Cells[fmMain.sgTable.Col, fmMain.sgTable.Row] := stText;
end;


end.

