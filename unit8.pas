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
    dbEditor: TMemo;
    pnTasks: TPanel;
    bnOK: TButton;
    procedure bnOKClick(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  fmEditor: TfmEditor;

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
  dbeditor.Font.Name := fmMain.dbText.Font.Name;
end;

procedure TfmEditor.FormShow(Sender: TObject);
begin
  dbEditor.Text := fmMain.sgTable.Cells[fmMain.sgTable.Col, fmMain.sgTable.Row];
  dbEditor.SelStart := 0;
end;

procedure TfmEditor.FormHide(Sender: TObject);
var
  stText: String;
begin
  stText := dbEditor.Text;
  stText := UTF8StringReplace(stText, #9, ' ', [rfReplaceAll]);
  fmMain.sgTable.Cells[fmMain.sgTable.Col, fmMain.sgTable.Row] := stText;
end;

procedure TfmEditor.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
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

end.

