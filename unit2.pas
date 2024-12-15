// **********************************************************************
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

unit Unit2;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, Menus,
  LazUTF8, translate, CocoaAll, CocoaUtils, CocoaTextEdits;

type

  { TfmSearch }

  TfmSearch = class(TForm)
    bnFirst: TButton;
    bnNext: TButton;
    bnOK: TButton;
    bnReplace: TButton;
    edFind: TEdit;
    edReplace: TEdit;
    lbReplace: TLabel;
    lbFind: TLabel;
    mmMenuModal: TMainMenu;
    procedure bnFirstClick(Sender: TObject);
    procedure bnNextClick(Sender: TObject);
    procedure bnOKClick(Sender: TObject);
    procedure bnReplaceClick(Sender: TObject);
    procedure edFindKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
  private

  public

  end;

var
  fmSearch: TfmSearch;

resourcestring

  msgFnd001 = 'Text not found.';
  msgFnd002 = 'Replace all the recurrences of';
  msgFnd003 = 'with';

implementation

uses Unit1;

  {$R *.lfm}

  { TfmSearch }

procedure TfmSearch.FormCreate(Sender: TObject);
begin
  if LowerCase(UTF8Copy(NSStringToString(
    NSLocale.preferredLanguages.objectAtIndex(0)), 1, 2)) = 'it' then
  begin
    translate.TranslateTo('mxmarkedit.it');
  end;
end;

procedure TfmSearch.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = 27 then
  begin
    key := 0;
    Close;
  end;
end;

procedure TfmSearch.bnFirstClick(Sender: TObject);
var
  iPos: integer;
  rng: NSRange;
begin
  if ((edFind.Text = '') or (fmMain.dbText.Text = '')) then
  begin
    Exit;
  end;
  iPos := fmMain.UTF8CocoaPos(UTF8UpperCase(edFind.Text),
    UTF8UpperCase(fmMain.dbText.Text), 1);
  if iPos > 0 then
  begin
    fmMain.dbText.SelStart := iPos - 1;
    Application.ProcessMessages;
    rng.location := iPos - 1;
    rng.length := StrToNSString(edFind.Text, True).length;
    TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      showFindIndicatorForRange(rng);
  end
  else
  begin
    MessageDlg(msgFnd001, mtInformation, [mbOK], 0);
    if fmSearch.Visible = True then
    begin
        fmSearch.SetFocus;
    end;
  end;
end;

procedure TfmSearch.bnNextClick(Sender: TObject);
var
  iPos: integer;
  rng: NSRange;
begin
  if ((edFind.Text = '') or (fmMain.dbText.Text = '')) then
  begin
    Exit;
  end;
  iPos := fmMain.UTF8CocoaPos(UTF8UpperCase(edFind.Text),
    UTF8UpperCase(fmMain.dbText.Text), fmMain.dbText.SelStart +
    StrToNSString(edFind.Text, True).length + 1);
  if iPos > 0 then
  begin
    fmMain.dbText.SelStart := iPos - 1;
    Application.ProcessMessages;
    rng.location := iPos - 1;
    rng.length := StrToNSString(edFind.Text, True).length;
    TCocoaTextView(NSScrollView(fmMain.dbText.Handle).documentView).
      showFindIndicatorForRange(rng);
  end
  else
  begin
    MessageDlg(msgFnd001, mtInformation, [mbOK], 0);
    if fmSearch.Visible = True then
    begin
        fmSearch.SetFocus;
    end;
  end;
end;

procedure TfmSearch.bnReplaceClick(Sender: TObject);
var
  stFind, stReplace: string;
begin
  if MessageDlg(msgFnd002 + ' “' + edFind.Text + '” ' + LineEnding + msgFnd003 +
    ' “' + edReplace.Text + '”?', mtConfirmation, [mbOK, mbCancel], 0) = mrOk then
  begin
    stFind := UTF8StringReplace(edFind.Text, '\n', LineEnding, [rfReplaceAll]);
    stFind := UTF8StringReplace(stFind, '\t', #9, [rfReplaceAll]);
    stReplace := UTF8StringReplace(edReplace.Text, '\n', LineEnding, [rfReplaceAll]);
    stReplace := UTF8StringReplace(stReplace, '\t', #9, [rfReplaceAll]);
    fmMain.dbText.Text := UTF8StringReplace(fmMain.dbText.Text, stFind,
      stReplace, [rfIgnoreCase, rfReplaceAll]);
    fmMain.dbText.SelStart := 0;
    fmMain.Show;
  end;
end;

procedure TfmSearch.edFindKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if ((key = 13) and (Shift = [ssMeta])) then
  begin
    key := 0;
    bnNextClick(nil);
  end
  else
  if ((key = 13) and (Shift = [])) then
  begin
    key := 0;
    bnFirstClick(nil);
  end;
end;

procedure TfmSearch.bnOKClick(Sender: TObject);
begin
  Close;
end;

end.
