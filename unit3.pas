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

unit Unit3;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Menus, LazUTF8, CocoaAll, CocoaUtils, translate;

type

  { TfmOptions }

  TfmOptions = class(TForm)
    bnClose: TButton;
    bnStFontCodeColorMod: TButton;
    bnStFontFootColor: TButton;
    bnStFontLinkColor: TButton;
    bnStFontTextColorMod: TButton;
    bnStFontTitle3ColorMod: TButton;
    bnStFontTitle1ColorMod: TButton;
    bnStFontTitle2ColorMod: TButton;
    bnStFontTodoColor: TButton;
    cbStFontsMono: TComboBox;
    edPanOptions: TEdit;
    edPanTemplate: TEdit;
    edPanOutput: TEdit;
    edPanPath: TEdit;
    cbStFonts: TComboBox;
    cdColorDialog: TColorDialog;
    lbPanOptions: TLabel;
    lbPanTemplate: TLabel;
    lbPanOutput: TLabel;
    lbPanPath: TLabel;
    lnStFonts: TLabel;
    lnStFontsMono: TLabel;
    procedure bnCloseClick(Sender: TObject);
    procedure bnStFontCodeColorModClick(Sender: TObject);
    procedure bnStFontFootColorClick(Sender: TObject);
    procedure bnStFontLinkColorClick(Sender: TObject);
    procedure bnStFontTextColorModClick(Sender: TObject);
    procedure bnStFontTitle3ColorModClick(Sender: TObject);
    procedure bnStFontTitle2ColorModClick(Sender: TObject);
    procedure bnStFontTitle1ColorModClick(Sender: TObject);
    procedure bnStFontTodoColorClick(Sender: TObject);
    procedure cbStFontsChange(Sender: TObject);
    procedure cbStFontsMonoChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
  private

  public

  end;

var
  fmOptions: TfmOptions;

implementation

uses Unit1;

  {$R *.lfm}


  { TfmOptions }

procedure TfmOptions.FormCreate(Sender: TObject);
var
  fm: NSFontManager;
  fd: NSFontDescriptor;
  FontFamilies: NSArray;
  i: integer;
begin
  if LowerCase(UTF8Copy(NSStringToString(
    NSLocale.preferredLanguages.objectAtIndex(0)), 1, 2)) = 'it' then
  begin
    translate.TranslateTo('mxmarkedit.it');
  end;
  fm := NSFontManager.sharedFontManager;
  FontFamilies := fm.availableFontFamilies;
  for i := 0 to FontFamilies.Count - 1 do
  begin
    fd := fmMain.FindFont(NSStringToString(
      FontFamilies.objectAtIndex(i).description), 0);
    if NSStringToString(fd.fontAttributes.description) <> '' then
    begin
      cbStFonts.Items.Add(NSStringToString(FontFamilies.objectAtIndex(
        i).description));
    end;
  end;
  cbStFontsMono.Items := cbStFonts.Items;
  cbStFonts.ItemIndex := 0;
  cbStFontsMono.ItemIndex := 0;
end;

procedure TfmOptions.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = 27 then
  begin
    key := 0;
    Close;
  end;
end;

procedure TfmOptions.FormShow(Sender: TObject);
begin
  cbStFonts.ItemIndex := cbStFonts.Items.IndexOf(fmMain.dbText.Font.Name);
  cbStFontsMono.ItemIndex := cbStFontsMono.Items.IndexOf(stFontMono);
  edPanOptions.Text := pandocOptions;
  edPanTemplate.Text := pandocTemplate;
  edPanOutput.Text := pandocOutput;
  edPanPath.Text := pandocPath;
end;

procedure TfmOptions.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  pandocOptions := edPanOptions.Text;
  pandocTemplate := edPanTemplate.Text;
  pandocOutput := edPanOutput.Text;
  pandocPath := edPanPath.Text;
end;

procedure TfmOptions.cbStFontsChange(Sender: TObject);
begin
  fmMain.dbText.Font.Name := cbStFonts.Text;
  fmMain.FormatListTitleTodo;
end;

procedure TfmOptions.cbStFontsMonoChange(Sender: TObject);
begin
  stFontMono := cbStFontsMono.Text;
  fmMain.FormatListTitleTodo;
end;

procedure TfmOptions.FormActivate(Sender: TObject);
begin
  bnClose.SetFocus;
end;

procedure TfmOptions.bnStFontLinkColorClick(Sender: TObject);
begin
  cdColorDialog.Color := clLink;
  if cdColorDialog.Execute then
  begin
    clLink := cdColorDialog.Color;
    fmMain.FormatListTitleTodo;
  end;
end;

procedure TfmOptions.bnStFontFootColorClick(Sender: TObject);
begin
  cdColorDialog.Color := clFootnote;
  if cdColorDialog.Execute then
  begin
    clFootnote := cdColorDialog.Color;
    fmMain.FormatListTitleTodo;
  end;
end;

procedure TfmOptions.bnStFontTextColorModClick(Sender: TObject);
begin
  cdColorDialog.Color := fmMain.dbText.Font.Color;
  if cdColorDialog.Execute then
  begin
    fmMain.dbText.Font.Color := cdColorDialog.Color;
    fmMain.FormatListTitleTodo;
  end;
end;

procedure TfmOptions.bnStFontCodeColorModClick(Sender: TObject);
begin
  cdColorDialog.Color := clCode;
  if cdColorDialog.Execute then
  begin
    clCode := cdColorDialog.Color;
    fmMain.FormatListTitleTodo;
  end;
end;

procedure TfmOptions.bnStFontTitle1ColorModClick(Sender: TObject);
begin
  cdColorDialog.Color := clTitle1;
  if cdColorDialog.Execute then
  begin
    clTitle1 := cdColorDialog.Color;
    fmMain.FormatListTitleTodo;
  end;
end;

procedure TfmOptions.bnStFontTodoColorClick(Sender: TObject);
begin
  cdColorDialog.Color := clTodo;
  if cdColorDialog.Execute then
  begin
    clTodo := cdColorDialog.Color;
    fmMain.FormatListTitleTodo;
  end;
end;

procedure TfmOptions.bnStFontTitle2ColorModClick(Sender: TObject);
begin
  cdColorDialog.Color := clTitle2;
  if cdColorDialog.Execute then
  begin
    clTitle2 := cdColorDialog.Color;
    fmMain.FormatListTitleTodo;
  end;
end;

procedure TfmOptions.bnStFontTitle3ColorModClick(Sender: TObject);
begin
  cdColorDialog.Color := clTitle3;
  if cdColorDialog.Execute then
  begin
    clTitle3 := cdColorDialog.Color;
    fmMain.FormatListTitleTodo;
  end;
end;

procedure TfmOptions.bnCloseClick(Sender: TObject);
begin
  Close;
end;


end.
