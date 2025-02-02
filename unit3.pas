// **********************************************************************
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

unit Unit3;

{$mode objfpc}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, Menus, LazUTF8, CocoaAll, CocoaUtils, translate, LazFileUtils;

type

  { TfmOptions }

  TfmOptions = class(TForm)
    bnClose: TButton;
    bnSetColors: TButton;
    bnRemoveColors: TButton;
    bnStFontCodeColorMod: TButton;
    bnStFontFootColor: TButton;
    bnStFontLinkColor: TButton;
    bnStFontTextColorMod: TButton;
    bnStFontTitle3ColorMod: TButton;
    bnStFontTitle1ColorMod: TButton;
    bnStFontTitle2ColorMod: TButton;
    bnStFontTodoColor: TButton;
    cbStFontsMono: TComboBox;
    cbDelay: TComboBox;
    cbLineSpacing: TComboBox;
    edPanOptions: TEdit;
    edPanTemplate: TEdit;
    edPanOutput: TEdit;
    edPanPath: TEdit;
    cbStFonts: TComboBox;
    cdColorDialog: TColorDialog;
    edStMaxSize: TEdit;
    edStSize: TEdit;
    edStSizeLess: TButton;
    edStMaxSizeLess: TButton;
    edStMaxSizePlus: TButton;
    edStSizePlus: TButton;
    edStSizeMono: TEdit;
    edStSizeMonoLess: TButton;
    edStSizeMonoPlus: TButton;
    lbExistModel: TLabel;
    lbExistExe: TLabel;
    lbPanOptions: TLabel;
    lbPanTemplate: TLabel;
    lbPanOutput: TLabel;
    lbPanPath: TLabel;
    lbStMaxSize: TLabel;
    lbStSize: TLabel;
    lbStSizeMono: TLabel;
    lnStLineSpace: TLabel;
    lnStFonts: TLabel;
    lnStDelay: TLabel;
    lnStFontsMono: TLabel;
    procedure bnSetColorsClick(Sender: TObject);
    procedure bnRemoveColorsClick(Sender: TObject);
    procedure bnCloseClick(Sender: TObject);
    procedure bnStFontCodeColorModClick(Sender: TObject);
    procedure bnStFontFootColorClick(Sender: TObject);
    procedure bnStFontLinkColorClick(Sender: TObject);
    procedure bnStFontTextColorModClick(Sender: TObject);
    procedure bnStFontTitle3ColorModClick(Sender: TObject);
    procedure bnStFontTitle2ColorModClick(Sender: TObject);
    procedure bnStFontTitle1ColorModClick(Sender: TObject);
    procedure bnStFontTodoColorClick(Sender: TObject);
    procedure cbDelayChange(Sender: TObject);
    procedure cbLineSpacingChange(Sender: TObject);
    procedure cbStFontsChange(Sender: TObject);
    procedure cbStFontsMonoChange(Sender: TObject);
    procedure edPanPathChange(Sender: TObject);
    procedure edPanTemplateChange(Sender: TObject);
    procedure edStMaxSizeLessClick(Sender: TObject);
    procedure edStMaxSizePlusClick(Sender: TObject);
    procedure edStSizeLessClick(Sender: TObject);
    procedure edStSizeMonoLessClick(Sender: TObject);
    procedure edStSizeMonoPlusClick(Sender: TObject);
    procedure edStSizePlusClick(Sender: TObject);
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
  cbDelay.ItemIndex := iDelay;
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
var
  stLineSpacing: String;
begin
  cbStFonts.ItemIndex := cbStFonts.Items.IndexOf(fmMain.dbText.Font.Name);
  cbStFontsMono.ItemIndex := cbStFontsMono.Items.IndexOf(stFontMono);
  edStSize.Text := IntToStr(fmMain.dbText.Font.Size);
  edStSizeMono.text := IntToStr(iFontMonoSize);
  edStMaxSize.Text := IntToStr(iMaxSize);
  stLineSpacing := FormatFloat('0.0', iLineSpacing);
  stLineSpacing := stLineSpacing[1] + '.' + stLineSpacing[3];
  cbLineSpacing.ItemIndex := cbLineSpacing.Items.
    IndexOf(stLineSpacing);
  edPanOptions.Text := pandocOptions;
  edPanTemplate.Text := pandocTemplate;
  edPanOutput.Text := pandocOutput;
  edPanPath.Text := pandocPath;
  if FileExistsUTF8(edPanPath.Text + 'pandoc') then
  begin
    lbExistExe.Font.Color := clGreen;
  end
  else
  begin
    lbExistExe.Font.Color := clRed;
  end;
  if FileExistsUTF8(edPanTemplate.Text) then
  begin
    lbExistModel.Font.Color := clGreen;
  end
  else
  begin
    lbExistModel.Font.Color := clRed;
  end;
end;

procedure TfmOptions.FormActivate(Sender: TObject);
begin
  bnClose.SetFocus;
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

procedure TfmOptions.edPanPathChange(Sender: TObject);
begin
  if FileExistsUTF8(edPanPath.Text + 'pandoc') then
  begin
    lbExistExe.Font.Color := clGreen;
  end
  else
  begin
    lbExistExe.Font.Color := clRed;
  end;
end;

procedure TfmOptions.edPanTemplateChange(Sender: TObject);
begin
  if FileExistsUTF8(edPanTemplate.Text) then
  begin
    lbExistModel.Font.Color := clGreen;
  end
  else
  begin
    lbExistModel.Font.Color := clRed;
  end;
end;

procedure TfmOptions.edStSizePlusClick(Sender: TObject);
begin
  if fmMain.dbText.Font.Size < 128 then
  begin
    edStSize.Text := IntToStr(StrToInt(edStSize.Text) + 1);
    fmMain.dbText.Font.Size := StrToInt(edStSize.Text);
    fmMain.FormatListTitleTodo;
  end;
end;

procedure TfmOptions.edStSizeLessClick(Sender: TObject);
begin
  if fmMain.dbText.Font.Size > 6 then
  begin
    edStSize.Text := IntToStr(StrToInt(edStSize.Text) - 1);
    fmMain.dbText.Font.Size := StrToInt(edStSize.Text);
    fmMain.FormatListTitleTodo;
  end;
end;

procedure TfmOptions.edStSizeMonoPlusClick(Sender: TObject);
begin
  if iFontMonoSize < 128 then
  begin
    edStSizeMono.Text := IntToStr(StrToInt(edStSizeMono.Text) + 1);
    iFontMonoSize := StrToInt(edStSizeMono.Text);
    fmMain.FormatListTitleTodo;
  end;
end;

procedure TfmOptions.edStSizeMonoLessClick(Sender: TObject);
begin
  if iFontMonoSize > 6 then
  begin
    edStSizeMono.Text := IntToStr(StrToInt(edStSizeMono.Text) - 1);
    iFontMonoSize := StrToInt(edStSizeMono.Text);
    fmMain.FormatListTitleTodo;
  end;
end;

procedure TfmOptions.edStMaxSizePlusClick(Sender: TObject);
begin
  if iMaxSize < 4000000 then
  begin
    edStMaxSize.Text := IntToStr(StrToInt(edStMaxSize.Text) + 50000);
    iMaxSize := StrToInt(edStMaxSize.Text);
  end;
end;

procedure TfmOptions.edStMaxSizeLessClick(Sender: TObject);
begin
  if iMaxSize > 100000 then
  begin
    edStMaxSize.Text := IntToStr(StrToInt(edStMaxSize.Text) - 50000);
    iMaxSize := StrToInt(edStMaxSize.Text);
  end;
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

procedure TfmOptions.cbDelayChange(Sender: TObject);
begin
  iDelay := cbDelay.ItemIndex;
end;

procedure TfmOptions.cbLineSpacingChange(Sender: TObject);
begin
  case cbLineSpacing.ItemIndex of
    0: iLineSpacing := 0.8;
    1: iLineSpacing := 0.9;
    2: iLineSpacing := 1.0;
    3: iLineSpacing := 1.1;
    4: iLineSpacing := 1.2;
    5: iLineSpacing := 1.3;
    6: iLineSpacing := 1.4;
    7: iLineSpacing := 1.5;
    8: iLineSpacing := 1.6;
    9: iLineSpacing := 1.7;
    10: iLineSpacing := 1.8;
    11: iLineSpacing := 1.9;
    12: iLineSpacing := 2.0;
  end;
  fmMain.FormatListTitleTodo;
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

procedure TfmOptions.bnSetColorsClick(Sender: TObject);
begin
  if IsAppDark = True then
  begin
    fmMain.dbText.Font.Color := $00D6D6D6;
    clLink := $00F89442;
    clFootnote := $00FDA69E;
    clCode := $00D6AC77;
    clTodo := $00D88AFF;
    clTitle1 := $005EB2FB;
    clTitle2 := $0099F692;
    clTitle3 := $005EF3FF;
  end
  else
  begin
    fmMain.dbText.Font.Color := $00424242;
    clLink := $00DA6800;
    clFootnote := $00FF3794;
    clCode := $00932194;
    clTodo := $00E539E3;
    clTitle1 := $00511794;
    clTitle2 := $00519000;
    clTitle3 := $00009092
  end;
  fmMain.FormatListTitleTodo;
end;

procedure TfmOptions.bnRemoveColorsClick(Sender: TObject);
begin
  if IsAppDark = True then
  begin
    fmMain.dbText.Font.Color := clWhite;
    clLink := clSilver;
    clFootnote := clSilver;
    clCode := clSilver;
    clTodo := clWhite;
    clTitle1 := clWhite;
    clTitle2 := clWhite;
    clTitle3 := clWhite;
  end
  else
  begin
    fmMain.dbText.Font.Color := clBlack;
    clLink := clDkGray;
    clFootnote := clDkGray;
    clCode := clDkGray;
    clTodo := clBlack;
    clTitle1 := clBlack;
    clTitle2 := clBlack;
    clTitle3 := clBlack;
  end;
  fmMain.FormatListTitleTodo;
end;


end.
