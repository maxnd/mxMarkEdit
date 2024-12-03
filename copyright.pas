// ***********************************************************************
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

unit copyright;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  translate, LazUTF8, CocoaUtils, CocoaAll, LCLIntf;

type

  { TfmCopyright }

  TfmCopyright = class(TForm)
    imImagecopyright: TImage;
    imLogo: TImage;
    lbApp: TLabel;
    lbCopyrightAuthor2: TLabel;
    lbCopyrightAuthor3: TLabel;
    lbCopyrightDesc: TLabel;
    lbCopyrightName: TLabel;
    lbCopyrightVers: TLabel;
    lbCopyrightVers1: TLabel;
    lbOK: TLabel;
    rmCopyrightText: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure lbAppClick(Sender: TObject);
    procedure lbOKClick(Sender: TObject);
  private

  public

  end;

var
  fmCopyright: TfmCopyright;

implementation

{$R *.lfm}

{ TfmCopyright }

procedure TfmCopyright.FormCreate(Sender: TObject);
begin
  if LowerCase(UTF8Copy(NSStringToString(
    NSLocale.preferredLanguages.objectAtIndex(0)), 1, 2)) = 'it' then
  begin
    translate.TranslateTo('mxmarkedit.it');
  end;
  rmCopyrightText.Lines.Add(
    'This program is free software: you can redistribute it and/or modify it ' +
    'under the terms of the GNU General Public License as published by the Free ' +
    'Software Foundation, either version 3 of the License, or (at your option) ' +
    'any later version. You can read the version 3 of the Licence in ' +
    'http://www.gnu.org/licenses gpl-3.0.txt. For other information you can also ' +
    'see http://www.gnu.org/licenses.');
  rmCopyrightText.Lines.Add(
    'This program is distributed in the hope ' +
    'that it will be useful, but WITHOUT ANY WARRANTY; without even the implied ' +
    'warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU ' +
    'General Public License for more details.');
end;

procedure TfmCopyright.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = 27 then
  begin
    key := 0;
    Close;
  end;
end;

procedure TfmCopyright.lbAppClick(Sender: TObject);
begin
  OpenURL('https://github.com/maxnd/mxMarkEdit');
end;

procedure TfmCopyright.lbOKClick(Sender: TObject);
begin
  Close;
end;

end.
