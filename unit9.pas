// ***********************************************************************
// ***********************************************************************
// mxMarkEdit 1.x
// Author and copyright: Massimo Nardello, Modena (Italy) 2024 - 2026.
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

unit Unit9;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls;

type

  { TfmPicture }

  TfmPicture = class(TForm)
    bvPicture: TBevel;
    imPicture: TImage;
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure imPictureClick(Sender: TObject);
  private

  public

  end;

var
  fmPicture: TfmPicture;

implementation

{$R *.lfm}

{ TfmPicture }

procedure TfmPicture.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (((key = Ord('E')) and (Shift = [ssMeta]) or
  ((key = 40) and (Shift = [ssMeta])) or
  ((key = Ord('E')) and (Shift = [ssMeta, ssShift]) or
  ((key = 38) and (Shift = [ssMeta])) or (key = 27)))) then
  begin
    key := 0;
    Close;
  end;
end;

procedure TfmPicture.imPictureClick(Sender: TObject);
begin
  Close;
end;

end.

