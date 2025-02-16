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

program mxmarkedit;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  {$IFDEF HASAMIGA}
  athreads,
  {$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, unit1, unit2, unit3, unit4,unit5, unit6, unit7, unit8, unit9, copyright
  {$IFDEF Darwin}
    , clocale, iosxlocale, translate
  {$ENDIF}
  { you can add units after this };

{$R *.res}

begin
  Application.Title:='mxMarkEdit';
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TfmMain, fmMain);
  Application.CreateForm(TfmSearch, fmSearch);
  Application.CreateForm(TfmOptions, fmOptions);
  Application.CreateForm(TfmShortcuts, fmShortcuts);
  Application.CreateForm(TfmWords, fmWords);
  Application.CreateForm(TfmTasks, fmTasks);
  Application.CreateForm(TfmFiles, fmFiles);
  Application.CreateForm(TfmEditor, fmEditor);
  Application.CreateForm(TfmPicture, fmPicture);
  Application.CreateForm(TfmCopyright, fmCopyright);
  Application.Run;
end.

