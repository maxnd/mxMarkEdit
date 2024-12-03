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

unit translate;

{$mode ObjFPC}{$H+}

interface

uses
  Forms, LResources, LCLTranslator, Translations;

function TranslateTo(POFileName: string): boolean;

implementation

function TranslateTo(POFileName: string): boolean;
var
  res: TLResource;
  ii: integer;
  POFile: TPOFile;
  LocalTranslator: TUpdateTranslator;
begin
  Result := False;
  res := LazarusResources.Find(POFileName, 'PO');
  if res = nil then EXIT;
  POFile := TPOFile.Create(False);
  try
    POFile.ReadPOText(res.Value);
    Result := Translations.TranslateResourceStrings(POFile);
    if not Result then Exit;
    LocalTranslator := TPOTranslator.Create(POFile);
    LRSTranslator := LocalTranslator;
    for ii := 0 to Screen.CustomFormCount - 1 do
      LocalTranslator.UpdateTranslation(Screen.CustomForms[ii]);
    for ii := 0 to Screen.DataModuleCount - 1 do
      LocalTranslator.UpdateTranslation(Screen.DataModules[ii]);
    LRSTranslator := nil;
    LocalTranslator.Destroy;
  finally
  end;
end;

initialization
  {$I languages/language.rls}

end.
