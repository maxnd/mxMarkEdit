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

unit zotero;

{$mode ObjFPC}{$H+}
{$modeswitch objectivec1}

interface

uses
  Classes, SysUtils, SQLite3DS, DB, Forms, Controls, Graphics, Dialogs, DBGrids,
  StdCtrls, CocoaUtils, CocoaTextEdits, CocoaAll, LazUTF8, LazFileUtils, Clipbrd, Types;

type

  { TfmZotero }

  TfmZotero = class(TForm)
    bnClose: TButton;
    bnFind: TButton;
    bnInsert: TButton;
    cbInitName: TCheckBox;
    cbTitIt: TCheckBox;
    dbAuthors: TSqlite3Dataset;
    edZoteroPath: TEdit;
    edSearchTitle: TEdit;
    edSearchAuthor: TEdit;
    grItem: TDBGrid;
    dsDetail: TDataSource;
    dsItem: TDataSource;
    dbItem: TSqlite3Dataset;
    dbDetail: TSqlite3Dataset;
    grDetail: TDBGrid;
    lbTotalItems: TLabel;
    lbExistModel: TLabel;
    lbZoteroPath: TLabel;
    lbSearchTitle: TLabel;
    lbSearchAuthor: TLabel;
    meQuote: TMemo;
    procedure bnCloseClick(Sender: TObject);
    procedure bnInsertClick(Sender: TObject);
    procedure bnFindClick(Sender: TObject);
    procedure cbInitNameChange(Sender: TObject);
    procedure dbItemAfterScroll(DataSet: TDataSet);
    procedure edSearchTitleKeyDown(Sender: TObject; var Key: word;
      Shift: TShiftState);
    procedure edZoteroPathChange(Sender: TObject);
    procedure FormActivate(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
    procedure grDetailDblClick(Sender: TObject);
    procedure grDetailKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
    procedure grDetailMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
    procedure grItemMouseWheel(Sender: TObject; Shift: TShiftState;
      WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
  private

  public

  end;

var
  fmZotero: TfmZotero;

resourcestring

  zot001 = 'Total items:';
  zotmsg001 = 'It was not possible to access Zotero data. Check that data file ' +
    'is not locked and the app has the permission to access its folder.';

implementation

uses main;

  {$R *.lfm}

  { TfmZotero }

procedure TfmZotero.FormCreate(Sender: TObject);
begin
  if IsAppDark = True then
  begin
    grItem.SelectedColor := $005E5E5E;
    grDetail.SelectedColor := $005E5E5E;
    meQuote.Font.Color := clWhite;
  end
  else
  begin
    grItem.SelectedColor := $00EBEBEB;
    grDetail.SelectedColor := $00EBEBEB;
    meQuote.Font.Color := clBlack;
    meQuote.Color := clWhite;
  end;
  edZoteroPath.Text := stZoteroPath;
  dbItem.FileName := stZoteroPath;
  dbDetail.FileName := stZoteroPath;
  dbAuthors.FileName := stZoteroPath;
  grItem.FocusRectVisible := False;
  grDetail.FocusRectVisible := False;
end;

procedure TfmZotero.FormActivate(Sender: TObject);
begin
  TCocoaTextView(NSScrollView(meQuote.Handle).documentView).
    setFocusRingType(1);
  edSearchTitle.SetFocus;
end;

procedure TfmZotero.FormKeyDown(Sender: TObject; var Key: word; Shift: TShiftState);
begin
  if key = 27 then
  begin
    Close;
    key := 0;
  end
  else
  if ((key = Ord('I')) and (Shift = [ssMeta])) then
  begin
    bnInsertClick(nil);
    key := 0;
  end;
end;

procedure TfmZotero.grDetailDblClick(Sender: TObject);
begin
  if dbDetail.FieldByName('value').AsString <> '' then
  begin
    if dbDetail.FieldByName('fieldName').AsString = 'date' then
    begin
      meQuote.Text := meQuote.Text + ' ' +
        UTF8Copy(dbDetail.FieldByName('value').AsString, 1, 4);
    end
    else
    if dbDetail.FieldByName('fieldName').AsString = 'pages' then
    begin
      meQuote.Text := meQuote.Text + ' #' +
        dbDetail.FieldByName('value').AsString;
    end
    else
    if ((dbDetail.FieldByName('fieldName').AsString = 'publicationTitle') and
      (cbTitIt.Checked = True)) then
    begin;
      meQuote.Text := meQuote.Text + ' *' +
        dbDetail.FieldByName('value').AsString + '*';
    end
    else
    begin
      meQuote.Text := meQuote.Text + ' ' +
        dbDetail.FieldByName('value').AsString;
    end;
  end;
end;

procedure TfmZotero.grDetailKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key = 13 then
  begin
    grDetailDblClick(nil);
  end;
end;

procedure TfmZotero.grDetailMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
  if WheelDelta > 40 then
  begin
    grDetail.DataSource.DataSet.MoveBy(- 1);
  end
  else
  if WheelDelta < -40 then
  begin
    grDetail.DataSource.DataSet.MoveBy(1);
  end;
end;

procedure TfmZotero.grItemMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  Handled := True;
  if WheelDelta > 40 then
  begin
    grItem.DataSource.DataSet.MoveBy(- 1);
  end
  else
  if WheelDelta < -40 then
  begin
    grItem.DataSource.DataSet.MoveBy(1);
  end;
end;

procedure TfmZotero.edZoteroPathChange(Sender: TObject);
begin
  stZoteroPath := edZoteroPath.Text;
  if FileExistsUTF8(stZoteroPath) then
  begin
    lbExistModel.Font.Color := clGreen;
  end
  else
  begin
    lbExistModel.Font.Color := clRed;
  end;
end;

procedure TfmZotero.bnFindClick(Sender: TObject);
begin
  if ((edSearchAuthor.Text = '') and (edSearchTitle.Text = '')) then
  begin
    Exit;
  end;
  dbItem.Close;
  dbDetail.Close;
  dbAuthors.Close;
  dbItem.SQL := 'Select itemData.itemID, fields.fieldName, creators.firstName, ' +
    'creators.lastName, creatorTypes.creatorType, itemDataValues.value ' +
    'FROM fields, itemData, itemDataValues, creators, creatorTypes, itemCreators ' +
    'WHERE itemData.fieldID = fields.fieldID ' +
    'AND itemData.valueID = itemDataValues.valueID ' + 'AND fields.fieldID = 1 ';
  if edSearchTitle.Text <> '' then
  begin
    dbItem.SQL := dbItem.SQL + 'AND itemDataValues.value like ' +
      #39 + '%' + edSearchTitle.Text + '%' + #39 + ' ';
  end;
  if edSearchAuthor.Text <> '' then
  begin
    dbItem.SQL := dbItem.SQL + 'AND creators.lastName like ' + #39 +
      '%' + edSearchAuthor.Text + '%' + #39 + ' ';
  end;
  dbItem.SQL := dbItem.SQL + 'AND creators.creatorID = itemCreators.creatorID ' +
    'AND creatorTypes.creatorTypeID = itemCreators.creatorTypeID ' +
    'AND itemCreators.itemID = itemData.itemID';
  try
    dbItem.Open;
    lbTotalItems.Caption := zot001 + ' ' + IntToStr(dbItem.RecordCount);
    if dbItem.RecordCount > 0 then
    begin;
      grItem.SetFocus;
    end;
  except
    MessageDlg(zotmsg001, mtWarning, [mbOK], 0);
  end;
end;

procedure TfmZotero.bnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfmZotero.bnInsertClick(Sender: TObject);
var
  i, x: integer;
  stQuote, stField: string;
begin
  if ((dbItem.Active = True) and (dbItem.RecordCount > 0) and
     (fmMain.sgTable.Row < fmMain.sgTable.RowCount - 1) and
    (fmMain.sgTable.Cells[1, fmMain.sgTable.Row] = '')) then
  begin
    for i := 1 to fmMain.sgTable.ColCount - 1 do
    begin
      if fmMain.sgTable.Cells[i, fmMain.sgTable.RowCount - 1] <> '' then
      begin
        MessageDlg(msg012, mtWarning, [mbOK], 0);
        Exit;
      end;
    end;
    fmMain.sgTable.InsertColRow(False, fmMain.sgTable.Row);
    fmMain.sgTable.Row := fmMain.sgTable.Row - 1;
    stQuote := meQuote.Text + ' | ';
    for i := 2 to 7 do
    begin
      fmMain.sgTable.Cells[i, fmMain.sgTable.Row] :=
        UTF8Copy(stQuote, 1, UTF8Pos(' | ', stQuote) - 1);
      if i = 7 then
      begin
        fmMain.sgTable.Cells[i, fmMain.sgTable.Row] :=
          UTF8Copy(stQuote, 1, UTF8Pos(' | ', stQuote) - 1);
        if UTF8Pos('#', stQuote) > 0 then
        begin
          fmMain.sgTable.Cells[i, fmMain.sgTable.Row] :=
            StringReplace(fmMain.sgTable.Cells[i, fmMain.sgTable.Row],
            '#', '', []);
        end;
        if UTF8Pos(', #', stQuote) > 0 then
        begin
          fmMain.sgTable.Cells[i + 1, fmMain.sgTable.Row] :=
            UTF8Copy(stQuote, 1, UTF8Pos('#', stQuote) - 3);
        end
        else
        if UTF8Pos(' #', stQuote) > 0 then
        begin
          fmMain.sgTable.Cells[i + 1, fmMain.sgTable.Row] :=
            UTF8Copy(stQuote, 1, UTF8Pos('#', stQuote) - 2);
        end
        else
        if UTF8Pos('#', stQuote) > 0 then
        begin
          fmMain.sgTable.Cells[i + 1, fmMain.sgTable.Row] :=
            UTF8Copy(stQuote, 1, UTF8Pos('#', stQuote) - 1);
        end
        else
        begin
          fmMain.sgTable.Cells[i + 1, fmMain.sgTable.Row] :=
            UTF8Copy(stQuote, 1, UTF8Pos(' | ', stQuote) - 1);
        end;
      end;
      stQuote := UTF8Copy(stQuote, UTF8Pos(' | ', stQuote) + 3,
        UTF8Length(stQuote));
    end;
    stField := fmMain.sgTable.Cells[2, fmMain.sgTable.Row];
    i := 1;
    x := 96;
    while i <= fmMain.sgTable.RowCount - 1 do
    begin
      if ((i <> fmMain.sgTable.Row) and (fmMain.sgTable.Cells[2, i] =
        stField)) then
      begin
        Inc(x);
        stField := fmMain.sgTable.Cells[2, fmMain.sgTable.Row] + Chr(x);
        i := 1;
      end
      else
      begin
        Inc(i);
      end;
    end;
    fmMain.sgTable.Cells[2, fmMain.sgTable.Row] := stField;
    fmMain.sgTable.RowCount := csTableRowCount;
    blTableMod := True;
    stGridLoaded := stTableLoaded;
    fmMain.LabelFileNameChars;
    blTableSaved := False;
    meQuote.Clear;
  end;
end;

procedure TfmZotero.cbInitNameChange(Sender: TObject);
begin
  if dbItem.Active = True then
  begin
    dbItemAfterScroll(nil);
  end;
end;

procedure TfmZotero.dbItemAfterScroll(DataSet: TDataSet);
var
  i: integer;
  stKey, stTitIt: string;
begin
  try
    meQuote.Clear;
    dbAuthors.Close;
    dbAuthors.SQL := 'Select itemData.itemID, creators.firstName, ' +
      'creators.lastName, creatorTypes.creatorType FROM fields, itemData, ' +
      'itemDataValues, creators, creatorTypes, itemCreators ' +
      'WHERE itemData.fieldID = fields.fieldID AND ' +
      'itemData.valueID = itemDataValues.valueID AND ' +
      'fields.fieldID = 1 AND creators.creatorID = itemCreators.creatorID AND ' +
      'creatorTypes.creatorTypeID = itemCreators.creatorTypeID AND ' +
      'itemCreators.itemID = itemData.itemID AND ' + 'itemData.itemID = ' +
      dbItem.FieldByName('itemID').AsString + ' ORDER BY itemCreators.creatorTypeID';
    dbAuthors.Open;
    dbDetail.Close;
    dbDetail.SQL := 'Select itemData.itemID, fields.fieldName, ' +
      'itemDataValues.value ' + 'FROM fields, itemData, itemDataValues ' +
      'WHERE itemData.fieldID = fields.fieldID ' +
      'AND itemData.valueID = itemDataValues.valueID ' +
      'AND itemData.itemID = ' + dbItem.FieldByName('itemID').AsString +
      ' ' + 'AND fields.fieldName <> ''title'' ' + 'ORDER BY fields.fieldID DESC';
    dbDetail.Open;
    if dbAuthors.RecordCount > 0 then
    begin
      if cbTitIt.Checked = True then
      begin
        stTitIt := '*';
      end
      else
      begin
        stTitIt := '';
      end;
      stKey := dbAuthors.FieldByName('lastName').AsString;
      for i := 0 to dbAuthors.RecordCount - 1 do
      begin
        meQuote.Text := meQuote.Text + dbAuthors.FieldByName(
          'lastName').AsString + ' ';
        if cbInitName.Checked = True then
        begin
          meQuote.Text := meQuote.Text + UTF8Copy(
            dbAuthors.FieldByName('firstName').AsString, 1, 1) + '.';
        end
        else
        begin
          meQuote.Text := meQuote.Text + dbAuthors.FieldByName('firstName').AsString;
        end;
        if dbAuthors.FieldByName('creatorType').AsString <> 'author' then
        begin
          meQuote.Text := meQuote.Text + ' (' +
            dbAuthors.FieldByName('creatorType').AsString + ')';
        end;
        if i < dbAuthors.RecordCount - 1 then
        begin
          meQuote.Text := meQuote.Text + ' – ';
        end;
        dbAuthors.Next;
      end;
      meQuote.Text := meQuote.Text + ' | ';
      dbAuthors.First;
      for i := 0 to dbAuthors.RecordCount - 1 do
      begin
        if cbInitName.Checked = True then
        begin
          meQuote.Text := meQuote.Text + UTF8Copy(
            dbAuthors.FieldByName('firstName').AsString, 1, 1) + '. ';
        end
        else
        begin
          meQuote.Text := meQuote.Text + dbAuthors.FieldByName(
            'firstName').AsString + ' ';
        end;
        meQuote.Text := meQuote.Text + dbAuthors.FieldByName('lastName').AsString;
        if dbAuthors.FieldByName('creatorType').AsString <> 'author' then
        begin
          meQuote.Text := meQuote.Text + ' (' +
            dbAuthors.FieldByName('creatorType').AsString + ')';
        end;
        if i < dbAuthors.RecordCount - 1 then
        begin
          meQuote.Text := meQuote.Text + ' – ';
        end;
        dbAuthors.Next;
      end;
      meQuote.Text := meQuote.Text + ' | ';
    end;
    meQuote.Text := meQuote.Text + stTitIt +
      dbItem.FieldByName('value').AsString + stTitIt + ' | ';
    if UTF8Pos('.', dbItem.FieldByName('value').AsString) > 0 then
    begin
      meQuote.Text := meQuote.Text + stTitIt +
        UTF8Copy(dbItem.FieldByName('value').AsString, 1,
        UTF8Pos('.', dbItem.FieldByName('value').AsString) - 1) + stTitIt + ' |';
    end
    else
    begin
      meQuote.Text := meQuote.Text + stTitIt +
        dbItem.FieldByName('value').AsString + stTitIt + ' |';
    end;
    for i := 0 to dbDetail.RecordCount - 1 do
    begin
      if dbDetail.FieldByName('fieldName').AsString = 'date' then
      begin
        stKey := stKey + ' ' +
          UTF8Copy(dbDetail.FieldByName('value').AsString, 1, 4);
        Break;
      end;
      dbDetail.Next;
    end;
    dbDetail.First;
    meQuote.Text := stKey + ' | ' + meQuote.Text;
  except
    MessageDlg(zotmsg001, mtWarning, [mbOK], 0);
  end;
end;

procedure TfmZotero.edSearchTitleKeyDown(Sender: TObject; var Key: word;
  Shift: TShiftState);
begin
  if key = 13 then
  begin
    bnFindClick(nil);
  end;
end;

end.
