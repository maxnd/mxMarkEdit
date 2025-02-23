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

