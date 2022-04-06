unit ModalBox;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation;

type
  TModalBoxFrm = class(TForm)
    lbMessage: TLabel;
    mYes: TButton;
    bNo: TButton;
    procedure FormShow(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MessageText : string ;
  end;

var
  ModalBoxFrm: TModalBoxFrm;

implementation

{$R *.fmx}

procedure TModalBoxFrm.FormShow(Sender: TObject);
begin
    lbMessage.Text := MessageText ;
end;

end.
