unit ModalBox;
// -----------------------
// Modal user response box
// -----------------------
// Note. Usable in both modal and non-modal modes

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
    procedure mYesClick(Sender: TObject);
    procedure bNoClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    MessageText : string ;
    OK : Boolean ;
  end;

var
  ModalBoxFrm: TModalBoxFrm;

implementation

{$R *.fmx}

procedure TModalBoxFrm.bNoClick(Sender: TObject);
// ------------------
// No button clicked
// ------------------
begin
    OK := False ;
    Self.Hide ;
end;


procedure TModalBoxFrm.FormShow(Sender: TObject);
begin
    lbMessage.Text := MessageText ;
    OK := False ;
end;

procedure TModalBoxFrm.mYesClick(Sender: TObject);
// ------------------
// Yes button clicked
// ------------------
begin
    OK := True ;
    Self.Hide ;
end;

end.
