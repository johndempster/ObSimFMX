program ObSIm;

uses
  System.StartUpCopy,
  FMX.Forms,
  ObSImMain in 'ObSImMain.pas' {MainFrm},
  ObSImModel in 'ObSImModel.pas' {Model: TDataModule},
  ModalBox in 'ModalBox.pas' {ModalBoxFrm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.CreateForm(TModel, Model);
  Application.CreateForm(TModalBoxFrm, ModalBoxFrm);
  Application.Run;
end.
