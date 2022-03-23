program ObSIm;

uses
  System.StartUpCopy,
  FMX.Forms,
  ObSImMain in 'ObSImMain.pas' {MainFrm},
  SHARED in 'SHARED.PAS',
  ObSImModel in 'ObSImModel.pas' {Model: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainFrm, MainFrm);
  Application.CreateForm(TModel, Model);
  Application.Run;
end.
