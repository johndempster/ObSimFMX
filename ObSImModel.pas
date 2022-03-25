unit ObSImModel;
// -------------
// Tissue models
// -------------
// 28.01.22 Model code moved from ObSImMain to ObSimModel

interface

uses
  System.SysUtils, System.Classes, System.Math, System.strutils ;

const
    MaxDrugs = 100 ;
    // Drug available for applying to tissue flags
    MaxModel = 3 ;
    tGPIleum = 1 ;
    tChickBiventer = 2 ;
    tArterialRing = 4 ;
    tJejunum = 8 ;

    NumChannels = 1 ;
    chForce = 0 ;

    BackgroundNoiseStDev = 0.1 ;  // Background noise (gms)
    ForceStDev = 0.05 ;
    MaxMixingRate = 0.5 ;
    MeanRMax = 15.0 ;
    RMaxStDev = 0.05 ;
    cBathVolume = 10.0 ;          // Organ bath volume (ml)
    ReservoirVolume = 1000.0 ;   // Krebs solution reservoir volume (ml)
    dt = 0.05 ;



type

  TDrug = record
          Name : String ;
          ShortName : String ;
          FinalBathConcentration : single ;
          DisplayBathConcentration : single ;
          BathConcentration : single ;
          Conc : single ;
          EC50_HistR : single ;
          EC50_HistR_NC : single ;
          EC50_nAchR : single ;
          EC50_mAchR : single ;
          EC50_mAchR_NC : single ;
          EC50_OpR : Single ;
          EC50_Alpha_AdrenR : Single ;
          EC50_Alpha2_AdrenR : Single ;
          EC50_Beta_AdrenR : Single ;
          EC50_PLC_Inhibition : Single ;
          EC50_IP3R : Single ;
          EC50_CaI : Single ;
          EC50_CaChannelV : Single ;
          EC50_CaChannelR : Single ;
          EC50_CaStore : Single ;
          EC50_BTXB : Single ;
          EC50_BTXE : Single ;
          Antagonist : Boolean ;
          Unknown : Boolean ;
          Tissue : Integer ;
          Units : String ;
          end ;


  TModel = class(TDataModule)
    procedure DataModuleCreate(Sender: TObject);
  private
    { Private declarations }

    FModels : TStringList ; // Names of models available

    mAch_EC50 : Single ;        //
    nAch_EC50 : Single ;
    MaxReleasedAch : Single ;

     // Randomly vary maximal response of next agonist application


    // Set botulinum toxin binding to none
    BTXBFreeFraction : Single ;
    BTXEFreeFraction : Single ;

    CyclicNerveReleasedAch : Single ;

  public
    { Public declarations }
    ModelType : NativeInt ;                               // Type of model in use

    t : single ;                                        // Elapsed time since start of simulation
    tStimulus : single ;                                // Time of stimulus (s)
    tNextStimulus : single ;                            // Time for next stimulus (s)
    tStimulationPeriod : Single ;                       // Stimulation period (s)

    NerveReleasedNorAdrenaline : Single ;     // Noradrenaline released by mesenteric nerve
    StimFrequency : Single ;
    idxNoradrenaline : Integer ;
    Desensitisation : Single ;

    ChanNames : Array[0..NumChannels-1] of string  ;   // Channel names
    ChanUnits : Array[0..NumChannels-1] of string  ;   // Channel units
    ChanValues : Array[0..NumChannels-1] of single  ;  // Channel values

    NumDrugs : Integer ;                                // No. drugs available
    Drugs : Array[0..MaxDrugs-1] of TDrug ;             // Drug properties array
    ReservoirDrugs : Array[0..MaxDrugs-1] of TDrug ;    // Drug properties array
    NerveStimulationOn : Boolean ;                           // TRUE = nerve stimulation on
    MuscleStimulationOn : Boolean ;                          // TRUE = muscle stimulation on

    RMax : Single ;      // Maximal response in current use
    NextRMax : Single ;  // RMax after next agonist application



    iCaBath : Integer ;
    InitialMixing : Cardinal ;
    BathVolume : Single ;                               // Volume of organ bath (ml)

    procedure GetListOfModels( Models : TStrings ) ;
    procedure InitialiseModel( NewModelType : Integer ) ;
    procedure DoSimulationStep ;
    procedure DoGPIleumSimulationStep( CyclicNerveReleasedAch : single ) ;
    procedure DoJejunumSimulationStep ;
    procedure DoArterialRingSimulationStep ;
    procedure DoChickBiventerSimulationStep ;
    procedure GetListOfDrugs(
              DrugList : TStrings ;         // Return list of drugs
              DrugType : string ) ;         // Type of drug (Agonist,Antagonist,Unknown)


  end;

var
  Model: TModel;

implementation

{%CLASSGROUP 'FMX.Controls.TControl'}

{$R *.dfm}

procedure TModel.DataModuleCreate(Sender: TObject);
// --------------------------------------
// Initialisations when module is created
// --------------------------------------
begin

    FModels := TStringList.Create ;

    // Output channels
    ChanNames[0] := 'F' ;
    ChanUnits[0] := 'gms' ;
    ChanValues[0] := 0.0 ;

    BathVolume := cBathVolume ;
    tStimulationPeriod := 5.0 ;

end;

procedure TModel.GetListOfModels( Models : TStrings ) ;
// -----------------------------------------------
// Return list of model names with selection index
// -----------------------------------------------
begin
     Models.Clear ;
     Models.AddObject('Guinea Pig Ileum',TObject(tGPIleum));
     Models.AddObject('Chick Biventer Cervicis',TObject(tChickBiventer));
     Models.AddObject('Rabbit Arterial Ring',TObject(tArterialRing));
     Models.AddObject('Rabbit Jejunum',TObject(tJejunum));


end;


procedure TModel.GetListOfDrugs(
          DrugList : TStrings ;         // Return list of drugs
          DrugType : string ) ;         // Type of drug (Agonist,Antagonist,Unknown)
// ---------------------------------------
// Return list of drugs of specified type
// ---------------------------------------
var
    i : Integer ;
begin
     for i := 0 to NumDrugs-1 do
         begin
         if (Drugs[i].Tissue and ModelType) <> 0 then
            begin
            if ContainsText(DrugType,'ant') and (not Drugs[i].Unknown) then
               begin
               DrugList.AddObject( Drugs[i].Name, TObject(i)) ;
               end
            else if ContainsText(DrugType,'unk') then
               begin
               DrugList.AddObject( Drugs[i].Name, TObject(i)) ;
               end
            else
               begin
               DrugList.AddObject( Drugs[i].Name, TObject(i)) ;
               end;
           end ;
         end;
     end ;



procedure TModel.InitialiseModel(
          NewModelType : Integer     // Type of model selected
          ) ;
// ---------------------------------------
// Initialise model to starting conditions
// ---------------------------------------
var
    i : Integer ;
begin


    ModelType := NewModelType ;

     // Initialise drug EC50 list to no effect
     for i := 0 to High(Drugs) do
          begin
          Drugs[i].EC50_HistR := 1E30 ;
          Drugs[i].EC50_HistR_NC := 1E30 ;
          Drugs[i].EC50_nAchR := 1E30 ;
          Drugs[i].EC50_mAchR := 1E30 ;
          Drugs[i].EC50_mAchR_NC := 1E30 ;
          Drugs[i].EC50_OpR := 1E30 ;
          Drugs[i].EC50_Alpha_AdrenR := 1E30 ;
          Drugs[i].EC50_Alpha2_AdrenR := 1E30 ;
          Drugs[i].EC50_Beta_AdrenR := 1E30 ;
          Drugs[i].EC50_PLC_Inhibition := 1E30 ;
          Drugs[i].EC50_IP3R := 1E30 ;
          //Drugs[i].EC50_CaI := 1E30 ;
          Drugs[i].EC50_CaChannelV := 1E30 ;
          Drugs[i].EC50_CaChannelR := 1E30 ;
          Drugs[i].EC50_CaStore := 1E30 ;
          Drugs[i].EC50_BTXB := 1E30 ;
          Drugs[i].EC50_BTXE := 1E30 ;
          Drugs[i].Tissue := 0 ;
          Drugs[i].Units := 'M' ;
          Drugs[i].Unknown := False ;
          Drugs[i].FinalBathConcentration := 0.0 ;
          Drugs[i].DisplayBathConcentration := 0.0 ;
          Drugs[i].BathConcentration := 0.0 ;
          end ;

     NumDrugs := 0 ;

     Drugs[NumDrugs].Name := 'Histamine' ;
     Drugs[NumDrugs].ShortName := 'His' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_HistR := 2E-7*RandG(1.0,0.05) ;
     //Drugs[NumDrugs].EC50_mAchR := 1E-3*RandG(1.0,0.05) ; Removed V2.1
     Drugs[NumDrugs].Antagonist := false ;
     Drugs[NumDrugs].Tissue := tGPIleum + tChickBiventer ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Mepyramine' ;
     Drugs[NumDrugs].ShortName := 'Mep' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_HistR := 2E-10*RandG(1.0,0.05) ;
     Drugs[NumDrugs].EC50_mAchR := 1.5E-5*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tGPIleum + tChickBiventer ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Carbachol' ;
     Drugs[NumDrugs].ShortName := 'Cch' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_nAchR := 1E-5*RandG(1.0,0.05) ;
     Drugs[NumDrugs].EC50_mAchR := 5E-8*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Tissue := tGPIleum + tChickBiventer + tJejunum ;
     Drugs[NumDrugs].Antagonist := False ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Atropine' ;
     Drugs[NumDrugs].ShortName := 'Atr' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_HistR := 2E-6*RandG(1.0,0.05) ;
     Drugs[NumDrugs].EC50_mAchR := 1E-9*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tGPIleum + tChickBiventer + tJejunum ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Tubocurarine' ;
     Drugs[NumDrugs].ShortName := 'Tub' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_nAchR := 1E-6*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tGPIleum + tChickBiventer ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Morphine' ;
     Drugs[NumDrugs].ShortName := 'Mor' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_OpR := 4.0E-8*RandG(1.0,0.05) ; {21/8/18 3.5E-8->4.0E-8}
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tGPIleum + tChickBiventer ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Loperamide' ;
     Drugs[NumDrugs].ShortName := 'Lop' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_OpR := 1E-7*RandG(1.0,0.05) ; ;
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tGPIleum + tChickBiventer ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Naloxone' ;
     Drugs[NumDrugs].ShortName := 'Nal' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_OpR := 1.5E-6*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tGPIleum + tChickBiventer ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'KCL' ;
     Drugs[NumDrugs].ShortName := 'KCL' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_CaChannelV := 4E-2 ;
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tArterialRing ;
     Inc(NumDrugs) ;

     idxNoradrenaline := NumDrugs ;
     Drugs[NumDrugs].Name := 'Noradrenaline/Norepinephrine' ; // Alpha + beta adrenoceptor agonist
     Drugs[NumDrugs].ShortName := 'Nor' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_Alpha_AdrenR := 5E-6*RandG(1.0,0.05) ;
     Drugs[NumDrugs].EC50_Alpha2_AdrenR := 5E-6*RandG(1.0,0.05) ;
     Drugs[NumDrugs].EC50_Beta_AdrenR := 1E-5*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tArterialRing + tJejunum ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Phenylephrine' ; // alpha-adrenoceptor agonist (jejunum)
     Drugs[NumDrugs].ShortName := 'Phe' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_Alpha_AdrenR := 2E-6*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tGPIleum + tJejunum ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'U73122' ;
     Drugs[NumDrugs].ShortName := 'U73' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_PLC_Inhibition := 1E-8 ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tArterialRing ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Heparin' ;
     Drugs[NumDrugs].ShortName := 'Hep' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_IP3R := 0.01 ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tArterialRing ;
     Drugs[NumDrugs].Units := 'mg/ml' ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Ca' ;
     Drugs[NumDrugs].ShortName := 'Ca' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := 0 ;
     iCaBath := NumDrugs ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Prazosin' ; // Alpha-adrenoceptor antagonist
     Drugs[NumDrugs].ShortName := 'Pra' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_Alpha_AdrenR := 3E-8*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tJejunum + tArterialRing ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Propranolol' ; // Beta-adrenoceptor antagonist (jejunum)
     Drugs[NumDrugs].ShortName := 'Pro' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_Beta_AdrenR := 1E-7*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tJejunum + tArterialRing ;
     Inc(NumDrugs) ;

     // Yohimbine (alpha-2 adrenoceptors antagonist)
     Drugs[NumDrugs].Name := 'Yohimbine' ;
     Drugs[NumDrugs].ShortName := 'Yoh' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_Alpha2_AdrenR := 5E-6*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := True ; ;
     Drugs[NumDrugs].Tissue := tGPIleum ;
     Drugs[NumDrugs].Unknown := False ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Isoprenaline' ; // Beta-adrenoceptor antagonist (jejunum)
     Drugs[NumDrugs].ShortName := 'Iso' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_Beta_AdrenR := 2E-6*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tJejunum ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Nifedipine' ; // Calcium channel blocker
     Drugs[NumDrugs].ShortName := 'Nif' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_CaChannelV := 1E-7*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tArterialRing ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Thapsigargin' ; // SR Calcium uptake pump blocker
     Drugs[NumDrugs].ShortName := 'Tha' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_CaStore := 1E-7*RandG(1.0,0.05) ;  // Note thapsigargin is NOT an IP3R antagonist
     Drugs[NumDrugs].Antagonist := True ;                 // but no distinction is made in current model
     Drugs[NumDrugs].Tissue := tArterialRing ;            // between block of release from stores by IP3
     Inc(NumDrugs) ;                                      // depletion of stores

     Drugs[NumDrugs].Name := 'SKF96365' ; // SR channel blocker
     Drugs[NumDrugs].ShortName := 'SKF' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_CaChannelR := 5E-5*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tArterialRing ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Acetylcholine' ; // Cholinoceptor agonist
     Drugs[NumDrugs].ShortName := 'Ach' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_nAchR := 1E-5*RandG(1.0,0.05) ;
     Drugs[NumDrugs].EC50_mAchR := 4.2E-7*RandG(1.0,0.05) ; {21/8/18 4.2E-8->4.2E-7 Ach less potent on mAChr}
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tGPIleum + tChickBiventer + tJejunum ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Pilocarpine' ; // Cholinoceptor agonist
     Drugs[NumDrugs].ShortName := 'Pil' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_nAchR := 1E-5*RandG(1.0,0.05) ;
     Drugs[NumDrugs].EC50_mAchR := 1.65E-6*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tGPIleum + tChickBiventer  + tJejunum ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Hyoscine' ; // Cholinoceptor antagonist
     Drugs[NumDrugs].ShortName := 'Hyo' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_nAchR := 2E-6*RandG(1.0,0.05) ;
     Drugs[NumDrugs].EC50_mAchR := 1E-7*RandG(1.0,0.05) ; {21/8/18 1E-10 > 1E-7M Hyoscine less potent}
     Drugs[NumDrugs].Tissue := tGPIleum + tChickBiventer ;
     Drugs[NumDrugs].Antagonist := True ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Adrenaline/Epinephrine' ; // Alpha + beta adrenoceptor agonist
     Drugs[NumDrugs].ShortName := 'Adr' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_Alpha_AdrenR := 1E-5*RandG(1.0,0.05) ;
     Drugs[NumDrugs].EC50_Alpha2_AdrenR := 1E-5*RandG(1.0,0.05) ;
     Drugs[NumDrugs].EC50_Beta_AdrenR := 5E-6*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tArterialRing + tJejunum ;
     Inc(NumDrugs) ;

     // Unknown drugs

     Drugs[NumDrugs].Name := 'Drug 1' ; // Histamine antagonist / weak musc.
     Drugs[NumDrugs].ShortName := 'Dr1' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     // V1.8 2011-12
     // Drugs[NumDrugs].EC50_HistR := 1E30 ; // Mep=2E-10M
     //Drugs[NumDrugs].EC50_HistR_NC := 2E-9*RandG(1.0,0.05) ; // Low affinity, non-competitive action
     // V2.2 2012-13
     //Drugs[NumDrugs].EC50_HistR := 1E-8 ; // Mep=2E-10M    comp. ant 100X less potent than mepyramine
     //Drugs[NumDrugs].EC50_mAchR := 8E-6*RandG(1.0,0.05) ;

     // V2.3 2013-14
     //Drugs[NumDrugs].EC50_HistR := 1E30 ; // Mep=2E-10M    comp. ant 100X less potent than mepyramine
     //Drugs[NumDrugs].EC50_HistR_NC := 1E-9*RandG(1.0,0.05) ; // Non-comp ant slightly less potent than mepyramine
     //Drugs[NumDrugs].EC50_mAchR := 8E-6*RandG(1.0,0.05) ;

     // V2.5 2014-15
     Drugs[NumDrugs].EC50_HistR := 1.5E-11*RandG(1.0,0.05) ; // Mep=2E-10M competitive antagonist X10 more potent than mep
     Drugs[NumDrugs].EC50_HistR_NC := 1E30;//*RandG(1.0,0.05)
     Drugs[NumDrugs].EC50_mAchR := 8E-6*RandG(1.0,0.05) ;

     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Unknown := True ;
     Drugs[NumDrugs].Tissue := tGPIleum + tChickBiventer ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Drug 2' ; //
     Drugs[NumDrugs].ShortName := 'Dr2' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     // V1.8 2011-12 Muscarinic antagonist / weak hist.
     // Drugs[NumDrugs].EC50_HistR := 1E-6*RandG(1.0,0.05) ;
     // Drugs[NumDrugs].EC50_mAchR := 1E-10*RandG(1.0,0.05) ; // Atr=1E-9
     // V2.2 2012-13 Muscarinic antagonist (less potent than atropine)/ weak hist.
     //Drugs[NumDrugs].EC50_HistR := 1E-5*RandG(1.0,0.05) ;
     //Drugs[NumDrugs].EC50_mAchR := 1E30 ;
     //Drugs[NumDrugs].EC50_mAchR_NC := 5E-9*RandG(1.0,0.05) ; // non-comp. ant.

     //V2.3 2013 (Competitive antagonist (more potent that atropine)
     //Drugs[NumDrugs].EC50_HistR := 1E-5*RandG(1.0,0.05) ;
     //Drugs[NumDrugs].EC50_mAchR := 8E-11*RandG(1.0,0.05) ;
     //Drugs[NumDrugs].EC50_mAchR_NC := 1E30 ;//

     //V2.5 2014 (non-competitive antagonist (100X less potent that atropine)
     Drugs[NumDrugs].EC50_HistR := 1E-5*RandG(1.0,0.05) ;
     Drugs[NumDrugs].EC50_mAchR := 1E30 ;//8E-11*RandG(1.0,0.05) ;
     Drugs[NumDrugs].EC50_mAchR_NC := 1E-7*RandG(1.0,0.06) ;//

     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tGPIleum + tChickBiventer ;
     Drugs[NumDrugs].Unknown := True ;
     Inc(NumDrugs) ;

     // Drug A: (mu-opioid agonist) (10X more potent than morphine)
     Drugs[NumDrugs].Name := 'Drug A' ;
     Drugs[NumDrugs].ShortName := 'DrA' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_OpR := 5E-9*RandG(1.0,0.05) ; // Decreased from 1E-7 16.01.19
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tGPIleum ;
     Drugs[NumDrugs].Unknown := True ;
     Inc(NumDrugs) ;

     // Drug B: Clonidine: alpha 2 agonist
     Drugs[NumDrugs].Name := 'Drug B' ;
     Drugs[NumDrugs].ShortName := 'DrB' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_Alpha2_AdrenR := 5E-7*RandG(1.0,0.05) ; //Increased from 2E-6 16.01.19
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tGPIleum ;
     Drugs[NumDrugs].Unknown := True ;
     Inc(NumDrugs) ;

     // Drug C: Verapamil (Ca channel blocker)
     Drugs[NumDrugs].Name := 'Drug C' ;
     Drugs[NumDrugs].ShortName := 'DrC' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_CaChannelV := 1E-7*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tGPIleum ;
     Drugs[NumDrugs].Unknown := True ;
     Inc(NumDrugs) ;

     // Drug D: Oxybutynin: Muscarinic antagonist
     Drugs[NumDrugs].Name := 'Drug D' ;
     Drugs[NumDrugs].ShortName := 'DrD' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_HistR := 2E-6*RandG(1.0,0.05) ;
     Drugs[NumDrugs].EC50_mAchR := 1E-9*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tGPIleum ;
     Drugs[NumDrugs].Unknown := True ;
     Inc(NumDrugs) ;


     // Botulinum toxin B
     Drugs[NumDrugs].Name := 'Botulinum Toxin B' ;
     Drugs[NumDrugs].ShortName := 'BTXB' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_BTXB := 1e-2 ;
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tGPIleum ;
     Drugs[NumDrugs].Unknown := True ;
     Drugs[NumDrugs].Units := 'ml' ;
     Inc(NumDrugs) ;

     // Botulinum toxin B + Anti-B antibody
     Drugs[NumDrugs].Name := 'Botulinum Tox. A+B Antibody' ;
     Drugs[NumDrugs].ShortName := 'BTX-AB' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_BTXB := 1e-10 ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tGPIleum ;
     Drugs[NumDrugs].Unknown := True ;
     Drugs[NumDrugs].Units := 'ml' ;
     Inc(NumDrugs) ;

     // Sample A (Botulinum toxin B)
     Drugs[NumDrugs].Name := 'Sample A' ;
     Drugs[NumDrugs].ShortName := 'SamA' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_BTXB := 1e-2 ;
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tGPIleum ;
     Drugs[NumDrugs].Unknown := True ;
     Drugs[NumDrugs].Units := 'ml' ;
     Inc(NumDrugs) ;

     // Sample B (Botulinum toxin B)
     Drugs[NumDrugs].Name := 'Sample B' ;
     Drugs[NumDrugs].ShortName := 'SamB' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_BTXB := 1e-2 ;
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tGPIleum ;
     Drugs[NumDrugs].Unknown := True ;
     Drugs[NumDrugs].Units := 'ml' ;
     Inc(NumDrugs) ;

     // Sample C (Botulinum toxin E)
     Drugs[NumDrugs].Name := 'Sample C' ;
     Drugs[NumDrugs].ShortName := 'SamC' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_BTXE := 1e-2 ;
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tGPIleum ;
     Drugs[NumDrugs].Unknown := True ;
     Drugs[NumDrugs].Units := 'ml' ;
     Inc(NumDrugs) ;

     // Copy set of drugs into reservoir
     for i:= 0 to NumDrugs-1 do ReservoirDrugs[i] := Drugs[i] ;

     mAch_EC50 := 1E-6 ;
     nAch_EC50 := 2E-6 ;
     MaxReleasedAch := mAch_EC50*4.0 ;

     // Randomly vary maximal response of next agonist application
     NextRMax := MeanRMax*RandG( 1.0, RMaxStDev ) ;

     // Clear all drugs from organ bath & reservoir
     for i := 0 to NumDrugs-1 do begin
         Drugs[i].FinalBathConcentration := 0.0 ;
         Drugs[i].DisplayBathConcentration := 0.0 ;
         Drugs[i].BathConcentration := 0.0 ;
         ReservoirDrugs[i].FinalBathConcentration := 0.0 ;
         ReservoirDrugs[i].DisplayBathConcentration := 0.0 ;
         ReservoirDrugs[i].BathConcentration := 0.0 ;
         end ;

     // Set salt solution Ca concentration
     if {Integer(cbSolution.Items.Objects[cbSolution.ItemIndex]) = ZeroCaSoln} false then
        begin
        Drugs[iCaBath].FinalBathConcentration := 0.0 ;
        Drugs[iCaBath].DisplayBathConcentration := 0.0 ;
        end
     else
        begin
        Drugs[iCaBath].FinalBathConcentration := 2.5E-3 ;
        Drugs[iCaBath].DisplayBathConcentration := 2.5E-3 ;
        end ;

     // Set desensitisation to none ;
     Desensitisation := 0.0 ;

     // Set botulinum toxin binding to none
     BTXBFreeFraction := 1.0 ;
     BTXEFreeFraction := 1.0 ;

     // Clear stimulus frequency
     StimFrequency := 0.0 ;
     t := 0.0 ;                    // Init. time counter
     tStimulus := 0.0 ;
     tNextStimulus := 0.0 ;

end;

procedure TModel.DoSimulationStep  ;
begin
    case ModelType of
        tGPIleum : DoGPIleumSimulationStep( CyclicNerveReleasedAch ) ;
    end;
end;


procedure TModel.DoGPIleumSimulationStep(
         CyclicNerveReleasedAch : single
         ) ;
// ---------------------------------
// Compute next simulation time step
// ---------------------------------
const
    ReceptorReserve = 0.9 ;//0.997 ;
var
    i : Integer ;
    dConc : Single ;
    HisR : Single ;    // Histamine receptor activation
    mAchR : Single ;   // Muscarinic receptor activation
    OpR : Single ;     // Opioid receptor activation
    AlphaADR : Single ;  // Alpha adrenoceptor activation
    Sum : Single ;
    Efficacy : Single ;
    Occupancy : Single ;


    NerveReleasedAch : Single ;
    EndogenousOpiate : Single ;
    Activation50 : Single ;
    MaxDirectMuscleActivation,DirectMuscleActivation : single ;
    CaChannelOpenFraction : single ;
    MixingRate : Single ;
    tStimulusDecay : Single ; // Stimulus decay time (s)
begin

    // Update drug bath concentrations
    MixingRate := (MaxMixingRate*InitialMixing) / ( 100.0 + InitialMixing) ;
    for i := 0 to NumDrugs-1 do
        begin
        dConc := (Drugs[i].FinalBathConcentration - Drugs[i].BathConcentration)*MixingRate*dt ;
        Drugs[i].BathConcentration := Max(Drugs[i].BathConcentration + dConc,0.0) ;
        end ;

    // Opioid receptors located on cholinergic nerve terminals (block transmitter release)

    EndogenousOpiate := 0.2 ;
    // Opioid receptor activation
    Sum := EndogenousOpiate ;
    for i := 0 to NumDrugs-1 do
        begin
        Sum := Sum + Drugs[i].BathConcentration/Drugs[i].EC50_OpR ;
        end ;
    Occupancy := Sum / ( 1. + Sum ) ;

    Efficacy := EndogenousOpiate ;
    for i := 0 to NumDrugs-1 do if not Drugs[i].Antagonist then
        begin
        Efficacy := Efficacy + Drugs[i].BathConcentration/Drugs[i].EC50_OpR ;
        end ;
    Efficacy := Efficacy / ( Sum + 0.001 ) ;
    OpR :=  Efficacy*Occupancy ;

    // Alpha2-adrenoceptors located on cholinergic nerve terminals (block transmitter release)

    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        Sum := Sum + Drugs[i].BathConcentration/Drugs[i].EC50_Alpha2_AdrenR ;
        end ;
    Occupancy := Sum / ( 1. + Sum ) ;

    Efficacy := 0.0 ;
    for i := 0 to NumDrugs-1 do if not Drugs[i].Antagonist then
        begin
        Efficacy := Efficacy + Drugs[i].BathConcentration/Drugs[i].EC50_Alpha2_AdrenR ;
        end ;
    Efficacy := Efficacy / ( Sum + 0.001 ) ;
    AlphaADR :=  Efficacy*Occupancy ;

    // Botulinum toxin B binding

    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        Sum := Sum + Drugs[i].BathConcentration/Drugs[i].EC50_BTXB ;
        end ;
    Occupancy := Sum / ( 1. + Sum ) ;

    Efficacy := 0.0 ;
    for i := 0 to NumDrugs-1 do if not Drugs[i].Antagonist then
        begin
        Efficacy := Efficacy + Drugs[i].BathConcentration/Drugs[i].EC50_BTXB ;
        end ;
    Efficacy := Efficacy / ( Sum + 0.001 ) ;
    BTXBFreeFraction := BTXBFreeFraction*(1.0 - Efficacy*Occupancy*0.01) ;

    // Botulinum toxin E binding

    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        Sum := Sum + Drugs[i].BathConcentration/Drugs[i].EC50_BTXE ;
        end ;
    Occupancy := Sum / ( 1. + Sum ) ;

    Efficacy := 0.0 ;
    for i := 0 to NumDrugs-1 do if not Drugs[i].Antagonist then
        begin
        Efficacy := Efficacy + Drugs[i].BathConcentration/Drugs[i].EC50_BTXE ;
        end ;
    Efficacy := Efficacy / ( Sum + 0.001 ) ;
    BTXEFreeFraction := BTXEFreeFraction*(1.0 - Efficacy*Occupancy*0.01) ;

    // Acetylcholine released from nerve
    // (blocked by Opioid receptor activation)
    // (Opioids can only achieve 90% block)

    MaxReleasedAch := (mAch_EC50*0.4*(0.02 + (1-OpR)*(1-AlphaADR)))
                       * BTXBFreeFraction*BTXEFreeFraction ;         // 0.25
    MaxDirectMuscleActivation := 1.0 ;
    RMax := NextRMax ;

    // Stimulate nerve
    if NerveStimulationOn then
       begin
       // Nerve released ACh
       if t >= tNextStimulus then
          begin
          tStimulus := tNextStimulus ;
          tNextStimulus := tStimulus + tStimulationPeriod ;
          end;
       tStimulusDecay := t - tStimulus ;
       NerveReleasedAch := MaxReleasedAch*(1.0 - exp(-tStimulusDecay/0.1))*exp(-tStimulusDecay/0.25) ;
       DirectMuscleActivation := 0.0 ;
       end ;

    // Stimulate muscle
    if MuscleStimulationOn then
       begin
       // Direct activation of smooth muscle
       if t >= tNextStimulus then
          begin
          tStimulus := tNextStimulus ;
          tNextStimulus := tStimulus + tStimulationPeriod ;
          end;
       NerveReleasedAch := 0.0 ;
       tStimulusDecay := t - tStimulus ;
       DirectMuscleActivation := MaxDirectMuscleActivation*(1.0 - exp(-tStimulusDecay/0.1))*exp(-tStimulusDecay/0.25) ;
       end;

    if (not NerveStimulationOn) and (not MuscleStimulationOn) then
       begin
       NerveReleasedAch := 0.0 ;
       DirectMuscleActivation := 0.0 ;
       end;
//    NerveReleasedAch := NerveReleasedAch + CyclicNerveReleasedAch*MaxReleasedAch ;

    // Histamine receptor activation
    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        Sum := Sum + Drugs[i].BathConcentration/Drugs[i].EC50_HistR ;
        end ;
    Occupancy := Sum / ( 1. + Sum ) ;

    Efficacy := 0.0 ;
    for i := 0 to NumDrugs-1 do if not Drugs[i].Antagonist then
        begin
        Efficacy := Efficacy + Drugs[i].BathConcentration/Drugs[i].EC50_HistR ;
        end ;
    Efficacy := Efficacy / ( Sum + 0.001 ) ;
    HisR :=  Efficacy*Occupancy ;

    // Histamine receptor activation -> contraction
    Activation50 := 1.0 - ReceptorReserve ;
    Sum := HisR/Activation50 ;
    for i := 0 to NumDrugs-1 do
        begin
        Sum := Sum + Drugs[i].BathConcentration/Drugs[i].EC50_HistR_NC ;
        end ;
    Occupancy := Sum /( 1.0 + Sum )  ;

    Efficacy := (HisR/Activation50) / ( Sum + 0.001 ) ;
    HisR :=  Efficacy*Occupancy ;

    // Muscarinic cholinoceptor receptor activation

    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        Sum := Sum + Drugs[i].BathConcentration/Drugs[i].EC50_mAchR ;
        end ;
    Sum := Sum + (NerveReleasedAch/mAch_EC50) ;
    Occupancy := Sum / ( 1. + Sum ) ;

    Efficacy := 0.0 ;
    for i := 0 to NumDrugs-1 do if not Drugs[i].Antagonist then
        begin
        Efficacy := Efficacy + Drugs[i].BathConcentration/Drugs[i].EC50_mAchR ;
        end ;
    Efficacy := (Efficacy + (NerveReleasedAch/mAch_EC50))/ ( Sum + 0.001 ) ;
    mAchR :=  Efficacy*Occupancy ;

    // Muscarinic receptor activation -> contraction
    Activation50 := 1.0 - ReceptorReserve ;
    Sum := mAchR/Activation50 ;
    for i := 0 to NumDrugs-1 do
        begin
        Sum := Sum + Drugs[i].BathConcentration/Drugs[i].EC50_mAchR_NC ;
        end ;
    Occupancy := Sum /( 1.0 + Sum )  ;

    Efficacy := (mAchR/Activation50) / ( Sum + 0.001 ) ;
    mAchR :=  Efficacy*Occupancy ;

    // Voltage operated trans membrane Ca channels (L type)
    // Fraction of channels unblocked
    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        Sum := Sum + Drugs[i].BathConcentration/Drugs[i].EC50_CaChannelV ;
        end ;
    Occupancy := Sum / ( 1. + Sum ) ;
    Efficacy := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        Efficacy := Efficacy + Drugs[i].BathConcentration/Drugs[i].EC50_CaChannelV ;
        end ;
    Efficacy := Efficacy/ ( Sum + 0.001 ) ;
    CaChannelOpenFraction :=  (1.0 - efficacy*occupancy) ;

    // Contraction
    ChanValues[chForce] := RandG( 0.0, BackgroundNoiseStDev )
                          + RMax*CaChannelOpenFraction*Min(HisR + mAChR + DirectMuscleActivation,1.0) ;

    t := t + dt ;
    InitialMixing := InitialMixing + 1 ;
    end ;


procedure TModel.DoJejunumSimulationStep ;
// ------------------------------------------------------
// Jejunum Simulation - Compute next simulation time step
// ------------------------------------------------------
const
    TwoPi = 2.0*3.1415926535897932385 ;
    Period = 3.5 ;
    NerveNorAdrReleaseRate = 0.03*1E-6 ;
    NerveNorAdrUptakeRate  = 0.1 ;
    BackgroundNoiseStDev = 0.075 ;  // Background noise (gms)
var
    i : Integer ;
    dConc : Single ;
    Alpha_AdrenR : Single ; // Alpha adrenoceptor activation
    Beta_AdrenR : Single ; // Beta adrenoceptor activation
    Sum : Single ;
    Efficacy : Single ;
    Occupancy : Single ;
    t : Single ;
    CyclicContraction : Single ;
    A : Single ;
    dNorAdr : SIngle ;
    NerveNorAdrRelease : SIngle ;
    NerveReleasedAch : SIngle ;
    mAchR : SIngle ;
    MixingRate : Single ;
begin

    // Update drug bath concentrations
    MixingRate := (MaxMixingRate*Model.InitialMixing) / ( 100.0 + Model.InitialMixing) ;
    for i := 0 to Model.NumDrugs-1 do
        begin
        dConc := (Model.Drugs[i].FinalBathConcentration - Model.Drugs[i].BathConcentration)*MixingRate*dt ;
        Model.Drugs[i].BathConcentration := Max(Model.Drugs[i].BathConcentration + dConc,0.0) ;
        end ;

 // Cyclic sympathetic nerve transmitter release

    // Mesenteric nerve stimulation
    if NerveStimulationOn then
       begin
       NerveNorAdrRelease := StimFrequency*NerveNorAdrReleaseRate ;
       end
    else NerveNorAdrRelease := 0.0 ;

    dNorAdr := NerveNorAdrRelease - NerveReleasedNorAdrenaline*NerveNorAdrUptakeRate ;
    NerveReleasedNorAdrenaline := Max(NerveReleasedNorAdrenaline + dNorAdr,0.0) ;

    // Alpha-adrenoceptor activation
    Sum := NerveReleasedNorAdrenaline/Model.Drugs[idxNorAdrenaline].EC50_Alpha_AdrenR ;
    for i := 0 to NumDrugs-1 do
        begin
        Sum := Sum + Model.Drugs[i].BathConcentration/Model.Drugs[i].EC50_Alpha_AdrenR ;
        end ;
    Occupancy := Sum / ( 1. + Sum ) ;

    Efficacy := NerveReleasedNorAdrenaline/Model.Drugs[idxNorAdrenaline].EC50_Alpha_AdrenR ;
    for i := 0 to NumDrugs-1 do if not Model.Drugs[i].Antagonist then
        begin
        Efficacy := Efficacy + Model.Drugs[i].BathConcentration/Model.Drugs[i].EC50_Alpha_AdrenR ;
        end ;
    Efficacy := Efficacy / ( Sum + 0.001 ) ;
    Alpha_AdrenR :=  Efficacy*Occupancy ;

    // Beta-adrenoceptor activation
    Sum := NerveReleasedNorAdrenaline/Model.Drugs[idxNorAdrenaline].EC50_Beta_AdrenR ;
    for i := 0 to NumDrugs-1 do
        begin
        Sum := Sum + Model.Drugs[i].BathConcentration/Model.Drugs[i].EC50_Beta_AdrenR ;
        end ;
    Occupancy := Sum / ( 1. + Sum ) ;

    Efficacy := NerveReleasedNorAdrenaline/Model.Drugs[idxNorAdrenaline].EC50_Beta_AdrenR ;
    for i := 0 to NumDrugs-1 do if not Model.Drugs[i].Antagonist then
        begin
        Efficacy := Efficacy + Model.Drugs[i].BathConcentration/Model.Drugs[i].EC50_Beta_AdrenR ;
        end ;
    Efficacy := Efficacy / ( Sum + 0.001 ) ;
    Beta_AdrenR :=  Efficacy*Occupancy ;

    // Cylic contractions
    // Inhibited by alpha- and beta-adrenoceptors by separate mechanisms

//    t := NumPointsInBuf*dt ;

    A := sin((2*Pi*t)/Period) ;
    CyclicContraction := A*A*A*A*
                        Max( 1.0 - 2.0*(Alpha_AdrenR/(1.+Alpha_AdrenR)) - 2.0*(Beta_AdrenR/(1.+Beta_AdrenR)) ,0.0 ) ;

    NerveReleasedAch := CyclicContraction*mAch_EC50 ;

    // Muscarinic cholinoceptor receptor activation
    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do begin
        Sum := Sum + Model.Drugs[i].BathConcentration/Model.Drugs[i].EC50_mAchR ;
        end ;
    Sum := Sum + (NerveReleasedAch/mAch_EC50) ;
    Occupancy := Sum / ( 1. + Sum ) ;

    Efficacy := 0.0 ;
    for i := 0 to NumDrugs-1 do if not Model.Drugs[i].Antagonist then begin
        Efficacy := Efficacy + Model.Drugs[i].BathConcentration/Model.Drugs[i].EC50_mAchR ;
        end ;
    Efficacy := (Efficacy + (NerveReleasedAch/mAch_EC50))/ ( Sum + 0.001 ) ;
    mAchR :=  Efficacy*Occupancy ;

    RMax := NextRMax ;

    // Contraction
    ChanValues[chForce] := RandG( 0.0, BackgroundNoiseStDev )
                          + RMax*mAChR ;

    end ;


procedure TModel.DoArterialRingSimulationStep ;
// ------------------------------------------------
// Compute next rabbit arterial ring simulation time step
// ------------------------------------------------
const
    StimulusInterval = 1.0 ;
    DesOnRate = 0.0015 ;
    DesOffRate = 1.5E-3 ; // was 1E-3
var
    i : Integer ;
    dConc : Single ;
    Sum : Single ;
    Efficacy : Single ;
    Occupancy : Single ;
    AdrenR : Single ;      // Adrenergic receptor activitation
    IP3R : Single ;        // IP3 receptor activation
    PLCActivity : Single ; // Phospholipase C enzyme activity
    CaI : Single ;         // Internal calcium activation
    CaO_Multiplier : Single ;         // External Ca
    CaChannelOpenFraction : Single ;  // Fraction of Ca plasma membrane voltage-activated channels open
    CaChannelROpenFraction : Single ; // Fraction of Ca plasma membrane receptor-operated channels open
    CaStore : Single ;                 // Ca store uptake pump activity
    CaS : Single ;                    // Ca released from internal stores
    R : Single ;
    MixingRate : single ;
begin

    // Update drug bath concentrations
    MixingRate := (MaxMixingRate*Model.InitialMixing) / ( 100.0 + Model.InitialMixing) ;
    for i := 0 to NumDrugs-1 do
        begin
        dConc := (Model.Drugs[i].FinalBathConcentration - Model.Drugs[i].BathConcentration)*MixingRate*dt ;
        Model.Drugs[i].BathConcentration := Max(Model.Drugs[i].BathConcentration + dConc,0.0) ;
        end ;

    // Adrenergic receptor activation
    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        Sum := Sum + Model.Drugs[i].BathConcentration/Model.Drugs[i].EC50_Alpha_AdrenR ;
        end ;
    Occupancy := Sum / ( 1. + Sum ) ;

    Efficacy := 0.0 ;
    for i := 0 to NumDrugs-1 do if not Model.Drugs[i].Antagonist then
        begin
        Efficacy := Efficacy + Model.Drugs[i].BathConcentration/Model.Drugs[i].EC50_Alpha_AdrenR ;
        end ;
    Efficacy := Efficacy/ ( Sum + 0.001 ) ;
    AdrenR :=  efficacy*occupancy ;

    // Phospholipase C inhibition
    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        Sum := Sum + Model.Drugs[i].BathConcentration/Model.Drugs[i].EC50_PLC_Inhibition ;
        end ;
    Occupancy := Sum / ( 1. + Sum ) ;

    Efficacy := 0.0 ;
    for i := 0 to NumDrugs-1 do if Model.Drugs[i].Antagonist then
        begin
        Efficacy := Efficacy + Model.Drugs[i].BathConcentration/Model.Drugs[i].EC50_PLC_Inhibition ;
        end ;
    Efficacy := Efficacy/ ( Sum + 0.001 ) ;
    PLCActivity :=  1.0 - (efficacy*occupancy) ;

    // IP3 receptor activation
    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do
         begin
        Sum := Sum + Model.Drugs[i].BathConcentration/Model.Drugs[i].EC50_IP3R ;
        end ;
    Sum := Sum + 2.0*PLCActivity*AdrenR ;
    Occupancy := Sum / ( 1. + Sum ) ;

    Efficacy := 0.0 ;
    for i := 0 to NumDrugs-1 do if not Model.Drugs[i].Antagonist then
        begin
        Efficacy := Efficacy + Model.Drugs[i].BathConcentration/Model.Drugs[i].EC50_IP3R ;
        end ;
    Efficacy := Efficacy + 2.0*PLCActivity*AdrenR ;
    Efficacy := Efficacy/ ( Sum + 0.001 ) ;
    IP3R :=  efficacy*occupancy ;

    // Voltage operated trans membrane Ca channels (L type)
    // (Lets in external Ca to cell cytoplasm, opened by KCL, blocked by nifedipine)
    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        R := Model.Drugs[i].BathConcentration/Model.Drugs[i].EC50_CaChannelV ;
        Sum := Sum + R*R ;
        end ;
    Occupancy := Sum / ( 1. + Sum ) ;
    Efficacy := 0.0 ;
    for i := 0 to NumDrugs-1 do if not Model.Drugs[i].Antagonist then
        begin
        R := Model.Drugs[i].BathConcentration/Model.Drugs[i].EC50_CaChannelV ;
        Efficacy := Efficacy + R*R ;
        end ;
    Efficacy := Efficacy/ ( Sum + 0.001 ) ;
    CaChannelOpenFraction :=  efficacy*occupancy ;

    // Ca internal stores (1=full, 0=empty)
    // Note depleted by thapsigargin
    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        R := Model.Drugs[i].BathConcentration/(Model.Drugs[i].EC50_CaStore) ;
        Sum := Sum + R ;
        end ;
    CaStore := 1.0 / ( 1.0 + Sum ) ;

    // Fraction of receptor operated Ca channels blocked
    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do begin
        R := Model.Drugs[i].BathConcentration/(Model.Drugs[i].EC50_CaChannelR) ;
        Sum := Sum + R ;
        end ;
    CaChannelROpenFraction :=  1./( 1. + Sum) ;

    // Internal Ca concentration


    // Ca influx via voltage operated Ca channels
    R := (CaChannelOpenFraction*Model.Drugs[iCaBath].BathConcentration)/5E-4 ;
    Sum :=  R*R ;
    Occupancy := Sum / ( 1. + Sum ) ;
    Efficacy := R*R ;
    Efficacy := Efficacy/ ( Sum + 0.001 ) ;
    CaI :=  efficacy*occupancy ;

    // Calcium from Ca stores

    // Increased capacity of Ca stores via external Ca influx throught receptor operated channels
    R := (CaChannelROpenFraction*Model.Drugs[iCaBath].BathConcentration)/2.5E-4 ;
    CaO_Multiplier := 1.0 + (R / (1.0 + R )) ;

    CaS := IP3R*CaStore*CaO_Multiplier ; ;

    Desensitisation := Desensitisation +
                       ((1.0-Desensitisation)*DesOnRate*CaS) - (Desensitisation*DesOffRate) ;
    CaS := CaS*(1.0 - Desensitisation) ;

    // Contraction
    ChanValues[chForce] := RandG( 0.0, BackgroundNoiseStDev )
                          + RMax*RMax*((1./(1. + exp(-8.*(CaI+CaS-0.5)))) - (1.0/(1.0 + exp(-8.0*(-0.5)))) ) ;
    // Tension relative to CaI & CaS = 0

    end ;


procedure TModel.DoChickBiventerSimulationStep ;
// ------------------------------------------------
// Compute next Chick Biventer simulation time step
// ------------------------------------------------
//
const
    StimulusInterval = 1.0 ;
var
    i : Integer ;
    dConc : Single ;
    nAchR : Single ;
    Sum : Single ;
    Efficacy : Single ;
    Occupancy : Single ;
    t : Single ;
    NerveReleasedAch : Single ;
    R : Single ;
    RNerve : Single ;
    MixingRate : single ;
    tStimulusDecay : single ;
    Force : single ;
begin

    // Update drug bath concentrations
    MixingRate := (MaxMixingRate*Model.InitialMixing) / ( 100.0 + Model.InitialMixing) ;
    for i := 0 to NumDrugs-1 do
        begin
        dConc := (Model.Drugs[i].FinalBathConcentration - Model.Drugs[i].BathConcentration)*MixingRate*dt ;
        Model.Drugs[i].BathConcentration := Max(Model.Drugs[i].BathConcentration + dConc,0.0) ;
        end ;

    // Nerve stimulation
    MaxReleasedAch := nAch_EC50*4.0 ;
    RMax := NextRMax ;

    // Stimulate nerve
    if NerveStimulationOn then
       begin
       // Nerve released ACh
       if t >= tNextStimulus then
          begin
          tStimulus := tNextStimulus ;
          tNextStimulus := tStimulus + tStimulationPeriod ;
          end;
       tStimulusDecay := t - tStimulus ;
       NerveReleasedAch := MaxReleasedAch*(1.0 - exp(-tStimulusDecay/0.05))*exp(-tStimulusDecay/0.1) ;
//       DirectMuscleActivation := 0.0 ;
       end ;

    // Nicotinic cholinoceptor receptor activation
    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        R := Model.Drugs[i].BathConcentration/Model.Drugs[i].EC50_nAchR ;
        Sum := Sum + R*R  ;
        end ;
    RNerve := NerveReleasedAch/nAch_EC50 ;
//    outputdebugstring(pchar(format('%.5g %.5g',[NerveReleasedAch,nAch_EC50])));
    Sum := Sum + (RNerve*RNerve) ;
    Occupancy := sum / ( 1. + sum ) ;

    Efficacy := 0.0 ;
    for i := 0 to NumDrugs-1 do if not Model.Drugs[i].Antagonist then begin
        R := Model.Drugs[i].BathConcentration/Model.Drugs[i].EC50_nAchR ;
        Efficacy := Efficacy + R*R ;
        end ;
    Efficacy := (Efficacy + (RNerve*RNerve))/ ( Sum + 0.001 ) ;
    nAchR :=  efficacy*occupancy ;

    Force := RandG( 0.0, BackgroundNoiseStDev )
             + RMax*nAchR ;

    // Stimulate muscle
    if MuscleStimulationOn then
       begin
       // Direct activation of smooth muscle
       if t >= tNextStimulus then
          begin
          tStimulus := tNextStimulus ;
          tNextStimulus := tStimulus + tStimulationPeriod ;
          end;
       NerveReleasedAch := 0.0 ;
       tStimulusDecay := t - tStimulus ;
       Force := Force*(1.0 - exp(-tStimulusDecay/0.05))*exp(-tStimulusDecay/0.1) ;
       Force := Min(Force,RMax) ;
       end;

    // Contraction
    ChanValues[chForce] := RandG( 0.0, BackgroundNoiseStDev )
                          + Force ;

    end ;





end.
