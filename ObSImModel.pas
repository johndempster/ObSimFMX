unit ObSImModel;
// -------------
// Tissue models
// -------------
// 28.01.22 Model code moved from ObSImMain to ObSimModel
// 18.04.22 Rat diaphragm model (from Twitch simulation) added

interface

uses
  System.SysUtils, System.Classes, System.Math, System.strutils, FMX.ListBox ;

const
    MaxDrugs = 100 ;
    // Drug available for applying to tissue flags
    MaxModel = 4 ;
    tGPIleum = 1 ;
    tChickBiventer = 2 ;
    tArterialRing = 4 ;
    tJejunum = 8 ;
    tRatDiaphragm = 16 ;

    dtAgonist = 0 ;
    dtAntagonist = 1 ;
    dtUnknown = 3 ;

    NumChannels = 1 ;
    chForce = 0 ;

    BackgroundNoiseStDev = 0.1 ;  // Background noise (gms)
    ForceStDev = 0.05 ;
    MaxMixingRate = 0.5 ;
    MeanRMax = 15.0 ;
    RMaxStDev = 0.05 ;
    cBathVolume = 10.0 ;          // Organ bath volume (ml)
    cReservoirVolume = 1000.0 ;   // Krebs solution reservoir volume (ml)
    dt = 0.15 ;                    // Simulation time step (s)

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
          EC50_AchEsterase : single ;
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
          EC50_NaChannel : Single ;
          EC50_KChannel : Single ;
          EC50_ECCoupling : Single ;
          EC50_Ca : Single ;
          EC50_Mg : Single ;
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

    NerveStimulationOn : Boolean ;                           // TRUE = nerve stimulation on
    MuscleStimulationOn : Boolean ;                          // TRUE = muscle stimulation on

    // Set botulinum toxin binding to none
    BTXBFreeFraction : Single ;
    BTXEFreeFraction : Single ;

    procedure SetNerveStimulation( Value : Boolean ) ;
    procedure SetMuscleStimulation( Value : Boolean ) ;
    procedure UpdateBathConcentrations ;

  public
    { Public declarations }
    ModelType : NativeInt ;                               // Type of model in use

    t : single ;                                        // Elapsed time since start of simulation
    tStimulus : single ;                                // Time of stimulus (s)
    tNerveStimulusDecay : single ;                      // Nerve stimulus decay time
    tMuscleStimulusDecay : single ;                     // Muscle stimulus decay time
    tStimulationPeriod : Single ;                       // Stimulation period (s)

    NerveReleasedNorAdrenaline : Single ;     // Noradrenaline released by mesenteric nerve
    NerveReleasedAch : Single ;               // Ach released by mesenteric nerve
    DirectMuscleActivation : single ;         // Fraction of muscles activated by electrical stimulation

    StimFrequency : Single ;                  // Stimulus frequency (Hz)
    idxNoradrenaline : Integer ;
    Desensitisation : Single ;                // receptor desensitization factor (1=none,0=full)

    ChanNames : Array[0..NumChannels-1] of string  ;   // Channel names
    ChanUnits : Array[0..NumChannels-1] of string  ;   // Channel units
    ChanValues : Array[0..NumChannels-1] of single  ;  // Channel values

    NumDrugs : Integer ;                                // No. drugs available
    Drugs : Array[0..MaxDrugs-1] of TDrug ;             // Drug properties array
    ReservoirDrugs : Array[0..MaxDrugs-1] of TDrug ;    // Drug properties array

    RMax : Single ;                                     // Maximal response in current use
    NextRMax : Single ;                                 // RMax after next agonist application

    iCaBath : Integer ;                                 // Bath Ca concentration index
    iMgBath : Integer ;                                 // Bath Mg concentration index
    InitialMixing : Cardinal ;
    BathVolume : Single ;                               // Volume of organ bath (ml)
    ReservoirVolume : Single ;                          // Volume of reservoir (ml)

    procedure GetListOfModels( Models : TStrings ) ;
    procedure InitialiseModel( NewModelType : Integer ) ;
    procedure DoSimulationStep ;
    procedure DoGPIleumSimulationStep ;
    procedure DoJejunumSimulationStep ;
    procedure DoArterialRingSimulationStep ;
    procedure DoChickBiventerSimulationStep ;
    procedure DoRatDiaphragmSimulationStep ;

    procedure GetListOfDrugs(
              DrugList : TComboBox ;         // Return list of drugs
              DrugType : Integer ) ;         // Type of drug (Agonist,Antagonist,Unknown)
    property NerveStimulation : Boolean read NerveStimulationOn write SetNerveStimulation ;
    property MuscleStimulation : Boolean read MuscleStimulationOn write SetMuscleStimulation ;

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
    ReservoirVolume := cReservoirVolume ;
    tStimulationPeriod := 2.5 ;

end;

procedure TModel.GetListOfModels( Models : TStrings ) ;
// -----------------------------------------------
// Return list of model names with selection index
// -----------------------------------------------
begin

     Models.Clear ;
     Models.AddObject('Guinea Pig Ileum',TObject(tGPIleum));
     Models.AddObject('Chick Biventer Cervicis',TObject(tChickBiventer));
     Models.AddObject('Rat Diaphragm',TObject(tRatDiaphragm));
     Models.AddObject('Rabbit Arterial Ring',TObject(tArterialRing));
     Models.AddObject('Rabbit Jejunum',TObject(tJejunum));

end;


procedure TModel.GetListOfDrugs(
          DrugList : TComboBox ;         // Return list of drugs
          DrugType : Integer ) ;        // Type of drug (Agonist,Antagonist,Unknown)
// ---------------------------------------
// Return list of drugs of specified type
// ---------------------------------------
var
    i : Integer ;
begin

     DrugList.Clear ;

     for i := 0 to NumDrugs-1 do
         begin
         if (Drugs[i].Tissue and ModelType) <> 0 then
            begin
            if (DrugType = dtAntagonist) and Drugs[i].Antagonist and (not Drugs[i].Unknown) then
               begin
               // Antagonist drug
               DrugList.Items.AddObject( Drugs[i].Name, TObject(i)) ;
               end
            else if (DrugType = dtUnknown) and Drugs[i].Unknown then
               begin
               // Unknown drug
               DrugList.Items.AddObject( Drugs[i].Name, TObject(i)) ;
               end
            else if (DrugType = dtAgonist) and (not Drugs[i].Antagonist) and (not Drugs[i].Unknown) then
               begin
               // Agonist drug
               DrugList.Items.AddObject( Drugs[i].Name, TObject(i)) ;
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
          Drugs[i].EC50_AchEsterase := 1E30 ;
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
          Drugs[i].EC50_NaChannel := 1E30 ;
          Drugs[i].EC50_KChannel := 1E30 ;
          Drugs[i].EC50_ECCoupling := 1E30 ;
          Drugs[i].EC50_Ca := 1E30 ;
          Drugs[i].EC50_Mg := 1E30 ;
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
     Drugs[NumDrugs].Tissue := tGPIleum ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Mepyramine' ;
     Drugs[NumDrugs].ShortName := 'Mep' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_HistR := 2E-10*RandG(1.0,0.05) ;
     Drugs[NumDrugs].EC50_mAchR := 1.5E-5*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tGPIleum ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Carbachol' ;
     Drugs[NumDrugs].ShortName := 'Cch' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_nAchR := 5E-5*RandG(1.0,0.05) ;
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
     Drugs[NumDrugs].Tissue := tGPIleum + tChickBiventer + tJejunum  + tRatDiaphragm ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Tubocurarine' ;
     Drugs[NumDrugs].ShortName := 'Tub' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_nAchR := 1E-6*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tGPIleum + tChickBiventer + tRatDiaphragm ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Morphine' ;
     Drugs[NumDrugs].ShortName := 'Mor' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_OpR := 4.0E-8*RandG(1.0,0.05) ; {21/8/18 3.5E-8->4.0E-8}
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tGPIleum ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Loperamide' ;
     Drugs[NumDrugs].ShortName := 'Lop' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_OpR := 1E-7*RandG(1.0,0.05) ; ;
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tGPIleum ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Naloxone' ;
     Drugs[NumDrugs].ShortName := 'Nal' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_OpR := 1.5E-6*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tGPIleum ;
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
     Drugs[NumDrugs].Name := 'Noradrenaline (Norepinephrine)' ; // Alpha + beta adrenoceptor agonist
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

     Drugs[NumDrugs].Name := 'Calcium' ;
     Drugs[NumDrugs].ShortName := 'Ca' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tRatDiaphragm ;
     Drugs[NumDrugs].Unknown := True ;
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
     Drugs[NumDrugs].EC50_nAchR := 5E-4*RandG(1.0,0.05) ;
     Drugs[NumDrugs].EC50_mAchR := 4.2E-7*RandG(1.0,0.05) ; {21/8/18 4.2E-8->4.2E-7 Ach less potent on mAChr}
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tGPIleum + tChickBiventer + tJejunum + tRatDiaphragm ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Neostigmine' ; // Cholinesterase inhibitor
     Drugs[NumDrugs].ShortName := 'Neo' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_AchEsterase := 1E-7*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tChickBiventer  + tRatDiaphragm ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Suxamethonium' ; // Depolarizing neuromuscular blocker / Nicotinic agonist
     Drugs[NumDrugs].ShortName := 'Sux' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_nAchR := 1E-6*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tChickBiventer  + tRatDiaphragm ;
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

     Drugs[NumDrugs].Name := 'Adrenaline (Epinephrine)' ; // Alpha + beta adrenoceptor agonist
     Drugs[NumDrugs].ShortName := 'Adr' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_Alpha_AdrenR := 1E-5*RandG(1.0,0.05) ;
     Drugs[NumDrugs].EC50_Alpha2_AdrenR := 1E-5*RandG(1.0,0.05) ;
     Drugs[NumDrugs].EC50_Beta_AdrenR := 5E-6*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := False ;
     Drugs[NumDrugs].Tissue := tArterialRing + tJejunum ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Tetrodotoxin' ;
     Drugs[NumDrugs].ShortName := 'TTX' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_NaChannel := 1E-6*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tRatDiaphragm  + tChickBiventer ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := '4-aminopyridine' ;
     Drugs[NumDrugs].ShortName := '4AP' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_KChannel := 2E-4*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tRatDiaphragm  + tChickBiventer ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Dantrolene' ;
     Drugs[NumDrugs].ShortName := 'DAN' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_ECCoupling := 5E-6*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tRatDiaphragm ;
     Inc(NumDrugs) ;

     Drugs[NumDrugs].Name := 'Magnesium' ;
     Drugs[NumDrugs].ShortName := 'Mg' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_Mg := 1.0 ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tRatDiaphragm ;
     Drugs[NumDrugs].Unknown := True ;
     iMgBath := NumDrugs ;
     Inc(NumDrugs) ;


     // Unknown drugs

     // MP220: Oxybutynin: Muscarinic antagonist
     Drugs[NumDrugs].Name := 'MP220' ;
     Drugs[NumDrugs].ShortName := 'MP220' ;
     Drugs[NumDrugs].FinalBathConcentration := 0.0 ;
     Drugs[NumDrugs].BathConcentration := 0.0 ;
     Drugs[NumDrugs].EC50_HistR := 2E-6*RandG(1.0,0.05) ;
     Drugs[NumDrugs].EC50_mAchR := 1E-9*RandG(1.0,0.05) ;
     Drugs[NumDrugs].Antagonist := True ;
     Drugs[NumDrugs].Tissue := tGPIleum ;
     Drugs[NumDrugs].Unknown := True ;
     Inc(NumDrugs) ;

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
     for i := 0 to NumDrugs-1 do
         begin
         Drugs[i].FinalBathConcentration := 0.0 ;
         Drugs[i].DisplayBathConcentration := 0.0 ;
         Drugs[i].BathConcentration := 0.0 ;
         ReservoirDrugs[i].FinalBathConcentration := 0.0 ;
         ReservoirDrugs[i].DisplayBathConcentration := 0.0 ;
         ReservoirDrugs[i].BathConcentration := 0.0 ;
         end ;

     // Set bath Ca and Mg concentrations to default Krebs values
     Drugs[iCaBath].FinalBathConcentration := 2.5E-3 ;
     Drugs[iCaBath].DisplayBathConcentration := 2.5E-3 ;
     Drugs[iMgBath].FinalBathConcentration := 1E-3 ;
     Drugs[iMgBath].DisplayBathConcentration := 1E-3 ;

     // Set desensitisation to none ;
     Desensitisation := 0.0 ;

     // Set botulinum toxin binding to none
     BTXBFreeFraction := 1.0 ;
     BTXEFreeFraction := 1.0 ;

     // Clear stimulus frequency
     StimFrequency := 0.0 ;
     t := 0.0 ;                      // Init. time counter
     tNerveStimulusDecay := 1E30 ;   // Nerve stimulation decay disabled
     tMuscleStimulusDecay := 1E30 ;  // Musxle stimulation decay disabled
     NerveReleasedAch := 0.0 ;       // Clear any nerve-released Ach
     DirectMuscleActivation := 0.0 ; // Clear any direct muscle activation

end;

procedure TModel.DoSimulationStep  ;
// ----------------------------
// Execute simulation time step
// ----------------------------
begin
    case ModelType of
        tGPIleum : DoGPIleumSimulationStep ;
        tJejunum : DoJejunumSimulationStep ;
        tArterialRing : DoArterialRingSimulationStep ;
        tChickBiventer : DoChickBiventerSimulationStep ;
        tRatDiaphragm : Model.DoRatDiaphragmSimulationStep ;
    end;
end;


procedure TModel.DoGPIleumSimulationStep ;
// ---------------------------------
// Compute next simulation time step
// ---------------------------------
const
    ReceptorReserve = 0.9 ;//0.997 ;
    tStimulationPeriod = 5.0 ;
var
    i : Integer ;
    HisR : Single ;    // Histamine receptor activation
    mAchR : Single ;   // Muscarinic receptor activation
    OpR : Single ;     // Opioid receptor activation
    AlphaADR : Single ;  // Alpha adrenoceptor activation
    Sum : Single ;
    Efficacy,EfficacySum : Single ;
    Occupancy,OccupancySum : Single ;
    EndogenousOpiate : Single ;
    Activation50 : Single ;
    MaxDirectMuscleActivation : single ;
    CaChannelOpenFraction : single ;
begin

    // Update drug bath concentrations
    UpdateBathConcentrations ;

    // Opioid receptors located on cholinergic nerve terminals (block transmitter release)

    EndogenousOpiate := 0.2 ;
    // Opioid receptor activation
    OccupancySum := EndogenousOpiate ;
    EfficacySum := EndogenousOpiate ;
    for i := 0 to NumDrugs-1 do
        begin
        OccupancySum := OccupancySum + Drugs[i].BathConcentration/Drugs[i].EC50_OpR ;
        if not Drugs[i].Antagonist then
           begin
           EfficacySum := EfficacySum + Drugs[i].BathConcentration/Drugs[i].EC50_OpR ;
           end;
        end ;
    Occupancy := OccupancySum / ( 1. + OccupancySum ) ;
    Efficacy := EfficacySum / ( OccupancySum + 0.001 ) ;
    OpR :=  Efficacy*Occupancy ;

    // Alpha2-adrenoceptors located on cholinergic nerve terminals (block transmitter release)
    OccupancySum := 0.0 ;
    EfficacySum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        OccupancySum := OccupancySum + Drugs[i].BathConcentration/Drugs[i].EC50_Alpha2_AdrenR ;
        if not Drugs[i].Antagonist then
           begin
           EfficacySum := EfficacySum + Drugs[i].BathConcentration/Drugs[i].EC50_Alpha2_AdrenR ;
           end ;
        end ;
    Occupancy := OccupancySum / ( 1. + OccupancySum ) ;
    Efficacy := EfficacySum / ( OccupancySum + 0.001 ) ;
    AlphaADR :=  Efficacy*Occupancy ;

    // Botulinum toxin B binding
    OccupancySum := 0.0 ;
    EfficacySum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        OccupancySum := OccupancySum + Drugs[i].BathConcentration/Drugs[i].EC50_BTXB ;
        if not Drugs[i].Antagonist then
           begin
           EfficacySum := EfficacySum + Drugs[i].BathConcentration/Drugs[i].EC50_BTXB ;
           end ;
        end ;
    Occupancy := OccupancySum / ( 1. + OccupancySum ) ;
    Efficacy := EfficacySum / ( OccupancySum + 0.001 ) ;
    BTXBFreeFraction := BTXBFreeFraction*(1.0 - Efficacy*Occupancy*0.01) ;

    // Botulinum toxin E binding
    OccupancySum := 0.0 ;
    EfficacySum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        OccupancySum := OccupancySum + Drugs[i].BathConcentration/Drugs[i].EC50_BTXE ;
        if not Drugs[i].Antagonist then
           begin
           EfficacySum := EfficacySum + Drugs[i].BathConcentration/Drugs[i].EC50_BTXE ;
           end ;
        end ;
    Occupancy := OccupancySum / ( 1. + OccupancySum ) ;
    Efficacy := EfficacySum / ( OccupancySum + 0.001 ) ;
    BTXEFreeFraction := BTXEFreeFraction*(1.0 - Efficacy*Occupancy*0.01) ;

    // Acetylcholine released from nerve
    // (blocked by Opioid receptor activation)
    // (Opioids can only achieve 90% block)

    MaxDirectMuscleActivation := 1.3 ;
    RMax := NextRMax ;

    // Stimulate nerve
    if NerveStimulationOn then
       begin
       // Nerve released ACh
       if t >= tStimulus then
          begin
//          NerveReleasedAch := MaxReleasedAch ;
          tNerveStimulusDecay := 0.0 ;
          tStimulus := tStimulus + tStimulationPeriod ;
          end;
       end ;

    if tNerveStimulusDecay < 10.0 then
       begin
       MaxReleasedAch := (mAch_EC50*0.4*(0.02 + (1-OpR)*(1-AlphaADR)))
                         * BTXBFreeFraction*BTXEFreeFraction ;         // 0.25
       NerveReleasedAch := MaxReleasedAch*(1.0 - exp(-tNerveStimulusDecay/0.1))*exp(-tNerveStimulusDecay/0.25) ;
       tNerveStimulusDecay := tNerveStimulusDecay + dt ;
       end;

    // Stimulate muscle
    if MuscleStimulationOn then
       begin
       // Direct activation of smooth muscle
       if t >= tStimulus then
          begin
          tMuscleStimulusDecay := 0.0 ;
          tStimulus := tStimulus + tStimulationPeriod ;
          end;
       end ;

    if tMuscleStimulusDecay < 10.0 then
       begin
       DirectMuscleActivation := MaxDirectMuscleActivation*(1.0 - exp(-tMuscleStimulusDecay/0.1))*exp(-tMuscleStimulusDecay/0.25) ;
       tMuscleStimulusDecay := tMuscleStimulusDecay + dt ;
       end;

    // Histamine receptor activation
    OccupancySum := 0.0 ;
    EfficacySum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        OccupancySum := OccupancySum + Drugs[i].BathConcentration/Drugs[i].EC50_HistR ;
        if not Drugs[i].Antagonist then
           begin
           EfficacySum := EfficacySum + Drugs[i].BathConcentration/Drugs[i].EC50_HistR ;
           end ;
        end ;
    Occupancy := OccupancySum / ( 1. + OccupancySum ) ;
    Efficacy := EfficacySum / ( OccupancySum + 0.001 ) ;
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
    OccupancySum := NerveReleasedAch/mAch_EC50 ;
    EfficacySum := NerveReleasedAch/mAch_EC50 ;
    for i := 0 to NumDrugs-1 do
        begin
        OccupancySum := OccupancySum + Drugs[i].BathConcentration/Drugs[i].EC50_mAchR ;
        if not Drugs[i].Antagonist then
           begin
           EfficacySum := EfficacySum + Drugs[i].BathConcentration/Drugs[i].EC50_mAchR ;
           end ;
        end ;
    Occupancy := OccupancySum / ( 1. + OccupancySum ) ;
    Efficacy := EfficacySum / ( OccupancySum + 0.001 ) ;
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
    OccupancySum := 0.0 ;
    EfficacySum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        OccupancySum := OccupancySum + Drugs[i].BathConcentration/Drugs[i].EC50_CaChannelV ;
        if not Drugs[i].Antagonist then
           begin
           EfficacySum := EfficacySum + Drugs[i].BathConcentration/Drugs[i].EC50_CaChannelV ;
           end ;
        end ;
    Occupancy := OccupancySum / ( 1. + OccupancySum ) ;
    Efficacy := EfficacySum / ( OccupancySum + 0.001 ) ;
    CaChannelOpenFraction :=  (1.0 - efficacy*occupancy) ;

    // Contraction
    ChanValues[chForce] := RandG( 0.0, BackgroundNoiseStDev )
                          + RMax*CaChannelOpenFraction*Min(HisR + mAChR + DirectMuscleActivation,1.0) ;

    t := t + dt ;

    end ;


procedure TModel.DoJejunumSimulationStep ;
// ------------------------------------------------------
// Jejunum Simulation - Compute next simulation time step
// ------------------------------------------------------
const
    TwoPi = 2.0*3.1415926535897932385 ;
    Period = 7.0 ;
    NerveNorAdrReleaseRate = 0.03*1E-6 ;
    NerveNorAdrUptakeRate  = 0.1 ;
    BackgroundNoiseStDev = 0.075 ;  // Background noise (gms)
var
    i : Integer ;
    Alpha_AdrenR : Single ; // Alpha adrenoceptor activation
    Beta_AdrenR : Single ; // Beta adrenoceptor activation
    Efficacy,OccupancySum : Single ;
    Occupancy,EfficacySum : Single ;
    CyclicContraction : Single ;
    A : Single ;
    dNorAdr : SIngle ;
    NerveNorAdrRelease : SIngle ;
    NerveReleasedAch : SIngle ;
    mAchR : SIngle ;
begin

    // Update drug bath concentrations
    UpdateBathConcentrations ;

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
    OccupancySum := NerveReleasedNorAdrenaline/Model.Drugs[idxNorAdrenaline].EC50_Alpha_AdrenR ; ;
    EfficacySum := OccupancySum ;
    for i := 0 to NumDrugs-1 do
        begin
        OccupancySum := OccupancySum + Drugs[i].BathConcentration/Drugs[i].EC50_Alpha_AdrenR ;
        if not Drugs[i].Antagonist then
           begin
           EfficacySum := EfficacySum + Drugs[i].BathConcentration/Drugs[i].EC50_Alpha_AdrenR ;
           end ;
        end ;
    Occupancy := OccupancySum / ( 1. + OccupancySum ) ;
    Efficacy := EfficacySum / ( OccupancySum + 0.001 ) ;
    Alpha_AdrenR :=  Efficacy*Occupancy ;

    // Beta-adrenoceptor activation
    OccupancySum := NerveReleasedNorAdrenaline/Model.Drugs[idxNorAdrenaline].EC50_Beta_AdrenR ; ;
    EfficacySum := OccupancySum ;
    for i := 0 to NumDrugs-1 do
        begin
        OccupancySum := OccupancySum + Drugs[i].BathConcentration/Drugs[i].EC50_Beta_AdrenR ;
        if not Drugs[i].Antagonist then
           begin
           EfficacySum := EfficacySum + Drugs[i].BathConcentration/Drugs[i].EC50_Beta_AdrenR ;
           end ;
        end ;
    Occupancy := OccupancySum / ( 1. + OccupancySum ) ;
    Efficacy := EfficacySum / ( OccupancySum + 0.001 ) ;
    Beta_AdrenR :=  Efficacy*Occupancy ;

    // Cylic contractions
    // Inhibited by alpha- and beta-adrenoceptors by separate mechanisms

    A := sin((2*Pi*t)/Period) ;
    CyclicContraction := A*A*A*A*
                        Max( 1.0 - 2.0*(Alpha_AdrenR/(1.+Alpha_AdrenR)) - 2.0*(Beta_AdrenR/(1.+Beta_AdrenR)) ,0.0 ) ;

    NerveReleasedAch := CyclicContraction*mAch_EC50 ;

    // Muscarinic cholinoceptor receptor activation
    OccupancySum := NerveReleasedAch/mAch_EC50 ;
    EfficacySum := OccupancySum ;
    for i := 0 to NumDrugs-1 do
        begin
        OccupancySum := OccupancySum + Drugs[i].BathConcentration/Drugs[i].EC50_mAchR ;
        if not Drugs[i].Antagonist then
           begin
           EfficacySum := EfficacySum + Drugs[i].BathConcentration/Drugs[i].EC50_mAchR ;
           end ;
        end ;
    Occupancy := OccupancySum / ( 1. + OccupancySum ) ;
    Efficacy := EfficacySum / ( OccupancySum + 0.001 ) ;
    mAchR :=  Efficacy*Occupancy ;

    RMax := NextRMax ;

    // Contraction
    ChanValues[chForce] := RandG( 0.0, BackgroundNoiseStDev )
                          + RMax*mAChR ;

    t := t + dt ;

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
    Sum : Single ;
    Efficacy,EfficacySum : Single ;
    Occupancy,OccupancySum : Single ;
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

begin

    // Update drug bath concentrations
    UpdateBathConcentrations ;

    // Alpha-adrenoceptor activation
    OccupancySum := 0.0 ;
    EfficacySum := OccupancySum ;
    for i := 0 to NumDrugs-1 do
        begin
        R := Drugs[i].BathConcentration/Drugs[i].EC50_Alpha_AdrenR ;
        OccupancySum := OccupancySum + R ;
        if not Drugs[i].Antagonist then EfficacySum := EfficacySum + R ;
        end ;
    Occupancy := OccupancySum / ( 1. + OccupancySum ) ;
    Efficacy := EfficacySum / ( OccupancySum + 0.001 ) ;
    AdrenR :=  Efficacy*Occupancy ;

    // Phospholipase C inhibition
    // (Note. Antagonist flag is ignored for drugs with PLC inhibition activity)

    OccupancySum := 0.0 ;
    EfficacySum := OccupancySum ;
    for i := 0 to NumDrugs-1 do
        begin
        R := Drugs[i].BathConcentration/Drugs[i].EC50_PLC_Inhibition ;
        OccupancySum := OccupancySum + R ;
        EfficacySum := EfficacySum + R ;
        end ;
    Occupancy := OccupancySum / ( 1. + OccupancySum ) ;
    Efficacy := EfficacySum / ( OccupancySum + 0.001 ) ;
    PLCActivity :=  1.0 - (Efficacy*Occupancy) ;

    // IP3 receptor activation
    // (Start with IP3 production due to effect of alpha adrenoceptor activation
    //  then add effects of exogenous agonists and antagonists)

    OccupancySum := 2.0*PLCActivity*AdrenR ; ;
    EfficacySum := OccupancySum ;
    for i := 0 to NumDrugs-1 do
        begin
        R := Drugs[i].BathConcentration/Drugs[i].EC50_IP3R ;
        OccupancySum := OccupancySum + R ;
        if not Drugs[i].Antagonist then EfficacySum := EfficacySum + R ;
        end ;
    Occupancy := OccupancySum / ( 1. + OccupancySum ) ;
    Efficacy := EfficacySum / ( OccupancySum + 0.001 ) ;
    IP3R := Efficacy*Occupancy ;

    // Voltage operated trans membrane Ca channels (L type)
    // (Lets in external Ca to cell cytoplasm, opened by KCL, blocked by nifedipine)

    OccupancySum := 0.0 ;
    EfficacySum := OccupancySum ;
    for i := 0 to NumDrugs-1 do
        begin
        R := Model.Drugs[i].BathConcentration/Model.Drugs[i].EC50_CaChannelV ;
        OccupancySum := OccupancySum + R*R ;
        if not Drugs[i].Antagonist then EfficacySum := EfficacySum + R*R ;
        end ;
    Occupancy := OccupancySum / ( 1. + OccupancySum ) ;
    Efficacy := EfficacySum / ( OccupancySum + 0.001 ) ;
    CaChannelOpenFraction := Efficacy*Occupancy ;

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
    RMax := NextRMax ;
    ChanValues[chForce] := RandG( 0.0, BackgroundNoiseStDev )
                          + RMax*((1./(1. + exp(-8.*(CaI+CaS-0.5)))) - (1.0/(1.0 + exp(-8.0*(-0.5)))) ) ;
    // Tension relative to CaI & CaS = 0

    t := t + dt ;

    end ;

procedure TModel.UpdateBathConcentrations ;
// -------------------------------------
// Update concentration of drugs in bath
// -------------------------------------
var
    i : Integer ;
    MixingRate,dConc : single ;
begin

    MixingRate := (MaxMixingRate*InitialMixing) / ( 100.0 + InitialMixing) ;
    for i := 0 to NumDrugs-1 do
        begin
        dConc := (Model.Drugs[i].FinalBathConcentration - Model.Drugs[i].BathConcentration)*MixingRate*dt ;
        Model.Drugs[i].BathConcentration := Max(Model.Drugs[i].BathConcentration + dConc,0.0) ;
        end ;
    InitialMixing := InitialMixing + 1 ;

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
 //   tStimulusDecay : single ;
    Conc : Single ;
    nAchR : Single ;
    AchEsterase : Single ;          // Cholinesterase inhibition
    nAchRDesensitization : Single ; // Nicotinic junctional receptor desensitization
    Sum : Single ;
    Efficacy,EfficacySum : Single ;
    Occupancy,OccupancySum : Single ;
    NaChannelBlock : single ;        // Fraction of Na channels blocked
    KChannelBlock : single ;        // Fraction of K channels blocked
    R : Single ;
    Force : single ;
begin

    // Update drug bath concentrations
    UpdateBathConcentrations ;

    // Na channel block (1=complete, 0=none)
    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        R := Drugs[i].BathConcentration/Drugs[i].EC50_NaChannel ;
        Sum := Sum + R  ;
        end ;
    NaChannelBlock := sum / ( 1. + sum ) ;

    // K channel block (1=complete, 0=none)
    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        R := Drugs[i].BathConcentration/Drugs[i].EC50_KChannel ;
        Sum := Sum + R  ;
        end ;
    KChannelBlock := sum / ( 1. + sum ) ;

    // Cholinesterase inhibition (1=fully inhibited, 0= fully active)
    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        R := Drugs[i].BathConcentration/Drugs[i].EC50_AchEsterase ;
        Sum := Sum + R  ;
        end ;
    AchEsterase := sum / (1.0 + sum) ;

    // Nicotinic cholinoceptor receptor activation by agonists in bath

    OccupancySum := 0.0 ;
    EfficacySum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        Conc := Drugs[i].BathConcentration ;
        if ContainsText(Drugs[i].Name,'acetylcholine') then Conc := Conc*(1.0 + AchEsterase*10.0) ;
        R := Conc / Drugs[i].EC50_nAchR ;
        if not Drugs[i].Antagonist then
           begin
//           R := R*(1.0 + AchEsterase*10.0) ;
           EfficacySum := EfficacySum + R*R ;
           end;
        OccupancySum := OccupancySum + R*R  ;
        end ;
    Occupancy := OccupancySum / ( 1. + OccupancySum ) ;
    Efficacy := EfficacySum / ( OccupancySum + 0.001 ) ;
    nAchR :=  Efficacy*Occupancy ;

    // Desensitization of junctional receptors after activation by nicotinic agonists
    nAchRDesensitization := 1.0/(1.0 + (nAchR*nAChR)*6000.0) ;

    // Stimulate nerve
    if NerveStimulationOn then
       begin
       // Nerve released ACh
       if t >= tStimulus then
          begin
          tNerveStimulusDecay := 0.0 ;
          tStimulus := tStimulus + tStimulationPeriod ;
          end;
       end ;

    // Add nerve activated juntional nicotinic receptors to total
    if tNerveStimulusDecay < 10.0 then
       begin
       // Amount of Ach released, increased by block of K channels, reduced by block of Na channels
       MaxReleasedAch :=  5*nAch_ec50*(1.0 + KCHannelBlock*10.0)*(1.0 - NaChannelBlock) ;
       NerveReleasedAch := MaxReleasedAch*(1.0 - exp(-tNerveStimulusDecay/0.1))*exp(-tNerveStimulusDecay/0.25) ;
       R := (NerveReleasedAch*nAchRDesensitization*(1.0 + AchEsterase*10.0))/nAch_EC50 ;
       OccupancySum := OccupancySum + (R*R) ;
       EfficacySum := EfficacySum + (R*R) ;
       Occupancy := OccupancySum / ( 1. + OccupancySum ) ;
       Efficacy := EfficacySum / ( OccupancySum + 0.001 ) ;
       nAchR :=  Efficacy*Occupancy ;
       tNerveStimulusDecay := tNerveStimulusDecay + dt ;
       end ;

    // Stimulate muscle
    if MuscleStimulationOn then
       begin
       // Direct activation of smooth muscle
       if t >= tStimulus then
          begin
          tStimulus := tStimulus + tStimulationPeriod ;
          tMuscleStimulusDecay := 0.0 ;
          end;
       end;

    if tMuscleStimulusDecay < 10.0 then
       begin
       DirectMuscleActivation := 2.0*(1.0 - exp(-tMuscleStimulusDecay/0.1))*exp(-tMuscleStimulusDecay/0.25) ;
       tMuscleStimulusDecay := tMuscleStimulusDecay + dt ;
       end;

    Force := RandG( 0.0, BackgroundNoiseStDev ) ;
    RMax := NextRMax ;
    Force := Force + RMax*Min(nAchR + DirectMuscleActivation,1.0) ;
    // Na channel block
    Force := Force*(1.0 - NaChannelBlock) ;

    // Contraction
    ChanValues[chForce] := RandG( 0.0, BackgroundNoiseStDev ) + Force ;

    t := t + dt ;

    end ;


procedure TModel.DoRatDiaphragmSimulationStep ;
// ------------------------------------------------
// Compute next rat diagphragm simulation time step
// ------------------------------------------------
//
const
    StimulusInterval = 1.0 ;
var
    i : Integer ;
    Conc : Single ;
    nAchR : Single ;
    AchEsterase : Single ;          // Cholinesterase inhibition
    nAchRDesensitization : Single ; // Nicotinic junctional receptor desensitization
    Sum : Single ;
    Efficacy,EfficacySum : Single ;
    Occupancy,OccupancySum : Single ;
//    NerveReleasedAch : Single ;
    R : Single ;
//    RNerve : Single ;
    Force : single ;
    NaChannelBlock : single ;        // Fraction of Na channels blocked
    KChannelBlock : single ;        // Fraction of K channels blocked
    ECCouplingBlock : single ;       // Excitation-contraction coupling block
    QuantalContent : single ;       // No. of transmitter quanta released per stimulus
begin

    // Update drug bath concentrations
    UpdateBathConcentrations ;

    // Stimulate nerve
    if NerveStimulationOn then
       begin
       // Nerve released ACh
       if t >= tStimulus then
          begin
//          NerveReleasedAch := MaxReleasedAch ;
          tNerveStimulusDecay := 0.0 ;
          tStimulus := tStimulus + tStimulationPeriod ;
          end;
       end ;

    // Na channel block (1=complete, 0=none)
    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        R := Drugs[i].BathConcentration/Drugs[i].EC50_NaChannel ;
        Sum := Sum + R  ;
        end ;
    NaChannelBlock := sum / ( 1. + sum ) ;

    // K channel block (1=complete, 0=none)
    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        R := Drugs[i].BathConcentration/Drugs[i].EC50_KChannel ;
        Sum := Sum + R  ;
        end ;
    KChannelBlock := sum / ( 1. + sum ) ;

    if tNerveStimulusDecay < 10.0 then
       begin
       // Quantal content determined by 4th power of Ca/Mg ratio
       r := Drugs[ICaBath].BathConcentration / Drugs[IMgBath].BathConcentration ;
       QuantalContent := (r*r*r*r)/0.4 ;
       MaxReleasedAch :=  5*nAch_ec50*QuantalContent/(1.0 + QuantalContent) ;
       // Amount of Ach released, increased by block of K channels, reduced by block of Na channels
       MaxReleasedAch :=  MaxReleasedAch*(1.0 + KCHannelBlock*10.0)*(1.0 - NaChannelBlock) ;
       // Release time course
       NerveReleasedAch := MaxReleasedAch*(1.0 - exp(-tNerveStimulusDecay/0.1))*exp(-tNerveStimulusDecay/0.2) ;
       tNerveStimulusDecay := tNerveStimulusDecay + dt ;
       end;

    // EC coupling block (1=complete, 0=none)
    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        R := Drugs[i].BathConcentration/Drugs[i].EC50_ECCoupling ;
        Sum := Sum + R  ;
        end ;
    ECCouplingBlock := sum / ( 1. + sum ) ;

    // Cholinesterase inhibition (1=fully inhibited, 0= fully active)
    Sum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        R := Drugs[i].BathConcentration/Drugs[i].EC50_AchEsterase ;
        Sum := Sum + R  ;
        end ;
    AchEsterase := sum / (1.0 + sum) ;


    // Nicotinic cholinoceptor receptor activation by agonists in bath

    OccupancySum := 0.0 ;
    EfficacySum := 0.0 ;
    for i := 0 to NumDrugs-1 do
        begin
        Conc := Drugs[i].BathConcentration ;
        R := Conc / Drugs[i].EC50_nAchR ;
        if not Drugs[i].Antagonist then
           begin
           R := R*(1.0 + AchEsterase*10.0) ;
           EfficacySum := EfficacySum + R*R ;
           end;
        OccupancySum := OccupancySum + R*R  ;
        end ;
    Occupancy := OccupancySum / ( 1. + OccupancySum ) ;
    Efficacy := EfficacySum / ( OccupancySum + 0.001 ) ;
    nAchR :=  Efficacy*Occupancy ;

    // Desensitization of junctional receptors after acivation by nicotinic agonists
    nAchRDesensitization := 1.0/(1.0 + (nAchR*nAChR)*6000.0) ;

    // Add nerve activated juntional nicotinic receptors to total
    R := (NerveReleasedAch*nAchRDesensitization*(1.0 + AchEsterase*10.0))/nAch_EC50 ;
    EfficacySum := {Sum +} (R*R) ;
    OccupancySum := OccupancySum + R*R ;
    Occupancy := OccupancySum / ( 1. + OccupancySum ) ;
    Efficacy := EfficacySum / ( OccupancySum + 0.001 ) ;
    nAchR :=  efficacy*occupancy ;

    // Stimulate muscle
    if MuscleStimulationOn then
       begin
       // Direct activation of smooth muscle
       if t >= tStimulus then
          begin
          tStimulus := tStimulus + tStimulationPeriod ;
          tMuscleStimulusDecay := 0.0 ;
          end;
       end;

    if tMuscleStimulusDecay < 10.0 then
       begin
       DirectMuscleActivation := 2.0*(1.0 - exp(-tMuscleStimulusDecay/0.1))*exp(-tMuscleStimulusDecay/0.2) ;
       tMuscleStimulusDecay := tMuscleStimulusDecay + dt ;
       end;

    RMax := NextRMax ;
    Force := RMax*Min(nAchR + DirectMuscleActivation,1.0) ;
    // Na channel and E-C coupling block
    Force := Force*(1.0 - NaChannelBlock)*(1.0 - ECCouplingBlock) ;

    // Contraction
    ChanValues[chForce] := RandG( 0.0, BackgroundNoiseStDev ) + Force ;

    t := t + dt ;

    end ;



procedure TModel.SetNerveStimulation( Value : Boolean ) ;
// --------------------------
// Set nerve stimulation flag
// --------------------------
begin
     NerveStimulationOn := Value ;
     if NerveStimulationOn then tStimulus := t
                           else tStimulus := 1E30 ;
end;


procedure TModel.SetMuscleStimulation( Value : Boolean ) ;
// --------------------------
// Set Muscle stimulation flag
// --------------------------
begin
     MuscleStimulationOn := Value ;
     if MuscleStimulationOn then tStimulus := t
                            else tStimulus := 1E30 ;
end;




end.
