unit ObSImMain;
// -----------------------------------------------
// Organ Bath Pharmacology Simulation
// (c) J. Dempster, University of Strathclyde 2005
// -----------------------------------------------
// 6/2/2005 V1.0
// 10/2/2005 V1.0.13
//                     .OBS File header size increased to 4096
// 16/11/2006 V1.2 ... File header increased to 502*40
//                     Missing trace after reloading file fixed
//                     1M stock solutions now available
//                     User warned when trying to add more than 1ml with syringe
// 18/09/2007 V1.3 ... Opioid receptor block of GP-ileum nerve stim added
//                     Morphine, loperamide, naloxone
// 27/02/2008 V1.4 ... Rabbit arterial ring prep added
// 02/10/2008 V1.5.0 ... Unknown drugs A (hist ant.) and B (musc. ant.) added
// 09/02/2009 V1.5.1 FP overflow error in Chick Biventer simulation
//                   when no nerve stimulus on, now fixed.
// 12/02/2009 V1.6.0 Jejunum simulation added.
//                   Preparations now selected from dialog box.
//                   Prazosin & propranolol added to arterial ring
//                   In GP Ileum, muscarinic effects of histamine
//                   no longer cause an increase in maximal response
//                   at very high concentrations
// 19/08/2009 V1.6.1 DrugA histamine antagonist EC50=3E-11
//                   DrugB muscarinic antagonist EC50=4E-9
// 23/09/2011 V1.7   Now uses .CHM help file
//                   Time axis of display now has calibration ticks
// 30/09/2011        Ca no longer appears in wash annotation for GPI after clearing reservoir
// 08/11/2011 V1.8   Drug EC50 settings now stored in .OBS data file and restored when reloaded
// 28/02/2012 V1.9   Nifedipine, thapsigargin & SKF96365 added to Arterial Ring Simulation
//                   calcium channels and stores now modelled.
// 09/03/2012 V2.0   Load and Save Experiment now work again
//                   (ANSIChar used for file header data and associated APPEND../READ.. functions (shared.pas)
//                   Zero level in arterial ring experiment now actually rather than 0.3 gms
//                   Densensitisation rate to adrenoceptor stimulation slowed down
// 07/11/2012 V2.1   Histamine no longer acts on muscarinic receptors
//                   (this fixes non-unity Schild plot slope for mepyramine)
//                   Pilocarpine and Hyoscine added at request of Keren Bielby-Clarke (U Bradford)
// 28/11/2012 V2.2   NextRMax now stored in .OBS file.
//                   Measurement cursor no longer disappears at right edge of display
//                   Array Full changed to File data header full and only displayed once
//                   MKPOINTnn= and MKTEXTxx= changed to MKPOnn= and MKTxx= to save space in header
// 14/11/2013 V2.3
// 18/11/2014 V2.5   Updated with 2014 settings for Drugs A and B.
//                   NextRMax now stored correctly in INI file
// 17/12/2014 V2.6   Rabbit jejunum model now incorporates muscarinic receptors on smooth muscle
//                   Adrenaline, Noradrenaline, Ach & Pilocarpine added
// 28/10/2015 V2.7   Additional unknown drugs added and listed in unknowns menu.
//                   GP Ileum: Max nerve released Ach reduced to 25% of EC50 to allow
//                   nerve-evoked contractions to be superimposed on top of low conc CCH additions
//                   Dilution formula page added
//                   Rate of change of drug concentration in bath now limited to 1E-8M per stwp
//                   to avoid overfast transitions when very high drug concs used
//                   Unknowns Drugs now 1,2 A-D
// 10/11/2015        Volumes added now limited to 0.05 - 1 ml.
// 02/03/2016  V2.8  Rate of change of concentration no longer limited but now reduced within
//                   first 100 steps after drug addition.
//                   Better chosen vertical and horizontal calibration bars now used for prints and copy images
//                   Printer exception when no default printer set or printers available now handled
//                   allowing application to start without a printer.
// 25/01/2017 V2.9   Botulinum toxin block of Ach neurotransmission added
//                   with forensic samples A-C. Stock solutions listed as dilution from sample
// 21.08.18   V3.0   Ach_mEC50 and hyoscine increased to make ACH responses similar to those in real guinea pig ileum lab. class
//                   Morphine EC50 increased slightly
//                   Added concentrations now vary with a C.V. of 10% to increase radom variability of responses
// 26/01/17        BTX + antibody renamed Botulinum Tox A+B antibody
// 16.01.19 V3.1     Chart annotation of Unknown drugs now works correctly
//                   Drug A now opioid agonist 10X more potent than morphine
//                   Drug B now adrenoceptor agonist which blocks  transmitter release a GP ileum which is 10X less potent than morphine
// 111.12.19 V3.2    Rabbit Arterial Ring: List out of range error now trapped when no unknown drugs defined
// 15.04.20  V4.0    Multi-device version on FireMonkey platform
// 25.03.22          shared.pas removed from project file header KEY=VALUES strings in file header now saved/read using TStringLIst
// 06.04.22          User dialogs now use ModalBoxFrm
// 01.06.22  V4.0    Web Help link to GitHub Wiki added
// 02.07.22  V4.1    Testing complete on Mac OS
// 23.08.22  V4.1    Build for Windows

interface

uses
  System.SysUtils, System.StrUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.Ani, FMX.TabControl,
  FMX.ListBox, FMX.EditBox, FMX.NumberBox,
  SESNumberBox, FMX.Objects, SESScopeDisplay, System.IOUtils, System.ANsiStrings,
  FMX.Menus, FMX.Platform, ObSimModel, FMX.Layouts, System.Actions, FMX.ActnList ;

const
    MaxPoints = 1000000 ;
    MaxDisplayPoints = 2000 ;
    MaxMarkers = 500 ;
    NumBytesPerMarker = 40 ;
    FileHeaderSize = (MaxMarkers+10)*NumBytesPerMarker ;
    DataFileExtension = '.OBS' ;

    StimulusInterval = 2.0 ;
    MaxSyringeVolume = 1.0 ;

    NormalSoln = 1 ;    // Normal K-H solution
    ZeroCaSoln = 2 ;    // Zero Ca K-H solution

    // Dilution formula result options
    DilVAdd = 0 ;
    DilFBC = 1 ;
    DilStockC = 2 ;
    DilVBath = 3 ;

type

  TMainFrm = class(TForm)
    DisplayGrp: TGroupBox;
    DisplayPage: TTabControl;
    ChartTab: TTabItem;
    ExperimentTab: TTabItem;
    DilutionTab: TTabItem;
    GPIleumSetup: TImageControl;
    BitmapAnimation1: TBitmapAnimation;
    ControlsGrp: TGroupBox;
    TissueGrp: TGroupBox;
    bNewExperiment: TButton;
    StimulusGrp: TGroupBox;
    DrugsTab: TTabControl;
    AgonistTab: TTabItem;
    AntagonistTab: TTabItem;
    UnknownTab: TTabItem;
    ReservoirGrp: TGroupBox;
    bFlushReservoirToBath: TButton;
    bFreshReservoir: TButton;
    bStimulationOn: TButton;
    bStimulationOff: TButton;
    JejunumStimGrp: TGroupBox;
    StyleBook1: TStyleBook;
    cbSolution: TComboBox;
    lbSaltSolution: TLabel;
    cbAgonist: TComboBox ;
    cbAgonistStockConc: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    bAddAgonist: TButton;
    cbAntagonist: TComboBox;
    cbAntagonistStockConc: TComboBox;
    Label3: TLabel;
    Label4: TLabel;
    bAddAntagonist: TButton;
    cbUnknown: TComboBox;
    Label5: TLabel;
    cbUnknownStockConc: TComboBox;
    Label6: TLabel;
    bAddUnknown: TButton;
    edStimFrequency: TSESNumberBox;
    edAntagonistVolume: TSESNumberBox;
    edAgonistVolume: TSESNumberBox;
    edUnknownVolume: TSESNumberBox;
    scDisplay: TScopeDisplay;
    TDisplayPanel: TPanel;
    edTDisplay: TSESNumberBox;
    edStartTime: TSESNumberBox;
    sbDisplay: TScrollBar;
    bTDisplayDouble: TButton;
    bTDisplayHalf: TButton;
    lbTDisplay: TLabel;
    lbStartTime: TLabel;
    StimulationTypeGrp: TGroupBox;
    rbNerve: TRadioButton;
    rbMuscle: TRadioButton;
    ArterialRingSetup: TImageControl;
    JejunumSetup: TImageControl;
    ChickBiventerSetup: TImageControl;
    Timer: TTimer;
    bRecord: TButton;
    bStop: TButton;
    MenuBar1: TMenuBar;
    mnFile: TMenuItem;
    mnNewExperiment: TMenuItem;
    mnLoadExperiment: TMenuItem;
    mnSaveExperiment: TMenuItem;
    mnEdit: TMenuItem;
    mnHelp: TMenuItem;
    mnPrint: TMenuItem;
    mnCopyData: TMenuItem;
    mnCopyImage: TMenuItem;
    mnExit: TMenuItem;
    OpenDialog: TOpenDialog;
    SaveDialog: TSaveDialog;
    DilutionGrp: TGroupBox;
    lbFormula: TLabel;
    cbDilutionResult: TComboBox;
    edDilNum1: TSESNumberBox;
    edDilNum2: TSESNumberBox;
    edDilDen: TSESNumberBox;
    lbEquals: TLabel;
    edDilResult: TSESNumberBox;
    Line1: TLine;
    lbDilEqnNum1: TLabel;
    lbDilEqnNum2: TLabel;
    lbDilEqnDen: TLabel;
    bCalculate: TButton;
    cbTissueType: TComboBox;
    mnWebHelp: TMenuItem;
    procedure FormShow(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
    procedure bNewExperimentClick(Sender: TObject);
    procedure bStimulationOnClick(Sender: TObject);
    procedure bFreshReservoirClick(Sender: TObject);
    procedure bStimulationOffClick(Sender: TObject);
    procedure cbAntagonistChange(Sender: TObject);
    procedure cbUnknownChange(Sender: TObject);
    procedure bRecordClick(Sender: TObject);
    procedure bStopClick(Sender: TObject);
    procedure edTDisplayKeyUp(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);
    procedure bAddAgonistClick(Sender: TObject);
    procedure bFlushReservoirToBathClick(Sender: TObject);
    procedure scDisplayMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Single);
    procedure mnExitClick(Sender: TObject);
    procedure mnCopyDataClick(Sender: TObject);
    procedure mnCopyImageClick(Sender: TObject);
    procedure bAddAntagonistClick(Sender: TObject);
    procedure bAddUnknownClick(Sender: TObject);
    procedure mnNewExperimentClick(Sender: TObject);
    procedure mnLoadExperimentClick(Sender: TObject);
    procedure mnSaveExperimentClick(Sender: TObject);
    procedure mnContentsClick(Sender: TObject);
    procedure mnPrintClick(Sender: TObject);
    procedure bCalculateClick(Sender: TObject);
    procedure cbDilutionResultChange(Sender: TObject);
    procedure bTDisplayHalfClick(Sender: TObject);
    procedure bTDisplayDoubleClick(Sender: TObject);
    procedure edStartTimeKeyUp(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure sbDisplayChange(Sender: TObject);
    procedure Action1Execute(Sender: TObject);
    procedure edStimFrequencyKeyUp(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
    procedure mnWebHelpClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; var KeyChar: Char;
      Shift: TShiftState);

  private
    { Private declarations }
    ADC : Array[0..MaxPoints-1] of SmallInt ;
    NumPointsInBuf : Integer ;   // No. of data points in buffer
    StartPoint : Integer ;
    NumPointsDisplayed : Integer ;
    ChangeDisplayWindow : Boolean ;
    ClearExperiment : Boolean ;

    MarkerList : TStringList ;   // Chart annotation list

    UnsavedData : Boolean ;  // Un-saved data flag
    HelpFilePath : string ;

    procedure NewExperiment ;
    procedure EraseExperimentQuery( ModalQuery : Boolean ) ;

    procedure SetStockConcentrationList(
              iDrug : NativeInt ;
              ComboBox : TComboBox ) ;
    function GetConcentration( MenuText : string ) : Single ;
    procedure AddChartAnnotations ;
    procedure UpdateDisplay( NewPoint : Single ) ;
    procedure AddDrugMarker( ChartAnnotation : String ) ;
    procedure LoadFromFile( FileName : String ) ;
    procedure SaveToFile( FileName : String ) ;
    procedure StopSimulation ;
    procedure UpdateDisplayDuration ;
    procedure SetDilutionEquation ;

    procedure SetComboBoxFontSize(
              ComboBox : TComboBox ;           // Combo box
              FontSize : Integer ) ;           // Size of text


  public
    { Public declarations }
    TissueIndex : Integer ;      // Menu index of tissue type in use
//    InitialMixing : Cardinal ;

    procedure AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                           Keyword : string ;    // Key
                           Value : single        // Value
                           ) ; Overload ;

    procedure AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                           Keyword : string ;    // Key
                           Value : Integer        // Value
                           ) ; Overload ;

    procedure AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                           Keyword : string ;    // Key
                           Value : NativeInt        // Value
                           ) ; Overload ;

    procedure AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                           Keyword : string ;    // Key
                           Value : String        // Value
                           ) ; Overload ;

   function GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                         KeyWord : string ;   // Key
                         Value : single       // Value
                         ) : Single ; Overload ;        // Return value

   function GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                         KeyWord : string ;   // Key
                         Value : Integer       // Value
                         ) : Integer ; Overload ;        // Return value

   function GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                         KeyWord : string ;   // Key
                         Value : NativeInt       // Value
                         ) : NativeInt ; Overload ;        // Return value

   function GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                         KeyWord : string ;   // Key
                         Value : string       // Value
                         ) : string ; Overload ;        // Return value

  function ExtractFloat ( CBuf : string ; Default : Single ) : extended ;
  function ExtractInt ( CBuf : string ) : longint ;

  end;

var
  MainFrm: TMainFrm;

implementation

uses
{$IFDEF MSWINDOWS}
winapi.shellapi,winapi.windows,
{$ENDIF}
{$IFDEF POSIX}
Posix.Stdlib , Posix.Unistd,
{$ENDIF POSIX}
System.Math, FMX.DialogService, ModalBox ;

{$R *.fmx}

const
    MaxADCValue = 2047 ;
    MinADCValue = -2048 ;
    NoiseStDev = 10 ;
    MaxDisplayForce = 20.0 ;



procedure TMainFrm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
// -------------------------------------------
// Check with user if program should be closed
// -------------------------------------------
begin
    if not UnSavedData then CanClose := True
    else
        begin
        ModalBoxFrm.Left := Self.Left + 10 ;
        ModalBoxFrm.Top := Self.Top + 10 ;
        ModalBoxFrm.Caption := 'Close Program' ;
        ModalBoxFrm.MessageText := 'Experiment not saved: Are you sure you want to close the program' ;
        if ModalBoxFrm.ShowModal = mrYes then CanClose := True
                                         else CanClose := False ;
        end;
end;


procedure TMainFrm.FormKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
// ----------------------------
// Process key presses on form
// ----------------------------
begin
//
//   Left and right arrow keys used to move vertical cursor on display

     case key of
          VKLEFT : scDisplay.MoveActiveVerticalCursor(-1) ;
          VKRIGHT : scDisplay.MoveActiveVerticalCursor(1) ;
          end ;
end;


procedure TMainFrm.FormShow(Sender: TObject);
// ------------------------------------------------
// Initialise controls when form is first displayed
// ------------------------------------------------
var
    FileName : String ;
    HelpFileName,LocalHelpFilePath : string ;
    i : Integer ;
begin

     // Find help file
     HelpFileName := 'obsim.chm' ;
     HelpFilePath := ExtractFilePath(ParamStr(0)) + HelpFileName ;
//     TPath.GetTempPath( 512, TempPath ) ;
     LocalHelpFilePath := TPath.GetTempPath + HelpFileName ;
 //    TFile.Copy( PCHar(Application.HelpFile),PCHar(LocalHelpFilePath),  false ) ;
//     if FileExists(LocalHelpFilePath) then Application.HelpFile := LocalHelpFilePath ;

     // Create annotation list
     MarkerList := TStringList.Create ;

     { Setuo chart display }
     scDisplay.MaxADCValue :=  MaxADCValue ;
     scDisplay.MinADCValue := MinADCValue ;
     scDisplay.DisplayGrid := True ;

     scDisplay.MaxPoints := MaxDisplayPoints ;
     scDisplay.NumPoints := 0 ;
     scDisplay.NumChannels := 1 ;

     { Set channel information }
     scDisplay.ChanOffsets[0] := 0 ;
     scDisplay.ChanUnits[0] := 'gms' ;
     scDisplay.ChanName[0] := 'F' ;
     scDisplay.ChanScale[0] := MaxDisplayForce / MaxADCValue ;
     scDisplay.ChanCalBar[0] := 1.0 ;
     scDisplay.yMin[0] := MinADCValue div 10 ;
     scDisplay.yMax[0] := MaxADCValue ;

     scDisplay.ChanVisible[0] := True ;

     scDisplay.xMin := 0 ;
     scDisplay.xMax := scDisplay.MaxPoints-1 ;
     scDisplay.xOffset := 0 ;
     scDisplay.TScale := 1/20.0 ;
     edTDisplay.Min := 1.0/scDisplay.TScale ;
     edTDisplay.Max := 1E5 ;
     edTDisplay.ValueScale := scDisplay.TScale ;
     edStartTime.ValueScale := scDisplay.TScale ;
     edTDisplay.Value := scDisplay.MaxPoints ;

     { Create a set of zero level cursors }
     scDisplay.ClearHorizontalCursors ;
     scDisplay.AddHorizontalCursor( 0, TAlphaColors.Red, True, '' ) ;
     scDisplay.HorizontalCursors[0] := 0 ;

     // Vertical readout cursor
     scDisplay.ClearVerticalCursors ;
     scDisplay.AddVerticalCursor(-1,TAlphaColors.Green, '?y') ;
     scDisplay.VerticalCursors[0] := scDisplay.MaxPoints div 2 ;

     // Dilution calculator
     cbDilutionResult.Clear ;
     cbDilutionResult.Items.Add('Volume to Add');
     cbDilutionResult.Items.Add('Final Bath Conc.');
     cbDilutionResult.Items.Add('Stock Solution Conc.');
     cbDilutionResult.Items.Add('Bath Volume');
     SetComboBoxFontSize( cbDilutionResult, 13 ) ;
     cbDilutionResult.ItemIndex := 0 ;
     SetDilutionEquation ;

     // Create tissue type list
     Model.GetListOfModels( cbTissueType.Items ) ;
     SetComboBoxFontSize( cbTissueType, 13 ) ;
     cbTissueType.ItemIndex := 0 ;

     // Initialise experiment
     NewExperiment ;


     // Load file named in parameter string

     FileName :=  '' ;
     for i := 1 to ParamCount do
         begin
         if i > 1 then FileName := FileName + ' ' ;
         FileName := FileName + ParamStr(i) ;
         end ;

     if ContainsText( ExtractFileExt(FileName),'.obs') then
        begin
        if FileExists(FileName) then LoadFromFile( FileName ) ;
        end ;

     Timer.Enabled := True ;
     Model.InitialMixing := 0 ;

     end;

procedure TMainFrm.SetComboBoxFontSize(
          ComboBox : TComboBox ;           // Combo box
          FontSize : Integer ) ;           // Size of text
// ----------------------------------------
// Set font size of items in combo box list
// ----------------------------------------
var
    i : Integer ;
begin
     for i := 0 to ComboBox.Items.Count -1 do
         begin
         ComboBox.ListBox.ListItems[i].TextSettings.Font.Size := FontSize ;
         ComboBox.ListBox.ListItems[i].StyledSettings := ComboBox.ListBox.ListItems[i].StyledSettings - [TStyledSetting.Size];
         end;
end;


procedure TMainFrm.NewExperiment ;
// ------------------------------------
// Start new experiment with new tissue
// ------------------------------------
var
    i : Integer ;
    ZeroCaSolnAvailable : Boolean ;
begin

    // Select tissue type from menu
    TissueIndex := cbTissueType.ItemIndex ;

    // Initialise model
    Model.InitialiseModel( NativeInt(cbTissueType.Items.Objects[cbTissueType.ItemIndex])) ;

     // Configure experiment options
     Case Model.ModelType of
        tGPIleum : begin
           StimulusGrp.Enabled := True ;
           rbNerve.Text := 'Nerve (10V,1ms)' ;
           rbNerve.IsChecked := True ;
           rbNerve.Enabled := True ;
           JejunumStimGrp.Visible := False ;
           StimulationTypeGrp.Visible := True ;
           rbMuscle.Text := 'Muscle (20V,10ms)' ;
           rbMuscle.IsChecked := False ;
           rbMuscle.Enabled := True ;
           GPIleumSetup.Visible := True ;
           ChickBiventerSetup.Visible := False ;
           ArterialRingSetup.Visible := False ;
           JejunumSetup.Visible := False ;
           ExperimentTab.Text := ' Experimental Setup (Guinea Pig Ileum) ' ;
           UnknownTab.Text := 'Unknown' ;
           ZeroCaSolnAvailable := False ;
           end ;
        tChickBiventer : begin
           StimulusGrp.Enabled := True ;
           rbNerve.Text := 'Nerve' ;
           rbNerve.IsChecked := True ;
           rbNerve.Enabled := True ;
           rbMuscle.Text := 'Muscle' ;
           rbMuscle.Enabled := True ;
           JejunumStimGrp.Visible := False ;
           StimulationTypeGrp.Visible := True ;
           GPIleumSetup.Visible := False ;
           ChickBiventerSetup.Visible := True ;
           ArterialRingSetup.Visible := False ;
           JejunumSetup.Visible := False ;
           ExperimentTab.Text := ' Experimental Setup (Chick Biventer Cervicis) ' ;
           UnknownTab.Text := 'Unknown' ;
           ZeroCaSolnAvailable := True ;
           end ;
        tRatDiaphragm : begin
           StimulusGrp.Enabled := True ;
           rbNerve.Text := 'Nerve' ;
           rbNerve.IsChecked := True ;
           rbNerve.Enabled := True ;
           rbMuscle.Text := 'Muscle' ;
           rbMuscle.Enabled := True ;
           JejunumStimGrp.Visible := False ;
           StimulationTypeGrp.Visible := True ;
           GPIleumSetup.Visible := False ;
           ChickBiventerSetup.Visible := True ;
           ArterialRingSetup.Visible := False ;
           JejunumSetup.Visible := False ;
           ExperimentTab.Text := ' Experimental Setup (Rat Diaphragm Muscle) ' ;
           UnknownTab.Text := 'Ion' ;
           ZeroCaSolnAvailable := True ;
           end ;
        tArterialRing : begin
           StimulusGrp.Enabled := False ;
           rbNerve.IsChecked := False ;
           rbNerve.Enabled := False ;
           rbMuscle.Enabled := False ;
           JejunumStimGrp.Visible := False ;
           StimulationTypeGrp.Visible := True ;
           GPIleumSetup.Visible := False ;
           ChickBiventerSetup.Visible := False ;
           ArterialRingSetup.Visible := True ;
           JejunumSetup.Visible := False ;
           ExperimentTab.Text := ' Experimental Setup (Rabbit Arterial Ring) ' ;
           UnknownTab.Text := 'Unknown' ;
           ZeroCaSolnAvailable := True ;
           end ;
        else begin
           // Jejunum
           StimulusGrp.Enabled := True ;
           JejunumStimGrp.Visible := True ;
           StimulationTypeGrp.Visible := False ;
           GPIleumSetup.Visible := False ;
           ChickBiventerSetup.Visible := False ;
           ArterialRingSetup.Visible := False ;
           JejunumSetup.Visible := True ;
           ExperimentTab.Text := ' Experimental Setup (Rabbit Jejunum) ' ;
           UnknownTab.Text := 'Unknown' ;
           ZeroCaSolnAvailable := True ;
           end ;
        end ;

     // Solutions list
     cbSolution.Clear ;
     cbSolution.Items.AddObject( 'Krebs-Henseleit (normal)', TObject(NormalSoln)) ;
     if ZeroCaSolnAvailable then begin
        cbSolution.Items.AddObject( 'Krebs-Henseleit (0 Ca)', TObject(ZeroCaSoln)) ;
        end ;
     cbSolution.ItemIndex := 0 ;

     // Create list of agonists
     cbAgonist.Clear ;
     Model.GetListOfDrugs( cbAgonist, dtAgonist ) ;
     SetComboBoxFontSize( cbAgonist, 13 ) ;

     if cbAgonist.Items.Count > 0 then
        begin
        cbAgonist.ItemIndex := 0 ;
        // Set up stock soln. concentration list
        SetStockConcentrationList( NativeInt(cbAgonist.Items.Objects[cbAgonist.ItemIndex]),cbAgonistStockConc ) ;
        end ;

     // Create list of agonists
     Model.GetListOfDrugs( cbAntagonist, dtAntagonist ) ;
     SetComboBoxFontSize( cbAntagonist, 13 ) ;
     if cbAntagonist.Items.Count > 0 then
        begin
        cbAntagonist.ItemIndex := 0 ;
        SetStockConcentrationList( NativeInt(cbAntagonist.Items.Objects[cbAntagonist.ItemIndex]),cbAntagonistStockConc ) ;
        end ;

     // Create list of unknown drugs
     Model.GetListOfDrugs( cbUnknown, dtUnknown ) ;
     SetComboBoxFontSize( cbUnknown, 13 ) ;
     if cbUnknown.Items.Count > 0 then
        begin
        cbUnknown.ItemIndex := 0 ;
        SetStockConcentrationList( NativeInt(cbUnknown.Items.Objects[cbAntagonist.ItemIndex]),cbUnknownStockConc ) ;
        end ;

     { Clear buffer  }
     for i := 0 to MaxPoints-1 do ADC[i] := 0 ;
     StartPoint :=  0 ;
     scDisplay.SetDataBuf( @ADC[StartPoint] ) ;
     scDisplay.XOffset := -1 ;
     NumPointsDisplayed := 0 ;
     NumPointsInBuf := 0 ;

     // Clear chart annotation
     MarkerList.Clear ;

     bRecord.Enabled := True ;
     bStop.Enabled := False ;

     sbDisplay.Max := scDisplay.MaxPoints ;
     sbDisplay.Enabled := False ;
     sbDisplay.Value := 0 ;

     bAddAgonist.Enabled := False ;
     bAddAntagonist.Enabled := False ;
     bFlushReservoirToBath.Enabled := False ;

     StopSimulation ;

     UnSavedData := False ;
     ChangeDisplayWindow := True ;

     end ;


procedure TMainFrm.Action1Execute(Sender: TObject);
var
    OK : Boolean ;
begin

     if not UnsavedData then NewExperiment
     else
        begin

        ModalBoxFrm.Left := Self.Left + 10 ;
        ModalBoxFrm.Top := Self.Top + 10 ;
        ModalBoxFrm.Caption := 'New Experiment' ;
        ModalBoxFrm.MessageText := 'Experiment not saved: Are you sure you want to erase it?' ;
        ModalBoxFrm.Show ;

        if ModalBoxFrm.ShowModal = mrYes then OK := True
                                         else OK := False ;
        if OK then NewExperiment ;
        end;


     Log.d('action1');
end;

procedure TMainFrm.AddChartAnnotations ;
// -------------------------------------
// Add drug annotations to chart display
// -------------------------------------
var
    i : Integer ;
    MarkerPosition : Integer ;
begin

     scDisplay.ClearMarkers ;
     for i := 0 to MarkerList.Count-1 do
         begin
         MarkerPosition := Integer(MarkerList.Objects[i]) - scDisplay.XOffset ;
         if (MarkerPosition > 0) and (MarkerPosition < scDisplay.MaxPoints) then
            begin
            scDisplay.AddMarker( MarkerPosition, MarkerList.Strings[i] ) ;
            end ;
         end ;
     end ;


procedure TMainFrm.edStartTimeKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
// ------------------------
// Start time - Key pressed
// ------------------------
begin
    if Key = 13 then
       begin
       sbDisplay.Value := Round(edStartTime.Value) ;
       UpdateDisplayDuration ;
       end;

    end;


procedure TMainFrm.edStimFrequencyKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
// -------------------------------------
// Key pressed in Stimulus frequency box
// -------------------------------------
var
  ChartAnnotation : string ;

begin
    if Key = 13 then
       begin
       Model.StimFrequency := EdStimFrequency.Value ;
       ChartAnnotation := format('Stim %3gHz',[edStimFrequency.Value]) ;
       AddDrugMarker( ChartAnnotation ) ;
       end;
end;

procedure TMainFrm.edTDisplayKeyUp(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
// ------------------------------
// Display duration - key pressed
// ------------------------------
begin
    if Key = 13 then
       begin
       UpdateDisplayDuration ;
       end;
    end;


procedure TMainFrm.UpdateDisplay(
           NewPoint : Single ) ;
// -------------------
// Update chart display
// -------------------
var
    StartPoints : Integer ;
begin

    ADC[NumPointsInBuf] := Round( NewPoint/scDisplay.ChanScale[0] ) ;
    Inc(NumPointsInBuf) ;
    Inc(NumPointsDisplayed) ;

    if NumPointsDisplayed >= scDisplay.MaxPoints then
       begin
       StartPoints := scDisplay.MaxPoints div 10 ;
       NumPointsDisplayed := StartPoints ;
       scDisplay.NumPoints := NumPointsDisplayed-1 ;
       sbDisplay.Max := sbDisplay.Max + scDisplay.MaxPoints ;
       edStartTime.Max := sbDisplay.Max ;
       sbDisplay.Value := NumPointsInBuf - StartPoints + 1 ;
       scDisplay.XOffset := Round(sbDisplay.Value) ;
       scDisplay.SetDataBuf( @ADC[Round(sbDisplay.Value)] ) ;
       edStartTime.Value := scDisplay.XOffset ;
       scDisplay.Repaint ;
       // Add annotations to chart
       AddChartAnnotations ;
       end
    else
       begin
       scDisplay.DisplayNewPoints( NumPointsInBuf - 1 - scDisplay.XOffset ) ;
       end;

    //scDisplay.Invalidate ;

    end ;


procedure TMainFrm.UpdateDisplayDuration ;
// ------------------------------
// Update display window duration
// ------------------------------
begin
    scDisplay.MaxPoints :=  Round(edTDisplay.Value) ;
    scDisplay.XMax := scDisplay.MaxPoints -1 ;
    scDisplay.VerticalCursors[0] := scDisplay.MaxPoints div 2 ;
    scDisplay.XOffset := Round(edStartTime.Value) ;
    sbDisplay.Value := Round(edStartTime.Value) ;
    scDisplay.SetDataBuf( @ADC[Round(sbDisplay.Value)] ) ;

    // Add annotations to chart
    AddChartAnnotations ;

    scDisplay.Repaint ;
    end;




procedure TMainFrm.SetStockConcentrationList(
          iDrug : NativeInt ;
          ComboBox : TComboBox ) ;
// ------------------------------------------
// Set list of available stock concentrations
// ------------------------------------------
var
    i : Integer ;
    x : DOuble ;
begin

//     iDrug := Integer(cbAgonist.Items.Objects[cbAgonist.ItemIndex]) ;
     if Model.Drugs[iDrug].Units = 'ml' then
        begin
        // Set up stock soln. concentration lists
        ComboBox.Clear ;
        ComboBox.Items.Add('1/1 dilution');
        ComboBox.Items.Add('1/10 dilution');
        ComboBox.ItemIndex := 0 ;
        end
     else if Model.Drugs[iDrug].Units = 'mg/ml' then
        begin
        // Set up stock soln. concentration lists
        ComboBox.Clear ;
        x := 10000.0 ;
        for i := 4 Downto -3 do
            begin
            ComboBox.Items.Add( format( '1E%d mg/ml',[i])) ;
            x := x/10.0 ;
            end ;
        ComboBox.ItemIndex := 3 ;
        end
     else
         begin
         // Set up stock soln. concentration lists
         ComboBox.Clear ;
         x := 1.0 ;
         for i := 0 Downto -8 do
            begin
            ComboBox.Items.Add( format( '1E%d M',[i]) ) ;
            x := x/10.0 ;
            end ;
         ComboBox.ItemIndex := 3 ;
         end ;

     // Set font size
     ComboBox.DropDownCount := ComboBox.Items.Count ;
     for i := 0 to ComboBox.Items.Count-1 do
         begin
         ComboBox.ListBox.ListItems[i].TextSettings.Font.Size := 15 ;
         ComboBox.ListBox.ListItems[i].StyledSettings := ComboBox.ListBox.ListItems[i].StyledSettings - [TStyledSetting.Size];
         end ;

     end;


function TMainFrm.GetConcentration( MenuText : string ) : Single ;
// -----------------------------------------------
// Return concentration from stock soln. menu text
// -----------------------------------------------
begin

     if ContainsText( MenuText, 'ml') then
       begin
       // Dilutions
       if ContainsText( MenuText, '1/1' ) then Result := 1.0
       else if ContainsText( MenuText, '1/10' ) then Result := 0.1
       else if ContainsText( MenuText, '1/100' ) then Result := 0.01
       else Result := ExtractFloat( MenuText, 1.0 ) ;
       end
    else if ContainsText( MenuText, 'mM') then
       begin
       // mM concentrations
       Result := ExtractFloat( MenuText, 1.0 )*1E-3 ;
       end
    else if ContainsText( MenuText, 'uM') then
       begin
       // Micromolar concentrations
       Result := ExtractFloat( MenuText, 1.0 )*1E-6 ;
       end
    else
       begin
       // Molar concentrations
       Result := ExtractFloat( MenuText, 1.0 ) ;
       end

end;



procedure TMainFrm.TimerTimer(Sender: TObject);
// ---------------------
// Timed event scheduler
// ---------------------
var
    NewPoint : Single ;
begin

     // Ensure that horizontal cursor remains at zero
     if scDisplay.HorizontalCursors[0] <> 0.0 then scDisplay.HorizontalCursors[0] := 0.0 ;

     if ClearExperiment and ModalBoxFrm.OK then
        begin
        NewExperiment ;
        ClearExperiment := False ;
        end;

     if not bRecord.Enabled then
        begin
        Model.DoSimulationStep ;
        NewPoint := Model.ChanValues[0] ;
        UpdateDisplay( NewPoint ) ;
//        InitialMixing := InitialMixing + 1 ;
        end
     else
        begin
        // Display
        if ChangeDisplayWindow then begin
           scDisplay.XOffset := Round(sbDisplay.Value) ;
           edStartTime.Value := scDisplay.XOffset ;
           scDisplay.SetDataBuf( @ADC[Round(sbDisplay.Value)] ) ;
           scDisplay.MaxPoints := Round(edTDisplay.Value);
           scDisplay.XMax := scDisplay.MaxPoints -1 ;
           scDisplay.NumPoints := Min( NumPointsInBuf-Round(sbDisplay.Value)-1,
                                       Round(sbDisplay.Max - sbDisplay.Value)) ;
           // Add annotations to chart
           AddChartAnnotations ;
           ChangeDisplayWindow := False ;
           scDisplay.Repaint ;
           end ;
        end ;


end;


procedure TMainFrm.AddDrugMarker(
          ChartAnnotation : String
          ) ;
// ------------------------------
// Add drug addition/wash marker
// ------------------------------
begin
     if MarkerList.Count < MaxMarkers then begin
        ChartAnnotation := ReplaceStr( ChartAnnotation, '-00', '-' ) ;
        ChartAnnotation := ReplaceStr( ChartAnnotation, '00E', '0E' ) ;
        MarkerList.AddObject( ChartAnnotation, TObject(NumPointsInBuf) ) ;
        scDisplay.AddMarker( NumPointsInBuf - scDisplay.XOffset, ChartAnnotation ) ;
        end ;
     end ;


procedure TMainFrm.bAddAgonistClick(Sender: TObject);
// --------------------------------------------
// Add volume of agonist stock solution to bath
// --------------------------------------------
var
     StockConcentration : Single ;
     AddedConcentration : Single ;
     iDrug : NativeInt ;
     ChartAnnotation : String ;
begin

     if edAgonistVolume.Value > MaxSyringeVolume then
        begin
        ShowMessage( format('Syringe can only deliver %.1f ml',[MaxSyringeVolume])) ;
        Exit ;
        end ;

     // Add drug
     iDrug := NativeInt(cbAgonist.Items.Objects[cbAgonist.ItemIndex]) ;
     StockConcentration := GetConcentration( cbAgonistStockConc.Items[cbAgonistStockConc.ItemIndex]) ;

     // Calculate change in final bath concentration
     AddedConcentration :=  (StockConcentration*edAgonistVolume.Value) / Model.BathVolume ;
     edAgonistVolume.Value := edAgonistVolume.Value ;

     // Update display bath concentration
     Model.Drugs[iDrug].DisplayBathConcentration := Model.Drugs[iDrug].DisplayBathConcentration
                                                    + AddedConcentration ;

     // Update final bath concentration (with 10% C.V. random variability to simulation variation in response)
     Model.Drugs[iDrug].FinalBathConcentration := Model.Drugs[iDrug].FinalBathConcentration
                                                  + AddedConcentration*RandG(1.0,0.1) ;

     Model.RMax := Model.NextRMax ;

     // Add chart annotation
     ChartAnnotation := format('%s %.3e %s',
                        [Model.Drugs[iDrug].ShortName,
                         Model.Drugs[iDrug].DisplayBathConcentration,
                         Model.Drugs[iDrug].Units] ) ;
     AddDrugMarker( ChartAnnotation ) ;
     Model.InitialMixing := 0 ;
     end;


procedure TMainFrm.bAddAntagonistClick(Sender: TObject);
// -----------------------------------------------
// Add volume of antagonist stock solution to bath
// -----------------------------------------------
var
     StockConcentration : Double ;
     AddedConcentration : Double ;
     iDrug : NativeInt ;
     ChartAnnotation : String ;
begin

     if edAntagonistVolume.Value > MaxSyringeVolume then
        begin
        ShowMessage( format('Syringe can only deliver %.1f ml',[MaxSyringeVolume])) ;
        Exit ;
        end ;

     // Add drug
     iDrug :=NativeInt(cbAntagonist.Items.Objects[cbAntagonist.ItemIndex]) ;
     StockConcentration := GetConcentration( cbAntagonistStockConc.Items[cbAntagonistStockConc.ItemIndex]) ;

     // Calculate change in final bath concentration
     AddedConcentration :=  (StockConcentration*edAntagonistVolume.Value) / Model.BathVolume ;
     edAntagonistVolume.Value := edAntagonistVolume.Value ;

     // Update display bath concentration
     Model.Drugs[iDrug].DisplayBathConcentration := Model.Drugs[iDrug].DisplayBathConcentration
                                            + AddedConcentration ;

     // Update final bath concentration (with 10% C.V. random variability to simulation variation in response)
     Model.Drugs[iDrug].FinalBathConcentration := Model.Drugs[iDrug].FinalBathConcentration
                                            + AddedConcentration*RandG(1.0,0.1) ;

     Model.RMax := Model.NextRMax ;

     // Add chart annotation
     ChartAnnotation := format('%s %.3e %s',
                        [Model.Drugs[iDrug].ShortName,
                         Model.Drugs[iDrug].DisplayBathConcentration,
                         Model.Drugs[iDrug].Units] ) ;
     AddDrugMarker( ChartAnnotation ) ;
     Model.InitialMixing := 0 ;
     end;


procedure TMainFrm.bAddUnknownClick(Sender: TObject);
// -------------------------------------------------
// Add volume of unknown drug stock solution to bath
// -------------------------------------------------
var
     StockConcentration : Single ;
     AddedConcentration : Single ;
     iDrug : NativeInt ;
     ChartAnnotation : String ;
begin

     if edUnknownVolume.Value > MaxSyringeVolume then
        begin
        ShowMessage( format('Syringe can only deliver %.1f ml',[MaxSyringeVolume])) ;
        Exit ;
        end ;

     // Add drug
     iDrug := NativeInt(cbUnknown.Items.Objects[cbUnknown.ItemIndex]) ;
     StockConcentration := GetConcentration( cbUnknownStockConc.Items[cbUnknownStockConc.ItemIndex]) ;

     // Calculate change in final bath concentration
     AddedConcentration :=  (StockConcentration*edUnknownVolume.Value) / Model.BathVolume ;
     edUnknownVolume.Value := edUnknownVolume.Value ;

     // Update display bath concentration
     Model.Drugs[iDrug].DisplayBathConcentration := Model.Drugs[iDrug].DisplayBathConcentration
                                            + AddedConcentration ;

     // Update final bath concentration (with 10% C.V. random variability to simulation variation in response)
     Model.Drugs[iDrug].FinalBathConcentration := Model.Drugs[iDrug].FinalBathConcentration
                                            + AddedConcentration*RandG(1.0,0.1) ;

     Model.RMax := Model.NextRMax ;

     // Add chart annotation
     ChartAnnotation := format('%s %.3e %s',
                        [Model.Drugs[iDrug].ShortName,
                         Model.Drugs[iDrug].DisplayBathConcentration,
                         Model.Drugs[iDrug].Units] ) ;
     AddDrugMarker( ChartAnnotation ) ;
     Model.InitialMixing := 0 ;
     end;


procedure TMainFrm.bCalculateClick(Sender: TObject);
// ----------------------------------
// Calculate selected dilution result
// ----------------------------------
var
    Units : string ;
    Result : single ;
begin

    case cbDilutionResult.ItemIndex of
      DilFBC : Units := 'M' ;
      DilStockC : Units := 'M' ;
      DilVAdd : Units := 'ml' ;
      DilVBath : Units := 'ml' ;
      end;

    if edDilDen.Value <> 0.0 then
       begin
       Result := (edDilNum1.Value * edDilNum2.Value ) / edDilDen.Value ;
       edDilResult.Text := format('%.4g %s',[Result,Units]);
       end
    else edDilResult.Text := 'Error:Divide by 0' ;

    end ;


procedure TMainFrm.bFlushReservoirToBathClick(Sender: TObject);
//  ----------------------------------
// Flush bath with reservoir solution
// ----------------------------------
var
    i : Integer ;
    ChartAnnotation : String ;
begin

     ChartAnnotation := 'Wash (' ;
     for i:= 0 to Model.NumDrugs-1 do
         begin
         Model.Drugs[i].FinalBathConcentration := Model.ReservoirDrugs[i].FinalBathConcentration ;
         Model.Drugs[i].DisplayBathConcentration := Model.ReservoirDrugs[i].DisplayBathConcentration ;
         if (Model.ReservoirDrugs[i].FinalBathConcentration > 0.0) and (i <> Model.iCaBath) then
            begin
            ChartAnnotation := ChartAnnotation + Model.ReservoirDrugs[i].ShortName + ' ' ;
            end ;
         end ;

     // Set salt solution Ca concentration
     if Integer(cbSolution.Items.Objects[cbSolution.ItemIndex])= ZeroCaSoln then
        begin
        Model.Drugs[Model.iCaBath].FinalBathConcentration := 0.0 ;
        Model.Drugs[Model.iCaBath].DisplayBathConcentration := 0.0 ;
        end
     else begin
        Model.Drugs[Model.iCaBath].FinalBathConcentration := 2.5E-3 ;
        Model.Drugs[Model.iCaBath].DisplayBathConcentration := 2.5E-3 ;
        end ;

     // Magnesium concentration (1 mM)
     Model.Drugs[Model.iMgBath].FinalBathConcentration := 1E-3 ;
     Model.Drugs[Model.iMgBath].DisplayBathConcentration := 1E-3 ;

     // Set type of solution in bath
     if Integer(cbSolution.Items.Objects[cbSolution.ItemIndex])= ZeroCaSoln then
        begin
        ChartAnnotation := ChartAnnotation + '0 Ca' ;
        end ;

     ChartAnnotation := ChartAnnotation + ')' ;

     AddDrugMarker( ChartAnnotation ) ;

     Model.InitialMixing := 0 ;

     end;


procedure TMainFrm.bFreshReservoirClick(Sender: TObject);
// -----------------------------------------------------------
// Replace solution in reservoir with fresh drug free solution
// -----------------------------------------------------------
var
    i : Integer ;
    ChartAnnotation : String ;
begin
     // Clear drugs
     for i := 0 to Model.NumDrugs-1 do
         begin
         Model.ReservoirDrugs[i].FinalBathConcentration := 0.0 ;
         Model.ReservoirDrugs[i].DisplayBathConcentration := 0.0 ;
         //Model.ReservoirDrugs[i].BathConcentration := 0.0 ;
         end ;

     // Set salt solution Ca concentration
          // Set salt solution Ca concentration
     if Model.ModelType = tArterialRing then
        begin
        if Integer(cbSolution.Items.Objects[cbSolution.ItemIndex])= ZeroCaSoln then
           begin
           Model.ReservoirDrugs[Model.iCaBath].FinalBathConcentration := 0.0 ;
           Model.ReservoirDrugs[Model.iCaBath].DisplayBathConcentration := 0.0 ;
           end
        else
           begin
           Model.ReservoirDrugs[Model.iCaBath].FinalBathConcentration := 2.5E-3 ;
           Model.ReservoirDrugs[Model.iCaBath].DisplayBathConcentration := 2.5E-3 ;
           end ;
        end ;

     ChartAnnotation := 'New Res.' ;
     AddDrugMarker( ChartAnnotation ) ;

     end;


procedure TMainFrm.bRecordClick(Sender: TObject);
// ----------------
// Start simulation
// ----------------
begin

     // Ensure selected tissue type menu item matches tissue type in use
     if TissueIndex <> cbTissueType.ItemIndex then cbTissueType.ItemIndex := TissueIndex ;

     bRecord.Enabled := False ;
     bStop.Enabled := True ;
     sbDisplay.Enabled := False ;
     bAddAgonist.Enabled := True ;
     bAddAntagonist.Enabled := True ;
     bFlushReservoirToBath.Enabled := True ;
     bFreshReservoir.Enabled := True ;
     bNewExperiment.Enabled := False ;
     TissueGrp.Enabled := False ;
     bStimulationOff.Enabled := False ;
     bStimulationOn.Enabled := True ;

     UnSavedData := True ;

     NumPointsDisplayed := 0 ;
     sbDisplay.Value := NumPointsInBuf + 1 ;
     scDisplay.XOffset := Round(sbDisplay.Value) ;
     scDisplay.SetDataBuf( @ADC[Round(sbDisplay.Value)] ) ;
     sbDisplay.Max := sbDisplay.Max + scDisplay.MaxPoints ;
     scDisplay.NumPoints := 0 ;
     // Add annotations to chart
     AddChartAnnotations ;

     end;


procedure TMainFrm.bStimulationOnClick(Sender: TObject);
// --------------------
// Start nerve stimulus
// --------------------
var
  ChartAnnotation : String ;
begin
     bStimulationOn.Enabled := False ;
     bStimulationOff.Enabled := True ;

     // Add chart annotation
     if Model.ModelType = tJejunum then
        begin
        Model.NerveStimulation := True ;
        Model.StimFrequency := edStimFrequency.Value ;
        edStimFrequency.Value := edStimFrequency.Value ;
        ChartAnnotation := format('Stim %.3gHz',[edStimFrequency.Value]) ;
        AddDrugMarker( ChartAnnotation ) ;
        end
     else
        begin
        if rbNerve.IsChecked then
           begin
           // Start nerve stimulation
           Model.MuscleStimulation := False ;
           Model.NerveStimulation := True ;
           AddDrugMarker( 'Stim(nv.):On' )
           end
        else
           begin
           // Start muscle stimulation
           Model.NerveStimulation := False ;
           Model.MuscleStimulation := True ;
           AddDrugMarker( 'Stim(mu.):On' ) ;
           end;
        end ;

     StimulationTypeGrp.Enabled := False ;

     end;


procedure TMainFrm.bStopClick(Sender: TObject);
// -------------------
// Stop button clicked
// -------------------
begin
    StopSimulation ;
end ;

procedure TMainFrm.bTDisplayDoubleClick(Sender: TObject);
// --------------------------------------------
// Increase display time window duration by 25%
// --------------------------------------------
begin
    edTDisplay.Value := edTDisplay.Value*1.25 ;
    UpdateDisplayDuration ;
end;

procedure TMainFrm.bTDisplayHalfClick(Sender: TObject);
// ----------------------------------
// Reduce display time window by half
// -----------------------------------
begin
    edTDisplay.Value := edTDisplay.Value/1.25 ;
    UpdateDisplayDuration ;
end;


procedure TMainFrm.StopSimulation ;
// ----------------
// Stop simulation
// ----------------
begin
     bRecord.Enabled := True ;
     bStop.Enabled := False ;
     sbDisplay.Enabled := True ;
     bAddAgonist.Enabled := False ;
     bAddAntagonist.Enabled := False ;
     bFlushReservoirToBath.Enabled := False ;
     bFreshReservoir.Enabled := False ;
     bNewExperiment.Enabled := True ;
     TissueGrp.Enabled := True ;
     bStimulationOff.Enabled := False ;
     bStimulationOn.Enabled := True ;
     StimulationTypeGrp.Enabled := True ;
     Model.MuscleStimulation := False ;
     Model.NerveStimulation := False ;

     end;


procedure TMainFrm.bStimulationOffClick(Sender: TObject);
// --------------------
// Stop nerve stimulus
// --------------------
var
  ChartAnnotation : String ;

begin
     bStimulationOn.Enabled := True ;
     bStimulationOff.Enabled := False ;
     StimulationTypeGrp.Enabled := True ;

     // Add chart annotation
     ChartAnnotation := 'Stim:Off' ;
     AddDrugMarker( ChartAnnotation ) ;

     Model.NerveStimulation := False ;
     Model.MuscleStimulation := False ;

     end;


procedure TMainFrm.bNewExperimentClick(Sender: TObject);
// ---------------------
// Select new experiment
// ---------------------
begin
     EraseExperimentQuery( false ) ;
     end;


procedure TMainFrm.EraseExperimentQuery( ModalQuery : Boolean ) ;
// -----------------------------------
// Query user to clear experiment data
// -----------------------------------
begin

     ClearExperiment := True ;
     if not UnSavedData then ModalBoxFrm.OK := True
     else
        begin
        ModalBoxFrm.Left := Self.Left + 10 ;
        ModalBoxFrm.Top := Self.Top + 10 ;
        ModalBoxFrm.Caption := 'New Experiment' ;
        ModalBoxFrm.MessageText := 'Experiment not saved: Are you sure you want to erase it?' ;
        if ModalQuery then ModalBoxFrm.ShowModal
                      else ModalBoxFrm.Show ;
        end ;

     Log.d('Eraseexperimentquery');

end;


procedure TMainFrm.cbAntagonistChange(Sender: TObject);
begin
    SetStockConcentrationList( Integer(cbAntagonist.Items.Objects[cbAntagonist.ItemIndex]),
                               cbAntagonistStockConc) ;
    end;

procedure TMainFrm.cbDilutionResultChange(Sender: TObject);
// ------------------------
// Dilution results changed
// ------------------------
begin
    SetDilutionEquation ;
    end;


Procedure TMainFrm.cbUnknownChange(Sender: TObject);
begin
      SetStockConcentrationList( Integer(cbUnknown.Items.Objects[cbUnknown.ItemIndex]),
                                cbUnknownStockConc ) ;
      end;



procedure TMainFrm.SaveToFile(
          FileName : String
          ) ;
// ----------------------------
// Save chart recording to file
// ----------------------------
var
   ANSIHeaderBuf : array[0..FileHeaderSize] of ansichar ;
   Header : TStringList ;
   i : Integer ;
   FileHandle : THandle ;
begin

     // Create file header Name=Value string list
     Header := TStringList.Create ;

     FileHandle := FileCreate( FileName ) ;
     if Integer(FileHandle) < 0 then Exit ;

     AddKeyValue( Header, 'NPOINTS', NumPointsInBuf ) ;
     AddKeyValue( Header, 'TISTYPE', Model.ModelType ) ;

     AddKeyValue( Header, 'NEXTRMAX', Model.NextRMax ) ;

     // Save drug EC50 settings
     for i := 0 to Model.NumDrugs-1 do
          begin
          AddKeyValue( Header, format('DRG%d_HIST',[i]), Model.Drugs[i].EC50_HistR ) ;
          AddKeyValue( Header, format('DRG%dEC50_HISTNC',[i]),Model.Drugs[i].EC50_HistR_NC);
          AddKeyValue( Header, format('DRG%d_N_ACH',[i]), Model.Drugs[i].EC50_nAchR ) ;
          AddKeyValue( Header, format('DRG%d_M_ACH',[i]), Model.Drugs[i].EC50_mAchR ) ;
          AddKeyValue( Header, format('DRG%dEC50_M_ACHNC',[i]),Model.Drugs[i].EC50_mAchR_NC);
          AddKeyValue( Header, format('DRG%d_OP',[i]), Model.Drugs[i].EC50_OpR ) ;
          AddKeyValue( Header, format('DRG%d_A_ADR',[i]), Model.Drugs[i].EC50_Alpha_AdrenR  ) ;
          AddKeyValue( Header, format('DRG%d_B_ADR',[i]), Model.Drugs[i].EC50_Beta_AdrenR ) ;
          AddKeyValue( Header, format('DRG%d_PLC',[i]), Model.Drugs[i].EC50_PLC_Inhibition ) ;
          AddKeyValue( Header, format('DRG%d_IPS',[i]), Model.Drugs[i].EC50_IP3R ) ;
//          AppendFloat( Header, format('DRG%d_CAI',[i]), Model.Drugs[i].EC50_CaI ) ;
          AddKeyValue( Header, format('DRG%d_CAS',[i]), Model.Drugs[i].EC50_CaStore ) ;
          AddKeyValue( Header, format('DRG%d_CAV',[i]), Model.Drugs[i].EC50_CaChannelV ) ;
          AddKeyValue( Header, format('DRG%d_CAR',[i]), Model.Drugs[i].EC50_CaChannelR ) ;
          end ;

     AddKeyValue( Header, 'NMARKERS', MarkerList.Count ) ;
     for i := 0 to MarkerList.Count-1 do begin
         AddKeyValue( Header, format('MKP%d',[i]), Integer(MarkerList.Objects[i])) ;
         AddKeyValue( Header, format('MKT%d',[i]), MarkerList[i] ) ;
         end ;

     // Get ANSIstring copy of header text adn write to file
//     AnsiHeader := AnsiString(Header.Text) ;
     for i := 0 to Length(Header.Text)-1 do
         begin
         AnsiHeaderBuf[i] := ANSIChar(Header.Text[i+1]);
         end;
     AnsiHeaderBuf[Length(Header.Text)] := #0 ;

//     pAnsiHeader :=  Addr(AnsiHeader[1]);
     FileWrite( FileHandle, AnsiHeaderBuf, Length(Header.Text)) ;

     // Write chart data

     FileSeek( FileHandle, FileHeaderSize, 0 ) ;
     FileWrite( FileHandle, ADC, NumPointsInBuf*2 ) ;
     // Close file
     FileClose( FileHandle ) ;

     // Free header
     Header.Free ;

     UnSavedData := False ;
     end ;


procedure TMainFrm.sbDisplayChange(Sender: TObject);
// ---------------------------
// Scroll bar sosition changed
// ---------------------------
begin
    ChangeDisplayWindow := True ;
end;


procedure TMainFrm.scDisplayMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Single);
// -------------------------------
// Mouse released on chart display
// -------------------------------
var
    ch : Integer ;
begin

     // Ensure that lower limit of display kept less than -10% of max.
     for ch := 0 to scDisplay.NumChannels-1 do
         if scDisplay.YMin[ch] < (MinADCValue div 10) then
            begin
            scDisplay.YMin[ch] := MinADCValue div 10 ;
            end;

     scDisplay.Repaint ;

end;

procedure TMainFrm.LoadFromFile(
          FileName : String
          ) ;
// ----------------------------
// Load chart recording from file
// ----------------------------
var
   AnsiHeaderBuf : Array[0..FileHeaderSize] of ANSIChar ;
   AnsiHeader : ANSIString ;
   Header : TStringList ;
   i : Integer ;
   FileHandle : THandle ;
   NumMarkers : Integer ;
   MarkerPoint : Integer ;
   MarkerText : String ;
   DataStart : Integer ;
begin

     // Create file header Name=Value string list
     Header := TStringList.Create ;

     NumPointsInBuf := 0 ;

     FileHandle := FileOpen( FileName, fmOpenRead ) ;
     if NativeInt(FileHandle) < 0 then Exit ;

     FileSeek( FileHandle, 0, 0 ) ;

     // Read header
     FileRead(FileHandle, ANSIHeaderBuf, FileHeaderSize ) ;
     ANSIHeader := ANSIString( ANSIHeaderBuf ) ;
     Header.Text := String(ANSIHeader) ;

     // Get tissue type
     Model.ModelType := NativeInt(GetKeyValue( Header, 'TISTYPE', Model.ModelType )) ;
     cbTissueType.ItemIndex := cbTissueType.Items.IndexOfObject(TObject(Model.ModelType)) ;
     NewExperiment ;

     NumPointsInBuf := 0 ;
     NumPointsInBuf := GetKeyValue( Header, 'NPOINTS', NumPointsInBuf ) ;

     Model.NextRMax := GetKeyValue( Header, 'NEXTRMAX', Model.NextRMax ) ;

     // Read drug EC50 settings
     for i := 0 to Model.NumDrugs-1 do
          begin
          Model.Drugs[i].EC50_HistR := GetKeyValue( Header, format('DRG%d_HIST',[i]), Model.Drugs[i].EC50_HistR ) ;
          Model.Drugs[i].EC50_HistR_NC := GetKeyValue( Header, format('DRG%dEC50_HISTNC',[i]),Model.Drugs[i].EC50_HistR_NC);
          Model.Drugs[i].EC50_nAchR := GetKeyValue( Header, format('DRG%d_N_ACH',[i]), Model.Drugs[i].EC50_nAchR ) ;
          Model.Drugs[i].EC50_mAchR := GetKeyValue( Header, format('DRG%d_M_ACH',[i]), Model.Drugs[i].EC50_mAchR ) ;
          Model.Drugs[i].EC50_Alpha_AdrenR := GetKeyValue( Header, format('DRG%d_OP',[i]), Model.Drugs[i].EC50_OpR ) ;
          Model.Drugs[i].EC50_Alpha_AdrenR := GetKeyValue( Header, format('DRG%d_A_ADR',[i]), Model.Drugs[i].EC50_Alpha_AdrenR  ) ;
          Model.Drugs[i].EC50_Beta_AdrenR := GetKeyValue( Header, format('DRG%d_B_ADR',[i]), Model.Drugs[i].EC50_Beta_AdrenR ) ;
          Model.Drugs[i].EC50_PLC_Inhibition := GetKeyValue( Header, format('DRG%d_PLC',[i]), Model.Drugs[i].EC50_PLC_Inhibition ) ;
          Model.Drugs[i].EC50_IP3R := GetKeyValue( Header, format('DRG%d_IPS',[i]), Model.Drugs[i].EC50_IP3R ) ;
          Model.Drugs[i].EC50_CaStore := GetKeyValue( Header, format('DRG%d_CAS',[i]), Model.Drugs[i].EC50_CaStore ) ;
          Model.Drugs[i].EC50_CaChannelV := GetKeyValue( Header, format('DRG%d_CAV',[i]), Model.Drugs[i].EC50_CaChannelV ) ;
          Model.Drugs[i].EC50_CaChannelR := GetKeyValue( Header, format('DRG%d_CAR',[i]), Model.Drugs[i].EC50_CaChannelR ) ;
          end ;

     NumMarkers := 0 ;
     NumMarkers := GetKeyValue( Header, 'NMARKERS', NumMarkers ) ;
     MarkerList.Clear ;
     MarkerPoint := 0 ;
     for i := 0 to NumMarkers-1 do
         begin
         MarkerPoint := GetKeyValue( Header, format('MKPOINT%d',[i]), MarkerPoint ) ;
         MarkerPoint := GetKeyValue( Header, format('MKP%d',[i]), MarkerPoint) ;
         MarkerText := GetKeyValue( Header, format('MKTEXT%d',[i]), MarkerText ) ;
         MarkerText := GetKeyValue( Header, format('MKT%d',[i]), MarkerText ) ;
         MarkerList.AddObject( MarkerText, TObject(MarkerPoint)) ;
         end ;

     if NumPointsInBuf > 0 then
        begin
        DataStart := FileSeek( FileHandle, 0,2 ) - NumPointsInBuf*2 ;
        FileSeek( FileHandle, DataStart, 0 );
        FileRead( FileHandle, ADC, NumPointsInBuf*2 ) ;
        end ;

     // Close data file
     FileClose( FileHandle ) ;

     Header.Free ;

     UnsavedData := False ;
     scDisplay.XOffset := 0 ;
     sbDisplay.Value := 0 ;
     sbDisplay.Max := NumPointsInBuf ;

     ChangeDisplayWindow := True ;

     end ;


procedure TMainFrm.mnCopyDataClick(Sender: TObject);
// -----------------------------
// Copy data points to clipboard
// -----------------------------
begin
    scDisplay.CopyDataToClipBoard ;
    end;


procedure TMainFrm.mnCopyImageClick(Sender: TObject);
// -----------------------------
// Copy image to clipboard
// -----------------------------
begin
    scDisplay.TCalBar := (scDisplay.XMax - scDisplay.XMin)*scDisplay.TScale*0.1 ;
    scDisplay.CopyImageToClipBoard ;
    end;


procedure TMainFrm.mnExitClick(Sender: TObject);
// ------------
// Stop Program
// ------------
begin
     Close ;
     end;


procedure TMainFrm.mnContentsClick(Sender: TObject);
// -----------------------
//  Help/Contents menu item
//  -----------------------
begin

    {$IFDEF MSWINDOWS}
     ShellExecute(0,'open', 'c:\windows\hh.exe',PChar(HelpFilePath),
     nil, SW_SHOWNORMAL) ;
    {$ENDIF}

     end;


procedure TMainFrm.mnLoadExperimentClick(Sender: TObject);
// -------------------------
// Load experiment from file
// -------------------------
begin

     EraseExperimentQuery( true ) ;

     if ModalBoxFrm.OK then
        begin

//      OpenDialog.options := [ofPathMustExist] ;
        OpenDialog.FileName := '' ;

        OpenDialog.DefaultExt := DataFileExtension ;
   //OpenDialog.InitialDir := OpenDirectory ;
        OpenDialog.Filter := format( ' Organ Bath Expt. (*%s)|*%s',
                                [DataFileExtension,DataFileExtension]) ;
        OpenDialog.Title := 'Load Experiment ' ;

       // Open selected data file
        if OpenDialog.execute then LoadFromFile( OpenDialog.FileName ) ;

        ModalBoxFrm.OK := False ;
        ClearExperiment := False ;
        end;

   end;


procedure TMainFrm.mnNewExperimentClick(Sender: TObject);
// ---------------------
// Select new experiment
// ---------------------
begin
     EraseExperimentQuery( false ) ;
     end;


procedure TMainFrm.mnPrintClick(Sender: TObject);
// ---------------------
// Print displayed trace
// ---------------------
begin
    scDisplay.Print ;
end;


procedure TMainFrm.mnSaveExperimentClick(Sender: TObject);
// -----------------------
// Save experiment to file
// -----------------------
begin

     { Present user with standard Save File dialog box }
//     SaveDialog.options := [ofHideReadOnly,ofPathMustExist] ;
     SaveDialog.FileName := '' ;
     SaveDialog.DefaultExt := DataFileExtension ;
     SaveDialog.Filter := format( '  Organ Bath Expt. (*%s)|*%s',
                                  [DataFileExtension,DataFileExtension]) ;
     SaveDialog.Title := 'Save Experiment' ;

     if SaveDialog.Execute then SaveToFile( SaveDialog.FileName ) ;

     end ;


procedure TMainFrm.mnWebHelpClick(Sender: TObject);
//
// Web Help - Wiki from GitHub repository
// --------------------------------------
var
  URL: string;
begin
  URL := 'https://github.com/johndempster/ObSimFMX/wiki';
{$IFDEF MSWINDOWS}
  URL := StringReplace(URL, '"', '%22', [rfReplaceAll]);
  ShellExecute(0, 'open', PChar(URL), nil, nil, SW_SHOWNORMAL);
  {$ENDIF}

  {$IFDEF MACOS}
      _system(PAnsiChar('open ' + AnsiString(URL)));
    {$ENDIF}
end;

procedure TMainFrm.SetDilutionEquation ;
begin
    case cbDilutionResult.ItemIndex of
      DilFBC : begin
          lbDilEqnNum1.Text := 'Stock Soln. Conc. (M)';
          lbDilEqnNum2.Text := 'Volume to Add (ml)';
          lbDilEqnDen.Text := 'Bath Volume (ml)';
          edDilNum1.Units := 'M';
          edDilNum2.Units := 'ml';
          edDilDen.Units := 'ml';
          edDilResult.Text := '' ;
          end;
      DilStockC : begin
          lbDilEqnNum1.Text := 'Final Bath Conc. (M)';
          lbDilEqnNum2.Text := 'Bath Volume (ml)';
          lbDilEqnDen.Text := 'Volume to Add (ml)';
          edDilNum1.Units := 'M';
          edDilNum2.Units := 'ml';
          edDilDen.Units := 'ml';
          edDilResult.Text := '' ;
          end;
      DilVAdd : begin
          lbDilEqnNum1.Text := 'Final Bath Conc. (M)';
          lbDilEqnNum2.Text := 'Bath Volume (ml)';
          lbDilEqnDen.Text := 'Stock Soln. Conc. (M)';
          edDilNum1.Units := 'M';
          edDilNum2.Units := 'ml';
          edDilDen.Units := 'M';
          edDilResult.Text := '' ;
          end;
      DilVBath : begin
          lbDilEqnNum1.Text := 'Stock Soln. Conc. (M)';
          lbDilEqnNum2.Text := 'Volume to Add (ml)';
          lbDilEqnDen.Text := 'Final Bath Conc. (M)';
          edDilNum1.Units := 'M';
          edDilNum2.Units := 'ml';
          edDilDen.Units := 'M';
          edDilResult.Text := '' ;
          end;

    end;
end ;


procedure TMainFrm.AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                                KeyWord : string ;    // Key
                                Value : single        // Value
                                 ) ;
// ---------------------
// Add Key=Single Value to List
// ---------------------
begin
     List.Add( Keyword + format('=%.4g',[Value]) ) ;
end;


procedure TMainFrm.AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                                KeyWord : string ;    // Key
                                Value : Integer        // Value
                                 ) ;
// ---------------------
// Add Key=Integer Value to List
// ---------------------
begin
     List.Add( Keyword + format('=%d',[Value]) ) ;
end;

procedure TMainFrm.AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                                KeyWord : string ;    // Key
                                Value : NativeInt        // Value
                                 ) ;
// ---------------------
// Add Key=NativeInt Value to List
// ---------------------
begin
     List.Add( Keyword + format('=%d',[Value] )) ;
end;


procedure TMainFrm.AddKeyValue( List : TStringList ;  // List for Key=Value pairs
                                KeyWord : string ;    // Key
                                Value : string        // Value
                                 ) ;
// ---------------------
// Add Key=string Value to List
// ---------------------
begin
     List.Add( Keyword + '=' + Value ) ;
end;


function TMainFrm.GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                               KeyWord : string ;   // Key
                               Value : single       // Value
                               ) : Single ;         // Return value
// ------------------------------
// Get Key=Single Value from List
// ------------------------------
var
    istart,idx : Integer ;
    s : string ;
begin

     idx := List.IndexOfName( Keyword ) ;
     if idx >= 0 then
        begin
        s := List[idx] ;
        // Find key=value separator and remove key
        istart := Pos( '=', s ) ;
        if istart > 0 then Delete( s, 1, istart ) ;
        Result := ExtractFloat( s, Value ) ;
        end
     else Result := Value ;

end;


function TMainFrm.GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                               KeyWord : string ;   // Key
                               Value : Integer       // Value
                               ) : Integer ;        // Return value
// ------------------------------
// Get Key=Integer Value from List
// ------------------------------
var
    istart,idx : Integer ;
    s : string ;
begin

     idx := List.IndexOfName( Keyword ) ;
     if idx >= 0 then
        begin
        s := List[idx] ;
        // Find key=value separator and remove key
        istart := Pos( '=', s ) ;
        if istart > 0 then Delete( s, 1, istart ) ;
        Result := STrToInt( s ) ;
        end
     else Result := Value ;

end;

function TMainFrm.GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                               KeyWord : string ;   // Key
                               Value : NativeInt       // Value
                               ) : NativeInt ;        // Return value
// ------------------------------
// Get Key=Integer Value from List
// ------------------------------
var
    istart,idx : Integer ;
    s : string ;
begin

     idx := List.IndexOfName( Keyword ) ;
     if idx >= 0 then
        begin
        s := List[idx] ;
        // Find key=value separator and remove key
        istart := Pos( '=', s ) ;
        if istart > 0 then Delete( s, 1, istart ) ;
        Result := STrToInt( s ) ;
        end
     else Result := Value ;

end;


function TMainFrm.GetKeyValue( List : TStringList ;  // List for Key=Value pairs
                               KeyWord : string ;   // Key
                               Value : string       // Value
                               ) : string ;        // Return value
// ------------------------------
// Get Key=Integer Value from List
// ------------------------------
var
    istart,idx : Integer ;
    s : string ;
begin

      idx := List.IndexOfName( Keyword ) ;
     if idx >= 0 then
        begin
        s := List[idx] ;
        // Find key=value separator and remove key
        istart := Pos( '=', s ) ;
        if istart > 0 then Delete( s, 1, istart ) ;
        Result := s ;
        end
     else Result := Value ;

end;


function TMainFrm.ExtractFloat ( CBuf : string ; Default : Single ) : extended ;
{ Extract a floating point number from a string which
  may contain additional non-numeric text }

var CNum : string ;
i : SmallInt ;

begin
     CNum := ' ' ;
     for i := 1 to length(CBuf) do begin
         if CharInSet( CBuf[i], ['0'..'9', 'E', 'e', '+', '-', '.', ',' ] ) then
            CNum := CNum + CBuf[i]
         else CNum := CNum + ' ' ;
         end ;

     { Correct for use of comma/period as decimal separator }
     if (formatsettings.DECIMALSEPARATOR = '.') and (Pos(',',CNum) <> 0) then
        CNum[Pos(',',CNum)] := formatsettings.DECIMALSEPARATOR ;
     if (formatsettings.DECIMALSEPARATOR = ',') and (Pos('.',CNum) <> 0) then
        CNum[Pos('.',CNum)] := formatsettings.DECIMALSEPARATOR ;

     try
        ExtractFloat := StrToFloat( CNum ) ;
     except
        on E : EConvertError do ExtractFloat := Default ;
        end ;
     end ;

function TMainFrm.ExtractInt ( CBuf : string ) : longint ;
{ Extract a 32 bit integer number from a string which
  may contain additional non-numeric text }
Type
    TState = (RemoveLeadingWhiteSpace, ReadNumber) ;
var
   CNum : string ;
   i : integer ;
   Quit : Boolean ;
   State : TState ;

begin
     CNum := '' ;
     i := 1;
     Quit := False ;
     State := RemoveLeadingWhiteSpace ;
     while not Quit do begin

           case State of

           { Ignore all non-numeric ansicharacters before number }
           RemoveLeadingWhiteSpace : begin
               if CharInSet( CBuf[i], ['0'..'9','E','e','+','-','.'] ) then State := ReadNumber
                                                            else i := i + 1 ;
               end ;

           { Copy number into string CNum }
           ReadNumber : begin
                { End copying when a non-numeric ansicharacter
                or the end of the string is encountered }
                if CharInSet( CBuf[i], ['0'..'9','E','e','+','-','.'] ) then begin
                   CNum := CNum + CBuf[i] ;
                   i := i + 1 ;
                   end
                else Quit := True ;
                end ;
           else end ;

           if i > Length(CBuf) then Quit := True ;
           end ;
     try
        ExtractInt := StrToInt( CNum ) ;
     except
        ExtractInt := 1 ;
        end ;
     end ;





end.
