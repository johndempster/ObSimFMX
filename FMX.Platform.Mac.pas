{*******************************************************}
{                                                       }
{              Delphi FireMonkey Platform               }
{                                                       }
{ Copyright(c) 2011-2023 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
{                                                       }
{*******************************************************}
// 10.7.24 Modified to fix Sonomo display scaling problem

unit FMX.Platform.Mac;

interface

{$SCOPEDENUMS ON}

uses
  System.TypInfo, System.Types, System.UITypes, System.Classes, System.Generics.Collections, Macapi.MetalKit,
  Macapi.ObjectiveC, Macapi.CocoaTypes, Macapi.Foundation, Macapi.AppKit, FMX.Types, FMX.Platform, FMX.Text,
  FMX.Forms, FMX.Controls, FMX.Graphics;

type
  TMacWindowHandle = class(TWindowHandle)
  private class var
    FWindowHandles: TList<TMacWindowHandle>;
  private
    FHandle: TOCLocal;
    FBufferSize: TSize;
    FBuffer: CGContextRef;
    FBits: Pointer;
    FTrackingArea: NSTrackingArea;
    function GetWindow: NSWindow;
    function GetView: NSView;
    function GetMTView: MTKView;
    function GetForm: TCommonCustomForm;
    procedure UpdateLayer(const Ctx: CGContextRef);
    procedure CreateBuffer;
    procedure FreeBuffer;
  protected
    function GetScale: Single; override;
  public
    constructor Create(const AHandle: TOCLocal);
    destructor Destroy; override;
    class function FindForm(const window: NSWindow): TCommonCustomForm;
    property Wnd: NSWindow read GetWindow;
    property View: NSView read GetView;
    property MTView: MTKView read GetMTView;
    property TrackingArea: NSTrackingArea read FTrackingArea;
    property Form: TCommonCustomForm read GetForm;
    property Handle: TOCLocal read FHandle;
  end;

{ TFMXAlertDelegate }

  AlertDelegate = interface(NSObject)
    ['{F7AE5530-0A75-4303-8F62-7ABCEB6EF8FE}']
    procedure alertDidEndSelector(alert: Pointer; returnCode: NSInteger; contextInfo: Pointer); cdecl;
  end;

  TFMXAlertDelegate = class(TOCLocal, NSAlertDelegate)
  public
    Modal: Boolean;
    Results: array of Integer;
    Result: Integer;
    constructor Create; //Needed to build this object correctly.
    function GetObjectID: Pointer;
    function GetObjectiveCClass: PTypeInfo; override;
    procedure alertDidEndSelector(alert: Pointer; returnCode: NSInteger; contextInfo: Pointer); cdecl;
  end;

function WindowHandleToPlatform(const AHandle: TWindowHandle): TMacWindowHandle;

procedure RegisterCorePlatformServices;
procedure UnregisterCorePlatformServices;

function PlatformHookObserverCallback(CancelIdle: Boolean; Mask: NSUInteger = NSAnyEventMask): Boolean;
procedure PlatformAlertCreated;
procedure PlatformAlertReleased;
function PlatformAlertCount: Integer;

implementation

uses
  System.RTLConsts, System.SysUtils, System.SyncObjs, System.Rtti, System.Math, System.Messaging,
  System.RegularExpressions, System.StrUtils, System.Variants, System.IOUtils, Macapi.QuartzCore,
  Macapi.KeyCodes, Macapi.CoreGraphics, Macapi.ObjCRuntime, Macapi.CoreFoundation, Macapi.Helpers, Macapi.Mach,
  FMX.Consts, FMX.Dialogs, FMX.Dialogs.Mac, FMX.Menus, FMX.Canvas.Mac, FMX.Canvas.GPU, FMX.Context.Mac, FMX.KeyMapping,
  FMX.ExtCtrls, FMX.Gestures, FMX.Gestures.Mac, FMX.Styles, FMX.TextLayout, FMX.Types3D, FMX.Controls.Mac,
  FMX.Forms.Border.Mac, FMX.Helpers.Mac, FMX.Surfaces, FMX.Context.Metal, FMX.Platform.Screen.Mac,
  FMX.Platform.SaveState.Mac, FMX.Platform.Metrics.Mac, FMX.Platform.Timer.Mac, FMX.Platform.Logger.Mac;

const
  IntervalPress = 1 / SecsPerDay / 10;
  WaitMessageTimeout = 0.1;
  SSpotlightFeature = 'Help';

type

{$M+}

  TEventKind = (Other, Key, MouseOther, MouseDown, MouseUp, MouseMove);

  TEventRec = record
    Event: NSEvent;
    EventKind: TEventKind;
    Window: NSWindow;
    View: NSView;
    Form: TCommonCustomForm;
    FormMousePos: TPointF;
    ScreenMousePos: TPointF;
    Shift: TShiftState;
    Button: TMouseButton;
    MouseInContent: Boolean;
    HandledInApp: Boolean;
    constructor Create(const AEvent: NSEvent);
  end;

function IsPopupForm(const AForm: TCommonCustomForm): Boolean;
begin
  Result := (AForm <> nil) and ((AForm.FormStyle = TFormStyle.Popup) or (AForm.Owner is TPopup) or (AForm is TCustomPopupForm));
end;

  { TEventRec }

constructor TEventRec.Create(const AEvent: NSEvent);
var
  AutoReleasePool: NSAutoreleasePool;
  LMousePoint, LScreenPos: NSPoint;
  R: TRectF;
begin
  // In this record contains the existing information about event.
  FillChar(Self, SizeOf(Self), 0);
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    Self.Event := AEvent;
  {$REGION 'event kind'}
    case AEvent.&type of
      NSKeyDown, NSKeyUp:
        Self.EventKind := TEventKind.Key;
      NSLeftMouseDown, NSRightMouseDown, NSOtherMouseDown:
        Self.EventKind := TEventKind.MouseDown;
      NSLeftMouseUp, NSRightMouseUp, NSOtherMouseUp:
        Self.EventKind := TEventKind.MouseUp;
      NSMouseMoved, NSLeftMouseDragged, NSRightMouseDragged, NSOtherMouseDragged:
        Self.EventKind := TEventKind.MouseMove;
      NSMouseEntered, NSMouseExited, NSScrollWheel:
        Self.EventKind := TEventKind.MouseOther;
    end;
  {$ENDREGION}
  {$REGION 'shift state'}
    Self.Shift := ShiftStateFromModFlags(AEvent.modifierFlags);
    case AEvent.&type of
      NSLeftMouseDown, NSLeftMouseUp, NSLeftMouseDragged:
        Self.Shift := Self.Shift + [ssLeft];
      NSRightMouseDown, NSRightMouseUp, NSRightMouseDragged:
        Self.Shift := Self.Shift + [ssRight];
      NSOtherMouseDown, NSOtherMouseUp, NSOtherMouseDragged:
        Self.Shift := Self.Shift + [ssMiddle];
      NSScrollWheel:
        if Abs(AEvent.deltaX) > Abs(AEvent.deltaY) then
          Self.Shift := Self.Shift + [ssHorizontal];
    end;
    if (Self.EventKind = TEventKind.MouseDown) and (AEvent.clickCount > 1) then
      Self.Shift := Self.Shift + [ssDouble];
  {$ENDREGION}
  {$REGION 'mouse button'}
    case AEvent.&type of
      NSLeftMouseDown, NSLeftMouseUp:
        Self.Button := TMouseButton.mbLeft;
      NSRightMouseDown, NSRightMouseUp:
        Self.Button := TMouseButton.mbRight;
      NSOtherMouseDown, NSOtherMouseUp:
        Self.Button := TMouseButton.mbMiddle;
    end;
  {$ENDREGION}
  {$REGION 'system objects, Form, mouse coodinates'}
    LMousePoint := AEvent.locationInWindow;
    LScreenPos := LMousePoint;
    if AEvent.window <> nil then
    begin
      Self.View := TNSView.Wrap(AEvent.window.contentView);
      Self.Window := AEvent.window;
      Self.Form := TMacWindowHandle.FindForm(AEvent.window);
      Self.FormMousePos := TPointF.Create(LMousePoint.X, Self.View.bounds.size.height - LMousePoint.y);
      LScreenPos := AEvent.window.convertBaseToScreen(LMousePoint);
    end;
    Self.ScreenMousePos := TPointF.Create(LScreenPos.X, MainScreenHeight - LScreenPos.y);
  {$ENDREGION}
  {$REGION 'test cursor pos in form'}
    if Self.Form <> nil then
    begin
      if Self.Form is TCustomPopupForm then
        Self.MouseInContent := TCustomPopupForm(Self.Form).ScreenContentRect.Contains(Self.ScreenMousePos)
      else
      begin
        R := TRectF.Create(0, 0, Self.View.bounds.size.width, Self.View.bounds.size.height);
        Self.MouseInContent := R.Contains(Self.FormMousePos);
      end;
    end;
  {$ENDREGION}
  finally
    AutoReleasePool.release;
  end;
end;

type
  TOpenMenuItem = class(TMenuItem);
  TOpenCustomForm = class(TCommonCustomForm);

  THookEvent = procedure (const EventRec: TEventRec; var CancelIdle, CancelDefaultAction: Boolean) of object;

  IFMXApplicationDelegate = interface(NSApplicationDelegate)
   ['{A54E08CA-77CC-4F22-B6D9-833DD6AB696D}']
    procedure onMenuClicked(sender: NSMenuItem); cdecl;
  end;

  TFMXApplicationDelegate = class(TOCLocal, IFMXApplicationDelegate)
  public
    function applicationShouldTerminate(Notification: NSNotification): NSInteger; cdecl;
    procedure applicationWillTerminate(Notification: NSNotification); cdecl;
    procedure applicationDidFinishLaunching(Notification: NSNotification); cdecl;
    function applicationDockMenu(sender: NSApplication): NSMenu; cdecl;
    procedure applicationDidHide(Notification: NSNotification); cdecl;
    procedure applicationDidUnhide(Notification: NSNotification); cdecl;
    procedure onMenuClicked(sender: NSMenuItem); cdecl;
  end;

  TFMXMenuDelegate = class(TOCLocal, NSMenuDelegate)
  public class var
    FFMXMenuDictionary: TDictionary<Pointer, TMenuItem>;
  public
    class procedure RegisterMenu(const ALocalId: Pointer; const AMenuItem: TMenuItem);
    class procedure UnregisterMenu(const ALocalId: Pointer); overload;
    class procedure UnregisterMenu(const AMenuItem: TMenuItem); overload;
    class function GetMenuItem(const ALocalId: Pointer): TMenuItem;

    procedure menuNeedsUpdate(menu: NSMenu); cdecl;
    function numberOfItemsInMenu(menu: NSMenu): NSInteger; cdecl;
    function menu(menu: NSMenu; updateItem: NSMenuItem; atIndex: NSInteger; shouldCancel: Boolean): Boolean; overload; cdecl;
    procedure menu(menu: NSMenu; willHighlightItem: NSMenuItem); overload; cdecl;
    function menuHasKeyEquivalent(menu: NSMenu; forEvent: NSEvent; target: Pointer; action: SEL): Boolean; cdecl;
    procedure menuWillOpen(menu: NSMenu); cdecl;
    procedure menuDidClose(menu: NSMenu); cdecl;
    function confinementRectForMenu(menu: NSMenu; onScreen: NSScreen): NSRect; cdecl;
  end;

  { TPlatformCocoa }

  TCustomCursor = class
  private
    FData: NSData;
    FImage: NSImage;
    FCursor: NSCursor;
  public
    constructor Create(const ABytes: Pointer; const ALength: NSUInteger);
    destructor Destroy; override;
    property Cursor: NSCursor read FCursor;
  end;

  TFMXWindow = class;

  IMacAppearanceChangedObserver = interface(NSObject)
  ['{DB84EAEC-C328-4461-BF65-7260A15BECBE}']
    [MethodName('observeValueForKeyPath:ofObject:change:context:')]
    procedure observeValueForKeyPath(keypath: Pointer; anObject: pointer; aChange: pointer; aContext: pointer ); cdecl;
  end;

  TMacAppearanceChangedObserver = class(TOCLocal)
  protected
    function GetObjectiveCClass: PTypeInfo; override;
  public
    { NSKeyValueObserving }

    [MethodName('observeValueForKeyPath:ofObject:change:context:')]
    procedure observeValueForKeyPath(keypath: Pointer; anObject: pointer; aChange: pointer; aContext: pointer ); cdecl;
  end;

  TPlatformCocoa = class(TInterfacedObject, IFMXApplicationService, IFMXWindowService, IFMXMenuService,
    IFMXDragDropService, IFMXCursorService, IFMXMouseService, IFMXDialogService, IFMXTextService, IFMXContextService,
    IFMXCanvasService, IFMXWindowBorderService, IFMXHideAppService, IFMXFullScreenWindowService, IFMXKeyMappingService,
    IFMXSystemAppearanceService, IFMXWindowConstraintsService)
  private type
    TCurrentMenu = (System, Default, Main);
    TMainMenuState = (Empty, Created, Recreated, Recreating);
  private
    NSApp: NSApplication;
    FFMXApplicationDelegate: TFMXApplicationDelegate;
    FFMXMenuDelegate: NSMenuDelegate;
    FRunLoopObserver: CFRunLoopObserverRef;
    FAppKitMod: HMODULE;
    FHandleCounter: TFmxHandle;
    FObjectiveCMap: TDictionary<TFmxHandle, IObjectiveC>;
    FNSHandles: TList<TFmxHandle>;
    FModalStack: TStack<TCommonCustomForm>;
    FAlertCount: Integer;
    FRestartModal: Boolean;
    FMenuBar: NSMenu;
    FMainMenu: NSMenu;
    FAppleMenu: NSMenu;
    FWindowMenu: NSMenu;
    FInDestroyMenuItem: Boolean;
    FDefaultMenu: TCurrentMenu;
    FClassHookCount: Integer;
    FTerminating: Boolean;
    FRunning: Boolean;
    FCursor: TCursor;
    FCustomCursor: TCustomCursor;
    FDisableClosePopups: Boolean;
    FCanSetState: Boolean;
    FPerformKeyExecuted: Boolean;
    FHookEvent: THookEvent;
    FMenuStack: TStack<IMenuView>;
    FTitle: string;
    FMainMenuState: TMainMenuState;
    FStoredButtons: NSUInteger;
    FKeyMapping: TKeyMapping;
    FWindows: TList<Integer>;
    { Appearance }
    FAppearanceChangedObserver: TMacAppearanceChangedObserver;
    function NewFmxHandle: TFmxHandle;
    procedure ValidateHandle(FmxHandle: TFmxHandle);
    function HandleToObjC(FmxHandle: TFmxHandle): IObjectiveC;
    function AllocHandle(const Objc: IObjectiveC): TFmxHandle;
    procedure DeleteHandle(FmxHandle: TFmxHandle);
    procedure CreateChildMenuItems(const AChildMenu: IItemsContainer; const AParentMenu: NSMenu);
    procedure DoReleaseWindow(AForm: TCommonCustomForm);
    procedure RemoveChildHandles(const AMenu: IItemsContainer);
    procedure HookFrame(const NSWin: NSWindow);
    procedure UnHookFrame(const NSWin: NSWindow);
    procedure ClearNSHandles;
    function NewNSMenu(const Text: string): NSMenu;
    function KeyProc(const Sender: TObject; const Form: TCommonCustomForm; const event: NSEvent; const IsInputContext,
      IsDown: Boolean): Boolean;
    procedure MouseEvent(const EventRec: TEventRec); overload;
    procedure MouseEvent(const Event: NSEvent); overload;
    procedure ClearMenuItems;
    procedure CreateMenu;
    procedure CreateAppleMenu;
    procedure UpdateAppleMenu(const AMenuItem: TMenuItem);

    procedure WakeMainThread(Sender: TObject);
    function HookObserverCallback(CancelIdle: Boolean; Mask: NSUInteger = NSAnyEventMask): Boolean;
    procedure MenuLoopEvent(const EventRec: TEventRec; var CancelIdle, CancelDefaultAction: Boolean);
    property HookEvent: THookEvent read FHookEvent write FHookEvent;
    function FindFormAtScreenPos(var AForm: TCommonCustomForm; const ScreenMousePos: TPointF): Boolean;
    function ShowContextMenu(const AForm: TCommonCustomForm; const AScreenMousePos: TPointF): Boolean;
    procedure ApplyConstraints(const AWindow: NSWindow; const AMinSize: TSizeF; const AMaxSize: TSizeF);
    { IFMXKeyMappingService }
    /// <summary>Registers a platform key as the given virtual key.</summary>
    function RegisterKeyMapping(const PlatformKey, VirtualKey: Word; const KeyKind: TKeyKind): Boolean;
    /// <summary>Unegisters a platform key as the given virtual key.</summary>
    function UnregisterKeyMapping(const PlatformKey: Word): Boolean;
    /// <summary>Obtains the virtual key from a given platform key.</summary>
    function PlatformKeyToVirtualKey(const PlatformKey: Word; var KeyKind: TKeyKind): Word;
    /// <summary>Obtains the platform key from a given virtual key.</summary>
    function VirtualKeyToPlatformKey(const VirtualKey: Word): Word;

    //Methods for alert creatinon and destruction tracking
    procedure AlertCreated;
    procedure AlertReleased;
    function AlertCount: Integer;
    function CalculateVisibleSubitemsCount(const AMenu: IItemsContainer): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    function DefaultAction(Key: Char; Shift: TShiftState): boolean;

    class function ClosePopupForms: Boolean;
    class function PrepareClosePopups(const SaveForm: TCommonCustomForm): Boolean;

    { IFMXApplicationService }
    procedure Run;
    function HandleMessage: Boolean;
    procedure WaitMessage;
    function GetDefaultTitle: string;
    function GetTitle: string;
    procedure SetTitle(const Value: string);
    function GetVersionString: string;
    function Terminating: Boolean;
    function Running: Boolean;
    procedure Terminate;
    { IFMXHideAppService }
    function GetHidden: Boolean;
    procedure SetHidden(const Value: boolean);
    procedure HideOthers;
    { IFMXWindowService }
    function FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
    function CreateWindow(const AForm: TCommonCustomForm): TWindowHandle;
    procedure DestroyWindow(const AForm: TCommonCustomForm);
    procedure ReleaseWindow(const AForm: TCommonCustomForm);
    procedure ShowWindow(const AForm: TCommonCustomForm);
    procedure HideWindow(const AForm: TCommonCustomForm);
    procedure BringToFront(const AForm: TCommonCustomForm);
    procedure SendToBack(const AForm: TCommonCustomForm);
    procedure Activate(const AForm: TCommonCustomForm);
    function ShowWindowModal(const AForm: TCommonCustomForm): TModalResult;
    function CanShowModal: Boolean;
    procedure InvalidateWindowRect(const AForm: TCommonCustomForm; R: TRectF);
    procedure InvalidateImmediately(const AForm: TCommonCustomForm);
    procedure SetWindowRect(const AForm: TCommonCustomForm; ARect: TRectF);
    function GetWindowRect(const AForm: TCommonCustomForm): TRectF;
    function GetClientSize(const AForm: TCommonCustomForm): TPointF;
    procedure SetClientSize(const AForm: TCommonCustomForm; const ASize: TPointF);
    procedure SetWindowCaption(const AForm: TCommonCustomForm; const ACaption: string);
    procedure SetCapture(const AForm: TCommonCustomForm);
    procedure SetWindowState(const AForm: TCommonCustomForm; const AState: TWindowState);
    procedure ReleaseCapture(const AForm: TCommonCustomForm);
    function ClientToScreen(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
    function ScreenToClient(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
    function GetWindowScale(const AForm: TCommonCustomForm): Single;
    { IFMXWindowConstraintsService }
    procedure SetConstraints(const AForm: TCommonCustomForm; const AMinWidth, AMinHeight, AMaxWidth, AMaxHeight: Single);
    { IFMXWindowBorderService }
    function CreateWindowBorder(const AForm: TCommonCustomForm): TWindowBorder;
    { IFMXMenuService }
    procedure StartMenuLoop(const AView: IMenuView);
    function ShortCutToText(ShortCut: TShortCut): string;
    procedure ShortCutToKey(ShortCut: TShortCut; var Key: Word; var Shift: TShiftState);
    function TextToShortCut(Text: string): Integer;
    procedure CreateOSMenu(AForm: TCommonCustomForm; const AMenu: IItemsContainer);
    procedure UpdateMenuItem(const AItem: IItemsContainer; AChange: TMenuItemChanges);
    procedure DestroyMenuItem(const AItem: IItemsContainer);
    function IsMenuBarOnWindowBorder: Boolean;
    procedure UpdateMenuBar;
    { IFMXDragDropService }
    procedure BeginDragDrop(AForm: TCommonCustomForm; const Data: TDragObject; ABitmap: TBitmap);
    { IFMXCursorService }
    procedure SetCursor(const ACursor: TCursor);
    function GetCursor: TCursor;
    { IFMXMouseService }
    function GetMousePos: TPointF;
    { IFMXDialogService }
    function DialogOpenFiles(const ADialog: TOpenDialog; var AFiles: TStrings; AType: TDialogType): Boolean;
    function DialogPrint(var ACollate, APrintToFile: Boolean;
      var AFromPage, AToPage, ACopies: Integer; AMinPage, AMaxPage: Integer; var APrintRange: TPrintRange;
      AOptions: TPrintDialogOptions): Boolean;
    function PageSetupGetDefaults(var AMargin, AMinMargin: TRect; var APaperSize: TPointF;
      AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
    function DialogPageSetup(var AMargin, AMinMargin :TRect; var APaperSize: TPointF;
      var AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
    function DialogSaveFiles(const ADialog: TOpenDialog; var AFiles: TStrings): Boolean;
    function DialogPrinterSetup: Boolean;
    function MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
        const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: THelpContext;
      const AHelpFileName: string): Integer; overload;
    procedure MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType; const AButtons: TMsgDlgButtons;
      const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: THelpContext; const AHelpFileName: string;
      const ACloseDialogProc: TInputCloseDialogProc); overload;
    function InputQuery(const ACaption: string; const APrompts: array of string;
        var AValues: array of string; const ACloseQueryFunc: TInputCloseQueryFunc = nil): Boolean; overload;
    procedure InputQuery(const ACaption: string; const APrompts, ADefaultValues: array of string;
        const ACloseQueryProc: TInputCloseQueryProc); overload;
    { IFMXTextService }
    function GetTextServiceClass: TTextServiceClass;
    { IFMXContextService }
    procedure RegisterContextClasses;
    procedure UnregisterContextClasses;
    { IFMXCanvasService }
    procedure RegisterCanvasClasses;
    procedure UnregisterCanvasClasses;
    { IFMXFullScreenWindowService }
    procedure SetFullScreen(const AForm: TCommonCustomForm; const AValue: Boolean);
    function GetFullScreen(const AForm: TCommonCustomForm): Boolean;
    procedure SetShowFullScreenIcon(const AForm: TCommonCustomForm; const AValue: Boolean);
    /// <summary>Is specified window destroyed?</summary>
    function IsWindowDestroyed(const AWindowId: Integer): Boolean;
    { IFMXSystemAppearanceService }
    function GetSystemThemeKind: TSystemThemeKind;
    function GetSystemColor(const AType: TSystemColorType): TAlphaColor;
  end;

{$M+}

  TTextServiceCocoa = class;

  TFMXViewBase = class(TOCLocal, NSTextInputClient)
  private
    FOwner: TFMXWindow;
    FShift: TShiftState;
    FMarkedRange: NSRange;
    FBackingStore: NSMutableAttributedString;//NSTextStorage;
    FSelectedRange: NSRange;
    FGestureControl: TComponent;
    FCurrentGestureEngine: TPlatformGestureEngine;
    FSwipePoint: TPointF;
    FEventInfo: TGestureEventInfo;
    FCurrentShift: TShiftState;
    FTrackingArea: NSTrackingArea;
  private
    procedure SetFirstGesturePoint(const AnEvent: NSEvent);
    procedure SetTrackGesturePoint(const AnEvent: NSEvent);
    function GetGestureControlUnderMouse(const APoint: TPointF): IGestureControl;
    function GetGestureEngineUnderMouse(const APoint: TPointF): TPlatformGestureEngine;
    procedure ReleaseGestureEngine;
    procedure DiscardGestureEngine;
    procedure DestroyTrackingArea;
  protected
    function GetNativeView: NSView;
  public
    constructor Create(const AOwner: TFMXWindow);
    destructor Destroy; override;
    function acceptsFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
    function resignFirstResponder: Boolean; cdecl;
    procedure drawRect(dirtyRect: NSRect); cdecl;
    procedure keyDown(event: NSEvent); cdecl;
    procedure keyUp(event: NSEvent); cdecl;
    procedure flagsChanged(theEvent: NSEvent); cdecl;
    procedure scrollWheel(event: NSEvent); cdecl;
    { mouse }
    procedure updateTrackingAreas; cdecl;
    procedure mouseMoved(theEvent: NSEvent); cdecl;
    procedure mouseDown(theEvent: NSEvent); cdecl;
    procedure mouseUp(theEvent: NSEvent); cdecl;
    procedure mouseDragged(theEvent: NSEvent); cdecl;
    procedure mouseExited(theEvent: NSEvent); cdecl;
    procedure rightMouseDown(theEvent: NSEvent); cdecl;
    procedure rightMouseUp(theEvent: NSEvent); cdecl;
    procedure rightMouseDragged(theEvent: NSEvent); cdecl;
    procedure otherMouseDown(theEvent: NSEvent); cdecl;
    procedure otherMouseUp(theEvent: NSEvent); cdecl;
    procedure otherMouseDragged(theEvent: NSEvent); cdecl;
    {Touch and Gestures}
    procedure magnifyWithEvent(event: NSEvent); cdecl;
    procedure rotateWithEvent(event: NSEvent); cdecl;
    procedure swipeWithEvent(event: NSEvent); cdecl;
    procedure touchesBeganWithEvent(event: NSEvent); cdecl;
    procedure touchesCancelledWithEvent(event: NSEvent); cdecl;
    procedure touchesEndedWithEvent(event: NSEvent); cdecl;
    procedure touchesMovedWithEvent(event: NSEvent); cdecl;
    { NSTextInputClient }
    procedure insertText(text: Pointer {NSString}; replacementRange: NSRange); cdecl;
    procedure doCommandBySelector(selector: SEL); cdecl;
    procedure setMarkedText(text: Pointer {NSString}; selectedRange: NSRange; replacementRange: NSRange); cdecl;
    procedure unMarkText; cdecl;
    function selectedRange: NSRange; cdecl;
    function markedRange: NSRange; cdecl;
    function hasMarkedText: Boolean; cdecl;
    function attributedSubstringForProposedRange(aRange: NSRange; actualRange: PNSRange): NSAttributedString; cdecl;
    function validAttributesForMarkedText: Pointer {NSArray}; cdecl;
    function firstRectForCharacterRange(aRange: NSRange; actualRange: PNSRange): NSRect; cdecl;
    function characterIndexForPoint(aPoint: NSPoint): NSUInteger; cdecl;
    function attributedString: NSAttributedString; cdecl;
    function fractionOfDistanceThroughGlyphForPoint(aPoint: NSPoint): CGFloat; cdecl;
    function baselineDeltaForCharacterAtIndex(anIndex: NSUInteger): CGFloat; cdecl;
    function windowLevel: NSInteger; cdecl;
    function drawsVerticallyForCharacterAtIndex(charIndex: NSUInteger): Boolean; cdecl;
    { Text Service }
    function FocusedTextService: TTextServiceCocoa;
    procedure UpdateTextServiceControl;
    { Notifications }
    procedure frameChanged(notification: NSNotification); cdecl;
    { OpenGL }
    procedure lockFocus; cdecl;
    procedure surfaceNeedsUpdate(notification: NSNotification); cdecl;
    { }
    property NativeView: NSView read GetNativeView;
    property Owner: TFMXWindow read FOwner;
  end;

  FMXView = interface(NSView)
    ['{56304E8C-08A2-4386-B116-D4E364FDC2AD}']
    function acceptsFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
    function resignFirstResponder: Boolean; cdecl;
    procedure drawRect(dirtyRect: NSRect); cdecl;
    procedure keyDown(event: NSEvent); cdecl;
    procedure keyUp(event: NSEvent); cdecl;
    procedure flagsChanged(theEvent: NSEvent); cdecl;
    procedure scrollWheel(event: NSEvent); cdecl;
    procedure lockFocus; cdecl;
    procedure surfaceNeedsUpdate(notification: NSNotification); cdecl;
    procedure setLayer(newLayer: CALayer); cdecl;
    { mouse }
    procedure updateTrackingAreas; cdecl;
    procedure mouseMoved(theEvent: NSEvent); cdecl;
    procedure mouseDown(theEvent: NSEvent); cdecl;
    procedure mouseUp(theEvent: NSEvent); cdecl;
    procedure mouseDragged(theEvent: NSEvent); cdecl;
    procedure mouseExited(theEvent: NSEvent); cdecl;
    procedure rightMouseDown(theEvent: NSEvent); cdecl;
    procedure rightMouseUp(theEvent: NSEvent); cdecl;
    procedure rightMouseDragged(theEvent: NSEvent); cdecl;
    procedure otherMouseDown(theEvent: NSEvent); cdecl;
    procedure otherMouseUp(theEvent: NSEvent); cdecl;
    procedure otherMouseDragged(theEvent: NSEvent); cdecl;
    { Touch and Gestures }
    procedure magnifyWithEvent(event: NSEvent); cdecl;
    procedure rotateWithEvent(event: NSEvent); cdecl;
    procedure swipeWithEvent(event: NSEvent); cdecl;
    procedure touchesBeganWithEvent(event: NSEvent); cdecl;
    procedure touchesCancelledWithEvent(event: NSEvent); cdecl;
    procedure touchesEndedWithEvent(event: NSEvent); cdecl;
    procedure touchesMovedWithEvent(event: NSEvent); cdecl;
    { Notifications }
    procedure frameChanged(notification: NSNotification); cdecl;
  end;

  TFMXView = class(TFMXViewBase, NSTextInputClient)
  public
    constructor Create(const AOwner: TFMXWindow; AFRameRect: NSRect);
    destructor Destroy; override;
    function GetObjectiveCClass: PTypeInfo; override;
    procedure setLayer(newLayer: CALayer); cdecl;
  end;

  FMXMTKView = interface(MTKView)
    ['{593CCE76-FAAB-451F-BE22-0F29D13886B6}']
    function acceptsFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
    function resignFirstResponder: Boolean; cdecl;
    procedure drawRect(dirtyRect: NSRect); cdecl;
    procedure keyDown(event: NSEvent); cdecl;
    procedure keyUp(event: NSEvent); cdecl;
    procedure flagsChanged(theEvent: NSEvent); cdecl;
    procedure scrollWheel(event: NSEvent); cdecl;
    procedure lockFocus; cdecl;
    procedure surfaceNeedsUpdate(notification: NSNotification); cdecl;
    procedure setLayer(newLayer: CALayer); cdecl;
    { mouse }
    procedure updateTrackingAreas; cdecl;
    procedure mouseMoved(theEvent: NSEvent); cdecl;
    procedure mouseDown(theEvent: NSEvent); cdecl;
    procedure mouseUp(theEvent: NSEvent); cdecl;
    procedure mouseDragged(theEvent: NSEvent); cdecl;
    procedure mouseExited(theEvent: NSEvent); cdecl;
    procedure rightMouseDown(theEvent: NSEvent); cdecl;
    procedure rightMouseUp(theEvent: NSEvent); cdecl;
    procedure rightMouseDragged(theEvent: NSEvent); cdecl;
    procedure otherMouseDown(theEvent: NSEvent); cdecl;
    procedure otherMouseUp(theEvent: NSEvent); cdecl;
    procedure otherMouseDragged(theEvent: NSEvent); cdecl;
    { Touch and Gestures }
    procedure magnifyWithEvent(event: NSEvent); cdecl;
    procedure rotateWithEvent(event: NSEvent); cdecl;
    procedure swipeWithEvent(event: NSEvent); cdecl;
    procedure touchesBeganWithEvent(event: NSEvent); cdecl;
    procedure touchesCancelledWithEvent(event: NSEvent); cdecl;
    procedure touchesEndedWithEvent(event: NSEvent); cdecl;
    procedure touchesMovedWithEvent(event: NSEvent); cdecl;
    { Notifications }
    procedure frameChanged(notification: NSNotification); cdecl;
  end;

  TFMXMTKView = class(TFMXViewBase, NSTextInputClient)
  public
    constructor Create(const AOwner: TFMXWindow; AFRameRect: NSRect);
    destructor Destroy; override;
    function GetObjectiveCClass: PTypeInfo; override;
    procedure setLayer(newLayer: CALayer); cdecl;
  end;

  FMXWindow = interface(NSWindow)
    ['{A4C4B329-38C4-401F-8937-1C380801B1C8}']
    function draggingEntered(Sender: Pointer): NSDragOperation; cdecl;
    procedure draggingExited(Sender: Pointer {id}); cdecl;
    function draggingUpdated(Sender: Pointer): NSDragOperation; cdecl;
    function performDragOperation(Sender: Pointer): Boolean; cdecl;
    function canBecomeKeyWindow: Boolean; cdecl;
    function canBecomeMainWindow: Boolean; cdecl;
    function acceptsFirstResponder: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
    function resignFirstResponder: Boolean; cdecl;
    function performKeyEquivalent(event: NSEvent): Boolean; cdecl;
  end;

  TFMXWindow = class(TOCLocal) //(NSWindow)
  private
    class var FNextWindowId: Integer;
    class constructor Create;
    class function GenerateWindowId: Integer;
  private
    FId: Integer;
    FViewObj: TFMXViewBase;
    FDelegate: NSWindowDelegate;
    function CanActivate: Boolean; inline;
  protected
    function GetView: NSView;
    procedure UpdateWindowState;
    function GetObjectiveCClass: PTypeInfo; override;
  public
    NeedUpdateShadow: Boolean;
    Wnd: TCommonCustomForm;
    LastEvent: NSEvent; // for DragNDrop
    DragOperation: Integer;
    constructor Create;
    destructor Destroy; override;
    { NSWindow }
    function windowShouldClose(Sender: Pointer {id}): Boolean; cdecl;
    procedure windowWillClose(notification: NSNotification); cdecl;
    procedure windowDidBecomeKey(notification: NSNotification); cdecl;
    procedure windowDidResignKey(notification: NSNotification); cdecl;
    procedure windowDidResize(notification: NSNotification); cdecl;
    procedure windowDidMiniaturize(notification: NSNotification); cdecl;
    procedure windowDidDeminiaturize(notification: NSNotification); cdecl;
    procedure windowDidEnterFullScreen(notification: NSNotification); cdecl;
    procedure windowDidExitFullScreen(notification: NSNotification); cdecl;
    procedure windowDidMove(notification: NSNotification); cdecl;
    procedure windowDidChangeBackingProperties(notification: NSNotification); cdecl;
    function draggingEntered(Sender: Pointer {NSDraggingInfo}): NSDragOperation; cdecl;
    procedure draggingExited(Sender: Pointer {NSDraggingInfo} {id}); cdecl;
    function draggingUpdated(Sender: Pointer {NSDraggingInfo}): NSDragOperation; cdecl;
    function performDragOperation(Sender: Pointer {NSDraggingInfo}): Boolean; cdecl;
    function acceptsFirstResponder: Boolean; cdecl;
    function canBecomeKeyWindow: Boolean; cdecl;
    function canBecomeMainWindow: Boolean; cdecl;
    function becomeFirstResponder: Boolean; cdecl;
    function resignFirstResponder: Boolean; cdecl;
    function performKeyEquivalent(event: NSEvent): Boolean; cdecl;

    /// <summary>Native window content view.</summary>
    property View: NSView read GetView;
    /// <summary>Unique window id.</summary>
    property Id: Integer read FId;
  end;
  PFMXWindow = ^TFMXWindow;

{ TTextServiceCocoa }

  TTextServiceCocoa = class(TTextService)
  private
    FMarkedText: string;
    FMarkedRange: NSRange;
    FSelectedRange: NSRange;
  protected
    procedure MarkedTextPositionChanged; override;
    procedure CaretPositionChanged; override;
    function GetMarketTextAttributes: TArray<TMarkedTextAttribute>; override;
  public
    procedure InternalSetMarkedText(const AMarkedText: string); override;
    function InternalGetMarkedText: string; override;

    procedure InternalStartIMEInput;
    procedure InternalBreakIMEInput;
    procedure InternalEndIMEInput;

    procedure RefreshImePosition; override;

    function TargetClausePosition: TPoint; override;

    procedure EnterControl(const AFormHandle: TWindowHandle); override;
    procedure ExitControl(const AFormHandle: TWindowHandle); override;

    function HasMarkedText: Boolean; override;

    procedure SetMarkedRange(const Value: NSRange);
    procedure SetSelectedRange(const Value: NSRange);

    { Deprecated }

    procedure DrawSingleLine(const ACanvas: TCanvas; const ARect: TRectF; const AFirstVisibleChar: Integer;
      const AFont: TFont; const AOpacity: Single; const AFlags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False); overload; override;
    procedure DrawSingleLine(const ACanvas: TCanvas; const S: string; const ARect: TRectF; const AFont: TFont;
      const AOpacity: Single; const AFlags: TFillTextFlags; const ATextAlign: TTextAlign;
      const AVTextAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False); overload; override;
  end;

procedure SetMenuBitmap(const ANSMenuItem: NSMenuItem; const Item: TMenuItem);
var
  Img: NSImage;
  LBitmap: TBitmap;
begin
  LBitmap := nil;
  if (ANSMenuItem <> nil) and (Item <> nil) then
  begin
    if Item.Images <> nil then
      LBitmap := Item.Images.Bitmap(TSizeF.Create(16, 16), Item.ImageIndex);
    if LBitmap = nil then
      LBitmap := Item.Bitmap;
    if (LBitmap <> nil) and not LBitmap.IsEmpty then
      Img := BitmapToMenuBitmap(LBitmap)
    else
      Img := nil;
    try
      ANSMenuItem.setImage(Img);
    finally
      if Img <> nil then
        Img.release;
    end;
  end;
end;

var
  PlatformCocoa: TPlatformCocoa;
  MultiDisplayMac: TMacMultiDisplay;

procedure RegisterCorePlatformServices;
var
  MetricsService: TMacMetricsServices;
begin
  PlatformCocoa := TPlatformCocoa.Create;
  TPlatformServices.Current.AddPlatformService(IFMXApplicationService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXHideAppService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXTimerService, TMacTimerService.Create);
  TPlatformServices.Current.AddPlatformService(IFMXWindowService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXWindowConstraintsService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXMenuService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXDragDropService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXCursorService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXMouseService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXDialogService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXTextService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXContextService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXCanvasService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXWindowBorderService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXLoggingService, TMacLoggerService.Create);
  TPlatformServices.Current.AddPlatformService(IFMXFullScreenWindowService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXKeyMappingService, PlatformCocoa);
  TPlatformServices.Current.AddPlatformService(IFMXSystemAppearanceService, PlatformCocoa);

  MetricsService := TMacMetricsServices.Create;
  TPlatformServices.Current.AddPlatformService(IFMXLocaleService, MetricsService);
  TPlatformServices.Current.AddPlatformService(IFMXDefaultMetricsService, MetricsService);
  TPlatformServices.Current.AddPlatformService(IFMXSystemInformationService, MetricsService);
  TPlatformServices.Current.AddPlatformService(IFMXSystemFontService, MetricsService);
  TPlatformServices.Current.AddPlatformService(IFMXListingService, MetricsService);

  MultiDisplayMac := TMacMultiDisplay.Create;
  TPlatformServices.Current.AddPlatformService(IFMXMultiDisplayService, MultiDisplayMac);
  TPlatformServices.Current.AddPlatformService(IFMXScreenService, MultiDisplayMac);
  TPlatformServices.Current.AddPlatformService(IFMXDeviceMetricsService, MultiDisplayMac);

  TPlatformServices.Current.AddPlatformService(IFMXSaveStateService, TMacSaveStateService.Create);
end;

procedure UnregisterCorePlatformServices;
begin
end;

type
  TMouseDownTimer = class(TTimer)
  private
    FEventRec: TEventRec;
    procedure TimerProc(Sender: TObject);
  protected
    procedure DoOnTimer; override;
  public
    constructor Create(const EventRec: TEventRec); reintroduce;
  end;

{ TMouseDownTimer }

constructor TMouseDownTimer.Create(const EventRec: TEventRec);
begin
  inherited Create(EventRec.Form);
  FEventRec := EventRec;
  OnTimer := TimerProc;
  Interval := 1;
  Enabled := True;
end;

{$WARN SYMBOL_DEPRECATED OFF}
procedure TMouseDownTimer.DoOnTimer;
begin
  Enabled := False;
  if FEventRec.Form <> nil then
    inherited;
  Release;
end;
{$WARN SYMBOL_DEPRECATED ON}

procedure TMouseDownTimer.TimerProc(Sender: TObject);
begin
  TPlatformCocoa.PrepareClosePopups(nil);
  TPlatformCocoa.ClosePopupForms;

  if (ssRight in FEventRec.Shift) or ((ssLeft in FEventRec.Shift) and (ssCtrl in FEventRec.Shift)) then
  begin
    try
      // Make sure the forms FMousePos has the correct coordinates for this event
      FEventRec.Form.MouseDown(FEventRec.Button, FEventRec.Shift, FEventRec.FormMousePos.X, FEventRec.FormMousePos.Y);
      PlatformCocoa.ShowContextMenu(FEventRec.Form, FEventRec.ScreenMousePos);
      FEventRec.Form.MouseUp(FEventRec.Button, FEventRec.Shift, FEventRec.FormMousePos.X, FEventRec.FormMousePos.Y);
    except
      HandleException(FEventRec.Form);
    end;
  end;
end;

type
  TKeyDownInfo = record
    VKKeyCode: Integer;
    Key: Word;
    KeyChar: Char;
    Initialized: Boolean;
  end;

  TDownKeyList = class(TList<TKeyDownInfo>)
  strict private
    class var FCurrent: TDownKeyList;
    class function GetCurrent: TDownKeyList; static;
  private
    procedure AddKeyDown(const VKKeyCode: Integer);
    procedure InitializeKeyDown(const Key: Word; const KeyChar: Char);
    function RemoveKeyDown(const VKKeyCode: Integer; var Key: Word; var KeyChar: Char): Boolean;
    class destructor UnInitialize;
    class property Current: TDownKeyList read GetCurrent;
  end;

  { TDownKeyList }

class function TDownKeyList.GetCurrent: TDownKeyList;
begin
  if FCurrent = nil then
    FCurrent := TDownKeyList.Create;
  Result := FCurrent;
end;

class destructor TDownKeyList.UnInitialize;
begin
  FCurrent.DisposeOf;
  FCurrent := nil;
end;

procedure TDownKeyList.AddKeyDown(const VKKeyCode: Integer);
var
  Info: TKeyDownInfo;
begin
  if (VKKeyCode > 0) and ((Count = 0) or (Last.VKKeyCode <> VKKeyCode)) then
  begin
    FillChar(Info, SizeOf(Info), 0);
    Info.VKKeyCode := VKKeyCode;
    Add(Info);
  end;
end;

procedure TDownKeyList.InitializeKeyDown(const Key: Word; const KeyChar: Char);
var
  Info: TKeyDownInfo;
  I, LastIndex: Integer;
begin
  LastIndex := -1;
  for I := Count - 1 downto 0 do
    if not Items[I].Initialized then
      LastIndex := I
    else
      Break;

  if LastIndex >= 0 then
  begin
    Info := Items[LastIndex];
    Info.Key := Key;
    Info.KeyChar := KeyChar;
    Info.Initialized := True;
    Items[LastIndex] := Info;
    for I := Count - 1 downto LastIndex + 1 do
      Delete(I);
  end;
end;

function TDownKeyList.RemoveKeyDown(const VKKeyCode: Integer; var Key: Word; var KeyChar: Char): Boolean;
var
  Info: TKeyDownInfo;
  I: Integer;
begin
  Result := False;
  if VKKeyCode > 0 then
    for I := Count - 1 downto 0 do
    begin
      Info := Items[I];
      if Info.VKKeyCode = VKKeyCode then
      begin
        Delete(I);
        if not Result and Info.Initialized then
        begin
          Key := Info.Key;
          KeyChar := Info.KeyChar;
          Result := True;
        end;
      end;
    end;
end;

procedure DoUpdateKey(var Key: Word; var Ch: WideChar; var Shift: TShiftState);
begin
  //Zero out Key, if you pressed the usual character
  if (Ch >= ' ') and ((Shift * [ssCtrl, ssCommand]) = []) then
  begin
    Exclude(Shift, ssAlt);
    Key := 0;
  end;
  //Zero out Ch, if you pressed a keypad of the Alt, Ctrl, or Cmd
  if (([ssAlt, ssCtrl, ssCommand] * Shift) <> []) and (Key > 0) then
    Ch := #0;
end;

procedure DoKeyUp(const Sender: TObject; Form: TCommonCustomForm; Key: Word; Ch: WideChar; Shift: TShiftState);
begin
  if Form <> nil then
  try
    if not TDownKeyList.Current.RemoveKeyDown(Key, Key, Ch) then
      DoUpdateKey(Key, Ch, Shift);
    Form.KeyUp(Key, Ch, Shift);
  except
    HandleException(Sender);
  end;
end;

function DoKeyDown(const Sender: TObject; const Form: TCommonCustomForm; var Key: Word; var Ch: WideChar;
  Shift: TShiftState): boolean;
begin
  TDownKeyList.Current.AddKeyDown(Key);
  Result := False;
  if Form <> nil then
  try
    DoUpdateKey(Key, Ch, Shift);
    TDownKeyList.Current.InitializeKeyDown(Key, Ch);
    Form.KeyDown(Key, Ch, Shift);
    Result := Key = 0;
  except
    HandleException(Sender);
  end;
end;

var
  NSFMXPBoardtype: NSString;

function SendOSXMessage(const Sender: TObject; const OSXMessageClass: TOSXMessageClass; const NSSender: NSObject): NSObject;
var
  MessageObject: TOSXMessageObject;
begin
  if OSXMessageClass = nil then
    raise EArgumentNilException.Create(SArgumentNil);
  MessageObject := TOSXMessageObject.Create(NSSender);
  try
    TMessageManager.DefaultManager.SendMessage(Sender, OSXMessageClass.Create(MessageObject, False), True);
    Result := MessageObject.ReturnValue;
  finally
    MessageObject.Free;
  end;
end;

{ TApplicationDelegate }

function TFMXApplicationDelegate.applicationDockMenu(sender: NSApplication): NSMenu;
var
  ReturnValue: NSObject;
begin
  ReturnValue := SendOSXMessage(Self, TApplicationDockMenuMessage, sender);
  if ReturnValue <> nil then
    Result := ReturnValue as NSMenu
  else
    Result := nil;
end;

function TFMXApplicationDelegate.applicationShouldTerminate(Notification: NSNotification): NSInteger;
begin
  if (Application = nil) or (PlatformCocoa = nil) or PlatformCocoa.Terminating or
    PlatformCocoa.DefaultAction('Q', [ssCommand]) then
    Result := NSTerminateNow
  else
    Result := NSTerminateCancel;
end;

procedure TFMXApplicationDelegate.applicationDidHide(Notification: NSNotification);
begin
end;

procedure TFMXApplicationDelegate.applicationWillTerminate(Notification: NSNotification);
begin
  SendOSXMessage(Self, TApplicationWillTerminateMessage, Notification);
end;

procedure TFMXApplicationDelegate.applicationDidUnhide(Notification: NSNotification);
begin
end;

procedure TFMXApplicationDelegate.onMenuClicked(sender: NSMenuItem);
begin
  SendOSXMessage(Self, TApplicationMenuClickedMessage, sender);
end;

procedure TFMXApplicationDelegate.applicationDidFinishLaunching(Notification: NSNotification);
begin
  SendOSXMessage(Self, TApplicationDidFinishLaunchingMessage, Notification);
end;

{ TPlatformCocoa }

constructor TPlatformCocoa.Create;
var
  AutoReleasePool: NSAutoreleasePool;
begin
  inherited;
  FDefaultMenu := TCurrentMenu.System;
  FAppKitMod := LoadLibrary('/System/Library/Frameworks/AppKit.framework/AppKit');
  AutoReleasePool := TNSAutoreleasePool.Alloc;
  try
    AutoReleasePool.init;
    NSApp := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
    FFMXApplicationDelegate := TFMXApplicationDelegate.Create;
    FFMXMenuDelegate := TFMXMenuDelegate.Create;
    NSApp.setDelegate(IFMXApplicationDelegate(FFMXApplicationDelegate));
    Application := TApplication.Create(nil);
    FObjectiveCMap := TDictionary<TFmxHandle, IObjectiveC>.Create;
  finally
    AutoReleasePool.release;
  end;
  FNSHandles := TList<TFmxHandle>.Create;
  NSFMXPBoardtype := StrToNSStr('NSFMXPBoardtype' + UIntToStr(NativeUInt(Pointer(Application))));
  FCanSetState := True;
  FRunning := False;
  FKeyMapping := TKeyMapping.Create;
  FWindows := TList<Integer>.Create;
  FAppearanceChangedObserver := TMacAppearanceChangedObserver.Create;
  NSApp.addObserver(NSObject(FAppearanceChangedObserver.Super),
                    StrToNSStr('effectiveAppearance'),
                    NSKeyValueObservingOptionNew,
                    nil);

  System.Classes.WakeMainThread := WakeMainThread;
end;

destructor TPlatformCocoa.Destroy;
begin
  NSApp.removeObserver(NSObject(FAppearanceChangedObserver.Super), StrToNSStr('effectiveAppearance'));
  FreeAndNil(FAppearanceChangedObserver);
  FreeAndNil(FWindows);
  ClearNSHandles;
  FNSHandles.Free;
  FreeAndNil(FMenuStack);
  FreeAndNil(FModalStack);
  FreeLibrary(FAppKitMod);
  FreeAndNil(Application);
  FObjectiveCMap.Free;
  TFMXMenuDelegate.FFMXMenuDictionary.Free;
  FKeyMapping.Free;
  inherited;
end;

function TPlatformCocoa.DefaultAction(Key: Char; Shift: TShiftState): boolean;
var
  Form: TCommonCustomForm;
begin
  Result := False;
  if Shift = [ssCommand] then
  begin
    if Key = 'Q' then
    begin
      try
        if Application.MainForm <> nil then
        begin
          Application.MainForm.Close;
          if not Terminating then
            Exit;
        end
        else
        begin
          if Screen <> nil then
            Screen.ActiveForm := nil;
          Application.Terminate;
        end;
      except
        HandleException(Application);
      end;
      Result := True;
    end
    else if Key = 'H' then
    begin
      SetHidden(not GetHidden);
      Result := True;
    end
    else if (Key = 'W') and (Screen <> nil) then
    begin
      Form := Screen.ActiveForm;
      if (Form <> nil) and (not (TFmxFormState.Modal in Form.FormState)) then
      begin
        Form.Close;
        Result := True;
      end;
    end;
  end;
  if (Shift = [ssAlt, ssCommand]) and (Key = 'H') then
  begin
    HideOthers;
    Result := True;
  end;
end;

function VKeyFromKeyCode(AKeyCode: Word; var ISFNkey: boolean): Integer;
var
  Kind: TKeyKind;
begin
  Result := PlatformCocoa.PlatformKeyToVirtualKey(AKeyCode, Kind);
  ISFNkey := False;
  ISFNkey := Kind = TKeyKind.Functional;
end;

function TPlatformCocoa.KeyProc(const Sender: TObject; const Form: TCommonCustomForm; const Event: NSEvent;
  const IsInputContext, IsDown: Boolean): Boolean;

  function IsApplicationCloseShortcut(const AKey: Char; const AShift: TShiftState): Boolean;
  begin
    Result := (AShift = [ssCommand]) and (AKey = 'Q');
  end;

var
  Shift: TShiftState;
  Key: Word;
  Ch: WideChar;
  ShortcutKey: string;
  I: Integer;
  NSChars: NSString;
  ISFNkey, IsModified: Boolean;
  LHandle: TMacWindowHandle;
begin
  Result := False;
  Shift := ShiftStateFromModFlags(Event.modifierFlags);
  IsModified := [ssAlt, ssCtrl, ssCommand] * Shift <> [];
  Key := VKeyFromKeyCode(Event.keyCode, ISFNkey);
  Ch := #0;
  // Do not handle the modified letters in the KeyDown event
  if not IsInputContext and not ISFNkey and IsDown then
  begin
    TDownKeyList.Current.AddKeyDown(Key);
    Exit;
  end;
  if (Key <> 0) and (IsInputContext or ISFNkey) then
  begin
    if not IsInputContext then
      Result := Key < vkSpace
    else
      Result := ISFNkey;
  end;
  if Result or ISFNkey or IsModified then
  begin
    Result := False;
    if Form <> nil then
    begin
      LHandle := WindowHandleToPlatform(Form.Handle);
      if (LHandle.Handle is TFMXWindow) and (not IsInputContext or (Key <> vkEscape)) then
      begin
        if IsModified and not ISFNkey then
        begin
          NSChars := Event.charactersIgnoringModifiers;
          if NSChars <> nil then
            ShortcutKey := NSStrToStr(NSChars);
          for I := 0 to ShortcutKey.Length - 1 do
            if (UpCase(ShortcutKey.Chars[I]) >= 'A') and (UpCase(ShortcutKey.Chars[I]) <= 'Z') then
              Key := Word(UpCase(ShortcutKey.Chars[I]));
        end;
        if IsDown then
          Result := DoKeyDown(Sender, Form, Key, Ch, Shift)
        else
          DoKeyUp(Sender, Form, Key, Ch, Shift);
      end;
    end;
    // We don't need to process Cmd+Q, because Cmd+Q is processed in TFMXApplicationDelegate.applicationShouldTerminate
    if not Result and IsDown and not IsApplicationCloseShortcut(Char(Key), Shift) then
      Result := DefaultAction(Char(Key), Shift);
  end
  else
  begin
    // handle is not modified by the letter
    NSChars := Event.charactersIgnoringModifiers;
    if NSChars <> nil then
      ShortcutKey := NSStrToStr(NSChars);
    for I := 0 to ShortcutKey.Length - 1 do
    begin
      Ch := ShortcutKey.Chars[I];
      Key := Word(Ch);
      if IsDown then
        Result := DoKeyDown(Sender, Form, Key, Ch, Shift)
      else
        DoKeyUp(Sender, Form, Key, Ch, Shift);
    end;
    if not IsInputContext then
      Result := True;
  end;
end;

procedure TPlatformCocoa.MouseEvent(const EventRec: TEventRec);
var
  LHandle: TMacWindowHandle;
  MenuDisplayed: Boolean;
  OldDisableClosePopups: Boolean;
  Obj: IControl;

  procedure InitLastEvent;
  begin
    if (EventRec.Form <> nil) and (EventRec.Form.Handle <> nil) and
      (EventRec.EventKind in [TEventKind.MouseDown, TEventKind.MouseUp, TEventKind.MouseMove]) then
    begin
      LHandle := WindowHandleToPlatform(EventRec.Form.Handle);
      if LHandle.Handle is TFMXWindow then
      begin
        if EventRec.EventKind = TEventKind.MouseUp then
          TFMXWindow(LHandle.Handle).LastEvent := nil
        else
          TFMXWindow(LHandle.Handle).LastEvent := EventRec.Event;
      end;
    end;
  end;

  procedure CleanLastEvent;
  begin
    if (EventRec.Form <> nil) and (EventRec.Form.Handle <> nil) and
      (EventRec.EventKind = TEventKind.MouseMove) and (LHandle.Handle is TFMXWindow) then
    begin
      LHandle := WindowHandleToPlatform(EventRec.Form.Handle);
      if LHandle.Handle is TFMXWindow then
        TFMXWindow(LHandle.Handle).LastEvent := nil;
    end;
  end;

begin
  if EventRec.Form = nil then
    Exit;
  InitLastEvent;
  try
    case EventRec.EventKind of
      TEventKind.MouseDown:
        begin
          // Popups
          PrepareClosePopups(EventRec.Form);
          // Activate
          if not EventRec.Form.Active and (EventRec.Form.FormStyle <> TFormStyle.Popup) then
          begin
            OldDisableClosePopups := FDisableClosePopups;
            try
              FDisableClosePopups := True;
              Activate(EventRec.Form);
            finally
              FDisableClosePopups := OldDisableClosePopups;
            end;
            Obj := EventRec.Form.ObjectAtPoint(EventRec.ScreenMousePos);
            if (Obj <> nil) and (Obj.GetObject is TControl) and (EventRec.Button = TMouseButton.mbLeft) and
              (TControl(Obj.GetObject).DragMode = TDragMode.dmAutomatic) then
              EventRec.Form.MouseDown(EventRec.Button, EventRec.Shift, EventRec.FormMousePos.X,
                EventRec.FormMousePos.Y)
            else
            begin
              LHandle := WindowHandleToPlatform(EventRec.Form.Handle);
              if LHandle.Handle is TFMXWindow then
                TFMXWindow(LHandle.Handle).LastEvent := nil;

              TMouseDownTimer.Create(EventRec);
            end;
          end
          else
          begin
            // Context menu and mouse down
            if (EventRec.Button = TMouseButton.mbRight) or ((EventRec.Button = TMouseButton.mbLeft) and
              (ssCtrl in EventRec.Shift)) then
              MenuDisplayed := ShowContextMenu(EventRec.Form, EventRec.ScreenMousePos)
            else
              MenuDisplayed := False;
            if not MenuDisplayed then
            try
              EventRec.Form.MouseDown(EventRec.Button, EventRec.Shift, EventRec.FormMousePos.X,
                EventRec.FormMousePos.Y);
            except
              HandleException(EventRec.Form);
            end;
          end;
        end;
      TEventKind.MouseUp:
        begin
          // mouse up
          try
            EventRec.Form.MouseUp(EventRec.Button, EventRec.Shift, EventRec.FormMousePos.X, EventRec.FormMousePos.Y);
          except
            HandleException(EventRec.Form);
          end;
          // popups
          ClosePopupForms;
        end;
      TEventKind.MouseMove:
        try
          EventRec.Form.MouseMove(EventRec.Shift, EventRec.FormMousePos.X, EventRec.FormMousePos.Y);
        except
          HandleException(EventRec.Form);
        end;
    end;
  finally
    CleanLastEvent;
  end;
end;

procedure TPlatformCocoa.MouseEvent(const Event: NSEvent);
var
  LEventRec: TEventRec;
begin
  LEventRec := TEventRec.Create(Event);
  MouseEvent(LEventRec);
end;

{ App =========================================================================}

procedure RunLoopObserverCallback(observer: CFRunLoopObserverRef; activity: CFRunLoopActivity; info: Pointer); cdecl;
const
  DefaultMask = NSSystemDefined or NSAppKitDefinedMask or NSApplicationDefinedMask or NSLeftMouseDownMask;
  WithoutEventHandling = 0;
begin
  if TThread.CurrentThread.ThreadID = MainThreadID then
    CheckSynchronize;

  if (PlatformCocoa <> nil) and (PlatformCocoa.NSApp <> nil) and (PlatformCocoa.FAlertCount = 0) then
  begin
    if (PlatformCocoa.FMainMenuState = TPlatformCocoa.TMainMenuState.Created) and (Application <> nil) and
      (Application.MainForm <> nil) then
    begin
      PlatformCocoa.FMainMenuState := TPlatformCocoa.TMainMenuState.Recreated;
      if TOpenCustomForm(Application.MainForm).MainMenu <> nil then
        TMainMenu(TOpenCustomForm(Application.MainForm).MainMenu).RecreateOSMenu
      else
        Application.MainForm.RecreateOsMenu;
    end;
    if PlatformCocoa.NSApp.isActive then
      PlatformCocoa.HookObserverCallback(False, WithoutEventHandling)
    else
      PlatformCocoa.HookObserverCallback(False, DefaultMask);
  end;
end;

class function TPlatformCocoa.ClosePopupForms: Boolean;
begin
  Result := False;
  if Screen <> nil then
    try
      Result := Screen.ClosePopupForms;
    except
      HandleException(Screen);
    end;
end;

class function TPlatformCocoa.PrepareClosePopups(const SaveForm: TCommonCustomForm): Boolean;
begin
  Result := False;
  if Screen <> nil then
    try
      Result := Screen.PrepareClosePopups(SaveForm);
    except
      HandleException(Screen);
    end;
end;

function TPlatformCocoa.HookObserverCallback(CancelIdle: Boolean; Mask: NSUInteger = NSAnyEventMask): Boolean;

  procedure Idle;
  var
    Done: Boolean;
  begin
    Done := False;
    if Application <> nil then
      try
        FPerformKeyExecuted := False;
        Application.DoIdle(Done);
      except
        HandleException(Application);
      end;
  end;

  procedure DefaultMouseAction(const AEventRec: TEventRec);
  var
    TopModal: Boolean;
  begin
    if AEventRec.Form <> nil then
    begin
      if (AEventRec.Form.FormStyle <> TFormStyle.Popup) and (AEventRec.Shift * [ssLeft, ssRight, ssMiddle] <> []) and
        (AEventRec.EventKind in [TEventKind.MouseMove, TEventKind.MouseDown]) and not AEventRec.MouseInContent then
      begin
        PrepareClosePopups(AEventRec.Form);
        ClosePopupForms;
      end;
      if AEventRec.EventKind = TEventKind.MouseDown then
      begin
        TopModal := (FModalStack = nil) or (FModalStack.Count = 0) or (FModalStack.Peek = AEventRec.Form);
        // if LeftClick on inactive form then perform event of mouse
        if (FAlertCount = 0) and TopModal and (AEventRec.Button = TMouseButton.mbLeft) and not AEventRec.Form.Active and
          (AEventRec.Form.FormStyle <> TFormStyle.Popup) then
          MouseEvent(AEventRec);
      end;
    end;
  end;

  procedure EmulateCaptionClick;
  var
    Buttons: NSUInteger;
    Form: TCommonCustomForm;
    Pos: TPointF;
  begin
    Buttons := TNSEvent.OCClass.pressedMouseButtons;
    try
      if (Buttons <> 0) and (Buttons <> FStoredButtons) and (Screen <> nil) and (Screen.PopupFormCount > 0) then
      try
        Pos := GetMousePos;
        if FindFormAtScreenPos(Form, Pos) then
        begin
          if Form.FormStyle <> TFormStyle.Popup then
          begin
            Pos := Form.ScreenToClient(Pos);
             if Pos.Y < 0 then
            begin
              PrepareClosePopups(nil);
              ClosePopupForms;
            end;
          end; // else do nothing
        end
        else
        begin
          PrepareClosePopups(nil);
          ClosePopupForms;
        end;
      except
        HandleException(Application);
      end;
    finally
      FStoredButtons := Buttons;
    end;
  end;

var
  AutoReleasePool: NSAutoreleasePool;
  CancelDefaultAction: Boolean;
  LEvent: NSEvent;
  LEventRec: TEventRec;
  OldDisableClosePopups: Boolean;
  TimeoutDate: NSDate;
const
  WaitTimeout = 0.001;
begin
  Result := False;
  CancelDefaultAction := False;
  LEvent := nil;
  if not CancelIdle then
    Idle;

  if Mask <> 0 then
  begin
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      TimeoutDate := TNSDate.Wrap(TNSDate.OCClass.dateWithTimeIntervalSinceNow(WaitTimeout));
      LEvent := NSApp.nextEventMatchingMask(Mask, TimeoutDate, NSDefaultRunLoopMode, True);
      try
        if LEvent <> nil then
        begin
          Result := True;
          LEventRec := TEventRec.Create(LEvent);
          LEventRec.HandledInApp := True;
          if Assigned(HookEvent) then
            try
              HookEvent(LEventRec, CancelIdle, CancelDefaultAction);
            except
              HandleException(LEventRec.Form);
            end;
          if not CancelDefaultAction then
            DefaultMouseAction(LEventRec);
        end;
      finally
        if not CancelDefaultAction and (LEvent <> nil) then
        begin
          OldDisableClosePopups := FDisableClosePopups;
          try
            FDisableClosePopups := True;
            NSApp.sendEvent(LEvent);
          finally
            FDisableClosePopups := OldDisableClosePopups;
          end;
        end;
        FPerformKeyExecuted := False;
      end;
    finally
      AutoReleasePool.release;
    end;
  end;
  if (Screen <> nil) and (Screen.PopupFormCount > 0) then
    EmulateCaptionClick;
end;

procedure TPlatformCocoa.Run;
begin
  FRunning := True;
  CreateMenu;
  Application.RealCreateForms;
  FRunLoopObserver := CFRunLoopObserverCreate(kCFAllocatorDefault, kCFRunLoopBeforeWaiting, True, 0,
    RunLoopObserverCallback, nil);
  CFRunLoopAddObserver(CFRunLoopGetCurrent, FRunLoopObserver, kCFRunLoopCommonModes);
  NSApp.Run;
end;

function TPlatformCocoa.Terminating: Boolean;
begin
  Result := FTerminating;
end;

function TPlatformCocoa.Running: Boolean;
begin
  Result := FRunning;
end;

procedure TPlatformCocoa.Terminate;
begin
  FRunning := False;
  FTerminating := True;
  TMessageManager.DefaultManager.SendMessage(nil, TApplicationTerminatingMessage.Create);
  NSApp.terminate(nil);
end;

function TPlatformCocoa.HandleToObjC(FmxHandle: TFmxHandle): IObjectiveC;
begin
  TMonitor.Enter(FObjectiveCMap);
  try
    ValidateHandle(FmxHandle);
    if FObjectiveCMap.ContainsKey(FmxHandle) then
      Result := FObjectiveCMap[FmxHandle]
    else
      Result := nil;
  finally
    TMonitor.Exit(FObjectiveCMap);
  end;
end;

function TPlatformCocoa.HandleMessage: Boolean;
begin
  HookObserverCallback(True);
  Result := False;
end;

procedure TPlatformCocoa.WaitMessage;
var
  TimeoutDate: NSDate;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    TimeoutDate := TNSDate.Wrap(TNSDate.OCClass.dateWithTimeIntervalSinceNow(WaitMessageTimeout));
    NSApp.nextEventMatchingMask(NSAnyEventMask, TimeoutDate, NSDefaultRunLoopMode, False);
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.WakeMainThread(Sender: TObject);
var
  NullEvent: NSEvent;
  Origin: NSPoint;
  AutoReleasePool: NSAutoReleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NullEvent := TNSEvent.Wrap(TNSEvent.OCClass.otherEventWithType(NSApplicationDefined, Origin, 0, Now, 0, nil, 0, 0, 0));
    TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication).postEvent(NullEvent, True);
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.GetDefaultTitle: string;
var
  AppNameKey: Pointer;
  AppBundle: NSBundle;
  NSAppName: NSString;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    AppNameKey := NSObjectToID(StrToNSStr('CFBundleName'));
    AppBundle := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle);
    NSAppName := TNSString.Wrap(AppBundle.infoDictionary.objectForKey(AppNameKey));
    Result := NSStrToStr(NSAppName);
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.GetTitle: string;
begin
  Result := FTitle;
end;

function TPlatformCocoa.GetVersionString: string;
var
  VersionObject: Pointer;
begin
  VersionObject := TNSBundle.Wrap(TNSBundle.OCClass.mainBundle).infoDictionary.objectForKey(
    NSObjectToID(StrToNSStr('CFBundleVersion'))); // do not localize
  if VersionObject <> nil then
    Result := NSStrToStr(TNSString.Wrap(VersionObject))
  else
    Result := string.Empty;
end;

procedure TPlatformCocoa.SetTitle(const Value: string);
begin
  if FTitle <> Value then
    FTitle := Value;
end;

function TPlatformCocoa.GetHidden: Boolean;
begin
  Result := NSApp.isHidden;
end;

procedure TPlatformCocoa.SetHidden(const Value: Boolean);
begin
  if Value <> GetHidden then
  begin
    if Value then
      NSApp.Hide(Self)
    else
      NSApp.UnHide(Self);
  end;
end;

procedure TPlatformCocoa.SetShowFullScreenIcon(const AForm: TCommonCustomForm; const AValue: Boolean);
var
  NSWin: NSWindow;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    if AValue then
      NSWin.setCollectionBehavior(NSWindowCollectionBehaviorFullScreenPrimary)
    else
      NSWin.setCollectionBehavior(NSWindowCollectionBehaviorFullScreenNone);
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.HideOthers;
begin
  NSApp.hideOtherApplications(Self);
end;

function TPlatformCocoa.RegisterKeyMapping(const PlatformKey, VirtualKey: Word; const KeyKind: TKeyKind): Boolean;
begin
  Result := FKeyMapping.RegisterKeyMapping(PlatformKey, VirtualKey, KeyKind);
end;

function TPlatformCocoa.UnregisterKeyMapping(const PlatformKey: Word): Boolean;
begin
  Result := FKeyMapping.UnregisterKeyMapping(PlatformKey);
end;

function TPlatformCocoa.PlatformKeyToVirtualKey(const PlatformKey: Word; var KeyKind: TKeyKind): Word;
begin
  Result := FKeyMapping.PlatformKeyToVirtualKey(PlatformKey, KeyKind);
end;

function TPlatformCocoa.VirtualKeyToPlatformKey(const VirtualKey: Word): Word;
begin
  Result := FKeyMapping.VirtualKeyToPlatformKey(VirtualKey);
end;

procedure TPlatformCocoa.AlertCreated;
begin
  Inc(FAlertCount);
end;

procedure TPlatformCocoa.AlertReleased;
begin
  if FAlertCount = 0 then
    raise EInvalidOperation.Create(SAlertCreatedReleasedInconsistency);
  Dec(FAlertCount);
end;

function TPlatformCocoa.AlertCount: Integer;
begin
  Result := FAlertCount;
end;

{ Window ======================================================================}

{ TFMXView }

function TFMXView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(FMXView);
end;

procedure TFMXView.setLayer(newLayer: CALayer);
var
  ContextObject: IContextObject;
begin
  NSView(super).setLayer(newLayer);

  if Supports(Owner.Wnd, IContextObject, ContextObject) then
    TCustomContextOpenGL(ContextObject.Context).FormContext.Update;
end;

constructor TFMXView.Create(const AOwner: TFMXWindow; AFrameRect: NSRect);
var
  V: Pointer;
begin
  inherited Create(AOwner);
  V := NSView(Super).initWithFrame(AFrameRect);
  if GetObjectID <> V then
    UpdateObjectID(V);
  NSView(Super).setAcceptsTouchEvents(True);

  // NSWindow has special mode for merging several windows in one. Also user can Show/Hide tabs panel in the Window.
  // In this cases NSWindow changes his content size without any legal way for catch this event. But technically,
  // when NSWindow changes content it leads to changing frame of all subviews (form's view). So we subscribe on frame
  // changed notification from form's view.
  //
  // When client size is changed we need to realign all controls on form and recreate canvas, because size is changed
  // and we do it in this notification handler.
  //
  // Pay attention! We use NSViewFrameDidChangeNotification instead of NSViewBoundsDidChangeNotification, because
  // for some reason in these cases (merging windows and show/hide tabs panel) View doesn't sent notification
  // NSViewBoundsDidChangeNotification.
  NSView(Super).setPostsFrameChangedNotifications(True);
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(GetObjectID, sel_getUid('frameChanged:'),
      NSStringToID(StrToNSStr('NSViewFrameDidChangeNotification')), GetObjectID);

  FillChar(FEventInfo, Sizeof(FEventInfo), 0);
  if TWindowStyle.GPUSurface in AOwner.Wnd.WindowStyle then
  begin
    if NSAppKitVersionNumber >= NSAppKitVersionNumber10_7 then
      NSView(Super).setWantsBestResolutionOpenGLSurface(True);
    TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(GetObjectID, sel_getUid('surfaceNeedsUpdate:'),
      NSStringToID(NSViewGlobalFrameDidChangeNotification), GetObjectID);
  end;
end;

destructor TFMXView.Destroy;
begin
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).removeObserver(GetObjectID);
  inherited;
end;

{ TFMXMTKView }

function TFMXMTKView.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(FMXMTKView);
end;

procedure TFMXMTKView.setLayer(newLayer: CALayer);
begin
  NSView(super).setLayer(newLayer);
end;

constructor TFMXMTKView.Create(const AOwner: TFMXWindow; AFrameRect: NSRect);
var
  V: Pointer;
begin
  inherited Create(AOwner);
  V := NSObjectToID(MTKView(Super).initWithFrame(AFrameRect, TCustomContextMetal.SharedDevice));
  MTKView(Super).setAcceptsTouchEvents(True);
  MTKView(Super).layer.setOpaque(not AOwner.Wnd.Transparency);
  MTKView(Super).setColorPixelFormat(PixelFormatToMTLPixelFormat(TCustomContextMetal.pixelFormat));

  // If enableSetNeedsDisplay and paused are YES, the view behaves similarly to a NSView object,
  // responding to calls to setNeedsDisplay. In this case, the view's internal draw loop is paused and updates
  // are event-driven instead.
  if GlobalEventDrivenDisplayUpdates then
  begin
    MTKView(Super).setPaused(True);
    MTKView(Super).setEnableSetNeedsDisplay(True);
  end
  else
    MTKView(Super).setPreferredFramesPerSecond(GlobalPreferredFramesPerSecond);

  // CF. TFMXView.Create
  NSView(Super).setPostsFrameChangedNotifications(True);
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).addObserver(GetObjectID, sel_getUid('frameChanged:'),
      NSStringToID(StrToNSStr('NSViewFrameDidChangeNotification')), GetObjectID);

  if GetObjectID <> V then
    UpdateObjectID(V);
end;

destructor TFMXMTKView.Destroy;
begin
  TNSNotificationCenter.Wrap(TNSNotificationCenter.OCClass.defaultCenter).removeObserver(GetObjectID);
  inherited;
end;

{ Text Service }

procedure TTextServiceCocoa.InternalSetMarkedText(const AMarkedText: string);
var
  TextInput: ITextInput;
begin
  if Supports(Owner, ITextInput, TextInput) then
  begin
    FMarkedText := AMarkedText;
    TextInput.IMEStateUpdated;
  end;
end;

procedure TTextServiceCocoa.InternalBreakIMEInput;
var
  TextInput: ITextInput;
begin
  if Supports(Owner, ITextInput, TextInput) then
  begin
    FMarkedText := string.Empty;
    TextInput.IMEStateUpdated;
  end;
end;

procedure TTextServiceCocoa.InternalEndIMEInput;
var
  TextInput: ITextInput;
begin
  if Supports(Owner, ITextInput, TextInput) then
  begin
    TextInput.EndIMEInput;
    FMarkedText := string.Empty;
  end;
end;

procedure TTextServiceCocoa.InternalStartIMEInput;
var
  TextInput: ITextInput;
begin
  if Supports(Owner, ITextInput, TextInput) then
    TextInput.StartIMEInput;
end;

procedure TTextServiceCocoa.MarkedTextPositionChanged;
begin
  inherited;
  RefreshImePosition;
end;

procedure TTextServiceCocoa.RefreshImePosition;

  function FindForm: TCommonCustomForm;
  var
    TmpObject: TFmxObject;
  begin
    TmpObject := Owner.GetObject;
    while (TmpObject <> nil) and not (TmpObject is TCommonCustomForm) do
      TmpObject := TmpObject.Parent;

    if TmpObject is TCommonCustomForm then
      Result := TCommonCustomForm(TmpObject)
    else
      Result := nil;
  end;

var
  Form: TCommonCustomForm;
begin
  inherited;
  Form := FindForm;
  if Form <> nil then
    WindowHandleToPlatform(Form.Handle).View.inputContext.invalidateCharacterCoordinates;
end;

function TTextServiceCocoa.InternalGetMarkedText: string;
begin
  Result := FMarkedText;
end;

function TTextServiceCocoa.TargetClausePosition: TPoint;
begin
  Result := CaretPosition;
  Result.X := Result.X + FixedInt(FSelectedRange.location + FSelectedRange.length);
end;

procedure TTextServiceCocoa.EnterControl(const AFormHandle: TWindowHandle);
begin
end;

procedure TTextServiceCocoa.ExitControl(const AFormHandle: TWindowHandle);
var
  Handle: TMacWindowHandle;
  Range: NSRange;
begin
  if not FMarkedText.IsEmpty then
  begin
    Handle := WindowHandleToPlatform(AFormHandle);
    Range.location := NSNotFound;
    Range.length := 0;
    TFMXWindow(Handle.Handle).FViewObj.insertText(TNSString.OCClass.stringWithString(StrToNSStr(FMarkedText)), Range);
  end;
end;

function TTextServiceCocoa.GetMarketTextAttributes: TArray<TMarkedTextAttribute>;

  function IsSelected(const AIndex: Integer): Boolean;
  begin
    Result := InRange(AIndex, FSelectedRange.location, FSelectedRange.location + FSelectedRange.Length - 1);
  end;

var
  I: Integer;
begin
  SetLength(Result, FMarkedText.Length);
                                                                                                     
  for I := 0 to FMarkedText.Length - 1 do
    if IsSelected(I) then
      Result[I] := TMarkedTextAttribute.TargetNotConverted
    else
      Result[I] := TMarkedTextAttribute.Converted;
end;

procedure TTextServiceCocoa.DrawSingleLine(const ACanvas: TCanvas; const ARect: TRectF; const AFirstVisibleChar: Integer;
  const AFont: TFont; const AOpacity: Single; const AFlags: TFillTextFlags; const ATextAlign: TTextAlign;
  const AVTextAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False);
var
  S: string;
begin
  S := CombinedText;
  S := S.Substring(AFirstVisibleChar - 1, S.Length - AFirstVisibleChar + 1);
  DrawSingleLine(ACanvas, S, ARect, AFont, AOpacity, AFlags, ATextAlign, AVTextAlign, AWordWrap);
end;

procedure TTextServiceCocoa.CaretPositionChanged;
begin
  inherited;
  RefreshImePosition;
end;

procedure TTextServiceCocoa.DrawSingleLine(const ACanvas: TCanvas; const S: string; const ARect: TRectF;
  const AFont: TFont; const AOpacity: Single; const AFlags: TFillTextFlags; const ATextAlign: TTextAlign;
  const AVTextAlign: TTextAlign = TTextAlign.Center; const AWordWrap: Boolean = False);

  procedure UnderlineRegion(const ARegions: TRegion);
  var
    I: Integer;
    Region: TRectF;
    HalfThickness: Single;
    StartPoint, EndPoint: TPointF;
  begin
    HalfThickness := ACanvas.Stroke.Thickness / 2;
    for I := Low(ARegions) to High(ARegions) do
    begin
      Region := ACanvas.AlignToPixel(ARegions[I]);

      StartPoint := TPointF.Create(Region.Left, Region.Bottom);
      StartPoint.Offset(-HalfThickness, -HalfThickness);
      EndPoint := Region.BottomRight;
      EndPoint.Offset(-HalfThickness, -HalfThickness);
      ACanvas.DrawLine(StartPoint, EndPoint, AOpacity);
    end;
  end;

  procedure UnderlineMarkedText(const ALayout: TTextLayout; const AAttributes: TArray<TMarkedTextAttribute>);
  var
    SavedState: TCanvasSaveState;
    Region: TRegion;
  begin
    SavedState := ACanvas.SaveState;
    try
      ACanvas.Stroke.Assign(ACanvas.Fill);
      ACanvas.Stroke.Thickness := 1;
      ACanvas.Stroke.Dash := TStrokeDash.Solid;

      Region := ALayout.RegionForRange(TTextRange.Create(CaretPosition.X, FMarkedText.Length));
      UnderlineRegion(Region);

      if FSelectedRange.length > 0 then
      begin
        ACanvas.Stroke.Thickness := 3;
        Region := ALayout.RegionForRange(TTextRange.Create(CaretPosition.X + Integer(FSelectedRange.location), FSelectedRange.length));
        UnderlineRegion(Region);
      end;
    finally
      ACanvas.RestoreState(SavedState);
    end;
  end;

var
  Layout: TTextLayout;
  Attributes: TArray<TMarkedTextAttribute>;
begin
  Layout := TTextLayoutManager.TextLayoutByCanvas(ACanvas.ClassType).Create;
  try
    Layout.BeginUpdate;
    try
      Layout.TopLeft := ARect.TopLeft;
      Layout.MaxSize := TPointF.Create(ARect.Width, ARect.Height);
      Layout.WordWrap := AWordWrap;
      Layout.HorizontalAlign := ATextAlign;
      Layout.VerticalAlign := AVTextAlign;
      Layout.Font := AFont;
      Layout.Color := ACanvas.Fill.Color;
      Layout.Opacity := AOpacity;
      Layout.RightToLeft := TFillTextFlag.RightToLeft in AFlags;
      Layout.Text := S;
    finally
      Layout.EndUpdate;
    end;
    Layout.RenderLayout(ACanvas);

    Attributes := GetMarketTextAttributes;
    if not FMarkedText.IsEmpty and (Length(Attributes) > 0) then
      UnderlineMarkedText(Layout, Attributes);
  finally
    FreeAndNil(Layout);
  end;
end;

function TTextServiceCocoa.HasMarkedText: Boolean;
begin
  Result := not FMarkedText.IsEmpty;
end;

procedure TTextServiceCocoa.SetMarkedRange(const Value: NSRange);
begin
  FMarkedRange := Value;
end;

procedure TTextServiceCocoa.SetSelectedRange(const Value: NSRange);
begin
  FSelectedRange := Value;
end;

function TPlatformCocoa.GetTextServiceClass: TTextServiceClass;
begin
  Result := TTextServiceCocoa;
end;

{ TFMXViewBase }

constructor TFMXViewBase.Create(const AOwner: TFMXWindow);
begin
  inherited Create;
  FOwner := AOwner;
  FBackingStore := TNSMutableAttributedString.Alloc;
  FBackingStore := TNSMutableAttributedString.Wrap(FBackingStore.initWithString(StrToNSStr('')));
  FMarkedRange.location := NSNotFound;
  FMarkedRange.length := 0;
  FSelectedRange.location := 0;
  FSelectedRange.length := 0;
  UpdateTextServiceControl;
end;

destructor TFMXViewBase.Destroy;
var
  AutoReleasePool: NSAutoreleasePool;
begin
  DestroyTrackingArea;
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    FBackingStore.release;
    FOwner := nil;
    NSView(Super).release;
  finally
    AutoreleasePool.release;
  end;
  inherited;
end;

procedure TFMXViewBase.DestroyTrackingArea;
begin
  if FTrackingArea <> nil then
  begin
    NativeView.removeTrackingArea(FTrackingArea);
    FTrackingArea.release;
    FTrackingArea := nil;
  end;
end;

type
  TNSRectArray = array [0..$FFFF] of NSRect;
  PNSRectArray = ^TNSRectArray;

procedure TFMXViewBase.drawRect(dirtyRect: NSRect);
var
  nctx: NSGraphicsContext;
  boundRect: NSRect;
  PaintControl: IPaintControl;
  UpdateRects: array of TRectF;
  R: PNSRectArray;
  I, RCount: NSInteger;
  AutoReleasePool: NSAutoreleasePool;
  ContextObject: IContextObject;
begin
  if FOwner = nil then
    Exit;

  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    boundRect := NSView(Super).bounds;

    if not GlobalUseMetal then
    begin
      NativeView.getRectsBeingDrawn(@R, @RCount);
      SetLength(UpdateRects, RCount);
      for I := 0 to RCount - 1 do
      begin
        UpdateRects[I] := RectF(R[I].origin.x, boundRect.size.height - R[I].origin.y - R[I].size.height,
          R[I].origin.x + R[I].size.width, boundRect.size.height - R[I].origin.y);
      end;
    end
    else
    begin
      SetLength(UpdateRects,1);
      UpdateRects[0] := dirtyRect.ToRectF;
    end;

    if (not GlobalUseMetal) and Supports(Owner.Wnd, IContextObject, ContextObject) then
      if NSObjectToID(TCustomContextOpenGL(ContextObject.Context).FormContext.View) <> NSObjectToID(NativeView) then
        TCustomContextOpenGL(ContextObject.Context).FormContext.setView(NativeView);

    nctx := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext);

    if Supports(FOwner.Wnd, IPaintControl, PaintControl) then
    begin
      if not (TWindowStyle.GPUSurface in FOwner.Wnd.WindowStyle) then
        PaintControl.ContextHandle := THandle(nctx.graphicsPort);
      PaintControl.PaintRects(UpdateRects);
      if not (TWindowStyle.GPUSurface in FOwner.Wnd.WindowStyle) then
        PaintControl.ContextHandle := 0;
    end;

    if FOwner.Wnd.Transparency then
      WindowHandleToPlatform(FOwner.Wnd.Handle).UpdateLayer(nctx.graphicsPort);

    if FOwner.NeedUpdateShadow and NSWindow(FOwner.Super).isVisible then
    begin
      NSWindow(FOwner.Super).invalidateShadow;
      FOwner.NeedUpdateShadow := False;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

function TFMXViewBase.GetGestureControlUnderMouse(const APoint: TPointF): IGestureControl;
var
  LObject: IControl;
  LGestureControl: TComponent;
begin
  LObject := FOwner.Wnd.ObjectAtPoint(FOwner.Wnd.ClientToScreen(APoint));
  if LObject <> nil then
    LGestureControl := LObject.GetObject
  else
    LGestureControl := FOwner.Wnd;
  Supports(LGestureControl, IGestureControl, Result);
end;

function TFMXViewBase.GetGestureEngineUnderMouse(const APoint: TPointF): TPlatformGestureEngine;
var
  LControl: IGestureControl;
begin
  Result := nil;
  LControl := GetGestureControlUnderMouse(APoint);
  if LControl <> nil then
    Result := TPlatformGestureEngine(LControl.TouchManager.GestureEngine);
end;

function TFMXViewBase.GetNativeView: NSView;
begin
  Result := NSView(Super);
end;

procedure TFMXViewBase.scrollWheel(event: NSEvent);
var
  H: Boolean;
  SS: TShiftState;
  D: Single;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    try
      SS := ShiftStateFromModFlags(event.modifierFlags);
      H := False;
      D := 0;
      if event.deltaY <> 0 then
        D := event.deltaY
      else if event.deltaX <> 0 then
      begin
        D := event.deltaX;
        SS := SS + [ssHorizontal];
      end;
      if D <> 0 then
        FOwner.Wnd.MouseWheel(SS, Round(D * 30), H)
    except
      HandleException(Self);
    end;
  finally
    AutoReleasePool.release;
  end;
end;

{$WARN SYMBOL_DEPRECATED OFF}
procedure TFMXViewBase.keyDown(event: NSEvent);
var
  IsKeyProcessed: Boolean;
begin
  FShift := [];
  IsKeyProcessed := False;
                                                          
  if not hasMarkedText then // IME's conversion window.is active.
  try
    if not PlatformCocoa.FPerformKeyExecuted then
      IsKeyProcessed := PlatformCocoa.KeyProc(Self, FOwner.Wnd, event, False, True);
  finally
    if csDestroying in FOwner.Wnd.ComponentState then
      FOwner.Wnd.Release;
  end;
  if not IsKeyProcessed and not (csDestroying in FOwner.Wnd.ComponentState) then
    NativeView.inputContext.handleEvent(event);
end;
{$WARN SYMBOL_DEPRECATED ON}

procedure TFMXViewBase.keyUp(event: NSEvent);
var
  K: word;
  Ch: WideChar;
  VKKeyCode: Integer;
  IsFNKey: boolean;
begin
  if hasMarkedText then
  begin
    // IME's conversion window.is active.
    VKKeyCode := VKeyFromKeyCode(event.keyCode, IsFNKey);
    TDownKeyList.Current.RemoveKeyDown(VKKeyCode, K, Ch);
  end
  else
    PlatformCocoa.KeyProc(Self, FOwner.Wnd, event, False, False);
end;

procedure TFMXViewBase.lockFocus;
var
  ContextObject: IContextObject;
begin
  NSView(Super).lockFocus;

  if (not GlobalUseMetal) and Supports(Owner.Wnd, IContextObject, ContextObject) then
    TCustomContextOpenGL(ContextObject.Context).FormContext.setView(NSView(Super));
end;

procedure TFMXViewBase.mouseMoved(theEvent: NSEvent);
begin
  if PlatformCocoa <> nil then
    PlatformCocoa.MouseEvent(theEvent);
end;

procedure TFMXViewBase.mouseDown(theEvent: NSEvent);
begin
  if PlatformCocoa <> nil then
    PlatformCocoa.MouseEvent(theEvent);
  SetFirstGesturePoint(theEvent);
end;

procedure TFMXViewBase.mouseUp(theEvent: NSEvent);
var
  SavedWindowId: Integer;
begin
  SavedWindowId := FOwner.Id;
  if PlatformCocoa <> nil then
    PlatformCocoa.MouseEvent(theEvent);

  // User can destroyes Form in mouse events. So when we get control back, this view can be removed.
  if not PlatformCocoa.IsWindowDestroyed(SavedWindowId) then
    ReleaseGestureEngine;
end;

procedure TFMXViewBase.mouseDragged(theEvent: NSEvent);
begin
  if PlatformCocoa <> nil then
    PlatformCocoa.MouseEvent(theEvent);
  SetTrackGesturePoint(theEvent);
end;

procedure TFMXViewBase.mouseExited(theEvent: NSEvent);
var
  Form: TCommonCustomForm;
begin
  Form := TMacWindowHandle.FindForm(theEvent.window);
  if Form <> nil then
    Form.MouseLeave;
end;

procedure TFMXViewBase.rightMouseDown(theEvent: NSEvent);
begin
  if PlatformCocoa <> nil then
    PlatformCocoa.MouseEvent(theEvent);
end;

procedure TFMXViewBase.rightMouseUp(theEvent: NSEvent);
begin
  if PlatformCocoa <> nil then
    PlatformCocoa.MouseEvent(theEvent);
end;

procedure TFMXViewBase.rightMouseDragged(theEvent: NSEvent);
begin
  if PlatformCocoa <> nil then
    PlatformCocoa.MouseEvent(theEvent);
end;

procedure TFMXViewBase.otherMouseDown(theEvent: NSEvent);
begin
  if PlatformCocoa <> nil then
    PlatformCocoa.MouseEvent(theEvent);
end;

procedure TFMXViewBase.otherMouseUp(theEvent: NSEvent);
begin
  if PlatformCocoa <> nil then
    PlatformCocoa.MouseEvent(theEvent);
end;

procedure TFMXViewBase.ReleaseGestureEngine;
const
  LGestureTypes: TGestureTypes = [TGestureType.Standard, TGestureType.Recorded, TGestureType.Registered];
var
  LEventInfo: TGestureEventInfo;
begin
  if FCurrentGestureEngine <> nil then
  begin
    if FCurrentGestureEngine.PointCount > 1 then
    begin
      FillChar(LEventInfo, Sizeof(FEventInfo), 0);
      if TPlatformGestureEngine.IsGesture(FCurrentGestureEngine.Points, FCurrentGestureEngine.GestureList,
        LGestureTypes, LEventInfo) then
        FCurrentGestureEngine.BroadcastGesture(FCurrentGestureEngine.Control, LEventInfo);
    end;
    // reset the points/touches
    FCurrentGestureEngine.ClearPoints;
    FCurrentGestureEngine := nil;
  end;
end;

procedure TFMXViewBase.DiscardGestureEngine;
begin
  if FCurrentGestureEngine <> nil then
  begin
    FCurrentGestureEngine.ClearPoints;
    FCurrentGestureEngine := nil;
  end;
end;

procedure TFMXViewBase.otherMouseDragged(theEvent: NSEvent);
begin
  if PlatformCocoa <> nil then
    PlatformCocoa.MouseEvent(theEvent);
end;

function TFMXViewBase.acceptsFirstResponder: Boolean;
begin
  Result := True;
end;

function TFMXViewBase.becomeFirstResponder: Boolean;
begin
  Result := True;
end;

function TFMXViewBase.resignFirstResponder: Boolean;
begin
  Result := True;
end;

procedure TFMXViewBase.magnifyWithEvent(event: NSEvent);
var
  Obj: IControl;
  LTouches: NSSet;
  LTouchesArray: NSArray;
  LPoint, LPoint2: NSPoint;
  LTouch: NSTouch;
  LDeviceSize: NSSize;
  I, J, Distance: Integer;
  GestureObj: IGestureControl;
begin
  { Use mouseLocation instead of locationInWindow because gesture events do not have locationInWindow set
   (gestures don't move the mouse cursor). We will consider that the gestures are for the control that is
   under the mouse cursor (on OSX the gestures are for the view under the mouse cursor).}
  LPoint := TNSEvent.OCClass.mouseLocation;
  LPoint.y := MainScreenHeight - LPoint.y;
  FEventInfo.Location := LPoint.ToPointF;

  // Get the control from "under" the gesture.
  Obj := FOwner.Wnd.ObjectAtPoint(FOwner.Wnd.ClientToScreen(FEventInfo.Location));
  if Obj = nil then
    FGestureControl := FOwner.Wnd
  else
    FGestureControl := Obj.GetObject;

  if Supports(FGestureControl, IGestureControl, GestureObj) then
    FGestureControl := GestureObj.GetFirstControlWithGesture(TInteractiveGesture.Zoom);

  if FGestureControl <> nil then
  begin
    LTouches := event.touchesMatchingPhase(NSTouchPhaseTouching, NSView(Super));
    if LTouches.count >= 2 then
    begin
      LTouchesArray := LTouches.allObjects;
      LTouch := TNSTouch.Wrap(LTouchesArray.objectAtIndex(0));
      LDeviceSize := LTouch.deviceSize;
      FEventInfo.Distance := 0; //reset the distance
      // Find the greatest distance between the touches.
      for I := 0 to LTouches.count - 2 do
      begin
        LTouch := TNSTouch.Wrap(LTouchesArray.objectAtIndex(I));
        LPoint := LTouch.normalizedPosition;
        for J := 1 to LTouches.count - 1 do
        begin
          LTouch := TNSTouch.Wrap(LTouchesArray.objectAtIndex(J));
          LPoint2 := LTouch.normalizedPosition;

          Distance := Round(Sqrt(Sqr(LPoint.x * LDeviceSize.width - LPoint2.x * LDeviceSize.width) +
            Sqr(LPoint.y * LDeviceSize.height - LPoint2.y * LDeviceSize.height)));
          if Distance > FEventInfo.Distance then
            FEventInfo.Distance := Distance;
        end;

        FEventInfo.GestureID := igiZoom;
        if Supports(FGestureControl, IGestureControl, GestureObj) then
          GestureObj.CMGesture(FEventInfo);
        FEventInfo.Flags := [];
      end
    end
  end
  else
    //send the message up the responder chain
    NSView(Super).magnifyWithEvent(event);
end;

procedure TFMXViewBase.rotateWithEvent(event: NSEvent);
var
  Obj: IControl;
  LPoint: NSPoint;
  GestureObj: IGestureControl;
begin
  // Use mouseLocation instead of locationInWindow because gesture events do not have locationInWindow set
  // (gestures don't move the mouse cursor). We will consider that the gestures are for the control that is
  // under the mouse cursor (on OSX the gestures are for the view under the mouse cursor).
  LPoint := TNSEvent.OCClass.mouseLocation;
  LPoint.y := MainScreenHeight - LPoint.y;
  FEventInfo.Location := LPoint.ToPointF;

  // Get the control from "under" the gesture.
  Obj := FOwner.Wnd.ObjectAtPoint(FOwner.Wnd.ClientToScreen(FEventInfo.location));
  if Obj <> nil then
    FGestureControl := Obj.GetObject
  else
    FGestureControl := FOwner.Wnd;
  if Supports(FGestureControl, IGestureControl, GestureObj) then
    FGestureControl := GestureObj.GetFirstControlWithGesture(TInteractiveGesture.Rotate);

  if FGestureControl <> nil then
  begin
    //Transform degrees in radians and add them to the existing angle of rotation.
    FEventInfo.Angle := FEventInfo.Angle + event.rotation * Pi / 180;
    FEventInfo.GestureID := igiRotate;
    if Supports(FGestureControl, IGestureControl, GestureObj) then
      GestureObj.CMGesture(FEventInfo);
    FEventInfo.Flags := [];
  end
  else
    //send the message up the responder chain
    NSView(Super).rotateWithEvent(event);
end;

procedure TFMXViewBase.surfaceNeedsUpdate(notification: NSNotification);
var
  ContextObject: IContextObject;
begin
  if (not GlobalUseMetal) and Supports(Owner.Wnd, IContextObject, ContextObject) then
    TCustomContextOpenGL(ContextObject.Context).FormContext.update;
end;

procedure TFMXViewBase.swipeWithEvent(event: NSEvent);
var
  Obj: IControl;
  LTouches: NSSet;
  LTouchesArray: NSArray;
  LPoint, LPoint2: NSPoint;
  LTouch: NSTouch;
  Width, Height: Single;
  GestureObj: IGestureControl;
begin
  // Use mouseLocation instead of locationInWindow because gesture events do not have locationInWindow set
  // (gestures don't move the mouse cursor). We will consider that the gestures are for the control that is
  // under the mouse cursor (on OSX the gestures are for the view under the mouse cursor).
  LPoint := TNSEvent.OCClass.mouseLocation;
  LPoint.y := MainScreenHeight - LPoint.y;

  FSwipePoint := LPoint.ToPointF;

  // Get the control from "under" the gesture.
  Obj := FOwner.Wnd.ObjectAtPoint(FOwner.Wnd.ClientToScreen(FSwipePoint));
  if Obj <> nil then
    FGestureControl := Obj.GetObject
  else
    FGestureControl := FOwner.Wnd;

  if Supports(FGestureControl, IGestureControl, GestureObj) then
    FGestureControl := GestureObj.GetFirstControlWithGesture(TInteractiveGesture.Pan);

  if FGestureControl <> nil then
  begin
    FEventInfo.Location := FSwipePoint;
    LTouches := event.touchesMatchingPhase(NSTouchPhaseTouching, NSView(Super));
    if LTouches.count = 2 then
    begin
      LTouchesArray := LTouches.allObjects;
      LTouch := TNSTouch.Wrap(LTouchesArray.objectAtIndex(0));
      LPoint := LTouch.normalizedPosition;
      LTouch := TNSTouch.Wrap(LTouchesArray.objectAtIndex(1));
      LPoint2 := LTouch.normalizedPosition;

      //the distance between the 2 fingers
      FEventInfo.Distance := Round(Sqrt(Sqr(LPoint.x - LPoint2.x) + Sqr(LPoint.y - LPoint2.y)));

      LPoint.X := Min(LPoint.x, LPoint2.x) + Abs(LPoint.x - LPoint2.x);
      LPoint.Y := 1.0 - Min(LPoint.y, LPoint2.y) + Abs(LPoint.y - LPoint2.y);

      Width := 0;
      Height := 0;
      if FGestureControl is TCommonCustomForm then
      begin
        Width := TCommonCustomForm(FGestureControl).ClientWidth;
        Height := TCommonCustomForm(FGestureControl).ClientHeight;
      end
      else if FGestureControl is TControl then
      begin
        Width := TControl(FGestureControl).Canvas.Width;
        Height := TControl(FGestureControl).Canvas.Height;
      end;

      LPoint.x := LPoint.x * Width;
      LPoint.y := LPoint.y * Height;

      if event.deltaX <> 0 then
      begin
        //horizontal swipe
        if event.deltaX > 0 then
        begin
          //swipe-left
          LPoint.x := LPoint.x + FEventInfo.Location.X;
          FEventInfo.Location := LPoint.ToPointF;
        end
        else
        begin
          // swipe-right
          LPoint.X := FEventInfo.Location.x - LPoint.x;
          FEventInfo.Location := LPoint.ToPointF;
        end;
      end
      else if event.deltaY <> 0 then
      begin
        //vertical swipe
        if event.deltaY > 0 then
        begin
          //swipe-up
          LPoint.y := LPoint.y + FEventInfo.Location.Y;
          FEventInfo.Location := LPoint.ToPointF;
        end
        else
        begin
          //swipe-down
          LPoint.y := FEventInfo.Location.Y - LPoint.Y;
          FEventInfo.Location := LPoint.ToPointF;
        end;
      end;

      FSwipePoint := FEventInfo.Location;
    end
    else
    begin
      // send the message up the responder chain
      NSView(Super).swipeWithEvent(event);
      Exit;
    end;

    // EventInfo.Distance := Sqrt(Sqr(Point.X - Source.X) + Sqr(Point.Y - Source.Y));;
    // EventInfo.InertiaVector := TPointF(SmallPointToPoint(InertiaVectorFromArgument(LGestureInfo.ullArguments)));
    FEventInfo.GestureID := igiPan;

    // send message to the control
    if Supports(FGestureControl, IGestureControl, GestureObj) then
      GestureObj.CMGesture(FEventInfo);
    FEventInfo.Flags := [];
  end
  else
    // send the message up the responder chain
    NSView(Super).swipeWithEvent(event);
end;

procedure TFMXViewBase.touchesBeganWithEvent(event: NSEvent);
var
  Obj: IControl;
  LTouches: NSSet;
  LTouchesArray: NSArray;
  LPoint, LPoint2: NSPoint;
  LLocation: TPointF;
  LTouch: NSTouch;
  Handled: Boolean;
  GestureObj: IGestureControl;
begin
  Handled := False;
  FillChar(FEventInfo, Sizeof(FEventInfo), 0);
  // Get the location of the gesture.
  // Use mouseLocation instead of locationInWindow because gesture events do not have locationInWindow set
  // (gestures don't move the mouse cursor). We will consider that the gestures are for the control that is
  // under the mouse cursor (on OSX the gestures are for the view under the mouse cursor).
  LPoint := TNSEvent.OCClass.mouseLocation;
  LPoint.y := MainScreenHeight - LPoint.y;
  LLocation := LPoint.ToPointF;

  DiscardGestureEngine;

  // Find the control from "under" the gesture.
  Obj := FOwner.Wnd.ObjectAtPoint(LLocation);
  if Obj <> nil then
    FGestureControl := Obj.GetObject
  else
    FGestureControl := FOwner.Wnd;

  if Supports(FGestureControl, IGestureControl, GestureObj) then
    FGestureControl := GestureObj.GetFirstControlWithGestureEngine;

  if Supports(FGestureControl, IGestureControl, GestureObj) then
  begin
    TPlatformGestureEngine(GestureObj.TouchManager.GestureEngine).InitialPoint := LLocation;

    // Use NSTouch.normalizedPosition to get the movement of the fingers on the trackpad.
    LTouches := event.touchesMatchingPhase(NSTouchPhaseTouching, NSView(Super));
    if LTouches.count = 2 then
    begin
      LTouchesArray := LTouches.allObjects;
      if FGestureControl <> nil then
      begin
        Handled := True;
        LTouch := TNSTouch.Wrap(LTouchesArray.objectAtIndex(0));
        LPoint := LTouch.normalizedPosition;
        LTouch := TNSTouch.Wrap(LTouchesArray.objectAtIndex(1));
        LPoint2 := LTouch.normalizedPosition;

        LPoint.x := Min(LPoint.x, LPoint2.x) + Abs(LPoint.x - LPoint2.x);
        LPoint.y := 1.0 - Min(LPoint.y, LPoint2.y) - Abs(LPoint.y - LPoint2.y);

        // Retain the points/touches.
        TPlatformGestureEngine(GestureObj.TouchManager.GestureEngine).ClearPoints;
        TPlatformGestureEngine(GestureObj.TouchManager.GestureEngine).AddPoint(LPoint.x * 100, LPoint.y * 100);
      end;
    end;
  end;

  //set the gfBegin flag for interactive gestures.
  FEventInfo.Flags := [TInteractiveGestureFlag.gfBegin];

  if not Handled then
    //send the message up the responder chain
    NSView(Super).touchesBeganWithEvent(event);
end;

procedure TFMXViewBase.touchesCancelledWithEvent(event: NSEvent);
var
  GestureObj: IGestureControl;
begin
  if Supports(FGestureControl, IGestureControl, GestureObj) then
  begin
    //Handle "end" flag for interactive gestures.
    if FEventInfo.GestureID > igiFirst then
    begin
      FEventInfo.Flags := [TInteractiveGestureFlag.gfEnd];
      // send message to the control
      GestureObj.CMGesture(FEventInfo);
      FillChar(FEventInfo, Sizeof(FEventInfo), 0);
    end;

    //reset the points/touches
    if GestureObj.TouchManager.GestureEngine <> nil then
      TPlatformGestureEngine(GestureObj.TouchManager.GestureEngine).ClearPoints;
    FGestureControl := nil;
  end
  else
    //send the message up the responder chain
    NSView(Super).touchesCancelledWithEvent(event);
end;

const
  MIN_NO_GESTURE_POINTS = 10;

procedure TFMXViewBase.touchesEndedWithEvent(event: NSEvent);
var
  LEngine: TPlatformGestureEngine;
  Handled: Boolean;
  GestureObj: IGestureControl;
const
  LGestureTypes: TGestureTypes = [TGestureType.Standard, TGestureType.Recorded, TGestureType.Registered];
begin
  Handled := False;

  if Supports(FGestureControl, IGestureControl, GestureObj) then
  begin
    // Handle "end" flag for interactive gestures.
    if FEventInfo.GestureID > igiFirst then
    begin
      FEventInfo.Flags := [TInteractiveGestureFlag.gfEnd];
      // send message to the control
      GestureObj.CMGesture(FEventInfo);
    end;

    if GestureObj.TouchManager.GestureEngine <> nil then
    begin
      // Handle standard gestures.
      LEngine := TPlatformGestureEngine(GestureObj.TouchManager.GestureEngine);
      if LEngine.PointCount > 1 then
      begin
        // Make sure there are at least MIN_NO_GESTURE_POINTS points.
        if LEngine.PointCount < MIN_NO_GESTURE_POINTS then
          LEngine.AddPoints(MIN_NO_GESTURE_POINTS - LEngine.PointCount);
        FillChar(FEventInfo, Sizeof(FEventInfo), 0);
        if TPlatformGestureEngine.IsGesture(LEngine.Points, LEngine.GestureList, LGestureTypes, FEventInfo) then
        begin
          LEngine.BroadcastGesture(FGestureControl, FEventInfo);
          Handled := True;
        end;
      end;
      // reset the points/touches
      TPlatformGestureEngine(GestureObj.TouchManager.GestureEngine).ClearPoints;
    end;
    FGestureControl := nil;
    FillChar(FEventInfo, Sizeof(FEventInfo), 0);
  end;

  if not Handled then
    NSView(Super).touchesEndedWithEvent(event);
end;

procedure TFMXViewBase.touchesMovedWithEvent(event: NSEvent);
var
  LTouches: NSSet;
  LTouchesArray: NSArray;
  LPoint, LPoint2: NSPoint;
  LTouch: NSTouch;
  Handled: Boolean;
  GestureObj: IGestureControl;
begin
  Handled := False;

  if Supports(FGestureControl, IGestureControl, GestureObj) and (GestureObj.TouchManager.GestureEngine <> nil) then
  begin
    // retain the points/touches
    LTouches := event.touchesMatchingPhase(NSTouchPhaseTouching, NSView(Super));
    if LTouches.count = 2 then
    begin
      LTouchesArray := LTouches.allObjects;
      // Retain the points/touches.
      if FGestureControl <> nil then
      begin
        Handled := True;
        LTouch := TNSTouch.Wrap(LTouchesArray.objectAtIndex(0));
        LPoint := LTouch.normalizedPosition;
        LTouch := TNSTouch.Wrap(LTouchesArray.objectAtIndex(1));
        LPoint2 := LTouch.normalizedPosition;

        LPoint.x := Min(LPoint.x, LPoint2.x) + Abs(LPoint.x - LPoint2.x);
        LPoint.y := 1.0 - Min(LPoint.y, LPoint2.y) - Abs(LPoint.y - LPoint2.y);
        // Retain the points/touches.
        TPlatformGestureEngine(GestureObj.TouchManager.GestureEngine).AddPoint(LPoint.x * 100, LPoint.y * 100);
      end;
    end;
  end;

  if not Handled then
    NSView(Super).touchesMovedWithEvent(event);
end;


{ NSTextInputClient }

function TFMXViewBase.firstRectForCharacterRange(aRange: NSRange; actualRange: PNSRange): NSRect;
var
  glyphRect: NSRect;
  R: TRectF;
  TSObj: ITextInput;
begin
  if Supports(FOwner.Wnd.Focused, ITextInput, TSObj) then
    R := TRectF.Create(TSObj.GetTargetClausePointF)
  else if FOwner.Wnd.Focused <> nil then
    R := TControl(FOwner.Wnd.Focused.GetObject).AbsoluteRect
  else
    R := TRectF.Empty;

  glyphRect := MakeNSRect(R.Left, NativeView.bounds.size.height - R.Bottom, R.Width, R.Height);
  // Convert the rect to screen coordinates
  glyphRect := TNSView.Wrap(NativeView.window.contentView).convertRect(glyphRect, NativeView);
  Result := NativeView.window.convertRectToScreen(glyphRect);
end;

procedure TFMXViewBase.flagsChanged(theEvent: NSEvent);
var
  Shift: TShiftState;
  Ignore: Boolean;
  Key: Word;

  procedure ProcessKeyIfChanged(const AShift: TShiftState; const AVirtualKey: Word);
  var
    Form: TCommonCustomForm;
    Key: Word;
    KeyChar: Char;
  begin
    Form := FOwner.Wnd;
    if (AShift * FCurrentShift = AShift) and not (AShift * Shift = AShift) then
    begin
      Key := AVirtualKey;
      KeyChar := #0;
      Form.KeyUp(Key, KeyChar, AShift);
    end;

    if not (AShift * FCurrentShift = AShift) and (AShift * Shift = AShift) then
    begin
      Key := AVirtualKey;
      KeyChar := #0;
      Form.KeyDown(Key, KeyChar, AShift);
    end;
  end;

begin
  Shift := ShiftStateFromModFlags(theEvent.modifierFlags);
  try
    Key := VKeyFromKeyCode(theEvent.keyCode, Ignore);
    ProcessKeyIfChanged([ssShift], Key);
    ProcessKeyIfChanged([ssCtrl], Key);
    ProcessKeyIfChanged([ssAlt], Key);
    ProcessKeyIfChanged([ssCommand], Key);
  finally
    FCurrentShift := Shift;
  end;

  NSView(Super).flagsChanged(theEvent);
end;

function TFMXViewBase.hasMarkedText: Boolean;
begin
  Result := FMarkedRange.location <> NSNotFound;
end;

function ToNSString(const text : Pointer; var NStr: NSString): Boolean;
begin
  if TNSObject.Wrap(text).isKindOfClass(objc_getClass(MarshaledAString('NSAttributedString'))) then
  begin
    NStr := TNSString.Wrap(objc_msgSend(text, sel_getUid(MarshaledAString('string'))));
    Result := True;
  end
  else
  begin
    NStr := TNSString.Wrap(text);
    Result := False;
  end;
end;

procedure TFMXViewBase.insertText(text: Pointer{NSString}; replacementRange: NSRange);
var
  I: Integer;
  K: Word;
  R : NSRange;
  Ch: WideChar;
  Str: string;
  NStr: NSString;
  AutoReleasePool: NSAutoreleasePool;
  TSC: ITextInput;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    if (FShift - [ssAlt, ssShift]) <> [] then
      Exit;
    if hasMarkedText then
    begin
      NativeView.inputContext.discardMarkedText;
      try
        if Supports(FOwner.Wnd.Focused, ITextInput, TSC) then
        begin
          TTextServiceCocoa(TSC.GetTextService).InternalSetMarkedText('');
          TTextServiceCocoa(TSC.GetTextService).InternalEndIMEInput;
        end;
      except
        HandleException(Self);
      end;
    end;
    NativeView.inputContext.invalidateCharacterCoordinates;
    ToNSString(text, NStr);
    if NStr.length > 0 then
    begin
      Str := NSStrToStr(NStr);
      for I := 0 to Str.Length - 1 do
      begin
        Ch := Str.Chars[I];
        K := Ord(Ch);
        DoKeyDown(Self, FOwner.Wnd, K, Ch, FShift);
      end;

      // Get a valid range
      if replacementRange.location = NSNotFound then
        if FMarkedRange.location <> NSNotFound then
          replacementRange := FMarkedRange
        else
          replacementRange := FSelectedRange
      else
      begin
        replacementRange.location := 0;
        replacementRange.length := 0;
      end;

      NativeView.inputContext.invalidateCharacterCoordinates;
      try
        if (FOwner.Wnd.Focused <> nil) then
          FOwner.Wnd.Focused.Repaint;
      except
        HandleException(Self);
      end;
    end;
    FBackingStore.beginEditing;
    R.location := 0;
    R.length := FBackingStore.mutableString.length;
    FBackingStore.deleteCharactersInRange(R);
    FBackingStore.endEditing;

    FMarkedRange.location := NSNotFound;
    FMarkedRange.length := 0;
    FSelectedRange.location := 0;
    FSelectedRange.length := 0;
    UpdateTextServiceControl;
  finally
    AutoReleasePool.release;
  end;
end;

function TFMXViewBase.selectedRange: NSRange;
begin
  Result := FSelectedRange;
end;

procedure TFMXViewBase.SetFirstGesturePoint(const AnEvent: NSEvent);
var
  LPoint: NSPoint;
  LLocation: TPointF;
  LWindow: NSWindow;
begin
  LWindow := AnEvent.window;
  if LWindow <> nil then
  begin
    LPoint := AnEvent.locationInWindow;
    LLocation := TPointF.Create(LPoint.X, NSView(Super).bounds.size.height - LPoint.y);
    FCurrentGestureEngine := GetGestureEngineUnderMouse(LLocation);
    if FCurrentGestureEngine <> nil then
    begin
      FCurrentGestureEngine.ClearPoints;
      FCurrentGestureEngine.InitialPoint := LLocation;
    end;
  end;
end;

procedure TFMXViewBase.setMarkedText(text: Pointer {NSString}; selectedRange, replacementRange: NSRange);
var
  NStr: NSString;
  IsAttrString: Boolean;
  TSC: ITextInput;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    if not hasMarkedText then
    try
      if Supports(FOwner.Wnd.Focused, ITextInput, TSC) then
        TTextServiceCocoa(TSC.GetTextService).InternalStartIMEInput;
      TDownKeyList.Current.Clear;
    except
      HandleException(Self);
    end;

    IsAttrString := ToNSString(text, NStr);

    NativeView.inputContext.invalidateCharacterCoordinates;
    // Get a valid range
    if replacementRange.location = NSNotFound then
      if FMarkedRange.location <> NSNotFound then
        replacementRange := FMarkedRange
      else
        replacementRange := FSelectedRange
    else
    begin
      replacementRange.location := 0;
      replacementRange.length := 0;
    end;

    // Add the text
    FBackingStore.beginEditing;
    try
      if NStr.length = 0 then
      begin
        FBackingStore.deleteCharactersInRange(replacementRange);
        NativeView.inputContext.discardMarkedText;
        FMarkedRange.location := NSNotFound;
        FMarkedRange.length := 0;
        FSelectedRange.location := 0;
        FSelectedRange.length := 0;
        UpdateTextServiceControl;
        try
          if Supports(FOwner.Wnd.Focused, ITextInput, TSC) then
          begin
            TTextServiceCocoa(TSC.GetTextService).InternalBreakIMEInput;
            TTextServiceCocoa(TSC.GetTextService).InternalEndIMEInput;
          end;
        except
          HandleException(Self);
        end;
      end
      else
      begin
        FSelectedRange.location := replacementRange.location + selectedRange.location;
        FSelectedRange.length := selectedRange.length;
        FMarkedRange.location := replacementRange.location;
        FMarkedRange.length := NStr.length;
        UpdateTextServiceControl;
        if IsAttrString then
          FBackingStore.replaceCharactersInRange(replacementRange, TNSAttributedString.Wrap(text))
        else
          FBackingStore.replaceCharactersInRange(replacementRange, TNSString.Wrap(text));
        try
          if Supports(FOwner.Wnd.Focused, ITextInput, TSC) then
            TSC.GetTextService.InternalSetMarkedText(NSStrToStr(NStr));
        except
          HandleException(Self);
        end;
      end;
    finally
      FBackingStore.endEditing;
    end;

    NativeView.inputContext.invalidateCharacterCoordinates;
    try
      if (FOwner.Wnd.Focused <> nil) then
        FOwner.Wnd.Focused.Repaint;
    except
      HandleException(Self);
    end;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TFMXViewBase.SetTrackGesturePoint(const AnEvent: NSEvent);
var
  LPoint: NSPoint;
  LLocation: TPointF;
  LWindow: NSWindow;
  LEngine: TPlatformGestureEngine;
begin
  if FCurrentGestureEngine <> nil then
  begin
    LWindow := AnEvent.window;
    if LWindow <> nil then
    begin
      LPoint := AnEvent.locationInWindow;
      LLocation := TPointF.Create(LPoint.X, NSView(Super).bounds.size.height - LPoint.y);
      LEngine := GetGestureEngineUnderMouse(LLocation);
      if FCurrentGestureEngine <> LEngine then
        ReleaseGestureEngine
      else
        FCurrentGestureEngine.AddPoint(LLocation.X, LLocation.Y);
    end;
  end;
end;

procedure TFMXViewBase.unMarkText;
var
  TSC: ITextInput;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
  //  NativeView.inputContext.invalidateCharacterCoordinates;
    FMarkedRange.location := NSNotFound;
    FMarkedRange.length := 0;
    UpdateTextServiceControl;

    try
      if Supports(FOwner.Wnd.Focused, ITextInput, TSC) then
      begin
        TTextServiceCocoa(TSC.GetTextService).InternalSetMarkedText('');
      end;
    except
      HandleException(Self);
    end;

    NativeView.inputContext.discardMarkedText;
  finally
    AutoReleasePool.release;
  end;
end;

function TFMXViewBase.validAttributesForMarkedText: Pointer {NSArray};
var
  Attribs: array[0..1] of Pointer;
  Attrib: NSString;
  AttrArray: NSArray;
begin
  Attrib := NSMarkedClauseSegmentAttributeName;
  Attribs[0] := NSObjectToID(Attrib);
  Attrib := NSGlyphInfoAttributeName;
  Attribs[1] := NSObjectToID(Attrib);
  AttrArray := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@Attribs[0], 2));
  Result := NSObjectToID(AttrArray);

//  Attrib := NSMarkedClauseSegmentAttributeName;
//  Attribs[0] := (Attrib as ILocalObject).GetObjectID;
//  AttrArray := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@Attribs[0], 1));
//  Result := (AttrArray as ILocalObject).GetObjectID;
end;

procedure TFMXViewBase.doCommandBySelector(selector: SEL);
begin
  NativeView.doCommandBySelector(selector);
end;

function TFMXViewBase.drawsVerticallyForCharacterAtIndex(charIndex: NSUInteger): Boolean;
begin
  Result := False;
end;

function TFMXViewBase.fractionOfDistanceThroughGlyphForPoint(aPoint: NSPoint): CGFloat;
begin
  Result := 0;
end;

procedure TFMXViewBase.frameChanged(notification: NSNotification);
var
  ContentFrame: NSRect;
begin
  ContentFrame := NativeView.frame;
  try
    FOwner.Wnd.RecreateResources;
    TOpenCustomForm(FOwner.Wnd).Realign;
  except
    HandleException(Self);
  end;
end;

function TFMXViewBase.windowLevel: NSInteger;
begin
  Result := NativeView.window.level;
end;

function TFMXViewBase.FocusedTextService: TTextServiceCocoa;
var
  TSC : ITextInput;
begin
  Result := nil;
  if Owner <> nil then
    if Owner.Wnd <> nil then
      if Supports(FOwner.Wnd.Focused, ITextInput, TSC) then
        Result := TTextServiceCocoa(TSC.GetTextService);
end;

procedure TFMXViewBase.UpdateTextServiceControl;
var
  TSC: ITextInput;
begin
  if Supports(FOwner.Wnd.Focused, ITextInput, TSC) then
  begin
    TTextServiceCocoa( TSC.GetTextService ).SetMarkedRange(FMarkedRange);
    TTextServiceCocoa( TSC.GetTextService ).SetSelectedRange(FSelectedRange);
  end;
end;

procedure TFMXViewBase.updateTrackingAreas;
var
  AutoReleasePool: NSAutoreleasePool;
  Options: NSTrackingAreaOptions;
begin
  NativeView.updateTrackingAreas;
  DestroyTrackingArea;

  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    Options := NSTrackingMouseMoved or NSTrackingActiveAlways or NSTrackingAssumeInside or NSTrackingMouseEnteredAndExited;
    FTrackingArea := TNSTrackingArea.Wrap(TNSTrackingArea.Alloc.initWithRect(NativeView.bounds, Options, GetObjectId, nil));
    NativeView.addTrackingArea(FTrackingArea);
  finally
    AutoReleasePool.release;
  end;
end;

function TFMXViewBase.attributedString: NSAttributedString;
begin
  Result := FBackingStore;
end;

function TFMXViewBase.attributedSubstringForProposedRange(aRange: NSRange; actualRange: PNSRange): NSAttributedString;
begin
  // Get a valid range
  if actualRange <> nil then
  begin
    if (aRange.location <> NSNotFound) and (aRange.location < (FBackingStore.length - 1)) then
      actualRange^.location := aRange.location
    else
      actualRange^.location := 0;
    if aRange.length <= (FBackingStore.length - actualRange^.location) then
      actualRange^.length := aRange.length
    else
      actualRange^.length := Max(0, Integer(FBackingStore.length - actualRange^.location - 1));

    // Get the backing store matching the range
    if (actualRange^.location = 0) and (actualRange^.length = FBackingStore.length) then
      Result := FBackingStore
    else
      Result := TNSAttributedString.Wrap(FBackingStore.attributedSubstringFromRange(actualRange^));
  end
  else
    Result := nil;
end;

function TFMXViewBase.baselineDeltaForCharacterAtIndex(anIndex: NSUInteger): CGFloat;
begin
  Result := 0;
end;

function TFMXViewBase.characterIndexForPoint(aPoint: NSPoint): NSUInteger;
begin
  Result := 0;
end;

function TFMXViewBase.markedRange: NSRange;
begin
  Result := FMarkedRange;
end;

{ TFMXWindow}

class function TFMXWindow.GenerateWindowId: Integer;
begin
  Result := FNextWindowId;
  Inc(FNextWindowId);
end;

function TFMXWindow.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(FMXWindow);
end;

function TFMXWindow.GetView: NSView;
begin
  Result := FViewObj.NativeView;
end;

function TFMXWindow.windowShouldClose(Sender: Pointer): Boolean;
var
  Action: TCloseAction;
begin
  Result := False;
  if Application = nil then
    Exit;
  if Application.Terminated then
    Exit;
  try
    Action := Wnd.Close;
    Result := (TFmxFormState.Modal in Wnd.FormState) or (Action in [TCloseAction.caHide, TCloseAction.caFree]);
  except
    HandleException(Self);
  end;
end;

procedure TFMXWindow.windowWillClose(notification: NSNotification);
var
  LParent: NSWindow;
begin
  if (Application <> nil) and (Application.MainForm <> nil) then
  begin
    LParent := WindowHandleToPlatform(Wnd.Handle).Wnd;
    while (LParent <> nil) and (LParent <> WindowHandleToPlatform(Application.MainForm.Handle).Wnd) do
      LParent := LParent.ParentWindow;
    if LParent <> nil then
      Application.Terminate;
  end;
end;

procedure TFMXWindow.windowDidBecomeKey(notification: NSNotification);
begin
  if Wnd.FormStyle <> TFormStyle.Popup then
  begin
    if (PlatformCocoa <> nil) and (not PlatformCocoa.FDisableClosePopups) then
    begin
      TPlatformCocoa.PrepareClosePopups(Wnd);
      TPlatformCocoa.ClosePopupForms;
    end;
    try
      if PlatformCocoa.FAlertCount = 0 then
        Wnd.Activate;
    except
      HandleException(Self);
    end;
  end;
end;

procedure TFMXWindow.windowDidChangeBackingProperties(notification: NSNotification);
begin
  if (Wnd = nil) or (Application = nil) or (Application.Terminated) then
    Exit;

  try
    Wnd.RecreateResources;
    TMessageManager.DefaultManager.SendMessage(nil, TScaleChangedMessage.Create(Wnd), True);
  except
    HandleException(Self);
  end;
end;

procedure TFMXWindow.windowDidResignKey(notification: NSNotification);
begin
  if (Wnd = nil) or (Application = nil) or (Application.Terminated) then
    Exit;
  try
    Wnd.Deactivate;
  except
    HandleException(Self);
  end;
end;

procedure TFMXWindow.UpdateWindowState;
var
  NSWin: NSWindow;
begin
  if (Wnd <> nil) and PlatformCocoa.FCanSetState then
  begin
    PlatformCocoa.FCanSetState := False;
    try
      NSWin := WindowHandleToPlatform(Wnd.Handle).Wnd;
      if NSWin.isMiniaturized then
        Wnd.WindowState := TWindowState.wsMinimized
      else if IsZoomed(NSWin) then
        Wnd.WindowState := TWindowState.wsMaximized
      else
        Wnd.WindowState := TWindowState.wsNormal;
    finally
      PlatformCocoa.FCanSetState := True;
    end;
  end;
end;

procedure TFMXWindow.windowDidResize(notification: NSNotification);
var
  LFrame: NSRect;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    UpdateWindowState;
    LFrame := NSWindow(Super).frame;
    try
      Wnd.SetBoundsF(LFrame.origin.x, MainScreenHeight - LFrame.origin.y - LFrame.size.height,
                     LFrame.size.width, LFrame.size.height);
    except
      HandleException(Self);
    end;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TFMXWindow.windowDidMiniaturize(notification: NSNotification);
begin
  UpdateWindowState;
end;

procedure TFMXWindow.windowDidDeminiaturize(notification: NSNotification);
begin
  UpdateWindowState;
end;

procedure TFMXWindow.windowDidEnterFullScreen(notification: NSNotification);
begin
  UpdateWindowState;
end;

procedure TFMXWindow.windowDidExitFullScreen(notification: NSNotification);
var
  NSWin: NSWindow;
begin
  if (Wnd <> nil) and PlatformCocoa.FCanSetState and (not Wnd.Visible) then
  begin
    PlatformCocoa.FCanSetState := False;
    try
      NSWin := WindowHandleToPlatform(Wnd.Handle).Wnd;
      PlatformCocoa.SetShowFullScreenIcon(Wnd, Wnd.ShowFullScreenIcon);
      NSWin.orderOut(nil);
    finally
      PlatformCocoa.FCanSetState := True;
    end;
  end;
  UpdateWindowState;
end;

procedure TFMXWindow.windowDidMove(notification: NSNotification);
var
  LFrame: NSRect;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    LFrame := NSWindow(Super).frame;
    try
      Wnd.SetBoundsF(LFrame.origin.x, MainScreenHeight - LFrame.origin.y - LFrame.size.height,
                     LFrame.size.width, LFrame.size.height);
    except
      HandleException(Self);
    end;
  finally
    AutoReleasePool.release;
  end;
end;

var
  GlobalData: TDragObject;

function GetDataObject(sender: NSDraggingInfo): TDragObject;
var
  PBoard: NSPasteboard;
  Str: NSString;
  Arr: NSArray;
  W: string;
  I: Integer;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    FillChar(Result, SizeOf(Result), 0);

    PBoard := sender.draggingPasteboard;
    if PBoard.types.containsObject(NSObjectToID(NSFMXPboardType)) then
    begin
      Result := GlobalData;
      Exit;
    end;

    if PBoard.types.containsObject(NSObjectToID(NSPasteboardTypeString)) then
    begin
      Str := PBoard.stringForType(NSPasteboardTypeString);
      W := NSStrToStr(str);
      Result.Data := W;
    end;

    if PBoard.types.containsObject(NSObjectToID(NSFilenamesPboardType)) then
    begin
      Arr := TNSArray.Wrap(PBoard.propertyListForType(NSFilenamesPboardType));
      SetLength(Result.Files, Arr.count);
      for I := 0 to Arr.count - 1 do
      begin
        Str := TNSString.Wrap(Arr.objectAtIndex(I));
        W := NSStrToStr(Str);
        Result.Files[I] := W;
      end;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

function TFMXWindow.draggingEntered(Sender: Pointer): NSDragOperation;
var
  mp: NSPoint;
  P: TPointF;
  DragInfo: NSDraggingInfo;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    DragInfo := TNSDraggingInfo.Wrap(Sender);
    mp := DragInfo.draggingLocation;
    mp.y := View.bounds.size.height - mp.y;
    P := PointF(mp.x, mp.y);
    try
      Wnd.DragEnter(GetDataObject(DragInfo), Wnd.ClientToScreen(P));
    except
      HandleException(Self);
    end;
    Result := NSDragOperationEvery;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TFMXWindow.draggingExited(Sender: Pointer {id});
begin
  try
    Wnd.DragLeave;
  except
    HandleException(Self);
  end;
end;

function TFMXWindow.draggingUpdated(Sender: Pointer): NSDragOperation;
var
  mp: NSPoint;
  P: TPointF;
  Operation: TDragOperation;
  DragInfo: NSDraggingInfo;
  AutoReleasePool: NSAutoreleasePool;
begin
  Result := NSDragOperationNone;
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    DragInfo := TNSDraggingInfo.Wrap(Sender);
    mp := DragInfo.draggingLocation;
    mp.y := View.bounds.size.height - mp.y;
    P := mp.ToPointF;
    Operation := TDragOperation.None;
    try
      Wnd.DragOver(GetDataObject(DragInfo), Wnd.ClientToScreen(P), Operation);
    except
      HandleException(Self);
    end;
    case Operation of
      TDragOperation.None:
        Result := NSDragOperationNone;
      TDragOperation.Move:
        Result := NSDragOperationMove;
      TDragOperation.Copy:
        Result := NSDragOperationCopy;
      TDragOperation.Link:
        Result := NSDragOperationLink;
    end;
    DragOperation := Result;
  finally
    AutoReleasePool.release;
  end;
end;

function TFMXWindow.performDragOperation(Sender: Pointer): Boolean;
var
  mp: NSPoint;
  P: TPointF;
  DragInfo: NSDraggingInfo;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    DragInfo := TNSDraggingInfo.Wrap(Sender);
    mp := DragInfo.draggingLocation;
    mp.y := View.bounds.size.height - mp.y;
    P := mp.ToPointF;
    try
      Wnd.DragDrop(GetDataObject(DragInfo), Wnd.ClientToScreen(P));
    except
      HandleException(Self);
    end;
    Result := True;
  finally
    AutoReleasePool.release;
  end;
end;

function TFMXWindow.performKeyEquivalent(event: NSEvent): Boolean;
var
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    Result := False;
    if NSObjectToID(NSWindow(Super).firstResponder) = NSObjectToID(GetView) then
    begin
      //IME's conversion window.is active.
      if (FViewObj <> nil) and FViewObj.hasMarkedText then
        Exit;
      if not PlatformCocoa.FPerformKeyExecuted then
        Result := PlatformCocoa.KeyProc(Self, Wnd, event, True, True);
      PlatformCocoa.FPerformKeyExecuted := True;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

function TFMXWindow.CanActivate: Boolean;
begin
  Result := (Wnd <> nil) and (Wnd.Handle <> nil) and (Wnd.FormStyle <> TFormStyle.Popup) and
    ((PlatformCocoa.FModalStack = nil) or (PlatformCocoa.FModalStack.Count = 0) or
     (PlatformCocoa.FModalStack.Peek = Wnd));
end;

function TFMXWindow.acceptsFirstResponder: Boolean;
begin
  Result := CanActivate;
end;

function TFMXWindow.becomeFirstResponder: Boolean;
begin
  Result := True;
end;

function TFMXWindow.resignFirstResponder: Boolean;
begin
  Result := True;
end;

function TFMXWindow.canBecomeKeyWindow: Boolean;
begin
  Result := CanActivate;
end;

function TFMXWindow.canBecomeMainWindow: Boolean;
begin
  Result := (Application <> nil) and (Application.MainForm = Wnd);
end;

class constructor TFMXWindow.Create;
begin
  FNextWindowId := 1;
end;

constructor TFMXWindow.Create;
begin
  inherited Create;
  FId := GenerateWindowId;
end;

destructor TFMXWindow.Destroy;
begin
  if FViewObj <> nil then
  begin
    FViewObj.NativeView.setHidden(True);
    NSWindow(Super).setContentView(nil);
    // A reference for the paint context was manually added when the window was
    // created. Clear the reference here to avoid leaking view objects.
    FViewObj._Release;
    FViewObj := nil;
  end;
  if FDelegate <> nil then
  begin
    FDelegate := nil;
    NSWindow(Super).setDelegate(nil);
  end;
  if Assigned(Wnd.Handle) then
    NSWindow(Super).close;
  Wnd := nil;
  NSWindow(Super).release;
  inherited;
end;

{ TFMXWindowDelegate }

type
  TFMXWindowDelegate = class(TOCLocal, NSWindowDelegate)
  private
    FWindow: TFMXWindow;
  public
    constructor Create(AOwner: TFMXWindow);
    destructor Destroy; override;
    function windowShouldClose(Sender: Pointer {id}): Boolean; cdecl;
    procedure windowWillClose(notification: NSNotification); cdecl;
    procedure windowDidBecomeKey(notification: NSNotification); cdecl;
    procedure windowDidResignKey(notification: NSNotification); cdecl;
    procedure windowDidResize(notification: NSNotification); cdecl;
    procedure windowDidMiniaturize(notification: NSNotification); cdecl;
    procedure windowDidDeminiaturize(notification: NSNotification); cdecl;
    procedure windowDidEnterFullScreen(notification: NSNotification); cdecl;
    procedure windowDidExitFullScreen(notification: NSNotification); cdecl;
    procedure windowDidMove(notification: NSNotification); cdecl;
    procedure windowDidChangeBackingProperties(notification: NSNotification); cdecl;
  end;

constructor TFMXWindowDelegate.Create(AOwner: TFMXWindow);
begin
  inherited Create;
  FWindow := AOwner;
end;

destructor TFMXWindowDelegate.Destroy;
begin
  FWindow := nil;
  objc_msgSend(GetObjectID, sel_getUid('release'));
  inherited;
end;

procedure TFMXWindowDelegate.windowDidBecomeKey(notification: NSNotification);
begin
  FWindow.windowDidBecomeKey(notification);
end;

procedure TFMXWindowDelegate.windowDidChangeBackingProperties(notification: NSNotification);
begin
  FWindow.windowDidChangeBackingProperties(notification);
end;

procedure TFMXWindowDelegate.windowDidMove(notification: NSNotification);
begin
  FWindow.windowDidMove(notification);
end;

procedure TFMXWindowDelegate.windowDidResignKey(notification: NSNotification);
begin
  FWindow.windowDidResignKey(notification);
end;

procedure TFMXWindowDelegate.windowDidResize(notification: NSNotification);
begin
  FWindow.windowDidResize(notification);
end;

procedure TFMXWindowDelegate.windowDidDeminiaturize(notification: NSNotification);
begin
  FWindow.windowDidDeminiaturize(notification);
end;

procedure TFMXWindowDelegate.windowDidEnterFullScreen(notification: NSNotification);
begin
  FWindow.windowDidEnterFullScreen(notification);
end;

procedure TFMXWindowDelegate.windowDidExitFullScreen(notification: NSNotification);
begin
  FWindow.windowDidExitFullScreen(notification);
end;

procedure TFMXWindowDelegate.windowDidMiniaturize(notification: NSNotification);
begin
  FWindow.windowDidMiniaturize(notification);
end;

function TFMXWindowDelegate.windowShouldClose(Sender: Pointer): Boolean;
begin
  Result := FWindow.windowShouldClose(Sender);
end;

procedure TFMXWindowDelegate.windowWillClose(notification: NSNotification);
var
  NSApp: NSApplication;
  ModWin: NSWindow;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSApp := TNSApplication.Wrap(TNSApplication.OCClass.sharedApplication);
    ModWin := NSApp.modalWindow;
    if (ModWin <> nil) and (FWindow <> nil) and
      ((ModWin as ILocalObject).GetObjectID = (FWindow.Super as ILocalObject).GetObjectID) then
      NSApp.abortModal;
    FWindow.windowWillClose(notification);
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.FindForm(const AHandle: TWindowHandle): TCommonCustomForm;
begin
  Result := WindowHandleToPlatform(AHandle).Form;
end;

procedure objc_msgSendNSRect(theReceiver: Pointer; theSelector: Pointer; dirtyRect: NSRect); cdecl;
  external libobjc name _PU + 'objc_msgSend';

procedure frameDrawRect(Self: Pointer; _cmd: Pointer; dirtyRect: NSRect); cdecl;
var
  nctx: NSGraphicsContext;
  windowRect: NSRect;
  cornerRadius: Single;
  Path: NSBezierPath;
  Wnd: NSWindow;
  WindowBorder: TWindowBorderCocoa;
  Form: TCommonCustomForm;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    Wnd := TNSView.Wrap(Self).window;
    Form := TMacWindowHandle.FindForm(Wnd);
    if (Form <> nil) and Form.Border.IsSupported then
    begin
      WindowBorder := TWindowBorderCocoa(Form.Border.WindowBorder);

      windowRect := Wnd.frame;
      windowRect.origin.x := 0;
      windowRect.origin.y := 0;
      cornerRadius := 4;

      nctx := TNSGraphicsContext.Wrap(TNSGraphicsContext.OCClass.currentContext);
      CGContextClearRect(nctx.graphicsPort, windowRect);

      Path := TNSBezierPath.Wrap(TNSBezierPath.OCClass.bezierPathWithRoundedRect(windowRect, cornerRadius, cornerRadius));
      Path.addClip;

      CGContextTranslateCTM(nctx.graphicsPort, 0, Wnd.frame.size.height);
//      CGContextScaleCTM(nctx.graphicsPort, 1, -1);
      WindowBorder.Paint(nctx.graphicsPort);
    end
    else
    begin
      objc_msgSendNSRect(Self, sel_getUid('drawRectOriginal:'), dirtyRect);
    end;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.HookFrame(const NSWin: NSWindow);
const
{$IFDEF CPU64BITS}
  MethodTypeString = 'v@:@{NSRect={NSPoint=dd}{NSSize=dd}}';
{$ELSE !CPU64BITS}
  MethodTypeString = 'v@:@{NSRect={NSPoint=ff}{NSSize=ff}}';
{$ENDIF CPU64BITS}
var
  FrameClass: Pointer;
  M1, M2: Pointer;
  AutoReleasePool: NSAutoreleasePool;
begin
  Inc(FClassHookCount);
  if FClassHookCount > 1 then Exit;
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    // Replace drawRect method on window frame
    FrameClass := object_getClass(TNSView.Wrap(NSWin.contentView).superview);
    class_addMethod(FrameClass, sel_getUid('drawRectOriginal:'), @frameDrawRect, MethodTypeString);
    M1 := class_getInstanceMethod(FrameClass, sel_getUid('drawRect:'));
    M2 := class_getInstanceMethod(FrameClass, sel_getUid('drawRectOriginal:'));
    method_exchangeImplementations(M1, M2);
    //
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.UnHookFrame(const NSWin: NSWindow);
var
  FrameClass: Pointer;
  M1, M2: Pointer;
  AutoReleasePool: NSAutoreleasePool;
begin
  Dec(FClassHookCount);
  if FClassHookCount > 0 then Exit;
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    // restore old
    FrameClass := object_getClass(TNSView.Wrap(NSWin.contentView).superview);
    M1 := class_getInstanceMethod(FrameClass, sel_getUid('drawRect:'));
    M2 := class_getInstanceMethod(FrameClass, sel_getUid('drawRectOriginal:'));
    method_exchangeImplementations(M1, M2);
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.CreateWindow(const AForm: TCommonCustomForm): TWindowHandle;
const
  NSWindowTabbingModeDisallowed: Integer = 2;
var
  Style: NSUInteger;
  FMXWin: TFMXWindow;
  NSWin: NSWindow;
  NSTitle: NSString;
  R: NSRect;
  LocalObj: ILocalObject;
  DraggedTypes: array[0..3] of Pointer;
  RegTypes: NSArray;
  PaintControl: IPaintControl;
  AutoReleasePool: NSAutoReleasePool;
  LParentWindow: NSWindow;
  ParentLevel, NewLevel: NSInteger;
  ContentView: NSView;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    FMXWin := TFMXWindow.Create;
    FMXWin.NeedUpdateShadow := True;
    NSWin := NSWindow(FMXWin.Super);
    if AForm.Transparency or (AForm.BorderStyle = TFmxFormBorderStyle.None) then
      Style := NSBorderlessWindowMask
    else
    begin
      Style := NSTitledWindowMask or NSUnifiedTitleAndToolbarWindowMask;
      if AForm.BorderStyle <> TFmxFormBorderStyle.None then
      begin
        if TBorderIcon.biMinimize in AForm.BorderIcons then
          Style := Style or NSMiniaturizableWindowMask;
        if TBorderIcon.biMaximize in AForm.BorderIcons then
          Style := Style or NSWindowZoomButton;
        if TBorderIcon.biSystemMenu in AForm.BorderIcons then
          Style := Style or NSClosableWindowMask;
      end;
      if AForm.BorderStyle in [TFmxFormBorderStyle.Sizeable, TFmxFormBorderStyle.SizeToolWin] then
        Style := Style or NSResizableWindowMask;
    end;
    R := TNSWindow.OCClass.contentRectForFrameRect(MakeNSRect(AForm.Left, MainScreenHeight - AForm.Top - AForm.height,
      AForm.width, AForm.height), Style);
    NSWin.initWithContentRect(R, Style, NSBackingStoreBuffered, False);
    NSWin.setAcceptsMouseMovedEvents(True);
    NSWin.setReleasedWhenClosed(False);
    NSWin.setShowsToolbarButton(True);
    NSWin.setBackgroundColor(TNSColor.Wrap(TNSColor.OCClass.clearColor));
    NSWin.setCollectionBehavior(NSWindowCollectionBehaviorFullScreenNone);
    if TOSVersion.Check(10, 12) then
      objc_msgSendP(NSObjectToID(NSWin), sel_getUid('setTabbingMode:'), Pointer(NSWindowTabbingModeDisallowed));

    ApplyConstraints(NSWin, AForm.Constraints.MinSize, AForm.Constraints.MaxSize);
    if Supports(NSWin, NSPanel) then
    begin
      (NSWin as NSPanel).setBecomesKeyOnlyIfNeeded(True);
      (NSWin as NSPanel).setWorksWhenModal(True);
      (NSWin as NSPanel).setFloatingPanel(True);
    end;
    NSWin.useOptimizedDrawing(True);
    NSTitle := StrToNSStr(AForm.Caption);
    NSWin.setTitle(NSTitle);

    // set level of window
    case TOpenCustomForm(AForm).FormStyle of
      TFormStyle.Popup: NewLevel := kCGMinimumWindowLevelKey;
      TFormStyle.StayOnTop: NewLevel := kCGFloatingWindowLevelKey;
    else
      NewLevel := kCGBaseWindowLevelKey;
    end;
    if (AForm.ParentForm <> nil) and (AForm.ParentForm.Handle <> nil) then
    begin
      LParentWindow := WindowHandleToPlatform(AForm.ParentForm.Handle).Wnd;
      if LParentWindow <> nil then
      begin
        ParentLevel := LParentWindow.level;
        if ParentLevel < kCGScreenSaverWindowLevelKey then
          Inc(ParentLevel);
        NewLevel := Max(NewLevel, ParentLevel);
      end;
    end;
    NSWin.setLevel(NewLevel);
    FMXWin.Wnd := AForm;

    HookFrame(NSWin);

    R := MakeNSRect(0, 0, R.size.width, R.size.height);

    if GlobalUseMetal then
      FMXWin.FViewObj := TFMXMTKView.Create(FMXWin, R)
    else
      FMXWin.FViewObj := TFMXView.Create(FMXWin, R);
    if Supports(FMXWin.FViewObj, ILocalObject, LocalObj) then
    begin
      if Supports(AForm, IPaintControl, PaintControl) then
        PaintControl.ContextHandle := THandle(LocalObj.GetObjectID);
      FMXWin.FViewObj._AddRef;
    end;
    FMXWin.View.setAutoresizingMask(NSViewWidthSizable or NSViewHeightSizable);
    ContentView := TNSView.Wrap(NSWin.contentView);
    ContentView.setAutoresizesSubviews(True);
    ContentView.addSubview(FMXWin.View);
    if not Supports(AForm, IContextObject) then
      ContentView.setWantsLayer(True);
    if AForm.Transparency then
    begin
      NSWin.setOpaque(False);
      NSWin.setHasShadow(False);
    end
    else
    begin
      NSWin.setOpaque(True);
      NSWin.setBackgroundColor(TNSColor.Wrap(TNSColor.OCClass.windowBackgroundColor));
    end;
    DraggedTypes[0] := NSObjectToID(NSPasteboardTypeString);
    DraggedTypes[1] := NSObjectToID(NSFMXPBoardtype);
    DraggedTypes[2] := NSObjectToID(NSFilenamesPboardType);
    DraggedTypes[3] := nil;
    RegTypes := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@DraggedTypes[0], 3));
    NSWin.registerForDraggedTypes(RegTypes);

    FMXWin.FDelegate := TFMXWindowDelegate.Create(FMXWin);
    NSWin.setDelegate(FMXWin.FDelegate);

    FWindows.Add(FMXWin.Id);
    Result := TMacWindowHandle.Create(FMXWin);
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.CreateWindowBorder(const AForm: TCommonCustomForm): TWindowBorder;
begin
  Result := FMX.Forms.Border.Mac.CreateWindowBorder(AForm);
end;

procedure TPlatformCocoa.DestroyWindow(const AForm: TCommonCustomForm);
var
  NSWin: NSWindow;
  LFMXWindow: TFMXWindow;
  PaintControl: IPaintControl;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    if AForm.Handle <> nil then
    begin
      LFMXWindow := TFMXWindow(WindowHandleToPlatform(AForm.Handle).Handle);
      LFMXWindow.FViewObj.FOwner := nil;
      FWindows.Remove(LFMXWindow.Id);
      NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
      NSWin.setParentWindow(nil);
      UnHookFrame(NSWin);
      if NSWin.isVisible then
        NSWin.orderOut(nil);
      if Supports(AForm, IPaintControl, PaintControl) then
        PaintControl.ContextHandle := 0;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.ReleaseWindow(const AForm: TCommonCustomForm);
begin
  if AForm <> nil then
    DoReleaseWindow(AForm);
end;

procedure TPlatformCocoa.DoReleaseWindow(AForm: TCommonCustomForm);
var
  AutoReleasePool: NSAutoreleasePool;
  LDisableClosePopups: Boolean;
  Wnd: NSWindow;
begin
  if AForm <> nil then
  begin
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      if AForm.Handle <> nil then
      begin
        LDisableClosePopups := FDisableClosePopups;
        try
          if AForm.FormStyle = TFormStyle.Popup then
            FDisableClosePopups := True;
          if (AForm.FormStyle = TFormStyle.Popup) and
              (AForm.ParentForm <> nil) and
             (AForm.ParentForm.FormStyle = TFormStyle.Popup) then
          begin
            Wnd := WindowHandleToPlatform(AForm.ParentForm.Handle).Wnd;
            if (Wnd <> nil) and (NSApp <> nil) then
              Wnd.makeKeyAndOrderFront(NSObjectToID(NSApp));
          end;
          Wnd := WindowHandleToPlatform(AForm.Handle).Wnd;
          if Wnd <> nil then
          begin
            Wnd.setOneShot(True);
            Wnd.orderOut(nil);
          end;
        finally
          FDisableClosePopups := LDisableClosePopups;
        end;
      end;
    finally
      AutoReleasePool.release;
    end;
  end;
end;

procedure TPlatformCocoa.SetWindowRect(const AForm: TCommonCustomForm; ARect: TRectF);
var
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    WindowHandleToPlatform(AForm.Handle).Wnd.setFrame(MakeNSRect(ARect.Left, MainScreenHeight - ARect.Bottom,
      ARect.Right - ARect.Left, ARect.Bottom - ARect.Top), True);
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.GetWindowRect(const AForm: TCommonCustomForm): TRectF;
var
  NSWin: NSWindow;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    Result := TRectF.Create(NSWin.frame.origin.x, MainScreenHeight - NSWin.frame.origin.y - NSWin.frame.size.height,
      NSWin.frame.origin.x + NSWin.frame.size.width, MainScreenHeight - NSWin.frame.origin.y);
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.GetWindowScale(const AForm: TCommonCustomForm): Single;
begin
  Result := WindowHandleToPlatform(AForm.Handle).Wnd.backingScaleFactor;
end;

procedure TPlatformCocoa.InputQuery(const ACaption: string; const APrompts, ADefaultValues: array of string;
	const ACloseQueryProc: TInputCloseQueryProc);
var
  LResult: TModalResult;
  LValues: array of string;
  I: Integer;
begin
  SetLength(LValues, Length(ADefaultValues));
  for I := Low(ADefaultValues) to High(ADefaultValues) do
    LValues[I] := ADefaultValues[I];
  if InputQuery(ACaption, APrompts, LValues) then
    LResult := mrOk
  else
    LResult := mrCancel;
  if Assigned(ACloseQueryProc) then
    ACloseQueryProc(LResult, LValues);
end;

procedure TPlatformCocoa.InvalidateImmediately(const AForm: TCommonCustomForm);
var
  WindowHandle: TMacWindowHandle;
begin
  if not GlobalEventDrivenDisplayUpdates then
    exit;

  WindowHandle := WindowHandleToPlatform(AForm.Handle);
  if WindowHandle.MTView <> nil then
    WindowHandle.MTView.draw
  else
    InvalidateWindowRect(AForm, AForm.ClientRect);
end;

procedure TPlatformCocoa.InvalidateWindowRect(const AForm: TCommonCustomForm; R: TRectF);
var
  View: NSView;
  AutoReleasePool: NSAutoreleasePool;
begin
  if not GlobalEventDrivenDisplayUpdates then
    exit;
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    if IntersectRect(R, RectF(0, 0, AForm.width, AForm.height)) then
    begin
      View := WindowHandleToPlatform(AForm.Handle).View;
      View.setNeedsDisplayInRect(MakeNSRect(R.Left, View.bounds.size.height - R.Bottom, R.Width, R.Height));
    end;
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.IsMenuBarOnWindowBorder: Boolean;
begin
  Result := False;
end;

function TPlatformCocoa.IsWindowDestroyed(const AWindowId: Integer): Boolean;
begin
  Result := not FWindows.Contains(AWindowId);
end;

constructor TFMXAlertDelegate.Create;
begin
  inherited;
end;

function TFMXAlertDelegate.GetObjectID: Pointer;
begin
  Result := inherited;
end;

function TFMXAlertDelegate.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(AlertDelegate);
end;

procedure TFMXAlertDelegate.alertDidEndSelector(alert: Pointer; returnCode: NSInteger; contextInfo: Pointer);
var
  R: NSInteger;
begin
  R := returnCode - NSAlertFirstButtonReturn;
  if InRange(R, 0, High(Results)) then
    Result := Results[R]
  else
    Result := mrCancel;
  Modal := False;
end;

function TPlatformCocoa.MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType;
  const AButtons: TMsgDlgButtons;	const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: THelpContext;
  const AHelpFileName: string): Integer;
var
  Alert: NSAlert;
  Delegate: TFMXAlertDelegate;
  Session: NSModalSession;
  NSWin: NSWindow;
  S: SEL;
  ActiveForm: TCommonCustomForm;
  R: NSInteger;
  AutoReleasePool: NSAutoreleasePool;

  procedure AddButtons(IsDefault: Boolean);
  var
    B: TMsgDlgBtn;
    procedure AddBtn(B: TMsgDlgBtn);
    begin
      SetLength(Delegate.Results, Length(Delegate.Results) + 1);
      Delegate.Results[High(Delegate.Results)] := ModalResults[B];
      Alert.addButtonWithTitle(StrToNSStr(LocalizedButtonCaption(B)));
    end;
  begin
    for B := Low(TMsgDlgBtn) to High(TMsgDlgBtn) do
      if (B in AButtons) and (IsDefault xor (B <> ADefaultButton)) then
        AddBtn(B);
    if (not IsDefault) and (Length(Delegate.Results) = 0) then
      AddBtn(ADefaultButton);
  end;
begin
  ActiveForm := nil;
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    Delegate := TFMXAlertDelegate.Create;
    try
      Delegate.Modal := True;

      if Screen <> nil then
      begin
        PrepareClosePopups(nil);
        ClosePopupForms;
        ActiveForm := Screen.ActiveForm;
        if (ActiveForm <> nil) and (ActiveForm.Visible) and (ActiveForm.Handle <> nil) and
          not (ActiveForm.Owner is TPopup) then
        begin
          NSWin := WindowHandleToPlatform(ActiveForm.Handle).Wnd;
          if NSWin <> nil then
            NSWin.retain;
        end;
      end;

      S := sel_getUid('alertDidEndSelector:returnCode:contextInfo:');
      Alert := TNSAlert.Create;
      Alert.setInformativeText(StrToNSStr(AMessage));
      Alert.setMessageText(StrToNSStr(LocalizedMessageDialogTitle(ADialogType)));
      if ADialogType = TMsgDlgType.mtWarning then
        Alert.setAlertStyle(NSWarningAlertStyle)
      else if ADialogType = TMsgDlgType.mtError then
        Alert.setAlertStyle(NSCriticalAlertStyle)
      else
        Alert.setAlertStyle(NSInformationalAlertStyle);

      AddButtons(True);
      AddButtons(False);

      if NSWin <> nil then
      begin
        Alert.beginSheetModalForWindow(NSWin, Delegate.GetObjectID, S, nil);
        if TFmxFormState.Modal in ActiveForm.FormState then
          Session := nil
        else
          Session := NSApp.beginModalSessionForWindow(NSWin);
        Inc(FAlertCount);
        try
          while Delegate.Modal do
          begin
            if Session <> nil then
              NSApp.runModalSession(Session);
            HookObserverCallback(False);
          end;
        finally
          Dec(FAlertCount);
          if Session <> nil then
            NSApp.endModalSession(Session);
        end;
        Result := Delegate.Result;
      end
      else
      begin
        R := Alert.runModal - NSAlertFirstButtonReturn;
        if (R >= 0) and (R < Length(Delegate.Results)) then
          Result := Delegate.Results[R]
        else
          Result := mrCancel;
      end;
    finally
      if NSWin <> nil then
        NSWin.release;
      if (ActiveForm <> nil) and (FAlertCount = 0) then
        ActiveForm.Activate;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.MessageDialog(const AMessage: string; const ADialogType: TMsgDlgType;
  const AButtons: TMsgDlgButtons; const ADefaultButton: TMsgDlgBtn; const AX, AY: Integer; const AHelpCtx: THelpContext;
  const AHelpFileName: string; const ACloseDialogProc: TInputCloseDialogProc);
var
  LResult: TModalResult;
begin
  LResult := MessageDialog(AMessage, ADialogType, AButtons, ADefaultButton, AX, AY, AHelpCtx, AHelpFileName);
  if Assigned(ACloseDialogProc) then
    ACloseDialogProc(LResult);
end;

function TPlatformCocoa.InputQuery(const ACaption: string; const APrompts: array of string;
  var AValues: array of string; const ACloseQueryFunc: TInputCloseQueryFunc): Boolean;
const
  InputWidth = 400;
  InputHeight = 24;
  VerticalSpacing = 60;
  PromptRelativePosition = 36;

  function CreatePrompt(const YPos: Single; const Prompt: string; out Password: Boolean): NSTextField;
  begin
    Result := TNSTextField.Wrap(TNSTextField.Alloc.initWithFrame(MakeNSRect(0, YPos, InputWidth, InputHeight)));
    Result.setDrawsBackground(False);
    Result.setEditable(False);
    Result.setSelectable(False);
    Result.setBordered(False);
    Result.setBackgroundColor(TNSColor.Wrap(TNSColor.OCClass.clearColor));
    Password := (Prompt.Length > 0) and (Prompt.Chars[0] < #32);
    Result.setStringValue(StrToNSStr(Prompt.Substring(IfThen(Password, 1, 0))));
  end;

  function CreateInput(const YPos: Single; const InitialValue: string; const Password: Boolean): NSTextField;
  begin
    if Password then
      Result := TNSSecureTextField.Wrap(TNSSecureTextField.Alloc.initWithFrame(MakeNSRect(0, YPos, InputWidth,
        InputHeight)))
    else
      Result := TNSTextField.Wrap(TNSTextField.Alloc.initWithFrame(MakeNSRect(0, YPos, InputWidth, InputHeight)));
    Result.setStringValue(StrToNSStr(InitialValue));
  end;

var
  Alert: NSAlert;
  Delegate: TFMXAlertDelegate;
  Session: NSModalSession;
  NSWin: NSWindow;
  S: SEL;
  View: NSView;
  Inputs: array of NSTextField;
  I: Integer;
  ActiveForm: TCommonCustomForm;
  AutoReleasePool: NSAutoreleasePool;
  Password: Boolean;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    Result := False;
    if (Length(AValues) > 0) and (Length(APrompts) > 0) then
    begin
      ActiveForm := nil;
      Delegate := TFMXAlertDelegate.Create;
      try
        Delegate.Modal := True;
        SetLength(Delegate.Results, 2);
        Delegate.Results[0] := ModalResults[TMsgDlgBtn.mbOK];
        Delegate.Results[1] := ModalResults[TMsgDlgBtn.mbCancel];

        if Screen <> nil then
        begin
          ActiveForm := Screen.ActiveForm;
          if (ActiveForm <> nil) and (ActiveForm.Visible) and (ActiveForm.Handle <> nil) and
            not (ActiveForm.Owner is TPopup) then
          begin
            NSWin := WindowHandleToPlatform(ActiveForm.Handle).Wnd;
            if NSWin <> nil then
              NSWin.retain;
          end;
        end;

        S := sel_getUid('alertDidEndSelector:returnCode:contextInfo:');
        Alert := TNSAlert.Wrap(TNSAlert.Alloc.init);
        Alert.setMessageText(StrToNSStr(ACaption));

        Alert.addButtonWithTitle(StrToNSStr(LocalizedButtonCaption(TMsgDlgBtn.mbOK)));
        Alert.addButtonWithTitle(StrToNSStr(LocalizedButtonCaption(TMsgDlgBtn.mbCancel)));

        View := TNSView.Wrap(TNSView.Alloc.initWithFrame(MakeNSRect(0, 0, InputWidth, Length(AValues) * VerticalSpacing)));

        SetLength(Inputs, Length(AValues));
        for I := 0 to High(Inputs) do
        begin
          View.addSubview(CreatePrompt(View.frame.size.height - I * VerticalSpacing - PromptRelativePosition,
            APrompts[I], Password));
          Inputs[I] := CreateInput(View.frame.size.height - I * VerticalSpacing - VerticalSpacing, AValues[I], Password);
          View.addSubview(Inputs[I]);
        end;

        Alert.setAccessoryView(View);

        if NSWin <> nil then
        begin
          Alert.beginSheetModalForWindow(NSWin, Delegate.GetObjectID, S, nil);
          if TFmxFormState.Modal in ActiveForm.FormState then
            Session := nil
          else
            Session := NSApp.beginModalSessionForWindow(NSWin);
          Inc(FAlertCount);
          try
            while Delegate.Modal do
            begin
              if Session <> nil then
                NSApp.runModalSession(Session);
              HookObserverCallback(False);
            end;
          finally
            Dec(FAlertCount);
            if Session <> nil then
              NSApp.endModalSession(Session);
          end;
          Result := Delegate.Result = mrOk;
        end
        else
          Result := Alert.runModal = NSAlertFirstButtonReturn;

        for I := 0 to High(Inputs) do
          AValues[I] := NSStrToStr(Inputs[I].stringValue);
      finally
        if NSWin <> nil then
          NSWin.release;
        if (ActiveForm <> nil) and (FAlertCount = 0) then
          ActiveForm.Activate;
      end;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.AllocHandle(const Objc: IObjectiveC): TFmxHandle;
begin
  Result := NewFmxHandle;
  TMonitor.Enter(FObjectiveCMap);
  try
    FObjectiveCMap.Add(Result, Objc);
  finally
    TMonitor.Exit(FObjectiveCMap);
  end;
end;

procedure TPlatformCocoa.ApplyConstraints(const AWindow: NSWindow; const AMinSize: TSizeF; const AMaxSize: TSizeF);
const
{$IF defined(CPU32BITS)}
  FLT_MAX = MaxSingle;
{$ELSEIF defined(CPU64BITS)}
  FLT_MAX = MaxDouble;
{$ENDIF}
var
  MaxSize: NSSize;
begin
  if IsZero(AMaxSize.Width) then
    MaxSize.width := FLT_MAX
  else
    MaxSize.width := AMaxSize.Width;
  if IsZero(AMaxSize.Height) then
    MaxSize.height := FLT_MAX
  else
    MaxSize.height := AMaxSize.Height;

  AWindow.setMinSize(NSSize.Create(AMinSize));
  AWindow.setMaxSize(MaxSize);
end;

function TPlatformCocoa.NewFmxHandle: TFmxHandle;
begin
{$IF defined(CPU64BITS)}
  Result := TInterlocked.Add(Int64(FHandleCounter), 16);
{$ELSE}
  Result := TInterlocked.Add(Integer(FHandleCounter), 16);
{$ENDIF}
end;

procedure TPlatformCocoa.SetWindowCaption(const AForm: TCommonCustomForm; const ACaption: string);
var
  NSWin: NSWindow;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    NSWin.setTitle(StrToNSStr(ACaption));
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.SetWindowState(const AForm: TCommonCustomForm; const AState: TWindowState);
var
  NSWin: NSWindow;
  AutoReleasePool: NSAutoreleasePool;
begin
  if (AForm.Visible or (TFmxFormState.Showing in AForm.FormState)) and FCanSetState then
  begin
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
      if NSWin <> nil then
      begin
        if (NSWin.styleMask and NSFullScreenWindowMask) <> 0 then
          AForm.WindowState := TWindowState.wsMaximized
        else
          case AState of
            TWindowState.wsMinimized:
              if not NSWin.isMiniaturized then
                NSWin.miniaturize(nil);
            TWindowState.wsNormal:
              begin
                if NSWin.isMiniaturized then
                  NSWin.deminiaturize(nil);
                if IsZoomed(NSWin) then
                  NSWin.zoom(nil);
              end;
            TWindowState.wsMaximized:
              begin
                if NSWin.isMiniaturized then
                  NSWin.deminiaturize(nil);
                if not IsZoomed(NSWin) then
                  NSWin.zoom(nil);
              end;
          end;
      end;
    finally
      AutoReleasePool.release;
    end;
  end;
end;

procedure TPlatformCocoa.RegisterCanvasClasses;
begin
  if GlobalUseGPUCanvas or GlobalUseMetal then
    FMX.Canvas.GPU.RegisterCanvasClasses;
  FMX.Canvas.Mac.RegisterCanvasClasses;
end;

procedure TPlatformCocoa.UnregisterCanvasClasses;
begin
  if GlobalUseGPUCanvas or GlobalUseMetal then
    FMX.Canvas.GPU.UnregisterCanvasClasses;
  FMX.Canvas.Mac.UnregisterCanvasClasses;
end;

procedure TPlatformCocoa.RegisterContextClasses;
begin
  if GlobalUseMetal then
    FMX.Context.Metal.RegisterContextClasses
  else
    FMX.Context.Mac.RegisterContextClasses;
end;

procedure TPlatformCocoa.UnregisterContextClasses;
begin
  if GlobalUseMetal then
    FMX.Context.Metal.UnregisterContextClasses
  else
    FMX.Context.Mac.UnregisterContextClasses;
end;

procedure TPlatformCocoa.ReleaseCapture(const AForm: TCommonCustomForm);
begin
//  Windows.ReleaseCapture;
end;

procedure TPlatformCocoa.SetCapture(const AForm: TCommonCustomForm);
begin
//  Windows.SetCapture(AHandle);
end;

function TPlatformCocoa.GetClientSize(const AForm: TCommonCustomForm): TPointF;
var
  LView: NSView;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    LView := WindowHandleToPlatform(AForm.Handle).View;
    Result := PointF(LView.frame.size.width, LView.frame.size.height);
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.HideWindow(const AForm: TCommonCustomForm);
var
  NSWin: NSWindow;
  Handle: TMacWindowHandle;
begin
  if (AForm <> nil) and (AForm.Handle <> nil) then
  begin
    Handle := WindowHandleToPlatform(AForm.Handle);
    NSWin := Handle.Wnd;
    if NSWin <> nil then
    begin
      // Metal bug: When user close NSWindow via red close button, it lead to stop MTView rendering and doesn't restore
      // rendering after open NSWindow. The only solution is unlink MTLView from NSWindow before closing.
      if Handle.MTView <> nil then
        Handle.MTView.removeFromSuperview;

      if (NSWin.styleMask and NSFullScreenWindowMask) <> 0 then
      begin
        NSWin.setCollectionBehavior(NSWindowCollectionBehaviorFullScreenPrimary);
        NSWin.toggleFullScreen(nil);
      end
      else
        NSWin.orderOut(nil);
      NSWin.setParentWindow(nil);
    end;
  end;
end;

procedure TPlatformCocoa.ShowWindow(const AForm: TCommonCustomForm);
var
  NSWin: NSWindow;
  frame: NSRect;
  AutoReleasePool: NSAutoReleasePool;
  LWindowState: TWindowState;
  ParentForm: TCommonCustomForm;
  Handle: TMacWindowHandle;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    Handle := WindowHandleToPlatform(AForm.Handle);
    NSWin := Handle.Wnd;
    // Metal bug: see explanation in TPlatformCocoa.HideWindow.
    if Handle.MTView <> nil then
      TNSView.Wrap(NSWin.contentView).addSubview(Handle.MTView);
    LWindowState := AForm.WindowState;
    if LWindowState <> TWindowState.wsMinimized then
    begin
      SetWindowState(AForm, LWindowState);
      NSWin.makeKeyAndOrderFront(NSObjectToID(NSApp));
    end;
    if AForm = Application.MainForm then
    begin
      NSWin.makeMainWindow;
      NSApp.activateIgnoringOtherApps(True);
    end
    else if IsPopupForm(AForm) then
    begin
      ParentForm := AForm.ParentForm;
      while (ParentForm <> nil) and not (TFmxFormState.Modal in ParentForm.FormState) do
        ParentForm := ParentForm.ParentForm;
      if ParentForm <> nil then
        NSWin.setParentWindow(WindowHandleToPlatform(ParentForm.Handle).Wnd);
    end;
    SetWindowState(AForm, LWindowState);
    frame := NSWin.frame;
    AForm.SetBoundsF(frame.origin.x, MainScreenHeight - frame.origin.y - frame.size.height,
                     frame.size.width, frame.size.height);
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.BringToFront(const AForm: TCommonCustomForm);
var
  AutoReleasePool: NSAutoreleasePool;
  NSWin: NSWindow;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    if (NSWin <> nil) and NSWin.isVisible then
      NSWin.orderFront(nil);
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.SendToBack(const AForm: TCommonCustomForm);
var
  AutoReleasePool: NSAutoreleasePool;
  NSWin: NSWindow;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    if (NSWin <> nil) and NSWin.isVisible then
      NSWin.orderBack(nil);
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.Activate(const AForm: TCommonCustomForm);
var
  AutoReleasePool: NSAutoreleasePool;
  NSWin: NSWindow;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    NSApp.activateIgnoringOtherApps(True);
    NSWin.makeKeyAndOrderFront(NSObjectToID(NSApp));
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.ShowWindowModal(const AForm: TCommonCustomForm): TModalResult;
var
  Session: NSModalSession;
  MR: NSInteger;
  NSWin: NSWindow;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    AForm.HandleNeeded;
    if FModalStack = nil then
      FModalStack := TStack<TCommonCustomForm>.Create;
    FRestartModal := False;
    FModalStack.Push(AForm);
    AForm.Show;
    try
      // Result := mrNone;
      NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
      if NSWin <> nil then
        NSWin.retain;
      AForm.ModalResult := mrNone;
      Session := NSApp.beginModalSessionForWindow(NSWin);
      AForm.BringToFront;
      while True do
      begin
        MR := NSApp.runModalSession(Session);
        // Print and Save dialogs will cause a MR value of NSModalResponseCancel or NSModalResponseOK
        if (MR <> NSModalResponseCancel) and (MR <> NSModalResponseOK) and (MR <> NSRunContinuesResponse) then
        begin
          if FRestartModal then
          begin
            FRestartModal := False;
            NSApp.endModalSession(Session);
            Session := NSApp.beginModalSessionForWindow(NSWin);
            Continue;
          end;
          if AForm.Visible then
            AForm.Hide;
          Result := AForm.ModalResult;
          if csDestroying in AForm.ComponentState then
            DoReleaseWindow(AForm);
          FModalStack.Pop;
          if FModalStack.Count > 0 then
            FRestartModal := True;
          Break;
        end;
        if AForm.ModalResult <> 0 then
        begin
          AForm.CloseModal;
          if AForm.ModalResult <> 0 then
          begin
            NSApp.stopModal;
            if FModalStack.Count = 1 then
              FRestartModal := False;
            Continue;
          end;
        end;
        HookObserverCallback(False);
      end;
      NSApp.endModalSession(Session);
    finally
      if NSWin <> nil then
        NSWin.release;
      if (FModalStack.Count > 0) and (FModalStack.Peek = AForm) then
        FModalStack.Pop;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.CalculateVisibleSubitemsCount(const AMenu: IItemsContainer): Integer;
var
  I: Integer;
  Native: INativeControl;
  Item: TFmxObject;
begin
  Result := 0;
  if Supports(AMenu, INativeControl, Native) and Native.HandleSupported then
    for I := 0 to AMenu.GetItemsCount - 1 do
    begin
      Item := AMenu.GetItem(I);
      if (Item is TMenuItem) and TMenuItem(Item).Visible then
        Inc(Result);
    end;
end;

function TPlatformCocoa.CanShowModal: Boolean;
begin
  Result := True;
end;

function TPlatformCocoa.ClientToScreen(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
var
  np: NSPoint;
  NSWin: NSWindow;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    np := CGPointMake(Point.X, Point.Y);
    np.y := TNSView.Wrap(NSWin.contentView).bounds.size.height - np.y;
    Result := CGPointToPointF(NSWin.convertBaseToScreen(np));
    Result.y := MainScreenHeight - Result.y;
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.ScreenToClient(const AForm: TCommonCustomForm; const Point: TPointF): TPointF;
var
  np: NSPoint;
  NSWin: NSWindow;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    np := CGPointMake(Point.X, Point.Y);
    np.y := MainScreenHeight - np.y;
    Result := CGPointToPointF(NSWin.convertScreenToBase(np));
    Result.y := TNSView.Wrap(NSWin.contentView).bounds.size.height - Result.y;
  finally
    AutoReleasePool.release;
  end;
end;

{ Menus }

function TPlatformCocoa.FindFormAtScreenPos(var AForm: TCommonCustomForm; const ScreenMousePos: TPointF): Boolean;
var
  I: Integer;
  ScreenPos: TPoint;

  function Contains(const Form: TCommonCustomForm): Boolean;
  var
    R: TRect;
  begin
    Result := False;
    if (Form <> nil) and Form.Visible then
    begin
      if Form is TCustomPopupForm then
        R := TRect.Create(TCustomPopupForm(Form).ScreenContentRect.Round)
      else
        R := TRect.Create(TPoint.Create(Form.Left, Form.Top), Form.Width, Form.Height);
      Result := R.Contains(ScreenPos);
      if Result then
        AForm := Form;
    end;
  end;

begin
  Result := False;
  if Screen <> nil then
  begin
    ScreenPos := TPoint.Create(Round(RoundTo(ScreenMousePos.X, 0)), Round(RoundTo(ScreenMousePos.y, 0)));
    // Popups
    for I := Screen.PopupFormCount - 1 downto 0 do
      if Contains(Screen.PopupForms[I]) then
        Exit(True);
    // top forms
    for I := Screen.FormCount - 1 downto 0 do
      if (Screen.Forms[I].FormStyle = TFormStyle.StayOnTop) and Contains(Screen.Forms[I]) then
        Exit(True);
    // active form
    if Contains(Screen.ActiveForm) then
      Exit(True);
    // other forms
    for I := Screen.FormCount - 1 downto 0 do
      if (Screen.Forms[I].FormStyle <> TFormStyle.StayOnTop) and Contains(Screen.Forms[I]) then
        Exit(True);
  end;
end;

function TPlatformCocoa.ShowContextMenu(const AForm: TCommonCustomForm; const AScreenMousePos: TPointF): Boolean;
var
  Obj: IControl;
begin
  Result := False;
  if AForm <> nil then
  begin
    Obj := IControl(AForm.ObjectAtPoint(AScreenMousePos));
    if (Obj <> nil) and (Obj.GetObject <> nil) then
      try
        Obj.SetFocus;
        Result := Obj.ShowContextMenu(AScreenMousePos);
      except
        HandleException(AForm);
      end;
  end;
end;

procedure TPlatformCocoa.MenuLoopEvent(const EventRec: TEventRec; var CancelIdle, CancelDefaultAction: Boolean);

  procedure EndLoop;
  var
    LView: IMenuView;
  begin
    LView := FMenuStack.Peek;
    while LView <> nil do
    begin
      LView.Loop := False;
      LView.Selected := nil;
      LView := LView.ParentView;
    end;
  end;

  procedure EndLoopAndClose;
  var
    LView: IMenuView;
  begin
    EndLoop;
    LView := FMenuStack.Peek;
    while (LView <> nil) do
    begin
      if LView.Parent is TPopup then
        TPopup(LView.Parent).IsOpen := False;
      LView := LView.ParentView;
    end;
  end;

  function IsItemSelectable(const Item: TFmxObject): Boolean;
  begin
    Result := (Item is TMenuItem) and TMenuItem(Item).Visible and (TMenuItem(Item).Text <> SMenuSeparator);
  end;

  function ForwardSelectNextMenuItem(AView: IMenuView; AStartInd, AEndInd: Integer): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    if AView <> nil then
      for I := AStartInd to AEndInd do
        if IsItemSelectable(AView.GetItem(I)) then
        begin
          AView.Selected := TMenuItem(AView.GetItem(I));
          Result := True;
          Break;
        end;
  end;

  function BackwardSelectNextMenuItem(AView: IMenuView; AStartInd, AEndInd: Integer): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    if AView <> nil then
      for I := AStartInd downto AEndInd do
        if IsItemSelectable(AView.GetItem(I)) then
        begin
          AView.Selected := TMenuItem(AView.GetItem(I));
          Result := True;
          Break;
        end;
  end;

  procedure SelectFirstMenuItem(AView: IMenuView);
  var
    I: Integer;
  begin
    if AView <> nil then
    begin
      I := 0;
      while (I < AView.GetItemsCount) and not IsItemSelectable(AView.GetItem(I)) do
        Inc(I);
      if I < AView.GetItemsCount then
        AView.Selected := TMenuItem(AView.GetItem(I));
    end;
  end;

  procedure SelectLastMenuItem(AView: IMenuView);
  var
    I: Integer;
  begin
    if AView <> nil then
    begin
      I := AView.GetItemsCount - 1;
      while (I >= 0) and not IsItemSelectable(AView.GetItem(I)) do
        Dec(I);
      if I >= 0 then
        AView.Selected := TMenuItem(AView.GetItem(I));
    end;
  end;

  procedure SelectPrevMenuItem(AView: IMenuView);
  begin
    if AView <> nil then
      if (AView.Selected = nil) or
        not BackwardSelectNextMenuItem(AView, AView.Selected.Index - 1, 0) then
        SelectLastMenuItem(AView);
  end;

  procedure SelectNextMenuItem(AView: IMenuView);
  begin
    if AView <> nil then
      if (AView.Selected = nil) or
        not ForwardSelectNextMenuItem(AView, AView.Selected.Index + 1, AView.GetItemsCount - 1) then
        SelectFirstMenuItem(AView);
  end;

  function RedirectMouseEventToParentForm(AView: IMenuView): Boolean;
  var
    Root: IRoot;
    Form: TCommonCustomForm;
    FormMousePos: TPointF;
    LEventRec: TEventRec;
  begin
    Result := False;
    if (AView <> nil) and (AView.Parent <> nil) and (AView.GetObject <> nil) then
    begin
      Root := AView.GetObject.Root;
      if (Root <> nil) and (Root.GetObject is TCommonCustomForm) then
      begin
        Form := TCommonCustomForm(Root.GetObject);
        while (Form <> nil) and (IsPopup(Form)) do
          Form := Form.ParentForm;
        if (Form <> nil) and (Form.Handle <> nil) and (EventRec.EventKind = TEventKind.MouseDown) then
        begin
          FormMousePos := Form.ScreenToClient(EventRec.ScreenMousePos);
          LEventRec := EventRec;
          LEventRec.Form := Form;
          LEventRec.FormMousePos := FormMousePos;
          LEventRec.Event := nil;
          TMouseDownTimer.Create(LEventRec);
          Result := True;
        end;
      end;
    end;
  end;

  procedure FindViewByEvent(var AView: IMenuView; var Item: TMenuItem);
  var
    Obj: IControl;
  begin
    AView := nil;
    Obj := nil;
    Item := nil;
    repeat
      if AView = nil then
        AView := FMenuStack.Peek
      else
        AView := AView.ParentView;
      if AView <> nil then
      begin
        Obj := AView.ObjectAtPoint(EventRec.ScreenMousePos);
        if (Obj <> nil) and (Obj.GetObject is TMenuItem) then
          Item := TMenuItem(Obj.GetObject);
      end;
    until (AView = nil) or (Item <> nil);
  end;

  function ScreenToAbsolute(const AView: IMenuView; const APoint: TPointF): TPointF;
  var
    Root: IRoot;
    Scene: IScene;
  begin
    if (AView = nil) or (AView.GetObject = nil) then
      Exit(APoint);

    Root := AView.GetObject.Root;
    if Supports(Root, IScene, Scene) then
      Result := Scene.ScreenToLocal(EventRec.ScreenMousePos)
    else
      Result := APoint;
  end;

  function IsInPopupForm(const APoint: TPointF): Boolean;
  var
    View: IMenuView;
    Control: TControl;
    AbsoluteMousePos: TPointF;
  begin
    View := FMenuStack.Peek;
    if View.GetObject is TControl then
    begin
      Control := TControl(View.GetObject);
      AbsoluteMousePos := ScreenToAbsolute(View, APoint);
      Result := Control.PointInObject(AbsoluteMousePos.X, AbsoluteMousePos.Y);
    end
    else
      Result := False;
  end;

var
  LForm: TCommonCustomForm;
  LFormMousePos: TPointF;
  Key: Word;
  LView, LCurrentView: IMenuView;
  Item: TMenuItem;
begin
  if (FMenuStack <> nil) and (FMenuStack.Count > 0) then
  begin
    LView := FMenuStack.Peek;
    case EventRec.EventKind of
      TEventKind.Other:
        begin
          case EventRec.Event.&type of
            NSAppKitDefined:
              if EventRec.Event.Subtype <> NSWindowMovedEventType then
                EndLoopAndClose;
            NSSystemDefined:
              if not FindFormAtScreenPos(LForm, EventRec.ScreenMousePos) then
                EndLoopAndClose;
          end;
        end;
      TEventKind.Key:
        begin
          Key := EventRec.Event.keyCode;
          if EventRec.Event.&type = NSKeyDown then
            case Key of
              KEY_ENTER, KEY_NUMPADENTER:
                if (LView.Selected <> nil) then
                begin
                  if LView.Selected.HavePopup then
                    LView.Selected.NeedPopup
                  else if LView.Selected.CanBeClicked then
                  begin
                    TOpenMenuItem(LView.Selected).Click;
                    EndLoopAndClose;
                  end;
                end
                else
                  EndLoopAndClose;
              KEY_ESC:
                begin
                  LView.Selected := nil;
                  EndLoopAndClose;
                end;
              KEY_LEFT:
                if (LView.ParentView <> nil) and LView.ParentView.IsMenuBar then
                begin
                  LView.Loop := False;
                  SelectPrevMenuItem(LView.ParentView);
                  if LView.ParentView.Selected <> nil then
                    LView.ParentView.Selected.NeedPopup;
                end;
              KEY_RIGHT:
                if (LView.ParentView <> nil) and LView.ParentView.IsMenuBar then
                begin
                  LView.Loop := False;
                  SelectNextMenuItem(LView.ParentView);
                  if LView.ParentView.Selected <> nil then
                    LView.ParentView.Selected.NeedPopup;
                end;
              KEY_UP:
                SelectPrevMenuItem(LView);
              KEY_DOWN:
                SelectNextMenuItem(LView);
              KEY_HOME, KEY_PAGUP:
                SelectFirstMenuItem(LView);
              KEY_END, KEY_PAGDN:
                SelectLastMenuItem(LView);
            end;
          CancelDefaultAction := True;
        end;
      TEventKind.MouseDown:
        begin
          FindViewByEvent(LCurrentView, Item);
          if (Item <> nil) and (LCurrentView = FMenuStack.Peek) then
          begin
            if not Item.IsSelected and Item.HavePopup then
              Item.NeedPopup
            else if Item.CanBeClicked then
            begin
              TOpenMenuItem(Item).Click;
              EndLoopAndClose;
            end;
          end
          else if (LCurrentView = nil) and not IsInPopupForm(EventRec.ScreenMousePos)then
          begin
            RedirectMouseEventToParentForm(LView);
            EndLoopAndClose;
            CancelDefaultAction := True;
          end
          else
            CancelDefaultAction := True;
        end;
      TEventKind.MouseMove:
        if (LView.GetObject <> nil) and (LView.GetObject.Root is TCommonCustomForm) then
        begin
          LForm := TCommonCustomForm(LView.GetObject.Root);
          LFormMousePos := LForm.ScreenToClient(EventRec.ScreenMousePos);
          try
            LForm.MouseMove(EventRec.Shift, LFormMousePos.X, LFormMousePos.y);
          except
            HandleException(LForm);
          end;
          CancelDefaultAction := True;
          CancelIdle := True;
        end;
    end;
  end;
end;

procedure TPlatformCocoa.StartMenuLoop(const AView: IMenuView);
var
  AutoReleasePool: NSAutoreleasePool;
begin
  if (AView <> nil) and (AView.GetObject <> nil) then
  begin
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      if FMenuStack = nil then
        FMenuStack := TStack<IMenuView>.Create;
      FMenuStack.Push(AView);
      try
        HookEvent := MenuLoopEvent;
        AView.Loop := True;
        try
          while AView.Loop do
            HookObserverCallback(False);
        finally
          AView.Loop := False;
        end;
      finally
        FMenuStack.Pop;
        if FMenuStack.Count = 0 then
        begin
          HookEvent := nil;
          FreeAndNil(FMenuStack);
        end;
      end;
    finally
      AutoReleasePool.release;
    end;
  end;
end;

procedure TPlatformCocoa.ShortCutToKey(ShortCut: TShortCut; var Key: Word; var Shift: TShiftState);
begin
  FMX.Helpers.Mac.ShortCutToKey(ShortCut, Key, Shift);
end;

function TPlatformCocoa.ShortCutToText(ShortCut: TShortCut): string;
begin
  Result := FMX.Helpers.Mac.ShortCutToText(ShortCut);
end;

function TPlatformCocoa.TextToShortCut(Text: string): Integer;
begin
  Result := FMX.Helpers.Mac.TextToShortCut(Text);
end;

{ TFMXOSMenuItem }

type
  FMXOSMenuItem = interface(NSMenuItem)
    ['{A922028A-C1EE-41AF-8345-26671E6879AD}']
    procedure DispatchMenuClick(Sender: Pointer); cdecl;
  end;

  TFMXOSMenuItem = class(TOCLocal)
  private
    FMXMenuItem: TMenuItem;
  public
    constructor Create(const AFMXMenuItem: TMenuItem);
    destructor Destroy; override;
    function GetObjectiveCClass: PTypeInfo; override;
    procedure DispatchMenuClick(Sender: Pointer); cdecl;
  end;

constructor TFMXOSMenuItem.Create(const AFMXMenuItem: TMenuItem);
var
  Key: Char;
  ModMask: NSUInteger;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    inherited Create;
    FMXMenuItem := AFMXMenuItem;
    ShortCutToMACKey(FMXMenuItem.ShortCut, Key, ModMask);
    UpdateObjectID(NSMenuItem(Super).initWithTitle(StrToNSStr(DelAmp(FMXMenuItem.Text)),
      sel_getUid(MarshaledAString('DispatchMenuClick:')), StrToNSStr(Key)));

    if AFMXMenuItem.IsChecked then
      NSMenuItem(Super).setState(NSOnState);

    if AFMXMenuItem.Enabled then
      NSMenuItem(Super).setEnabled(True)
    else
      NSMenuItem(Super).setEnabled(False);

    SetMenuBitmap(NSMenuItem(Super), AFMXMenuItem);

    NSMenuItem(Super).setKeyEquivalentModifierMask(ModMask);
    NSMenuItem(Super).setTarget(GetObjectID);
    TFMXMenuDelegate.RegisterMenu(Self.GetObjectID, AFMXMenuItem);
  finally
    AutoReleasePool.Release;
  end;
end;

destructor TFMXOSMenuItem.Destroy;
begin
  TFMXMenuDelegate.UnregisterMenu(Self.GetObjectID);
  NSMenuItem(Super).Release;
  inherited;
end;

procedure TFMXOSMenuItem.DispatchMenuClick(Sender: Pointer);
begin
  try
    if ((Now - Application.LastKeyPress) > IntervalPress) and
       (FMXMenuItem is TMenuItem) then
      TOpenMenuItem(FMXMenuItem).Click;
  except
    HandleException(Self);
  end;
end;

function TFMXOSMenuItem.GetObjectiveCClass: PTypeInfo;
begin
    Result := TypeInfo(FMXOSMenuItem);
end;

procedure TPlatformCocoa.CreateMenu;
var
  LWindowMenuItem: NSMenuItem;
begin
  FMainMenu := TNSMenu.Create;
  FMenuBar := TNSMenu.Wrap(FMainMenu.initWithTitle(StrToNSStr(string.Empty)));
  FMenuBar.setAutoenablesItems(False);
  NSApp.setMainMenu(FMenuBar);
  CreateAppleMenu;
  LWindowMenuItem := FMenuBar.addItemWithTitle(StrToNSStr(SMenuWindow), nil, StrToNSStr(string.Empty));
  FWindowMenu := TNSMenu.Create;
  FWindowMenu := TNSMenu.Wrap(FWindowMenu.initWithTitle(StrToNSStr(SMenuWindow)));
  FMenuBar.setSubmenu(FWindowMenu, LWindowMenuItem);
  NSApp.setWindowsMenu(FWindowMenu);
end;

procedure TPlatformCocoa.CreateAppleMenu;
var
  LAppleMenuItem: NSMenuItem;
begin
  LAppleMenuItem := FMenuBar.addItemWithTitle(StrToNSStr('Apple'), nil, StrToNSStr(''));
  FAppleMenu := TNSMenu.Create;
  FAppleMenu := TNSMenu.Wrap(FAppleMenu.initWithTitle(StrToNSStr('Apple')));
  FAppleMenu.setDelegate(FFMXMenuDelegate);
  FMenuBar.setSubmenu(FAppleMenu, LAppleMenuItem);
  UpdateAppleMenu(nil);
end;

procedure TPlatformCocoa.UpdateAppleMenu(const AMenuItem: TMenuItem);
var
  LMenu: NSMenu;
  LMenuItem: NSMenuItem;
  LKey: Char;
  LModifierMask: NSUInteger;
  I: Integer;
begin
  if AMenuItem <> nil then
  begin
    for I := FAppleMenu.numberOfItems - 1 downto 0 do
      FAppleMenu.removeItemAtIndex(I);
    CreateChildMenuItems(AMenuItem, FAppleMenu);
    FAppleMenu.addItem(TNSMenuItem.Wrap(TNSMenuItem.OCClass.separatorItem));
    TFMXMenuDelegate.RegisterMenu(NSObjectToId(FAppleMenu), AMenuItem);
  end
  else
    TFMXMenuDelegate.UnregisterMenu(NSObjectToId(FAppleMenu));

  LMenuItem := FAppleMenu.addItemWithTitle(StrToNSStr(SMenuServices), nil, StrToNSStr(''));
  if AMenuItem <> nil then
    TOpenMenuItem(AMenuItem).Handle := AllocHandle(FAppleMenu);
  LMenu := TNSMenu.Create;
  LMenu := TNSMenu.Wrap(LMenu.initWithTitle(StrToNSStr(SMenuServices)));
  FAppleMenu.setSubmenu(LMenu, LMenuItem);
  NSApp.setServicesMenu(LMenu);
  FAppleMenu.addItem(TNSMenuItem.Wrap(TNSMenuItem.OCClass.separatorItem));
  { Hide }
  LMenuItem := FAppleMenu.addItemWithTitle(StrToNSStr(Format(SMenuAppHide, [Application.Title])), sel_getUid(MarshaledAString('hide:')), StrToNSStr('h'));
  LMenuItem.setTarget(NSObjectToID(NSApp));
  { Hide Other }
  LMenuItem := FAppleMenu.addItemWithTitle(StrToNSStr(SMenuAppHideOthers), sel_getUid(MarshaledAString('hideOtherApplications:')), StrToNSStr('h'));
  LMenuItem.setKeyEquivalentModifierMask(NSCommandKeyMask or NSAlternateKeyMask);
  LMenuItem.setTarget(NSObjectToID(NSApp));
  { Show all }
  LMenuItem := FAppleMenu.addItemWithTitle(StrToNSStr(SMenuShowAll), sel_getUid(MarshaledAString('unhideAllApplications:')), StrToNSStr(''));
  LMenuItem.setTarget(NSObjectToID(NSApp));
  FAppleMenu.addItem(TNSMenuItem.Wrap(TNSMenuItem.OCClass.separatorItem));
  ShortCutToMACKey(CommandQShortCut, LKey, LModifierMask);
  { Terminate }
  LMenuItem := FAppleMenu.addItemWithTitle(StrToNSStr(Format(SMenuAppQuit, [Application.Title])), sel_getUid(MarshaledAString('terminate:')), 
    StrToNSStr(LKey));
  LMenuItem.setKeyEquivalentModifierMask(LModifierMask);
  LMenuItem.setTarget(NSObjectToID(NSApp));
end;

procedure TPlatformCocoa.CreateChildMenuItems(const AChildMenu: IItemsContainer; const AParentMenu: NSMenu);
var
  J: Integer;
  LChildMenuItem: TOpenMenuItem;
  LNSSubMenuItem: TFMXOSMenuItem;
  LNewSubMenu: NSMenu;
  Items: IItemsContainer;
  VisibleItemCount: Integer;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    AParentMenu.setAutoenablesItems(False);
    for J := 0 to AChildMenu.GetItemsCount - 1 do
      if AChildMenu.GetItem(J) is TMenuItem then
      begin
        LChildMenuItem := TOpenMenuItem(AChildMenu.GetItem(J));
        if LChildMenuItem.Visible then
        begin
          if LChildMenuItem.Text = SMenuSeparator then
            AParentMenu.addItem(TNSMenuItem.Wrap(TNSMenuItem.OCClass.separatorItem))
          else
          begin
            LNSSubMenuItem := TFMXOSMenuItem.Create(LChildMenuItem);
            LChildMenuItem.Handle := AllocHandle(LNSSubMenuItem);
            if Supports(AChildMenu.GetItem(J), IItemsContainer, Items) then
            begin
              VisibleItemCount := CalculateVisibleSubitemsCount(Items);
              if VisibleItemCount > 0 then
              begin
                LNewSubMenu := NewNsMenu(TMenuItem(AChildMenu.GetItem(J)).Text);
                TFMXMenuDelegate.RegisterMenu(NSObjectToID(LNewSubMenu), TMenuItem(AChildMenu.GetItem(J)));
                CreateChildMenuItems(Items, LNewSubMenu);
                NSMenuItem(LNSSubMenuItem.Super).setSubmenu(LNewSubMenu);
              end;
            end;
            AParentMenu.addItem(NSMenuItem(LNSSubMenuItem.Super));
          end;
        end;
      end;
  finally
    AutoReleasePool.Release;
  end;
end;

procedure TPlatformCocoa.RemoveChildHandles(const AMenu: IItemsContainer);
var
  I: Integer;
  MenuItem: TMenuItem;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    for I := 0 to AMenu.GetItemsCount - 1 do
      if AMenu.GetItem(I) is TMenuItem then
      begin
        MenuItem := TMenuItem(AMenu.GetItem(I));
        RemoveChildHandles(MenuItem);
        DestroyMenuItem(MenuItem);
      end;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.ClearNSHandles;
var
  I: Integer;
begin
  for I := FNSHandles.Count - 1 downto 0 do
    DeleteHandle(FNSHandles[I]);
  FNSHandles.Clear;
end;

function TPlatformCocoa.NewNSMenu(const Text: string): NSMenu;
var
  LNSMenu: NSMenu;
  S: NSString;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    inherited;
    LNSMenu := TNSMenu.Alloc;
    S := StrToNSStr(DelAmp(Text));
    Result := TNSMenu.Wrap(LNSMenu.initWithTitle(S));
    FNSHandles.Add(AllocHandle(Result));
    Result.setDelegate(FFMXMenuDelegate);
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.ClearMenuItems;
var
  I, LWindowItemIndex, LAppleMenuIndex: NSInteger;
begin
  LWindowItemIndex := FMenuBar.indexOfItemWithSubmenu(FWindowMenu);
  LAppleMenuIndex := FMenuBar.indexOfItemWithSubmenu(FAppleMenu);
  for I := FMenuBar.numberOfItems - 1 downto 0 do
    // Clear all but the Window and "Apple" menus
    if (I <> LWindowItemIndex) and (I <> LAppleMenuIndex) then
      FMenuBar.removeItemAtIndex(I);
end;

procedure TPlatformCocoa.CreateOSMenu(AForm: TCommonCustomForm; const AMenu: IItemsContainer);
var
  LNSMenuItem: NSMenuItem;
  LNewMenu: NSMenu;
  MenuItem, FirstMenuItem: TOpenMenuItem;
  I, J, VisibleItemCount: Integer;
  AutoReleasePool: NSAutoreleasePool;
  OldState: TMainMenuState;
  LWindowMenuIndex: NSInteger;
begin
  if FMainMenuState in [TMainMenuState.Empty, TMainMenuState.Recreated] then
  begin
    OldState := FMainMenuState;
    try
      FMainMenuState := TMainMenuState.Recreating;
      AutoReleasePool := TNSAutoreleasePool.Create;
      try
        ClearMenuItems;
        ClearNSHandles;
        VisibleItemCount := 0;
        if AMenu <> nil then
        begin
          RemoveChildHandles(AMenu);
          VisibleItemCount := CalculateVisibleSubitemsCount(AMenu);
        end;
        // TMainMenu items
        if VisibleItemCount > 0 then
        begin
          J := 0;
          FirstMenuItem := nil;
          for I := 0 to AMenu.GetItemsCount - 1 do
          begin
            if not (AMenu.GetItem(I) is TMenuItem) then
              Continue;

            MenuItem := TOpenMenuItem(AMenu.GetItem(I));
            if not MenuItem.Visible then
              Continue;

            if FirstMenuItem = nil then
            begin
              UpdateAppleMenu(MenuItem);
              FirstMenuItem := MenuItem;
            end
            else
            begin
              FDefaultMenu := TCurrentMenu.Main;
              if J = VisibleItemCount - 1 then
                LNewMenu := NewNSMenu(SSpotlightFeature)
              else
                LNewMenu := NewNSMenu('');

              LNSMenuItem := TNSMenuItem.Alloc;
              LNSMenuItem := TNSMenuItem.Wrap(LNSMenuItem.initWithTitle(StrToNSStr(''), nil, StrToNSStr('')));
              LNSMenuItem.setSubmenu(LNewMenu);
              SetMenuBitmap(LNSMenuItem, MenuItem);
              TFMXMenuDelegate.RegisterMenu(NSObjectToID(LNewMenu), MenuItem);
              TFMXMenuDelegate.RegisterMenu(NSObjectToID(LNSMenuItem), MenuItem);

              LNSMenuItem.setEnabled(MenuItem.Enabled);

              if MenuItem.IsChecked then
                LNSMenuItem.setState(NSOnState);

              if OldState > TMainMenuState.Empty then
                CreateChildMenuItems((MenuItem as IItemsContainer), LNewMenu);
              MenuItem.Handle := AllocHandle(LNSMenuItem);
              LWindowMenuIndex := FMenuBar.indexOfItemWithSubmenu(FWindowMenu);
              if MenuItem.Text.Equals(SSpotlightFeature) or (LWindowMenuIndex = -1) then
                FMenuBar.addItem(LNSMenuItem)
              else
                FMenuBar.insertItem(LNSMenuItem, LWindowMenuIndex);
              Inc(J);
            end;
          end;
          for I := 0 to AMenu.GetItemsCount - 1 do
            if AMenu.GetItem(I) <> FirstMenuItem then
              UpdateMenuItem(AMenu.GetItem(I) as IItemsContainer, [TMenuItemChange.Text]);
        end
        else
          UpdateAppleMenu(nil);
      finally
        AutoReleasePool.release;
      end;
    finally
      if OldState < TMainMenuState.Recreated then
        FMainMenuState := Succ(OldState)
      else
        FMainMenuState := OldState;
    end;
  end;
end;

procedure TPlatformCocoa.UpdateMenuBar;
begin
  ClearMenuItems;
end;

procedure TPlatformCocoa.UpdateMenuItem(const AItem: IItemsContainer; AChange: TMenuItemChanges);
var
  I: Integer;
  LMenuItem: TMenuItem;
  LHandle: TFmxHandle;
  LNativeMenuItem: TFMXOSMenuItem;
  LNSMenuItem: NSMenuItem;
  LNSMenu: NSMenu;
  IsTopLevelItem, IsFirstVisibleItem: Boolean;
  Key: Char;
  ModMask: NSUInteger;
  AutoReleasePool: NSAutoreleasePool;
  IObj: IObjectiveC;
  Parent: TFmxObject;
  MainMenu: TMainMenu;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    if AItem.GetObject is TMenuItem then
      LMenuItem := TMenuItem(AItem.GetObject)
    else
      Exit;

    LHandle := (LMenuItem as INativeControl).Handle;
    // Invisible item does not have a handle
    if (LHandle = 0) then
    begin
      // If you changed the visibility, then re-creates all the menus
      if (TMenuItemChange.Visible in AChange) and LMenuItem.Visible then
      begin
        Parent := LMenuItem;
        while (Parent is TFmxObject) and not (Parent is TMainMenu) do
          Parent := Parent.Parent;
        if Parent is TMainMenu then
          TMainMenu(Parent).RecreateOSMenu;
      end;
      Exit;
    end;
    // If you changed the visibility of top level menu, then re-creates all menu
    IsFirstVisibleItem := False;
    IsTopLevelItem := LMenuItem.Parent is TMainMenu;
    if IsTopLevelItem then
      MainMenu := TMainMenu(LMenuItem.Parent)
    else
      MainMenu := nil;
    if IsTopLevelItem and (TMenuItemChange.Visible in AChange) then
    begin
      MainMenu.RecreateOSMenu;
      Exit;
    end;
    // else update this menu item
    IObj := HandleToObjC(LHandle);
    if IObj <> nil then
    begin
      if not Supports(IObj, NSMenuItem, LNSMenuItem) then
        // IObj can be NSMenu (FAppleMenu)
        if Supports(IObj, NSMenu, LNSMenu) then
          LNSMenuItem := LNSMenu.itemWithTitle(StrToNSStr(SMenuServices))
        else
        begin
          LNativeMenuItem := IObj as TFMXOSMenuItem;
          LNSMenuItem := LNativeMenuItem.Super as NSMenuItem;
        end;
      // Determine whether the menu item is the first visible and change text
      if IsTopLevelItem then
        for I := 0 to MainMenu.ItemsCount - 1 do
          if (MainMenu.Items[I] is TMenuItem) and TMenuItem(MainMenu.Items[I]).Visible then
          begin
            IsFirstVisibleItem := MainMenu.Items[I] = LMenuItem;
            Break;
          end;
      if (not IsFirstVisibleItem) and ([TMenuItemChange.Text, TMenuItemChange.Bitmap] * AChange <> []) then
      begin
        if LNSMenuItem.submenu <> nil then
          LNSMenuItem.submenu.setTitle(StrToNSStr(DelAmp(LMenuItem.Text)));
        LNSMenuItem.setTitle(StrToNSStr(DelAmp(LMenuItem.Text)));
        SetMenuBitmap(LNSMenuItem, LMenuItem);
      end;
      // Change other params
      if (TMenuItemChange.ShortCut in AChange) then
      begin
        ShortCutToMACKey(LMenuItem.ShortCut, Key, ModMask);
        LNSMenuItem.setKeyEquivalent(StrToNSStr(Key));
        LNSMenuItem.setKeyEquivalentModifierMask(ModMask);
      end;
      if TMenuItemChange.Enabled in AChange then
        LNSMenuItem.setEnabled(LMenuItem.Enabled);
      if TMenuItemChange.Visible in AChange then
        LNSMenuItem.setHidden(not LMenuItem.Visible);
      if TMenuItemChange.Checked in AChange then
      begin
        if LMenuItem.IsChecked then
          LNSMenuItem.setState(NSOnState)
        else
          LNSMenuItem.setState(NSOffState);
      end;
    end
    else
      raise EInvalidFmxHandle.CreateFMT(SInvalidFmxHandle, [HexDisplayPrefix, SizeOf(LHandle) * 2, LHandle]);
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.DestroyMenuItem(const AItem: IItemsContainer);

  function IsAppleMenuItem(const AHandle: TFmxHandle): Boolean;
  begin
    Result := (HandleToObjC(AHandle) as ILocalObject).GetObjectId = NSObjectToId(FAppleMenu);
  end;

var
  P: TFmxObject;
  Native: INativeControl;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    if not FInDestroyMenuItem and (AItem <> nil) and (AItem.GetObject is TFmxObject) then
      P := AItem.GetObject
    else
      Exit;
    FInDestroyMenuItem := True;
    try
      if Supports(P, INativeControl, Native) and (Native.Handle <> 0) then
      begin
        if IsAppleMenuItem(Native.Handle) then
          Exit;

        if (P.Root <> nil) and (P.Root.GetObject is TCommonCustomForm) then
          CreateOSMenu(TCommonCustomForm(P.Root.GetObject), nil);
        DeleteHandle(Native.Handle);
        Native.Handle := 0;
      end;
    finally
      FInDestroyMenuItem := False;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.ValidateHandle(FmxHandle: TFmxHandle);
begin
  if (FmxHandle and $F <> 0) then
    raise EInvalidFmxHandle.CreateResFmt(@SInvalidFmxHandle, [HexDisplayPrefix, SizeOf(TFmxHandle) * 2, FmxHandle]);
end;

{ Drag and Drop ===============================================================}

procedure TPlatformCocoa.BeginDragDrop(AForm: TCommonCustomForm; const Data: TDragObject; ABitmap: TBitmap);

  function AdjustOffset(const MousePos: NSPoint; const Width, Height: Single): NSPoint;
  var
    Coord: TPointF;
  begin
    if Data.Source is TControl then
    begin
      // bottom left point of control at form
      Coord := TControl(Data.Source).LocalToAbsolute(TPointF.Create(0, TControl(Data.Source).Height));
      // distance between bottom bound of control and bottom bound of form
      Coord.Y := TNSView.Wrap(WindowHandleToPlatform(AForm.Handle).Wnd.contentView).bounds.size.height - Coord.y;
    end
    else
      Coord := MousePos.ToPointF - TPointF.Create(Width / 2, Height / 2);
    Result := CGPointMake(Coord.X, Coord.Y);
  end;

var
  Img: NSImage;
  Scale: Single;
  Pasteboard: NSPasteboard;
  PboardTypes: NSArray;
  FMXPBoardTypePtrs: array[0..1] of Pointer;
  FMXPBoardTypePtrCount: Integer;
  FMXWin: TFMXWindow;
  AutoReleasePool: NSAutoreleasePool;
  Control: IControl;
  PNGStream: TMemoryStream;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    FMXWin := TFMXWindow(WindowHandleToPlatform(AForm.Handle).Handle);
    if FMXWin.LastEvent <> nil then
    begin
      Img := BitmapToMacBitmap(ABitmap);

      Scale := GetWindowScale(AForm);
      if Scale > 1 then
        Img.setSize(CGSizeMake(ABitmap.Width / Scale, ABitmap.Height / Scale));

      Pasteboard := TNSPasteboard.Wrap(TNSPasteboard.OCClass.pasteboardWithName(NSDragPboard));
      FMXPBoardTypePtrs[0] := NSObjectToID(NSFMXPBoardType);
      FMXPBoardTypePtrCount := 1;
      if Data.Data.IsType<string> then
      begin
        FMXPBoardTypePtrs[FMXPBoardTypePtrCount] := NSObjectToID(NSPasteboardTypeString);
        Inc(FMXPBoardTypePtrCount);
      end
      else if Data.Data.IsType<TBitmap> or Data.Data.IsType<TBitmapSurface> then
      begin
        FMXPBoardTypePtrs[FMXPBoardTypePtrCount] := NSObjectToID(NSPasteboardTypePNG);
        Inc(FMXPBoardTypePtrCount);
      end;
      PboardTypes := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@FMXPBoardTypePtrs, FMXPBoardTypePtrCount));

      Pasteboard.declareTypes(PBoardTypes, NSObjectToID(Pasteboard));
      if FMXPBoardTypePtrCount = 2 then
        if Data.Data.IsType<string> then
          Pasteboard.setString(StrToNSStr(Data.Data.ToString), NSPasteboardTypeString)
        else if Data.Data.IsType<TBitmap> or Data.Data.IsType<TBitmapSurface> then
        begin
          PNGStream := TMemoryStream.Create;
          try
            if Data.Data.IsType<TBitmap> then
              //Saving to stream without converting it to PNG SaveToStream doing it
              Data.Data.AsType<TBitmap>.SaveToStream(PNGStream)
            else
              TBitmapCodecManager.SaveToStream(PNGStream, Data.Data.AsType<TBitmapSurface>, SPNGImageExtension);
            Pasteboard.setData(TNSData.Wrap(TNSData.OCClass.dataWithBytes(PNGStream.Memory, pngStream.size)), NSPasteboardTypePNG);
          finally
            PNGStream.Free;
          end;
        end;
      GlobalData := Data;

      FMXWin.DragOperation := NSDragOperationNone;
      NSWindow(FMXWin.Super).dragImage(Img,
        AdjustOffset(FMXWin.LastEvent.locationInWindow, Img.size.width, Img.size.height),
        CGSizeMake(0, 0), FMXWin.LastEvent, Pasteboard, NSObjectToID(FMXWin.View), True);
      if (FMXWin.DragOperation = NSDragOperationNone) and Supports(Data.Source, IControl, Control) then
        Control.DragEnd;

      FMXWin.LastEvent := nil;
    end;
  finally
    if Img <> nil then
      Img.release;
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.SetClientSize(const AForm: TCommonCustomForm; const ASize: TPointF);
var
  NSWin: NSWindow;
  OldFrame, Frame, R: NSRect;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    if NSWin.isVisible then
    begin
      OldFrame := NSWin.frame;
      R.origin := OldFrame.origin;
      R.size := CGSizeMake(ASize.X, ASize.Y);
      Frame := NSWin.frameRectForContentRect(R);
      Frame.origin.x := OldFrame.origin.x;
      Frame.origin.y := OldFrame.origin.y + OldFrame.size.height - Frame.size.height;
      NSWin.setFrame(Frame, True);
    end
    else
      NSWin.setContentSize(CGSizeMake(ASize.X, ASize.Y));
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.SetConstraints(const AForm: TCommonCustomForm; const AMinWidth, AMinHeight, AMaxWidth, AMaxHeight: Single);
var
  Handle: TMacWindowHandle;
begin
  if (AForm = nil) or not AForm.IsHandleAllocated then
    // We will set constraints in handle constructor later.
    Exit;

  Handle := WindowHandleToPlatform(AForm.Handle);
  ApplyConstraints(Handle.Wnd, TSizeF.Create(AMinWidth, AMinHeight), TSizeF.Create(AMaxWidth, AMaxHeight));
end;

procedure TPlatformCocoa.SetCursor(const ACursor: TCursor);
const
  SizeNWSECursor: array [0..192] of byte = (
    $89, $50, $4E, $47, $0D, $0A, $1A, $0A, $00, $00, $00, $0D, $49, $48, $44, $52, $00, $00, $00, $10, $00, $00, $00, $10, $08, $06, $00, $00, $00, $1F, $F3, $FF, $61, $00, $00, $00, $88, $49, $44, $41,
    $54, $78, $9C, $AC, $93, $4B, $0A, $C0, $20, $0C, $44, $45, $8A, $69, $D7, $5D, $7B, $00, $0F, $98, $EB, $6B, $15, $8C, $44, $F1, $1B, $3A, $20, $BA, $D0, $E7, $4C, $A2, $4A, $FD, $A1, $30, $D1, $36,
    $20, $4D, $69, $00, $40, $59, $8B, $00, $FC, $B0, $08, $60, $8C, $A9, $6E, $BF, $A2, $44, $0E, $08, $82, $88, $EA, $8D, $DA, $02, $78, $EF, $43, $0B, $63, $31, $EE, $29, $80, $67, $26, $88, $D6, $BA,
    $82, $58, $6B, $97, $69, $CA, $A6, $91, $93, $AD, $16, $3F, $51, $23, $48, $8A, $D9, $44, $EB, $8B, $AA, $3F, $2B, $F0, $3A, $4F, $16, $41, $A8, $C5, $47, $00, $96, $F7, $DC, $81, $73, $AE, $FB, $C8,
    $44, $0E, $C4, $1F, $6D, $A5, $0F, $00, $00, $FF, $FF, $03, $00, $FD, $DF, $FC, $72, $CD, $04, $2F, $27, $00, $00, $00, $00, $49, $45, $4E, $44, $AE, $42, $60, $82
  );
  SizeNESWCursor: array [0..211] of byte = (
    $89, $50, $4E, $47, $0D, $0A, $1A, $0A, $00, $00, $00, $0D, $49, $48, $44, $52, $00, $00, $00, $10, $00, $00, $00, $10, $08, $06, $00, $00, $00, $1F, $F3, $FF, $61, $00, $00, $00, $9B, $49, $44, $41,
    $54, $78, $9C, $9C, $93, $51, $0E, $C0, $10, $0C, $86, $3D, $88, $CC, $F3, $0E, $E3, $2A, $2E, $E2, $04, $6E, $E0, $C5, $5D, $DC, $4D, $4C, $93, $CD, $1A, $46, $AD, $7F, $D2, $14, $49, $3F, $D5, $96,
    $10, $0B, $95, $52, $48, $23, $55, $D6, $DA, $03, $80, $EB, $ED, $17, $20, $E7, $CC, $06, $1C, $29, $A5, $96, $85, $52, $AA, $79, $12, $A0, $AB, $62, $8C, $BC, $27, $9C, $55, $21, $84, $21, $18, $45,
    $CD, $01, $52, $4A, $E1, $9C, $FB, $0C, $F6, $DE, $F7, $5D, $79, $0B, $85, $4F, $26, $37, $C3, $42, $0E, $33, $70, $6F, $86, $14, $B7, $AB, $8D, $01, $5F, $85, $32, $C6, $C0, $42, $93, $00, $DC, $A2,
    $27, $D8, $5A, $0B, $DD, $58, $8F, $EC, $2C, $03, $18, $1E, $54, $13, $FE, $13, $B6, $01, $33, $ED, $02, $78, $5F, $B5, $EA, $02, $00, $00, $FF, $FF, $03, $00, $27, $CE, $7B, $C4, $F5, $A4, $B6, $D6,
    $00, $00, $00, $00, $49, $45, $4E, $44, $AE, $42, $60, $82
  );
  SizeAllCursor: array [0..174] of byte = (
    $89, $50, $4E, $47, $0D, $0A, $1A, $0A, $00, $00, $00, $0D, $49, $48, $44, $52, $00, $00, $00, $10, $00, $00, $00, $10, $08, $06, $00, $00, $00, $1F, $F3, $FF, $61, $00, $00, $00, $09, $70, $48, $59,
    $73, $00, $00, $0B, $13, $00, $00, $0B, $13, $01, $00, $9A, $9C, $18, $00, $00, $00, $61, $49, $44, $41, $54, $78, $9C, $AC, $53, $CB, $0A, $00, $20, $0C, $1A, $F4, $FF, $DF, $6C, $10, $74, $68, $0F,
    $17, $65, $E0, $A9, $74, $BA, $36, $03, $60, $04, $FB, $94, $6F, $28, $D9, $6C, $2C, $30, $91, $96, $DC, $89, $5C, $91, $99, $48, $95, $19, $49, $84, $E3, $2A, $13, $F0, $55, $B2, $CA, $C1, $49, $D5,
    $B0, $D2, $81, $17, $A5, $99, $3B, $04, $AB, $AF, $02, $DF, $11, $24, $4D, $94, $7C, $A3, $64, $90, $24, $A3, $2C, $59, $A6, $EB, $75, $9E, $00, $00, $00, $FF, $FF, $03, $00, $3A, $00, $A6, $5B, $CC,
    $0B, $A4, $58, $00, $00, $00, $00, $49, $45, $4E, $44, $AE, $42, $60, $82
  );
  WaitCursor: array [0..124] of byte = (
    $89, $50, $4E, $47, $0D, $0A, $1A, $0A, $00, $00, $00, $0D, $49, $48, $44, $52, $00, $00, $00, $10, $00, $00, $00, $10, $08, $06, $00, $00, $00, $1F, $F3, $FF, $61, $00, $00, $00, $44, $49, $44, $41,
    $54, $78, $9C, $62, $60, $C0, $0E, $FE, $E3, $C0, $44, $83, $21, $6E, $C0, $7F, $5C, $80, $18, $43, $70, $6A, $26, $D6, $10, $BA, $19, $80, $D3, $10, $6C, $0A, $C9, $33, $00, $59, $03, $45, $5E, $C0,
    $65, $00, $94, $4D, $5A, $38, $10, $B2, $1D, $C5, $10, $1C, $98, $68, $30, $84, $0C, $00, $00, $00, $00, $FF, $FF, $03, $00, $A9, $31, $25, $E9, $C0, $2C, $FB, $9B, $00, $00, $00, $00, $49, $45, $4E,
    $44, $AE, $42, $60, $82
  );
  HelpCursor: array [0..238] of byte = (
    $89, $50, $4E, $47, $0D, $0A, $1A, $0A, $00, $00, $00, $0D, $49, $48, $44, $52, $00, $00, $00, $12, $00, $00, $00, $12, $08, $06, $00, $00, $00, $56, $CE, $8E, $57, $00, $00, $00, $B6, $49, $44, $41,
    $54, $78, $9C, $A4, $94, $3B, $12, $80, $20, $0C, $44, $69, $6C, $6D, $6C, $BC, $83, $8D, $B5, $F7, $E0, $FE, $37, $01, $89, $93, $8C, $61, $F9, $18, $21, $33, $19, $15, $C9, $73, $B3, $46, $9D, $83,
    $88, $31, $52, $36, $03, $F7, $17, $C5, $1A, $E2, $BD, $0F, $74, $89, $49, $EB, $9F, $30, $06, $05, $81, $70, $51, $D0, $6B, $66, $18, $15, $49, $01, $9F, $9F, $29, $77, $BD, $CE, $F7, $E8, $B8, $98,
    $40, $1A, $D6, $00, $ED, $05, $4C, $79, $94, $B5, $C1, $80, $0B, $40, $D2, $1A, $A9, $5D, $BB, $AA, $30, $1B, $1E, $5D, $29, $B7, $AE, $57, $FC, $A4, $23, $ED, $CF, $D4, $00, $A4, $AF, $08, $D5, $C1,
    $5B, $FC, $0F, $11, $D0, $34, $44, $83, $A6, $20, $4E, $08, $EF, $A7, $61, $32, $B7, $0A, $A9, $F8, $53, $CE, $8E, $05, $E4, $CA, $21, $1C, $F2, $A7, $A6, $68, $BC, $3D, $F0, $28, $53, $64, $F9, $11,
    $48, $3C, $83, $59, $83, $FC, $8D, $85, $8B, $B7, $2F, $C8, $0D, $00, $00, $FF, $FF, $03, $00, $A5, $D1, $28, $C9, $B0, $25, $E3, $01, $00, $00, $00, $00, $49, $45, $4E, $44, $AE, $42, $60, $82
  );
var
  C: NSCursor;
  AutoReleasePool: NSAutoreleasePool;
  NewCustomCursor: TCustomCursor;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NewCustomCursor := nil;
    case ACursor of
      crCross: C := TNSCursor.Wrap(TNSCursor.OCClass.crosshairCursor);
      crArrow, crDefault: C := TNSCursor.Wrap(TNSCursor.OCClass.arrowCursor);
      crIBeam: C := TNSCursor.Wrap(TNSCursor.OCClass.IBeamCursor);
      crSizeNS: C := TNSCursor.Wrap(TNSCursor.OCClass.resizeUpDownCursor);
      crSizeWE: C := TNSCursor.Wrap(TNSCursor.OCClass.resizeLeftRightCursor);
      crUpArrow: C := TNSCursor.Wrap(TNSCursor.OCClass.resizeUpCursor);
      crDrag, crMultiDrag:  C := TNSCursor.Wrap(TNSCursor.OCClass.dragCopyCursor);
      crHSplit: C := TNSCursor.Wrap(TNSCursor.OCClass.resizeLeftRightCursor);
      crVSplit: C := TNSCursor.Wrap(TNSCursor.OCClass.resizeUpDownCursor);
      crNoDrop, crNo: C := TNSCursor.Wrap(TNSCursor.OCClass.operationNotAllowedCursor);
      crHandPoint: C := TNSCursor.Wrap(TNSCursor.OCClass.pointingHandCursor);
      crAppStart, crSQLWait, crHourGlass: NewCustomCursor := TCustomCursor.Create(@WaitCursor[0], Length(WaitCursor));
      crHelp:  NewCustomCursor := TCustomCursor.Create(@HelpCursor[0], Length(HelpCursor));
      crSizeNWSE: NewCustomCursor := TCustomCursor.Create(@SizeNWSECursor[0], Length(SizeNWSECursor));
      crSizeNESW: NewCustomCursor := TCustomCursor.Create(@SizeNESWCursor[0], Length(SizeNESWCursor));
      crSizeAll: NewCustomCursor := TCustomCursor.Create(@SizeAllCursor[0], Length(SizeAllCursor));
    else
      C := TNSCursor.Wrap(TNSCursor.OCClass.arrowCursor);
    end;
    if ACursor = crNone then
      TNSCursor.OCClass.setHiddenUntilMouseMoves(True)
    else
    begin
      TNSCursor.OCClass.setHiddenUntilMouseMoves(False);
      // Remove old custom cursor
      if FCustomCursor <> nil then
        FreeAndNil(FCustomCursor);
      // Set new custom cursor
      if NewCustomCursor <> nil then
      begin
        FCustomCursor := NewCustomCursor;
        C := FCustomCursor.Cursor;
      end;
      C.&set;
    end;
    FCursor := ACursor;
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.GetCursor: TCursor;
begin
  Result := FCursor;
end;

procedure TPlatformCocoa.SetFullScreen(const AForm: TCommonCustomForm; const AValue: Boolean);
var
  NSWin: NSWindow;
  AutoReleasePool: NSAutoreleasePool;
begin
  if AValue and not (TFmxFormState.Showing in AForm.FormState) then
    AForm.Visible := True;
  if AForm.Visible or (TFmxFormState.Showing in AForm.FormState) then
  begin
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
      NSWin.setCollectionBehavior(NSWindowCollectionBehaviorFullScreenPrimary);
      try
        if not (AValue = ((NSWin.styleMask and NSFullScreenWindowMask) > 0)) then
          NSWin.toggleFullScreen(nil);
      finally
        SetShowFullScreenIcon(AForm, AForm.ShowFullScreenIcon);
      end;
    finally
      AutoReleasePool.release;
    end;
  end;
end;

{ Mouse  ===============================================================}

function TPlatformCocoa.GetMousePos: TPointF;
var
  P: NSPoint;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    P := TNSEvent.OCClass.mouseLocation;
    Result := P.ToPointF;
    Result.y := MainScreenHeight - Result.y;
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.GetSystemColor(const AType: TSystemColorType): TAlphaColor;
begin
  if TOSVersion.Check(10, 14) then
    Result := NSColorToAlphaColor(TNSColor.Wrap(TNSColor.OCClass.controlAccentColor))
  else
    Result := TAlphaColorRec.Null;
end;

function TPlatformCocoa.GetSystemThemeKind: TSystemThemeKind;
begin
  if TOSVersion.Check(10, 14) then
  begin
    if NSAppearanceNameDarkAqua.compare(NSApp.effectiveAppearance.name) = NSOrderedSame then
      Result := TSystemThemeKind.Dark
    else
      Result := TSystemThemeKind.Light;
  end
  else
    Result := TSystemThemeKind.Unspecified;
end;

{ IFMXScreenService }

{ International ===============================================================}

function TPlatformCocoa.GetFullScreen(const AForm: TCommonCustomForm): Boolean;
var
  NSWin: NSWindow;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    NSWin := WindowHandleToPlatform(AForm.Handle).Wnd;
    Result := (NSWin.styleMask and NSFullScreenWindowMask) > 0;
  finally
    AutoReleasePool.release;
  end;
end;

{ Dialogs ===============================================================}

function TPlatformCocoa.DialogOpenFiles(const ADialog: TOpenDialog; var AFiles: TStrings; AType: TDialogType): Boolean;
var
  OpenFile: NSOpenPanel;
  DefaultExt: string;
  Filter: NSArray;
  outcome: NSInteger;
  I: Integer;
  AutoReleasePool: NSAutoreleasePool;

  function AllocFilterStr(const S: string; var Filter: NSArray): Boolean;
  var
    input, pattern: string;
    FileTypes: array of string;
    outcome, aux: TArray<string>;
    i, j: Integer;
    FileTypesNS: array of Pointer;
    NStr: NSString;
  begin
    // First, split the string by using '|' as a separator
    Result := False;
    input := S;
    pattern := '\|';

    outcome := TRegEx.Split(input, pattern);

    pattern := '\*\.';
    SetLength(FileTypes, 0);

    for i := 0 to length(outcome) - 1 do
    begin
      if Odd(i) then
        if outcome[i] <> '*.*' then
          if AnsiLeftStr(outcome[i], 2) = '*.' then
          begin
            aux := TRegEx.Split(outcome[i], pattern);
            for j := 0 to length(aux) - 1 do
            begin
              aux[j] := Trim(aux[j]);
              if aux[j] <> '' then
              begin
                if AnsiEndsStr(';', aux[j]) then
                  aux[j] := AnsiLeftStr(aux[j], length(aux[j]) - 1);
                SetLength(FileTypes, length(FileTypes) + 1);
                FileTypes[length(FileTypes) - 1] := aux[j];
              end;
            end;
          end;
    end;

    // create the NSArray from the FileTypes array
    SetLength(FileTypesNS, length(FileTypes));
    for i := 0 to length(FileTypes) - 1 do
    begin
      NStr := StrToNSStr(FileTypes[i]);
      FileTypesNS[i] := NSObjectToID(NStr);
    end;
    if length(FileTypes) > 0 then
    begin
      Filter := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@FileTypesNS[0], length(FileTypes)));
      Result := True;
    end;
  end;

begin
  Result := False;
  DefaultExt := ADialog.DefaultExt;

  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    OpenFile := TNSOpenPanel.Wrap(TNSOpenPanel.OCClass.openPanel);

    OpenFile.setAllowsMultipleSelection(TOpenOption.ofAllowMultiSelect in ADialog.Options);
    OpenFile.setCanChooseFiles(AType <> TDialogType.Directory);
    OpenFile.setCanChooseDirectories(AType = TDialogType.Directory);
    OpenFile.setCanCreateDirectories(True);

    AFiles.Clear;

    if ADialog.InitialDir <> '' then
      OpenFile.setDirectoryURL(TNSUrl.Wrap(TNSUrl.OCCLass.fileURLWithPath(StrToNSStr(ADialog.InitialDir))));

    if ADialog.FileName <> '' then
      OpenFile.setNameFieldStringValue(StrToNSStr(ADialog.FileName));

    if ADialog.Filter <> '' then
    begin
      if AllocFilterStr(ADialog.Filter, Filter) then
      begin
        OpenFile.setAllowedFileTypes(Filter);
        OpenFile.setAllowsOtherFileTypes(False);
      end;
    end;

    if ADialog.Title <> '' then
      OpenFile.setTitle(StrToNSStr(ADialog.Title));

    OpenFile.retain;
    try
      outcome := OpenFile.runModal;
      if (FModalStack <> nil) and (FModalStack.Count > 0) then
        FRestartModal := True;
      if outcome = NSOKButton then
      begin
        for I := 0 to OpenFile.URLs.count - 1 do
          AFiles.Add(NSStrToStr(TNSUrl.Wrap(OpenFile.URLs.objectAtIndex(I)).relativePath));

        if AFiles.Count > 0 then
          ADialog.FileName := AFiles[0];

        if DefaultExt <> '' then
          if ExtractFileExt(ADialog.FileName) = '' then
            ChangeFileExt(ADialog.FileName, DefaultExt);
        Result := True;
      end;
    finally
      OpenFile.release;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.DialogPrint(var ACollate, APrintToFile: Boolean;
      var AFromPage, AToPage, ACopies: Integer; AMinPage, AMaxPage: Integer; var APrintRange: TPrintRange;
      AOptions: TPrintDialogOptions): Boolean;
var
  printPanel: NSPrintPanel;
  printInfo: NSPrintInfo;
  outcome : NSInteger;
  dict: NSMutableDictionary;
  AutoReleasePool: NSAutoreleasePool;
begin
  Result := False;

  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    printInfo := TNSPrintInfo.Wrap(TNSPrintInfo.OCClass.sharedPrintInfo);
    printPanel := TNSPrintPanel.Wrap(TNSPrintPanel.OCClass.printPanel);
    dict := printInfo.dictionary;

    dict.setValue(TNSNumber.OCClass.numberWithBool(ACollate), NSPrintMustCollate);
    dict.setValue(TNSNumber.OCClass.numberWithInt(AFromPage), NSPrintFirstpage);
    dict.setValue(TNSNumber.OCClass.numberWithInt(AToPage), NSPrintLastPage);
    dict.setValue(TNSNumber.OCClass.numberWithInt(ACopies), NSPrintCopies);
    if APrintrange = TPrintRange.prAllPages then
      dict.setValue(TNSNumber.OCClass.numberWithBool(True), NSPrintAllPages);
    if TPrintDialogOption.poPrintToFile in AOptions then
      printInfo.setJobDisposition(NSPrintSaveJob);

    printPanel.retain;
    try
      outcome := printPanel.runModalWithPrintInfo(printInfo);
      if outcome = NSOKButton then
      begin
        ACollate := TNSNumber.Wrap(printInfo.dictionary.valueForKey(NSPrintMustCollate)).boolValue();
        ACopies := TNSNumber.Wrap(printInfo.dictionary.valueForKey(NSPrintCopies)).integerValue();
        if printInfo.jobDisposition = NSPrintSaveJob then
          APrintToFile := True;
        if TNSNumber.Wrap(printInfo.dictionary.valueForKey(NSPrintAllPages)).boolValue() = True then
          APrintRange := TPrintRange.prAllPages
        else
        begin
          APrintRange := TPrintRange.prPageNums;
          AFromPage := TNSNumber.Wrap(printInfo.dictionary.valueForKey(NSPrintFirstpage)).integerValue();
          AToPage := TNSNumber.Wrap(printInfo.dictionary.valueForKey(NSPrintLastPage)).integerValue();
        end;
        Result := True;
      end;
    finally
      printPanel.release;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.PageSetupGetDefaults(var AMargin, AMinMargin: TRect; var APaperSize: TPointF; AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
begin
  Result := False;
end;

function TPlatformCocoa.DialogPageSetup(var AMargin, AMinMargin :TRect; var APaperSize: TPointF;
  var AUnits: TPageMeasureUnits; AOptions: TPageSetupDialogOptions): Boolean;
const
  POINTS_PER_INCHES = 72;
  MM_PER_INCH = 25.4;
var
  pageSetup: NSPageLayout;
  printInfo: NSPrintInfo;
  outcome: NSInteger;
  AutoReleasePool: NSAutoreleasePool;
  PaperSize: NSSize;

  function ToPoints(Value: Single): Single;
  begin
    Result := Value /1000;
    Result := Result * POINTS_PER_INCHES;
    if AUnits = TPageMeasureUnits.pmMillimeters then
    begin
      Result := Result / MM_PER_INCH;
    end;
  end;

  function FromPoints(Value: Single): Single;
  begin
    Result := Value * 1000;
    Result := Result / POINTS_PER_INCHES;
    if AUnits = TPageMeasureUnits.pmMillimeters then
    begin
      Result := Result * MM_PER_INCH;
    end;
  end;

begin
  Result := False;

  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    printInfo := TNSPrintInfo.Wrap(TNSPrintInfo.OCClass.sharedPrintInfo);
    pageSetup := TNSPageLayout.Wrap(TNSPageLayout.OCClass.pageLayout);

    //Calculate paper size for MAC side
    PaperSize := CGSizeMake(ToPoints(APaperSize.X), ToPoints(APaperSize.Y));
    printInfo.setPaperSize(PaperSize);

    //If psoMargins is set, use the margins specified by the user,
    //else, let the panel use the defaults.
    if TPageSetupDialogOption.psoMargins in AOptions then
    begin
      printInfo.setLeftMargin(ToPoints(AMargin.Left));
      printInfo.setTopMargin(ToPoints(AMargin.Top));
      printInfo.setRightMargin(ToPoints(AMargin.Right));
      printInfo.setBottomMargin(ToPoints(AMargin.Bottom));
    end;

    printInfo.setHorizontallyCentered(False);
    printInfo.setVerticallyCentered(False);

    pageSetup.retain;
    try
      outcome := pageSetup.runModalWithPrintInfo(printInfo);
      if outcome = NSOKButton then
      begin
        APaperSize := CGSizeToSizeF(printInfo.paperSize);
        //transfrom from points into inches
        APaperSize.X := FromPoints(APaperSize.X);
        APaperSize.y := FromPoints(APaperSize.Y);

        // Set the margins to the values from the dialog.
        AMargin.Left := round(FromPoints(printInfo.LeftMargin));
        AMargin.Top := round(FromPoints(printInfo.TopMargin));
        AMargin.Right := round(FromPoints(printInfo.RightMargin));
        AMargin.Bottom := round(FromPoints(printInfo.BottomMargin));

        //if psoMinMargins is set in options, then adjust the margins to fit
        if TPageSetupDialogOption.psoMinMargins in AOptions then
        begin
          if AMargin.Left < AMinMargin.Left then
            AMargin.Left := AMinMargin.Left;
          if AMargin.Top < AMinMargin.Top then
            AMargin.Top := AMinMargin.Top;
          if AMargin.Right < AMinMargin.Right then
            AMargin.Right := AMinMargin.Right;
          if AMargin.Bottom < AMinMargin.Bottom then
            AMargin.Bottom := AMinMargin.Bottom;
        end;
        Result := True;
      end;
    finally
      pageSetup.release;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

function TPlatformCocoa.DialogPrinterSetup: Boolean;
begin
  Result := False;
end;

function TPlatformCocoa.DialogSaveFiles(const ADialog: TOpenDialog; var AFiles: TStrings): Boolean;
var
  SaveFile: NSSavePanel;
  DefaultExt: string;
  Filter: NSArray;
  outcome : NSInteger;
  FileName: TFileName;
  AutoReleasePool: NSAutoreleasePool;

  function AllocFilterStr(const AInput: string; var AFilter: NSArray): Boolean;
  var
    LFileTypes, LExtMasks, LFilters: TArray<string>;
    LFileTypePointers: TArray<Pointer>;
    LExt, LFileType: string;
    LExists: Boolean;
    I, J: Integer;
  begin
    Result := False;
    LFilters := TRegEx.Split(AInput, '\|');
    for I := 0 to Length(LFilters) - 1 do
    begin
      if Odd(I) and (LFilters[I] <> '.') and (AnsiLeftStr(LFilters[I], 2) = '*.') then
      begin
        LExtMasks := TRegEx.Split(LFilters[I], '\*\.');
        for J := 0 to Length(LExtMasks) - 1 do
        begin
          LExt := LExtMasks[J].Trim;
          if LExt <> '' then
          begin
            if AnsiEndsStr(';', LExt) then
              LExt := AnsiLeftStr(LExt, Length(LExt) - 1);
            LExists := False;
            for LFileType in LFileTypes do
            begin
              if LFileType.Equals(LExt) then
              begin
                LExists := True;
                Break;
              end;
            end;
            if not LExists then
            begin
              SetLength(LFileTypes, Length(LFileTypes) + 1);
              if SameText(LExt, ADialog.DefaultExt) then // NSSavePanel takes 1st one as default
              begin
                LFileTypes[High(LFileTypes)] := LFileTypes[0];
                LFileTypes[0] := LExt;
              end
              else
                LFileTypes[High(LFileTypes)] := LExt;
            end;
          end;
        end;
      end;
    end;
    SetLength(LFileTypePointers, Length(LFileTypes));
    for I := 0 to Length(LFileTypes) - 1 do
      LFileTypePointers[I] := StringToID(LFileTypes[I]);
    if Length(LFileTypes) > 0 then
    begin
      AFilter := TNSArray.Wrap(TNSArray.OCClass.arrayWithObjects(@LFileTypePointers[0], Length(LFileTypePointers)));
      Result := True;
    end;
  end;

begin
  Result := False;

  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    SaveFile := TNSSavePanel.Wrap(TNSSavePanel.OCClass.savePanel);

    if ADialog.InitialDir <> '' then
      SaveFile.setDirectoryURL(TNSUrl.Wrap(TNSUrl.OCCLass.fileURLWithPath(StrToNSStr(ADialog.InitialDir))));

    if ADialog.FileName <> '' then
      SaveFile.setNameFieldStringValue(StrToNSStr(ADialog.FileName));

    if ADialog.Filter <> '' then
    begin
      if AllocFilterStr(ADialog.Filter, Filter) then
        SaveFile.setAllowedFileTypes(Filter);
    end;

    if ADialog.Title <> '' then
      SaveFile.setTitle(StrToNSStr(ADialog.Title));

    SaveFile.retain;
    try
      outcome := SaveFile.runModal;
      if (FModalStack <> nil) and (FModalStack.Count > 0) then
        FRestartModal := True;
      if outcome = NSOKButton then
      begin
        FileName := NSStrToStr(SaveFile.URL.relativePath);
        if DefaultExt <> '' then
          if ExtractFileExt(FileName) = '' then
            ChangeFileExt(FileName, DefaultExt);
        ADialog.FileName := Filename;
        Result := True;
      end;
    finally
      SaveFile.release;
    end;
  finally
    AutoReleasePool.release;
  end;
end;

procedure TPlatformCocoa.DeleteHandle(FmxHandle: TFmxHandle);
var
  IObj: IObjectiveC;
  Item: NSMenuItem;
  Menu: NSMenu;
  AutoReleasePool: NSAutoreleasePool;
begin
  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    ValidateHandle(FmxHandle);
    TMonitor.Enter(FObjectiveCMap);
    try
      if FObjectiveCMap.TryGetValue(FmxHandle, IObj) then
      begin
        if IObj.QueryInterface(NSMenuItem, Item) = 0 then
        begin
          TFMXMenuDelegate.UnregisterMenu(NSObjectToId(Item));
          Item.release;
          Item := nil;
        end;
        if IObj.QueryInterface(NSMenu, Menu) = 0 then
        begin
          TFMXMenuDelegate.UnregisterMenu(NSObjectToId(Menu));
          Menu.release;
          Menu := nil;
        end;
        FObjectiveCMap.Remove(FmxHandle);
        IObj := nil;
      end;
    finally
      TMonitor.Exit(FObjectiveCMap);
    end;
  finally
    AutoReleasePool.release;
  end;
end;

{ TMacWindowHandle }

function WindowHandleToPlatform(const AHandle: TWindowHandle): TMacWindowHandle;
begin
  Result := TMacWindowHandle(AHandle);
end;

constructor TMacWindowHandle.Create(const AHandle: TOCLocal);
var
  AutoReleasePool: NSAutoreleasePool;
  boundRect: NSRect;
  Options: NSTrackingAreaOptions;
begin
  inherited Create;
  FHandle := AHandle;
  if IsPopup(Form) then
  begin
    AutoReleasePool := TNSAutoreleasePool.Create;
    try
      boundRect := MainScreenFrame;
      Options := NSTrackingMouseMoved or NSTrackingActiveAlways or NSTrackingAssumeInside or NSTrackingMouseEnteredAndExited;
      FTrackingArea := TNSTrackingArea.Wrap(TNSTrackingArea.Alloc.initWithRect(boundRect, Options, View.superview, nil));
      View.addTrackingArea(FTrackingArea);
    finally
      AutoReleasePool.release;
    end;
  end;
  if FWindowHandles = nil then
    FWindowHandles := TList<TMacWindowHandle>.Create;
  FWindowHandles.Add(Self);
end;

destructor TMacWindowHandle.Destroy;
begin
  if FTrackingArea <> nil then
  begin
    View.removeTrackingArea(FTrackingArea);
    FTrackingArea.release;
    FTrackingArea := nil;
  end;
  FWindowHandles.Remove(Self);
  if FWindowHandles.Count = 0 then
    FreeAndNil(FWindowHandles);
  FreeBuffer;
  TFMXWindow(FHandle).DisposeOf;
  inherited;
end;

function TMacWindowHandle.GetForm: TCommonCustomForm;
begin
  Result := TFMXWindow(FHandle).Wnd;
end;

function TMacWindowHandle.GetView: NSView;
begin
  Result := TFMXWindow(FHandle).View;
end;

function TMacWindowHandle.GetMTView: MTKView;
begin
  if (GlobalUseMetal) and (TWindowStyle.GPUSurface in Form.WindowStyle) then
    Result := MTKView(TFMXWindow(FHandle).View)
  else
    Result := nil;
end;

function TMacWindowHandle.GetWindow: NSWindow;
var
  LSuper: IObjectiveCInstance;
  LNSWindow: NSWindow;
begin
  LSuper := TFMXWindow(FHandle).Super;
  if LSuper.QueryInterface(NSWindow, LNSWindow) = S_OK then
    Result := LNSWindow
  else
    Result := nil;
end;

function TMacWindowHandle.GetScale: Single;
begin
// Modified to fix Sonomo display scaling problem
   if TOSVersion.Check(14) and not GlobalUseMetal then
    Result := 1
  else
    Result := Wnd.backingScaleFactor;
end;

procedure TMacWindowHandle.CreateBuffer;
var
  ColorSpace: CGColorSpaceRef;
begin
  FBufferSize := TSize.Create(Form.Width, Form.Height);
  GetMem(FBits, FBufferSize.Width * FBufferSize.Height * 4);
  ColorSpace := CGColorSpaceCreateDeviceRGB;
  FBuffer := CGBitmapContextCreate(FBits, FBufferSize.Width, FBufferSize.Height, 8, FBufferSize.Width * 4, ColorSpace, kCGImageAlphaPremultipliedLast);
  CGColorSpaceRelease(ColorSpace);
end;

procedure TMacWindowHandle.FreeBuffer;
begin
  if FBits <> nil then
  begin
    CGContextRelease(FBuffer);
    FreeMem(FBits);
    FBits := nil;
    FBufferSize := TSize.Create(0, 0);
  end;
end;

procedure TMacWindowHandle.UpdateLayer(const Ctx: CGContextRef);
var
  Img: CGImageRef;
  R: CGRect;
  ContextObject: IContextObject;
begin
  if Supports(Form, IContextObject, ContextObject) and (ContextObject.Context <> nil) then
  begin
    if FBits = nil then
      CreateBuffer
    else if (FBufferSize.Width <> Form.Width) or (FBufferSize.Height <> Form.Height) then
    begin
      FreeBuffer;
      CreateBuffer;
    end;
    { Copy from Context }
    ContextObject.Context.CopyToBits(FBits, FBufferSize.Width * 4, Rect(0, 0, FBufferSize.Width, FBufferSize.Height));
    { Draw }
    R := CGRectMake(0, 0, FBufferSize.Width, FBufferSize.Height);
    CGContextClearRect(Ctx, R);
    Img := CGBitmapContextCreateImage(FBuffer);
    CGContextDrawImage(Ctx, R, Img);
    CGImageRelease(Img);
  end;
end;

class function TMacWindowHandle.FindForm(const window: NSWindow): TCommonCustomForm;
var
  I: Integer;
  AutoReleasePool: NSAutoreleasePool;
begin
  if FWindowHandles = nil then
    Exit(nil);

  AutoReleasePool := TNSAutoreleasePool.Create;
  try
    Result := nil;
    for I := 0 to FWindowHandles.Count - 1 do
      if NSObjectToID(FWindowHandles[I].Wnd) = NSObjectToID(window) then
      begin
        Result := FWindowHandles[I].Form;
        Break;
      end;
  finally
    AutoReleasePool.release;
  end;
end;

{ TCursorInfo }

constructor TCustomCursor.Create(const ABytes: Pointer; const ALength: NSUInteger);
begin
  FData := TNSData.Wrap(TNSData.Alloc.initWithBytes(ABytes, ALength));
  FImage := TNSImage.Wrap(TNSImage.Alloc.initWithData(FData));
  FCursor := TNSCursor.Wrap(TNSCursor.Alloc.initWithImage(FImage, CGPointMake(10, 10)));
end;

destructor TCustomCursor.Destroy;
begin
  FCursor.release;
  FImage.release;
  FData.release;
end;

{ TFMXMenuDelegate }

function TFMXMenuDelegate.confinementRectForMenu(menu: NSMenu; onScreen: NSScreen): NSRect;
begin
  Result.origin.x := 0;
  Result.origin.y := 0;
  Result.size.width := 0;
  Result.size.height := 0;
end;

function TFMXMenuDelegate.menu(menu: NSMenu; updateItem: NSMenuItem; atIndex: NSInteger;
  shouldCancel: Boolean): Boolean;
begin
  Result := True;
end;

class function TFMXMenuDelegate.GetMenuItem(const ALocalId: Pointer): TMenuItem;
begin
  if FFMXMenuDictionary <> nil then
    FFMXMenuDictionary.TryGetValue(ALocalId, Result)
  else
    Result := nil;
end;

procedure TFMXMenuDelegate.menu(menu: NSMenu; willHighlightItem: NSMenuItem);
var
  LMenu: TMenuItem;
begin
  if (FFMXMenuDictionary <> nil) and FFMXMenuDictionary.TryGetValue(NSObjectToID(willHighlightItem), LMenu) then
    Application.Hint := LMenu.Hint
  else
    Application.Hint := string.Empty;
end;

procedure TFMXMenuDelegate.menuDidClose(menu: NSMenu);
begin
  Application.Hint := string.Empty;
end;

function TFMXMenuDelegate.menuHasKeyEquivalent(menu: NSMenu; forEvent: NSEvent; target: Pointer; action: SEL): Boolean;
begin
  Result := False;
end;

procedure TFMXMenuDelegate.menuNeedsUpdate(menu: NSMenu);
begin
end;

procedure TFMXMenuDelegate.menuWillOpen(menu: NSMenu);

  function HasVisibleSubitems(const AMenuItem: TMenuItem): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to AMenuItem.ItemsCount - 1 do
      if AMenuItem.Items[I].Visible then
        Exit(True);
  end;

var
  LMenu: TMenuItem;
begin
  if (FFMXMenuDictionary <> nil) and FFMXMenuDictionary.TryGetValue(NSObjectToID(menu), LMenu) then
  begin
    Application.Hint := LMenu.Hint;
    if HasVisibleSubitems(LMenu) then
      TOpenMenuItem(LMenu).Click;
  end
  else
    Application.Hint := string.Empty;
end;

function TFMXMenuDelegate.numberOfItemsInMenu(menu: NSMenu): NSInteger;
begin
  Result := -1;
end;

class procedure TFMXMenuDelegate.RegisterMenu(const ALocalId: Pointer; const AMenuItem: TMenuItem);
begin
  if FFMXMenuDictionary = nil then
    FFMXMenuDictionary := TDictionary<Pointer, TMenuItem>.Create;

  FFMXMenuDictionary.AddOrSetValue(ALocalId, AMenuItem);
end;

class procedure TFMXMenuDelegate.UnregisterMenu(const AMenuItem: TMenuItem);
var
  Items: TArray<TPair<Pointer, TMenuItem>>;
  I: Integer;
begin
  if FFMXMenuDictionary = nil then
    Exit;

  Items := FFMXMenuDictionary.ToArray;
  for I := Low(Items) to High(Items) do
    if Items[I].Value = AMenuItem then
      FFMXMenuDictionary.Remove(Items[I].Key);
end;

class procedure TFMXMenuDelegate.UnregisterMenu(const ALocalId: Pointer);
begin
  if FFMXMenuDictionary <> nil then
  begin
    FFMXMenuDictionary.Remove(ALocalId);
    if FFMXMenuDictionary.Count = 0 then
      FreeAndNil(FFMXMenuDictionary);
  end;
end;

function PlatformHookObserverCallback(CancelIdle: Boolean; Mask: NSUInteger = NSAnyEventMask): Boolean;
begin
  Result := PlatformCocoa.HookObserverCallback(CancelIdle, Mask);
end;

procedure PlatformAlertCreated;
begin
  PlatformCocoa.AlertCreated;
end;

procedure PlatformAlertReleased;
begin
  PlatformCocoa.AlertReleased;
end;

function PlatformAlertCount: Integer;
begin
  Result := PlatformCocoa.AlertCount;
end;

{ TMacAppearanceChangedObserver }

function TMacAppearanceChangedObserver.GetObjectiveCClass: PTypeInfo;
begin
  Result := TypeInfo(IMacAppearanceChangedObserver);
end;

procedure TMacAppearanceChangedObserver.observeValueForKeyPath(keypath, anObject, aChange, aContext: pointer);
var
  Message: TSystemAppearanceChangedMessage;
begin
  Message := TSystemAppearanceChangedMessage.Create(TSystemAppearance.Create, True);
  TMessageManager.DefaultManager.SendMessage(nil, Message, True);
end;

end.
